# ==============================================================
# Photosynthesis / Respiration processing
# EXACT column names - NO clean_names, NO renaming
# Match respirometry filenames to master$`fragment.ID.full`
# Split each file into light/dark segments using run metadata times
# Final plots focus on Samara vs Papagayo comparison for NP, R, GP
# ==============================================================
# This script reads Presens respirometry Excel files, matches each file to the
# metadata table, calculates oxygen slopes for light and dark segments,
# corrects for blanks using the Pocillopora-specific blank chamber,
# normalizes by surface area, runs statistics, and exports plots.

rm(list = ls())

# ----------------------------
# Packages
# ----------------------------
# Load all required packages. If a package is missing, install it first.
# These packages are used for data handling, time parsing, slope fitting, statistics, and Excel export.
pkgs <- c("tidyverse", "readxl", "lubridate", "LoLinR", "hms", "rstatix", "writexl")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(tidyverse)
library(readxl)
library(lubridate)
library(LoLinR)
library(hms)
library(rstatix)
library(writexl)

# ----------------------------
# PATHS
# ----------------------------
# Define the input files and output folder.
# The script reads raw respirometry files, metadata, and surface area data, then saves all outputs to out_dir.
respi_folder     <- "C:\\Costa rica-2026\\TPC\\RAW-Presens"
master_meta_xlsx <- "C:\\Costa rica-2026\\TPC\\TPC_master_sample_metadata.xlsx"
run_meta_xlsx    <- "C:\\Costa rica-2026\\TPC\\TPC_run_metadata.xlsx"
sa_csv           <- "C:\\Costa rica-2026\\TPC\\SA.csv"
out_dir          <- "C:\\Costa rica-2026\\TPC\\Out_R"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# Blank chamber rule
# ----------------------------
# Define which chamber is the dedicated blank chamber for Pocillopora.
# Blank correction will use only blanks from this chamber.
pocillopora_blank_chamber <- 8L

# ----------------------------
# Fixed colors for sites
# ----------------------------
# Keep site colors constant across all figures.
# Papagayo is blue and Samara is orange.
site_colors <- c(
  Papagayo = "#0072B2",
  Samara   = "#E69F00"
)

# ----------------------------
# REQUIRED COLUMN NAMES (exact)
# ----------------------------
# These are the exact column names required in each table.
# The script stops if any required column is missing.
req_master <- c(
  "fragment.ID.full","fragment_ID","Date","Site","species","Chamber","Sensor","Run",
  "Temp.Cat","Chamber.Vol.end.ml","Chamber.Vol.start.ml","Vol.coral"
)

req_run <- c("Run","Chamber","Light_Level","Date","Start.time","Stop.time","Note","light_dark")
req_sa <- c("fragment_ID","SA")
req_respi <- c("Date","Oxygen","Temperature")

# ----------------------------
# Read MASTER metadata
# ----------------------------
# Read the main sample metadata file and check that all required columns are present.
# Then clean the data types and calculate the chamber water volume in liters.
master <- read_excel(master_meta_xlsx)

missing_master <- setdiff(req_master, names(master))
if (length(missing_master) > 0) {
  stop("MASTER metadata is missing columns: ", paste(missing_master, collapse = ", "))
}

master <- master %>%
  mutate(
    `fragment.ID.full`     = trimws(as.character(`fragment.ID.full`)),
    fragment_ID            = trimws(as.character(fragment_ID)),
    Site                   = trimws(as.character(Site)),
    species                = as.character(species),
    Chamber                = as.integer(Chamber),
    Run                    = as.integer(Run),
    Temp.Cat               = suppressWarnings(as.numeric(Temp.Cat)),
    `Chamber.Vol.start.ml` = suppressWarnings(as.numeric(`Chamber.Vol.start.ml`)),
    `Chamber.Vol.end.ml`   = suppressWarnings(as.numeric(`Chamber.Vol.end.ml`)),
    Vol.coral              = suppressWarnings(as.numeric(Vol.coral))
  ) %>%
  mutate(
    Site = factor(Site, levels = c("Papagayo", "Samara")),
    water_volume_ml = `Chamber.Vol.end.ml`,
    water_volume_l  = water_volume_ml / 1000
  )

# ----------------------------
# Read RUN metadata
# ----------------------------
# Read the run metadata that defines the light and dark measurement windows.
# Only minimal cleaning is done here because exact column names are preserved.
run_meta <- read_excel(run_meta_xlsx)

missing_run <- setdiff(req_run, names(run_meta))
if (length(missing_run) > 0) {
  stop("RUN metadata is missing columns: ", paste(missing_run, collapse = ", "))
}

run_meta <- run_meta %>%
  mutate(
    Run        = as.integer(Run),
    Chamber    = as.integer(Chamber),
    light_dark = tolower(trimws(as.character(light_dark)))
  )

# ----------------------------
# Read SA
# ----------------------------
# Read the surface area file and merge it into the master metadata using fragment_ID.
# Surface area is later used to normalize the corrected rates.
sa <- read.csv(sa_csv, stringsAsFactors = FALSE, check.names = FALSE)

missing_sa <- setdiff(req_sa, names(sa))
if (length(missing_sa) > 0) {
  stop("SA.csv is missing columns: ", paste(missing_sa, collapse = ", "))
}

sa <- sa %>%
  mutate(
    fragment_ID = trimws(as.character(fragment_ID)),
    SA = suppressWarnings(as.numeric(SA))
  )

master <- master %>%
  left_join(sa, by = "fragment_ID")

# ----------------------------
# Define blanks
# ----------------------------
# Mark blank chambers based on fragment_ID starting with "bk".
# These rows are used later to calculate blank correction.
master <- master %>%
  mutate(
    is_blank = ifelse(grepl("^bk", fragment_ID, ignore.case = TRUE), 1L, 0L)
  )

# ----------------------------
# List respirometry files
# ----------------------------
# Search the raw Presens folder recursively for Excel files.
# Temporary Excel lock files are removed from the list before processing.
respi_files <- list.files(
  respi_folder,
  pattern = "\\.(xlsx|xls)$",
  full.names = TRUE,
  recursive = TRUE
)

respi_files <- respi_files[!grepl("^~\\$", basename(respi_files))]

if (length(respi_files) == 0) {
  stop("No .xlsx/.xls files found in respi_folder.")
}

# =========================================================
# Problematic time windows
# =========================================================
# Define specific time ranges that should be excluded from selected runs or fragments.
# These are known problematic intervals that should not be used for slope fitting.
remove_run1_start <- hms::as_hms("11:04:00")
remove_run1_stop  <- hms::as_hms("11:11:00")

remove_poc3_start <- hms::as_hms("10:50:00")
remove_poc3_stop  <- hms::as_hms("11:20:00")

# ----------------------------
# Helper: compute slope from one segment
# ----------------------------
# Calculate the oxygen slope for one light or dark segment.
# The function first tries rankLocReg, and if that fails, it uses a simple linear model.
compute_rate <- function(df) {
  miss <- setdiff(req_respi, names(df))
  if (length(miss) > 0) return(NULL)
  
  d <- df %>%
    mutate(
      datetime = as.POSIXct(`Date`, tz = "UTC"),
      Oxygen = suppressWarnings(as.numeric(Oxygen)),
      Temperature = suppressWarnings(as.numeric(Temperature))
    ) %>%
    filter(!is.na(datetime), !is.na(Oxygen), !is.na(Temperature)) %>%
    arrange(datetime)
  
  if (nrow(d) < 50) return(NULL)
  
  d <- d %>%
    mutate(sec = as.numeric(difftime(datetime, first(datetime), units = "secs")))
  
  d_trim <- d %>% filter(sec >= 60)
  if (nrow(d_trim) < 50) return(NULL)
  
  d_thin <- d_trim %>%
    slice(seq(1, n(), by = 20)) %>%
    mutate(sec = as.numeric(difftime(datetime, first(datetime), units = "secs")))
  
  slope <- NA_real_
  intercept <- NA_real_
  method_used <- "rankLocReg"
  
  try_rlr <- try({
    regs <- LoLinR::rankLocReg(
      xall = d_thin$sec,
      yall = d_thin$Oxygen,
      alpha = 0.5,
      method = "pc",
      verbose = FALSE
    )
    
    intercept <- regs$allRegs[1, "b0"]
    slope     <- regs$allRegs[1, "b1"]
  }, silent = TRUE)
  
  if (inherits(try_rlr, "try-error") || is.na(slope)) {
    method_used <- "lm"
    fit <- lm(Oxygen ~ sec, data = d_thin)
    slope <- coef(fit)[["sec"]]
    intercept <- coef(fit)[["(Intercept)"]]
  }
  
  tibble(
    Intercept    = intercept,
    `umol.L.sec` = slope,
    Temp.C       = mean(d_thin$Temperature, na.rm = TRUE),
    method       = method_used
  )
}

# ----------------------------
# Process all files
# ----------------------------
# Loop through every respirometry file, match it to one metadata row,
# clean the data, remove problematic times, split into segments, and calculate rates.
out_list <- list()

for (f in respi_files) {
  fname <- basename(f)
  file_id <- tools::file_path_sans_ext(fname)
  
  if (!file_id %in% master$`fragment.ID.full`) {
    warning(paste0("No match in MASTER for file_id: ", file_id, " | file: ", fname))
    next
  }
  
  meta_row <- master %>% filter(`fragment.ID.full` == file_id)
  if (nrow(meta_row) != 1) {
    warning(paste0("Expected exactly 1 master row for: ", file_id))
    next
  }
  
  dat <- read_excel(f)
  
  miss <- setdiff(req_respi, names(dat))
  if (length(miss) > 0) {
    warning(paste0("Missing required columns in: ", fname))
    next
  }
  
  dat <- dat %>%
    mutate(
      datetime = as.POSIXct(`Date`, tz = "UTC"),
      Oxygen = suppressWarnings(as.numeric(Oxygen)),
      Temperature = suppressWarnings(as.numeric(Temperature)),
      time_of_day = hms::as_hms(datetime)
    ) %>%
    filter(!is.na(datetime), !is.na(Oxygen), !is.na(Temperature)) %>%
    arrange(datetime)
  
  if (meta_row$Run[1] == 1) {
    dat <- dat %>%
      filter(!(time_of_day >= remove_run1_start & time_of_day <= remove_run1_stop))
  }
  
  if (meta_row$fragment_ID[1] == "POC_3") {
    dat <- dat %>%
      filter(!(time_of_day >= remove_poc3_start & time_of_day <= remove_poc3_stop))
  }
  
  if (nrow(dat) == 0) {
    warning(paste0("No valid rows in file after cleaning: ", fname))
    next
  }
  
  file_start <- min(dat$datetime, na.rm = TRUE)
  file_end   <- max(dat$datetime, na.rm = TRUE)
  
  rm_sub <- run_meta %>%
    filter(
      Run == meta_row$Run[1],
      Chamber == meta_row$Chamber[1]
    )
  
  if (nrow(rm_sub) == 0) {
    warning(paste0("No run_meta rows for: ", file_id))
    next
  }
  
  file_date <- as.Date(file_start)
  
  rm_sub <- rm_sub %>%
    mutate(
      start_dt = as.POSIXct(
        paste(file_date, format(`Start.time`, "%H:%M:%S")),
        tz = "UTC"
      ),
      stop_dt = as.POSIXct(
        paste(file_date, format(`Stop.time`, "%H:%M:%S")),
        tz = "UTC"
      )
    ) %>%
    filter(stop_dt > file_start & start_dt < file_end)
  
  if (nrow(rm_sub) == 0) {
    warning(paste0("No overlapping run_meta windows for: ", file_id))
    next
  }
  
  for (j in seq_len(nrow(rm_sub))) {
    seg <- dat %>%
      filter(datetime >= rm_sub$start_dt[j] & datetime <= rm_sub$stop_dt[j]) %>%
      transmute(
        Date = datetime,
        Oxygen = Oxygen,
        Temperature = Temperature
      )
    
    rate <- compute_rate(seg)
    if (is.null(rate)) {
      warning(paste0("Could not compute segment rate for: ", fname, " | ", rm_sub$light_dark[j]))
      next
    }
    
    out_list[[length(out_list) + 1]] <- tibble(
      file = fname,
      `fragment.ID.full` = file_id,
      Date = meta_row$Date[1],
      Run = meta_row$Run[1],
      Chamber = meta_row$Chamber[1],
      light_dark = rm_sub$light_dark[j],
      segment_start = rm_sub$start_dt[j],
      segment_stop = rm_sub$stop_dt[j]
    ) %>%
      bind_cols(rate)
  }
}

Photo.R <- bind_rows(out_list)

if (nrow(Photo.R) == 0) {
  stop("No files were processed. Check filenames and required columns.")
}

write.csv(Photo.R, file.path(out_dir, "Photo.R_raw_rates.csv"), row.names = FALSE)

# ----------------------------
# Join Photo.R with MASTER + RUN meta
# ----------------------------
# Add the relevant metadata back onto the calculated segment rates.
# This brings in sample information, surface area, and run metadata for later calculations.
Photo.R <- Photo.R %>%
  left_join(master, by = c("fragment.ID.full", "Date", "Run", "Chamber")) %>%
  left_join(run_meta, by = c("Run", "Chamber", "Date", "light_dark"))

# ----------------------------
# Compute umol/sec
# ----------------------------
# Convert the oxygen slope from umol/L/sec into total chamber oxygen change per second.
# This uses the chamber water volume in liters.
Photo.R <- Photo.R %>%
  mutate(
    umol.sec = `umol.L.sec` * as.numeric(water_volume_l)
  )

# ----------------------------
# Blank correction
# ----------------------------
# Use only the Pocillopora-specific blank chamber for blank correction.
# Blank matching is done by Run + Temp.Cat + light_dark, and only from Chamber 8.
blank_candidates <- Photo.R %>%
  filter(is_blank == 1) %>%
  arrange(Run, Temp.Cat, light_dark, Chamber, fragment_ID)

write.csv(
  blank_candidates,
  file.path(out_dir, "POCILLOPORA_blank_candidates_all.csv"),
  row.names = FALSE
)

blank_tbl <- Photo.R %>%
  filter(
    is_blank == 1,
    Chamber == pocillopora_blank_chamber
  ) %>%
  group_by(Run, Temp.Cat, light_dark) %>%
  summarize(
    blank.rate = mean(umol.sec, na.rm = TRUE),
    blank_fragments = paste(unique(fragment_ID), collapse = " | "),
    blank_chambers = paste(unique(Chamber), collapse = " | "),
    n_blanks = n(),
    .groups = "drop"
  )

write.csv(
  blank_tbl,
  file.path(out_dir, "POCILLOPORA_blank_summary_used.csv"),
  row.names = FALSE
)

Photo.R <- Photo.R %>%
  left_join(blank_tbl, by = c("Run", "Temp.Cat", "light_dark")) %>%
  mutate(
    umol.sec.corr = umol.sec - blank.rate
  )

blank_assignment_check <- Photo.R %>%
  filter(is_blank == 0) %>%
  select(
    `fragment.ID.full`,
    fragment_ID,
    Site,
    species,
    Run,
    Chamber,
    Temp.Cat,
    light_dark,
    umol.sec,
    blank.rate,
    blank_fragments,
    blank_chambers,
    n_blanks,
    umol.sec.corr
  ) %>%
  arrange(Run, Temp.Cat, light_dark, Chamber, fragment_ID)

write.csv(
  blank_assignment_check,
  file.path(out_dir, "POCILLOPORA_blank_assignment_check.csv"),
  row.names = FALSE
)

missing_blank_rows <- Photo.R %>%
  filter(is_blank == 0, is.na(blank.rate)) %>%
  select(`fragment.ID.full`, fragment_ID, Run, Chamber, Temp.Cat, light_dark)

if (nrow(missing_blank_rows) > 0) {
  write.csv(
    missing_blank_rows,
    file.path(out_dir, "POCILLOPORA_missing_blank_matches.csv"),
    row.names = FALSE
  )
  warning("Some non-blank rows did not find a matching Pocillopora blank in Chamber 8. See POCILLOPORA_missing_blank_matches.csv")
}

# ----------------------------
# Normalize to SA only for non-blanks
# ----------------------------
# Keep only coral samples and normalize corrected rates by surface area.
# Final units are umol O2 per cm2 per hour.
Rates <- Photo.R %>%
  filter(is_blank == 0) %>%
  mutate(
    umol.cm2.hr = (umol.sec.corr * 3600) / as.numeric(SA)
  )

write.csv(Rates, file.path(out_dir, "TT_Rates_clean.csv"), row.names = FALSE)

# ----------------------------
# Plot 1: raw fragment curves
# ----------------------------
# Plot each individual fragment across temperature.
# This is useful for checking raw trajectories before building summary figures.
p1 <- ggplot(
  Rates,
  aes(x = Temp.C, y = umol.cm2.hr, group = fragment_ID, color = fragment_ID)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~ Site + species + light_dark, scales = "free_y") +
  labs(
    title = "Pocillopora- Raw fragment curves",
    x = "Temperature (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1}))
  )

ggsave(
  filename = file.path(out_dir, "curves_by_fragment.png"),
  plot = p1,
  width = 12,
  height = 8,
  dpi = 300
)

# ==============================================================
# Build NP / R / GP data
# ==============================================================
# Reshape the cleaned rates so light values become NP and dark values become R.
# Then calculate GP as NP + abs(R) and combine all three into one long-format table.

rates_clean <- Rates %>%
  select(
    `fragment.ID.full`,
    fragment_ID,
    Site,
    species,
    Run,
    Temp.Cat,
    light_dark,
    umol.cm2.hr
  )

rates_wide <- rates_clean %>%
  mutate(rate_type = ifelse(light_dark == "light", "NP", "R")) %>%
  select(-light_dark) %>%
  pivot_wider(
    names_from = rate_type,
    values_from = umol.cm2.hr
  ) %>%
  mutate(
    GP = NP + abs(R),
    Temp.C = Temp.Cat
  )

gp_long <- rates_wide %>%
  select(`fragment.ID.full`, fragment_ID, Site, species, Run, Temp.Cat, Temp.C, GP) %>%
  mutate(light_dark = "gross") %>%
  rename(umol.cm2.hr = GP)

rates_long_with_gp <- bind_rows(
  rates_clean %>% mutate(Temp.C = Temp.Cat),
  gp_long
)

# ======================================================
# STATISTICS: Site comparison per temperature
# ======================================================
# Prepare the dataset for statistics by assigning each row to NP, R, or GP.
# The comparison is always between Papagayo and Samara within each temperature category.
data_stat <- rates_long_with_gp %>%
  mutate(
    Site = factor(Site, levels = c("Papagayo", "Samara")),
    rate_group = case_when(
      light_dark == "light" ~ "NP",
      light_dark == "dark"  ~ "R",
      light_dark == "gross" ~ "GP"
    )
  ) %>%
  filter(!is.na(umol.cm2.hr), !is.na(Site), !is.na(Temp.Cat), !is.na(rate_group))

# Check normality within each Site for every temperature and rate group.
# These results are exported so you can inspect the assumptions behind each test.
normality_results <- data_stat %>%
  group_by(rate_group, Temp.Cat, Site) %>%
  summarise(
    n = dplyr::n(),
    shapiro_p = {
      x <- umol.cm2.hr[is.finite(umol.cm2.hr)]
      if (length(x) >= 3) {
        tryCatch(shapiro.test(x)[["p.value"]], error = function(e) NA_real_)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

# For each temperature/rate subset, choose the statistical test automatically.
# Use Welch t-test when both groups look normal, otherwise use Wilcoxon.
run_test <- function(df) {
  df <- df %>%
    filter(!is.na(umol.cm2.hr), is.finite(umol.cm2.hr), !is.na(Site)) %>%
    mutate(Site = droplevels(factor(Site, levels = c("Papagayo", "Samara"))))
  
  sites_present <- unique(as.character(df$Site))
  sites_present <- sites_present[!is.na(sites_present)]
  
  if (length(sites_present) != 2) {
    return(
      tibble(
        test = "Not possible",
        p = NA_real_,
        statistic = NA_real_,
        effect_size = NA_real_
      )
    )
  }
  
  counts <- df %>%
    count(Site, name = "n")
  
  if (nrow(counts) != 2 || any(counts$n < 2)) {
    return(
      tibble(
        test = "Too few observations",
        p = NA_real_,
        statistic = NA_real_,
        effect_size = NA_real_
      )
    )
  }
  
  norm <- df %>%
    group_by(Site) %>%
    summarise(
      n = dplyr::n(),
      p_norm = {
        x <- umol.cm2.hr[is.finite(umol.cm2.hr)]
        if (length(x) >= 3) {
          tryCatch(shapiro.test(x)[["p.value"]], error = function(e) NA_real_)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  normal <- all(!is.na(norm$p_norm) & norm$p_norm > 0.05)
  
  if (normal) {
    test_obj <- tryCatch(
      t.test(umol.cm2.hr ~ Site, data = df, var.equal = FALSE),
      error = function(e) NULL
    )
    
    eff_obj <- tryCatch(
      rstatix::cohens_d(df, umol.cm2.hr ~ Site),
      error = function(e) NULL
    )
    
    return(
      tibble(
        test = "Welch t-test",
        p = if (is.null(test_obj)) NA_real_ else unname(test_obj[["p.value"]]),
        statistic = if (is.null(test_obj)) NA_real_ else unname(test_obj[["statistic"]]),
        effect_size = if (is.null(eff_obj)) NA_real_ else eff_obj$effsize[1]
      )
    )
    
  } else {
    test_obj <- tryCatch(
      wilcox.test(umol.cm2.hr ~ Site, data = df, exact = FALSE),
      error = function(e) NULL
    )
    
    eff_obj <- tryCatch(
      rstatix::wilcox_effsize(df, umol.cm2.hr ~ Site),
      error = function(e) NULL
    )
    
    return(
      tibble(
        test = "Wilcoxon",
        p = if (is.null(test_obj)) NA_real_ else unname(test_obj[["p.value"]]),
        statistic = if (is.null(test_obj)) NA_real_ else unname(test_obj[["statistic"]]),
        effect_size = if (is.null(eff_obj)) NA_real_ else eff_obj$effsize[1]
      )
    )
  }
}

# Run the test for every temperature and rate group.
# Then calculate BH-adjusted p-values and convert them into significance stars.
stats_results <- data_stat %>%
  group_by(rate_group, Temp.Cat) %>%
  group_modify(~run_test(.x)) %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    signif_label = case_when(
      is.na(p_adj)  ~ "",
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ ""
    )
  )

write_xlsx(
  list(
    Normality = normality_results,
    Statistics = stats_results
  ),
  file.path(out_dir, "Site_comparison_statistics.xlsx")
)

print(stats_results)

# Count the number of observations in each Site x Temp x rate group combination.
# This is helpful for checking whether low n explains weak or missing statistics.
count_check <- data_stat %>%
  count(rate_group, Temp.Cat, Site) %>%
  tidyr::pivot_wider(names_from = Site, values_from = n, values_fill = 0)

write.csv(count_check, file.path(out_dir, "Site_comparison_counts_check.csv"), row.names = FALSE)
print(count_check)

# =========================================================
# Plotting data
# =========================================================
# Prepare the final plotting table with consistent labels for NP, R, and GP.
# This table is used to build all site-comparison summary plots.
plot_data_site <- rates_long_with_gp %>%
  filter(!is.na(umol.cm2.hr)) %>%
  mutate(
    Site = factor(Site, levels = c("Papagayo", "Samara")),
    rate_group = case_when(
      light_dark == "light" ~ "NP",
      light_dark == "dark"  ~ "R",
      light_dark == "gross" ~ "GP",
      TRUE ~ light_dark
    )
  )

# Summarize the mean, SD, median, min, and max for each Site x Temp x rate group.
# These values are used for plotting and later Excel export.
summary_site_rate <- plot_data_site %>%
  group_by(Site, Temp.Cat, rate_group) %>%
  summarise(
    n = dplyr::n(),
    mean = mean(umol.cm2.hr, na.rm = TRUE),
    sd = sd(umol.cm2.hr, na.rm = TRUE),
    median = median(umol.cm2.hr, na.rm = TRUE),
    min = min(umol.cm2.hr, na.rm = TRUE),
    max = max(umol.cm2.hr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Site = factor(Site, levels = c("Papagayo", "Samara"))
  )

# Calculate the vertical position of significance stars for each panel.
# The stars are placed slightly above the highest observed value.
star_positions <- plot_data_site %>%
  group_by(rate_group, Temp.Cat) %>%
  summarise(
    y_max = max(umol.cm2.hr, na.rm = TRUE),
    y_min = min(umol.cm2.hr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    y_pos = ifelse(
      y_max >= 0,
      y_max + 0.08 * pmax(abs(y_max), 1e-6),
      y_max + 0.15 * abs(y_min)
    )
  ) %>%
  left_join(
    stats_results %>% select(rate_group, Temp.Cat, signif_label, p_adj),
    by = c("rate_group", "Temp.Cat")
  )

# ----------------------------
# Plot 2: mean ± SD + significance
# ----------------------------
# Plot mean response across temperature for each site, with SD error bars.
# Significance stars show between-site differences at each temperature.
p2 <- ggplot(
  summary_site_rate,
  aes(x = Temp.Cat, y = mean, color = Site, group = Site)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25) +
  geom_text(
    data = star_positions,
    aes(x = Temp.Cat, y = y_pos, label = signif_label),
    inherit.aes = FALSE,
    color = "black",
    size = 5
  ) +
  scale_color_manual(
    values = site_colors,
    breaks = c("Papagayo", "Samara"),
    drop = FALSE
  ) +
  theme_bw() +
  facet_wrap(~ rate_group, scales = "free_y") +
  labs(
    title = "Pocillopora- Samara vs Papagayo across temperature",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    color = "Site"
  )

ggsave(
  filename = file.path(out_dir, "Site_comparison_NP_R_GP_mean_sd.png"),
  plot = p2,
  width = 12,
  height = 6,
  dpi = 300
)

# ----------------------------
# Plot 3: boxplots + significance
# ----------------------------
# Show the full distribution of values by site at each temperature using boxplots.
# Significance stars are added above each temperature group.
p3 <- ggplot(
  plot_data_site,
  aes(x = factor(Temp.Cat), y = umol.cm2.hr, fill = Site)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA
  ) +
  geom_text(
    data = star_positions,
    aes(x = factor(Temp.Cat), y = y_pos, label = signif_label),
    inherit.aes = FALSE,
    color = "black",
    size = 5
  ) +
  scale_fill_manual(
    values = site_colors,
    breaks = c("Papagayo", "Samara"),
    drop = FALSE
  ) +
  theme_bw() +
  facet_wrap(~ rate_group, scales = "free_y") +
  labs(
    title = "Pocillopora- Distribution by site across temperature",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    fill = "Site"
  )

ggsave(
  filename = file.path(out_dir, "Site_comparison_NP_R_GP_boxplots.png"),
  plot = p3,
  width = 13,
  height = 6,
  dpi = 300
)

# ----------------------------
# Plot 4: site panels
# ----------------------------
# Plot mean ± SD again, but separate each site into its own panel.
# This makes the shape of the thermal response easier to inspect within each site.
p4 <- ggplot(
  summary_site_rate,
  aes(x = Temp.Cat, y = mean, color = Site, group = Site)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25) +
  scale_color_manual(
    values = site_colors,
    breaks = c("Papagayo", "Samara"),
    drop = FALSE
  ) +
  theme_bw() +
  facet_grid(rate_group ~ Site, scales = "free_y") +
  labs(
    title = "Pocillopora- NP, R, and GP by site",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    color = "Site"
  )

ggsave(
  filename = file.path(out_dir, "Site_panels_NP_R_GP.png"),
  plot = p4,
  width = 12,
  height = 8,
  dpi = 300
)

message("Done. Outputs written to: ", out_dir)

# =========================================================
# EXPORT EXCEL FILES FOR TPC ANALYSIS - POCILLOPORA
# =========================================================
# Export the main processed tables to Excel for downstream TPC analysis or manual inspection.
# This includes cleaned rates, the combined Photo.T table, and the summary table.

if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)

# ---------------------------------------------------------
# 1. Export cleaned rates (NP and R)
# ---------------------------------------------------------
# Save the cleaned and normalized NP/R dataset to Excel.
write_xlsx(
  Rates,
  file.path(out_dir, "POCILLOPORA_TT_Rates_clean.xlsx")
)

# ---------------------------------------------------------
# 2. Create Photo.T table similar to original script
# NP + R + GP combined
# ---------------------------------------------------------
# Create a long-format table containing NP, R, and GP together.
# individual.ID is extracted from fragment_ID to match the original output style.
Photo.T <- rates_long_with_gp %>%
  mutate(
    individual.ID = sub("_.*$", "", fragment_ID),
    rate.type = case_when(
      light_dark == "light" ~ "NP",
      light_dark == "dark"  ~ "R",
      light_dark == "gross" ~ "GP"
    )
  ) %>%
  select(
    individual.ID,
    Temp.Cat,
    Temp.C,
    umol.cm2.hr,
    rate.type,
    light_dark,
    fragment_ID,
    Site,
    species,
    Run,
    `fragment.ID.full`
  )

write_xlsx(
  Photo.T,
  file.path(out_dir, "POCILLOPORA_Photo.T.xlsx")
)

# ---------------------------------------------------------
# 3. Export summary table
# ---------------------------------------------------------
# Export the summary table used in the plots for quick inspection of means and variability.
write_xlsx(
  summary_site_rate,
  file.path(out_dir, "POCILLOPORA_summary_site_rate.xlsx")
)

message("Excel exports for Pocillopora TPC created successfully.")
message("Pocillopora blank correction used Chamber ", pocillopora_blank_chamber, " only.")