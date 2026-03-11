# ==============================================================
# Photosynthesis / Respiration processing from Pyro TXT files
# EXACT file matching: TXT filename (without extension)
# must match master$`fragment.ID.full`
# Output folder: OUTPUT-PORITES
# ==============================================================
# This script reads Pyro TXT files, matches them to metadata,
# calculates oxygen slopes for light/dark phases, applies blank correction
# using the Porites-specific blank chamber, normalizes by surface area,
# performs statistics, and exports plots/tables.

rm(list = ls())

# ----------------------------
# Packages
# ----------------------------
# Load all required packages. If one is missing, install it first.
# These packages are used for data cleaning, time parsing, slope fitting, stats, and Excel export.
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
# Define all input and output paths.
# The script reads TXT, metadata, and SA files, then writes all outputs into out_dir.
respi_folder     <- "C:\\Costa rica-2026\\TPC\\Pyro\\RAW-TPC-Pyro"
master_meta_xlsx <- "C:\\Costa rica-2026\\TPC\\TPC_master_sample_metadata.xlsx"
run_meta_xlsx    <- "C:\\Costa rica-2026\\TPC\\TPC_run_metadata.xlsx"
sa_csv           <- "C:\\Costa rica-2026\\TPC\\SA.csv"
out_dir          <- "C:\\Costa rica-2026\\TPC\\OUTPUT-PORITES"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# Blank chamber rule
# ----------------------------
# Define which chamber is the dedicated blank chamber for Porites.
# Blank correction will use only blanks from this chamber.
porites_blank_chamber <- 10L

# ----------------------------
# Fixed colors for sites
# ----------------------------
# Set fixed colors for plotting so site colors stay consistent across all figures.
# Papagayo is blue and Samara is orange.
site_colors <- c(
  Papagayo = "#0072B2",
  Samara   = "#E69F00"
)

# ----------------------------
# REQUIRED COLUMN NAMES (exact)
# ----------------------------
# These are the exact column names expected in each metadata file.
# The script stops with an error if any required column is missing.
req_master <- c(
  "fragment.ID.full","fragment_ID","Date","Site","species","Chamber","Sensor","Run",
  "Temp.Cat","Chamber.Vol.end.ml","Chamber.Vol.start.ml","Vol.coral"
)

req_run <- c("Run","Chamber","Light_Level","Date","Start.time","Stop.time","Note","light_dark")
req_sa <- c("fragment_ID","SA")
req_respi <- c("Date","Oxygen","Temperature")

# =========================================================
# Helper: parse time values
# =========================================================
# Convert time values from Excel or text into hms format.
# This handles different formats such as HH:MM, HH:MM:SS, Excel numeric time, or POSIX time.
parse_time_to_hms <- function(x) {
  out <- vector("list", length(x))
  
  for (i in seq_along(x)) {
    xi <- x[[i]]
    
    if (is.na(xi)) {
      out[[i]] <- NA
      next
    }
    
    if (inherits(xi, "POSIXt")) {
      out[[i]] <- hms::as_hms(xi)
      next
    }
    
    if (inherits(xi, "hms") || inherits(xi, "difftime")) {
      out[[i]] <- hms::as_hms(xi)
      next
    }
    
    if (is.numeric(xi)) {
      secs <- round(as.numeric(xi) * 86400)
      out[[i]] <- hms::as_hms(secs)
      next
    }
    
    xs <- trimws(as.character(xi))
    if (grepl("^\\d{1,2}:\\d{2}$", xs)) xs <- paste0(xs, ":00")
    
    if (grepl("^\\d{1,2}:\\d{2}:\\d{2}$", xs)) {
      out[[i]] <- hms::as_hms(xs)
      next
    }
    
    parsed <- suppressWarnings(lubridate::hms(xs))
    if (!is.na(parsed)) {
      out[[i]] <- hms::as_hms(parsed)
      next
    }
    
    out[[i]] <- NA
  }
  
  out <- unlist(out)
  hms::as_hms(out)
}

# =========================================================
# Helper: read one Pyro TXT file
# V1  = Date
# V2  = Time
# V4  = Oxygen
# V12 = Temperature
# =========================================================
# Read one TXT file from the Pyro output and extract only the useful columns.
# The function finds where the real data starts, then keeps Date, Oxygen, and Temperature.
read_pyro_txt <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  
  if (length(lines) == 0) {
    stop("Empty TXT file: ", basename(file_path))
  }
  
  data_start <- grep("^\\d{2}-\\d{2}-\\d{4}\t", lines)
  if (length(data_start) == 0) {
    stop("No data rows starting with date found in file: ", basename(file_path))
  }
  data_start <- data_start[1]
  
  dat_raw <- read.delim(
    text = paste(lines[data_start:length(lines)], collapse = "\n"),
    sep = "\t",
    header = FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fill = TRUE,
    quote = "",
    comment.char = ""
  )
  
  if (nrow(dat_raw) == 0) {
    stop("Could not parse TXT data rows in file: ", basename(file_path))
  }
  
  if (ncol(dat_raw) < 12) {
    stop("TXT file has fewer than 12 columns: ", basename(file_path))
  }
  
  out <- dat_raw %>%
    transmute(
      Date_chr = paste(V1, V2),
      Oxygen = suppressWarnings(as.numeric(V4)),
      Temperature = suppressWarnings(as.numeric(V12))
    )
  
  dt <- suppressWarnings(as.POSIXct(out$Date_chr, format = "%d-%m-%Y %H:%M:%OS", tz = "UTC"))
  
  out <- out %>%
    transmute(
      Date = dt,
      Oxygen = Oxygen,
      Temperature = Temperature
    ) %>%
    filter(!is.na(Date), !is.na(Oxygen), !is.na(Temperature))
  
  out
}

# ----------------------------
# Read MASTER metadata
# ----------------------------
# Read the main sample metadata table and check that all needed columns exist.
# Then clean types and calculate water volume in mL and liters for later rate conversion.
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
# Read the run timing table that defines light and dark windows for each run/chamber.
# Parse start and stop times so each TXT file can be split into the correct segments.
run_meta <- read_excel(run_meta_xlsx)

missing_run <- setdiff(req_run, names(run_meta))
if (length(missing_run) > 0) {
  stop("RUN metadata is missing columns: ", paste(missing_run, collapse = ", "))
}

run_meta <- run_meta %>%
  mutate(
    Run        = as.integer(Run),
    Chamber    = as.integer(Chamber),
    light_dark = tolower(trimws(as.character(light_dark))),
    Start.hms  = parse_time_to_hms(`Start.time`),
    Stop.hms   = parse_time_to_hms(`Stop.time`)
  )

if (any(is.na(run_meta$Start.hms)) || any(is.na(run_meta$Stop.hms))) {
  bad_rows <- run_meta %>%
    filter(is.na(Start.hms) | is.na(Stop.hms)) %>%
    select(Run, Chamber, Date, `Start.time`, `Stop.time`)
  print(bad_rows)
  stop("Some Start/Stop times could not be parsed in TPC_run_metadata.xlsx")
}

# ----------------------------
# Read SA
# ----------------------------
# Read the surface area table and merge it into the master metadata using fragment_ID.
# SA will later be used to normalize oxygen rates to area-specific units.
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
# These records will be used later for blank correction of oxygen rates.
master <- master %>%
  mutate(
    is_blank = ifelse(grepl("^bk", fragment_ID, ignore.case = TRUE), 1L, 0L)
  )

# ----------------------------
# List TXT files
# ----------------------------
# Search the raw data folder recursively for all TXT files.
# Only files whose name matches fragment.ID.full in the metadata will be processed.
respi_files <- list.files(
  respi_folder,
  pattern = "\\.(txt|TXT)$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(respi_files) == 0) {
  stop("No .txt files found in respi_folder.")
}

# =========================================================
# Problematic time windows
# =========================================================
# Define known time windows that should be removed from specific files or runs.
# This allows exclusion of noisy or problematic periods before rate fitting.
remove_run1_start <- hms::as_hms("11:04:00")
remove_run1_stop  <- hms::as_hms("11:11:00")

por_problem_windows <- tibble(
  fragment_pattern = c("11_24", "14_24", "17_24"),
  start_time = c("10:45:00", "10:45:00", "10:45:00"),
  stop_time  = c("10:59:00", "10:58:00", "10:48:00")
) %>%
  mutate(
    start_hms = hms::as_hms(start_time),
    stop_hms  = hms::as_hms(stop_time)
  )

# Apply the time-window exclusions defined above.
# For run 1 and for selected fragment IDs, rows within the problematic times are removed.
remove_problem_windows <- function(dat, meta_row, file_id, por_problem_windows) {
  if (nrow(dat) == 0) return(dat)
  
  dat <- dat %>%
    mutate(time_of_day = hms::as_hms(datetime))
  
  if (!is.na(meta_row$Run[1]) && meta_row$Run[1] == 1) {
    dat <- dat %>%
      filter(!(time_of_day >= remove_run1_start & time_of_day <= remove_run1_stop))
  }
  
  for (k in seq_len(nrow(por_problem_windows))) {
    patt <- por_problem_windows$fragment_pattern[k]
    if (identical(file_id, patt) || identical(meta_row$`fragment.ID.full`[1], patt)) {
      dat <- dat %>%
        filter(!(time_of_day >= por_problem_windows$start_hms[k] &
                   time_of_day <= por_problem_windows$stop_hms[k]))
    }
  }
  
  dat
}

# =========================================================
# Helper: compute slope from one segment
# =========================================================
# For one light or dark segment, calculate the oxygen slope over time.
# First try rankLocReg from LoLinR, and if that fails, fall back to a simple linear model.
compute_rate <- function(df) {
  miss <- setdiff(req_respi, names(df))
  if (length(miss) > 0) return(NULL)
  
  d <- df %>%
    mutate(
      datetime = as.POSIXct(Date, tz = "UTC"),
      Oxygen = suppressWarnings(as.numeric(Oxygen)),
      Temperature = suppressWarnings(as.numeric(Temperature))
    ) %>%
    filter(!is.na(datetime), !is.na(Oxygen), !is.na(Temperature)) %>%
    arrange(datetime)
  
  if (nrow(d) < 50) return(NULL)
  
  d <- d %>%
    mutate(sec = as.numeric(difftime(datetime, first(datetime), units = "secs")))
  
  d_trim <- d %>% filter(sec >= 120)
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

# =========================================================
# Process all files
# =========================================================
# Loop through all TXT files, match each one to metadata,
# split it into light/dark segments, calculate slopes, and save all raw rates.
out_list <- list()

for (f in respi_files) {
  fname <- basename(f)
  file_id <- trimws(tools::file_path_sans_ext(fname))
  
  if (!file_id %in% master$`fragment.ID.full`) {
    next
  }
  
  meta_row <- master %>% filter(`fragment.ID.full` == file_id)
  if (nrow(meta_row) != 1) {
    next
  }
  
  dat <- tryCatch(read_pyro_txt(f), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) {
    next
  }
  
  dat <- dat %>%
    mutate(
      datetime = as.POSIXct(Date, tz = "UTC"),
      Oxygen = suppressWarnings(as.numeric(Oxygen)),
      Temperature = suppressWarnings(as.numeric(Temperature))
    ) %>%
    filter(!is.na(datetime), !is.na(Oxygen), !is.na(Temperature)) %>%
    arrange(datetime)
  
  dat <- remove_problem_windows(dat, meta_row, file_id, por_problem_windows)
  if (nrow(dat) == 0) {
    next
  }
  
  file_start <- min(dat$datetime, na.rm = TRUE)
  file_end   <- max(dat$datetime, na.rm = TRUE)
  file_date  <- as.Date(file_start)
  
  rm_sub <- run_meta %>%
    filter(Run == meta_row$Run[1], Chamber == meta_row$Chamber[1]) %>%
    mutate(
      start_dt = as.POSIXct(file_date, tz = "UTC") + as.numeric(Start.hms),
      stop_dt  = as.POSIXct(file_date, tz = "UTC") + as.numeric(Stop.hms)
    ) %>%
    filter(stop_dt > file_start & start_dt < file_end)
  
  if (nrow(rm_sub) == 0) {
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
    if (is.null(rate)) next
    
    out_list[[length(out_list) + 1]] <- tibble(
      file = fname,
      `fragment.ID.full` = file_id,
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
  stop("No files were processed. The next place to inspect is run_meta overlap or too-short segments after time removal.")
}

write.csv(Photo.R, file.path(out_dir, "PORITES_Photo_R_raw_rates.csv"), row.names = FALSE)

# ----------------------------
# Join with MASTER
# ----------------------------
# Add sample metadata back onto the calculated raw slopes.
# This brings in Site, species, Temp.Cat, chamber volume, SA, and blank identity.
Photo.R <- Photo.R %>%
  left_join(
    master %>%
      select(`fragment.ID.full`, fragment_ID, Site, species, Run, Chamber, Temp.Cat,
             water_volume_l, SA, is_blank),
    by = c("fragment.ID.full", "Run", "Chamber")
  )

# ----------------------------
# Compute umol/sec
# ----------------------------
# Convert slopes from concentration change per liter per second
# into total chamber oxygen change per second using water volume.
Photo.R <- Photo.R %>%
  mutate(
    umol.sec = `umol.L.sec` * as.numeric(water_volume_l)
  )

# ----------------------------
# Blank correction
# ----------------------------
# Use only the Porites-specific blank chamber for blank correction.
# Blank matching is done by Run + Temp.Cat + light_dark, and only from Chamber 10.
blank_candidates <- Photo.R %>%
  filter(is_blank == 1) %>%
  arrange(Run, Temp.Cat, light_dark, Chamber, fragment_ID)

write.csv(
  blank_candidates,
  file.path(out_dir, "PORITES_blank_candidates_all.csv"),
  row.names = FALSE
)

blank_tbl <- Photo.R %>%
  filter(
    is_blank == 1,
    Chamber == porites_blank_chamber
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
  file.path(out_dir, "PORITES_blank_summary_used.csv"),
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
  file.path(out_dir, "PORITES_blank_assignment_check.csv"),
  row.names = FALSE
)

missing_blank_rows <- Photo.R %>%
  filter(is_blank == 0, is.na(blank.rate)) %>%
  select(`fragment.ID.full`, fragment_ID, Run, Chamber, Temp.Cat, light_dark)

if (nrow(missing_blank_rows) > 0) {
  write.csv(
    missing_blank_rows,
    file.path(out_dir, "PORITES_missing_blank_matches.csv"),
    row.names = FALSE
  )
  warning("Some non-blank rows did not find a matching Porites blank in Chamber 10. See PORITES_missing_blank_matches.csv")
}

# ----------------------------
# Normalize to SA only for non-blanks
# ----------------------------
# Keep only real coral samples and normalize corrected oxygen rates by surface area.
# Final units are umol O2 per cm2 per hour.
Rates <- Photo.R %>%
  filter(is_blank == 0) %>%
  mutate(
    umol.cm2.hr = (umol.sec.corr * 3600) / as.numeric(SA)
  )

write.csv(Rates, file.path(out_dir, "PORITES_TT_Rates_clean.csv"), row.names = FALSE)

# =========================================================
# Build NP / R / GP data
# =========================================================
# Reshape the cleaned rates so light = NP and dark = R,
# then calculate GP as NP + abs(R) and combine all three into one long table.
rates_clean <- Rates %>%
  select(`fragment.ID.full`, fragment_ID, Site, species, Run, Temp.Cat, light_dark, umol.cm2.hr)

rates_wide <- rates_clean %>%
  mutate(rate_type = ifelse(light_dark == "light", "NP", "R")) %>%
  select(-light_dark) %>%
  pivot_wider(names_from = rate_type, values_from = umol.cm2.hr) %>%
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
# Prepare a statistical dataset with NP, R, and GP grouped consistently.
# Comparisons are done between Papagayo and Samara within each temperature category.
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

# Check normality separately for each Site within each temperature and rate group.
# These results are exported so you can inspect distribution assumptions.
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

# Run the appropriate test between sites for one subset of data.
# Use Welch t-test if both groups look normal, otherwise use Wilcoxon.
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

# Apply the site-comparison test for every temperature and rate group.
# Also calculate BH-adjusted p-values and significance stars for plotting/export.
stats_results <- data_stat %>%
  group_by(rate_group, Temp.Cat) %>%
  group_modify(~run_test(.x)) %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    signif_label = case_when(
      is.na(p)  ~ "",
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

write_xlsx(
  list(
    Normality = normality_results,
    Statistics = stats_results
  ),
  file.path(out_dir, "PORITES_Site_comparison_statistics.xlsx")
)

print(stats_results)

# Count how many observations exist in each Site x Temp x rate group combination.
# This helps verify whether low sample size explains missing or weak statistics.
count_check <- data_stat %>%
  count(rate_group, Temp.Cat, Site) %>%
  tidyr::pivot_wider(names_from = Site, values_from = n, values_fill = 0)

write.csv(count_check, file.path(out_dir, "PORITES_Site_comparison_counts_check.csv"), row.names = FALSE)
print(count_check)

# =========================================================
# Plotting data
# =========================================================
# Prepare a clean plotting table with consistent labels for NP, R, and GP.
# This table is used for all summary and boxplot figures.
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

# Summarize mean, SD, median, and range by Site x Temp x rate group.
# These summaries are used for line plots and exported later to Excel.
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

# Calculate where significance stars should be placed on each panel.
# The star is positioned slightly above the maximum observed value.
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

# =========================================================
# Plot 1: raw fragment curves
# =========================================================
# Plot each fragment separately across temperature to visualize raw individual trajectories.
# Facets are split by Site, species, and light/dark condition.
p1 <- ggplot(
  Rates,
  aes(x = Temp.C, y = umol.cm2.hr, group = fragment_ID, color = fragment_ID)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~ Site + species + light_dark, scales = "free_y") +
  labs(
    title = "Porites- Raw fragment curves",
    x = "Temperature (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1}))
  )

ggsave(
  filename = file.path(out_dir, "PORITES_curves_by_fragment.png"),
  plot = p1,
  width = 12,
  height = 8,
  dpi = 300
)

# =========================================================
# Plot 2: mean ± SD + significance
# =========================================================
# Plot site means across temperature with SD error bars.
# Significance stars show site differences at each temperature for each rate type.
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
    title = "Porites- Samara vs Papagayo across temperature",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    color = "Site"
  )

ggsave(
  filename = file.path(out_dir, "PORITES_Site_comparison_NP_R_GP_mean_sd.png"),
  plot = p2,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================================
# Plot 3: boxplots + significance
# =========================================================
# Show the full distribution of values by Site at each temperature using boxplots.
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
    title = "Porites- Distribution by site across temperature",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    fill = "Site"
  )

ggsave(
  filename = file.path(out_dir, "PORITES_Site_comparison_NP_R_GP_boxplots.png"),
  plot = p3,
  width = 13,
  height = 6,
  dpi = 300
)

# =========================================================
# Plot 4: site panels
# =========================================================
# Plot mean ± SD again, but split panels by both rate group and site.
# This gives a cleaner visual view of each site separately.
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
    title = "Porites- NP, R, and GP by site",
    x = "Temperature category (°C)",
    y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
    color = "Site"
  )

ggsave(
  filename = file.path(out_dir, "PORITES_Site_panels_NP_R_GP.png"),
  plot = p4,
  width = 12,
  height = 8,
  dpi = 300
)

# =========================================================
# EXPORT EXCEL FILES FOR TPC ANALYSIS
# =========================================================
# Export the main cleaned outputs to Excel for downstream TPC fitting or manual inspection.
# This includes cleaned rates, a combined Photo.T table, and a summary table.
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)

# ---------------------------------------------------------
# 1. Export cleaned rates (NP and R)
# ---------------------------------------------------------
# Save the cleaned NP and R table to Excel.
# This table contains blank-corrected and SA-normalized values.
write_xlsx(
  Rates,
  file.path(out_dir, "PORITES_TT_Rates_clean.xlsx")
)

# ---------------------------------------------------------
# 2. Create Photo.T table similar to original script
# NP + R + GP combined
# ---------------------------------------------------------
# Build a long-format table containing NP, R, and GP together.
# individual.ID is extracted from fragment_ID to mimic the original format.
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
  file.path(out_dir, "PORITES_Photo.T.xlsx")
)

# ---------------------------------------------------------
# 3. Optional: export summary for quick inspection
# ---------------------------------------------------------
# Export the summary statistics table used for plotting.
# This is useful for quick checks of means, SD, medians, and sample size.
write_xlsx(
  summary_site_rate,
  file.path(out_dir, "PORITES_summary_site_rate.xlsx")
)

message("Excel exports for TPC created successfully.")
message("Porites blank correction used Chamber ", porites_blank_chamber, " only.")
message("Done. Outputs written to: ", out_dir)