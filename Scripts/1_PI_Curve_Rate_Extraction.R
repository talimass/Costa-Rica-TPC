
############# "Oxygen flux rate extractions " #############


############################################################
## FULL CLEAN PI-CURVE SCRIPT
############################################################

library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(hms)

############################################################
## Paths
############################################################

path.p <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costar-Rica-TPC/Data/1_pi_curves"
output_path <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costar-Rica-TPC/Output"

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

############################################################
## File List
############################################################

file.names <- list.files(path = path.p, pattern = "csv$")
file.names <- file.names[!grepl("metadata", file.names)]
file.names <- file.names[!grepl("^bk", file.names)]   # remove blanks

############################################################
## Read Metadata
############################################################

sample.info <- read_csv(
  file.path(path.p, "1_pi_curves_sample_metadata - 1_pi_curves_sample_metadata.csv"),
  show_col_types = FALSE
)

run.info <- read_csv(
  file.path(path.p, "1_pi_curves_run_metadata - 1_pi_curves_run_metadata.csv"),
  show_col_types = FALSE
)

############################################################
## Clean Sample Metadata
############################################################

sample.info2 <- sample.info %>%
  mutate(
    Chamber = as.integer(Chamber.Channel),
    Chamber.Vol.L = Chamber.Vol.L / 1000,
    colony_id = as.character(colony_id)
  )

############################################################
## Convert Run Times to Minutes
############################################################

run.info2 <- run.info %>%
  mutate(
    Chamber = as.integer(Chamber),
    Start.sec = as.numeric(as_hms(Start.time)),
    Stop.sec  = as.numeric(as_hms(Stop.time)),
    Stop.sec  = if_else(Stop.sec < Start.sec,
                        Stop.sec + 24 * 3600,
                        Stop.sec)
  ) %>%
  group_by(Run, Chamber) %>%
  mutate(
    run_start_sec = min(Start.sec),
    Start.min = (Start.sec - run_start_sec) / 60,
    Stop.min  = (Stop.sec  - run_start_sec) / 60
  ) %>%
  ungroup()

############################################################
## Keep All Light Steps
############################################################

run_intervals <- run.info2 %>%
  select(Run, Chamber, Date, Light_Level, Light_Value,
         Start.min, Stop.min) %>%
  arrange(Run, Chamber, Start.min)

############################################################
## Join Metadata
############################################################

metadata <- left_join(sample.info2,
                      run_intervals,
                      by = c("Run", "Chamber"))

metadata$Date <- as_date(as.character(metadata$Date.y),
                         format = "%Y%m%d")

metadata <- metadata %>%
  select(-Date.x, -Date.y) %>%
  select(Species, colony_id, Run, Temp.Cat,
         Chamber.Vol.L, Date,
         Chamber, Light_Level, Light_Value,
         Start.min, Stop.min) %>%
  arrange(colony_id, Light_Value)

############################################################
## Read Oxygen Files
############################################################

df <- tibble(file.name = file.names) %>%
  mutate(
    colony_id = tools::file_path_sans_ext(file.name),
    colony_id = as.character(colony_id),
    
    info = map(colony_id,
               ~ filter(metadata, colony_id == .x)),
    
    data0_raw = map(file.name,
                    ~ read_csv(file.path(path.p, .x),
                               show_col_types = FALSE)),
    
    data0 = map(data0_raw, ~ .x %>%
                  transmute(
                    Time  = `Delta T [min]`,
                    Value = Oxygen,
                    Temp  = Temperature
                  ) %>%
                  filter(complete.cases(.)))
  ) %>%
  select(-data0_raw)

############################################################
## Sanity Check (IMPORTANT)
############################################################

check <- df %>%
  transmute(file.name,
            colony_id,
            n_metadata_rows = map_int(info, nrow))

print(check)

############################################################
## Split Oxygen Data by Light Intervals
############################################################

df2 <- df %>%
  mutate(intervals = map2(data0, info, function(.x, .y) {
    
    if (nrow(.y) == 0) {
      stop("No metadata match for file.")
    }
    
    .y <- .y %>% arrange(Start.min)
    
    breaks <- c(.y$Start.min[1], .y$Stop.min)
    breaks <- unique(breaks)
    
    out <- split(
      .x,
      f = cut(.x$Time,
              breaks = breaks,
              labels = as.character(.y$Light_Value),
              include.lowest = TRUE)
    )
    
    # Convert named list to tibble manually
    tibble(
      Light_Value = names(out),
      data = out
    )
  })) %>%
  unnest(intervals) %>%
  unnest(data)


############################################################
## Thin Data
############################################################

thin_par <- 20

df3 <- df2 %>%
  group_by(file.name, Light_Value) %>%
  slice(seq(1, n(), thin_par)) %>%
  ungroup()


############################################################
## Build PI Plot
############################################################

pi_plot <- df3 %>%
  mutate(Light_Value = as.numeric(Light_Value)) %>%
  group_by(colony_id, Light_Value) %>%
  summarise(mean_O2 = mean(Value, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Light_Value,
             y = mean_O2,
             color = colony_id,
             group = colony_id)) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = "Light intensity",
       y = "Mean O2",
       title = "PI Curve") +
  theme_bw()

print(pi_plot)

############################################################
## Save Plot
############################################################

ggsave(file.path(output_path, "PI_curve.png"),
       pi_plot, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_path, "PI_curve.pdf"),
       pi_plot, width = 8, height = 6)

cat("PI plot saved to:", output_path, "\n")




#######################################
############################################################
## PI-CURVE ANALYSIS (NO SURFACE AREA NORMALIZATION)
############################################################
## ---------------------------
## Fit LoLinR regressions per colony x light interval
## ---------------------------
fit_reg <- function(dat) {
  rankLocReg(
    xall = as.numeric(dat$Time),
    yall = dat$Value,
    alpha = 0.2,
    method = "pc",
    verbose = FALSE
  )
}

future::plan(multisession)

df_reg <- df3 %>%
  group_by(colony_id, Light_Value) %>%
  nest() %>%
  mutate(
    rankLcRg = future_map(data, fit_reg),
    MeanTemp = map_dbl(data, ~ mean(.x$Temp, na.rm = TRUE))
  ) %>%
  ungroup()

## ---------------------------
## Extract slopes (µmol L^-1 s^-1) and convert to µmol s^-1 via chamber volume
## ---------------------------
df_rates <- df_reg %>%
  mutate(micromol.L.s = map_dbl(rankLcRg, ~ .x$allRegs$b1[1])) %>%
  select(colony_id, Light_Value, micromol.L.s, MeanTemp)

mx <- metadata %>%
  select(colony_id, Chamber.Vol.L, Temp.Cat) %>%
  distinct()

pr <- left_join(df_rates, mx, by = "colony_id") %>%
  mutate(
    # Volume correction IS applied here
    micromol.s = micromol.L.s * Chamber.Vol.L
  )

## ---------------------------
## Blank correction (only if blanks exist in pr)
## Blanks = colony_id starting with "bk" (case-insensitive)
## ---------------------------
has_blanks <- any(grepl("^bk", pr$colony_id, ignore.case = TRUE))

if (has_blanks) {
  blanks <- pr %>%
    filter(grepl("^bk", colony_id, ignore.case = TRUE)) %>%
    group_by(Temp.Cat, Light_Value) %>%
    summarise(micromol.s.blank = mean(micromol.s, na.rm = TRUE), .groups = "drop")
  
  pr <- pr %>%
    left_join(blanks, by = c("Temp.Cat", "Light_Value")) %>%
    mutate(micromol.s.adj = micromol.s - micromol.s.blank) %>%
    filter(!grepl("^bk", colony_id, ignore.case = TRUE))
} else {
  warning("No blank files found in pr (colony_id starting with 'bk'). Using uncorrected micromol.s as micromol.s.adj.")
  pr <- pr %>%
    mutate(micromol.s.blank = NA_real_,
           micromol.s.adj = micromol.s)
}

## ---------------------------
## (Surface area normalization disabled for now)
## ---------------------------
# sa <- read_csv(file.path(output_path, "1_surface_area_final.csv"))
# pr <- left_join(pr, select(sa, colony_id, surface.area.cm2), by = "colony_id")
# pr <- pr %>%
#   mutate(micromol.cm2.s = micromol.s.adj / surface.area.cm2,
#          micromol.cm2.h = micromol.cm2.s * 3600)

## ---------------------------
## PI plot from rates (µmol s^-1; volume corrected; blank corrected if available)
## ---------------------------
# Ensure Light_Value is truly numeric (and not character/factor)
pr_plot <- pr %>%
  mutate(
    Light_Value = as.numeric(as.character(Light_Value))
  ) %>%
  filter(!is.na(Light_Value)) %>%
  arrange(Light_Value)

# Set x-axis to start at 0 and use a continuous scale
pi_rate_plot <- ggplot(pr_plot,
                       aes(x = Light_Value,
                           y = micromol.s.adj,
                           color = colony_id,
                           group = colony_id)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    limits = c(0, max(pr_plot$Light_Value, na.rm = TRUE)),
    breaks = sort(unique(pr_plot$Light_Value))  # show your actual light steps
  ) +
  labs(
    x = "Light intensity",
    y = "O2 rate (µmol s⁻¹)",
    title = "PI Curve (Volume corrected; blank corrected if available)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(pi_rate_plot)
######## remove outlier 
pr_plot <- pr %>%
  mutate(Light_Value = as.numeric(Light_Value)) %>%
  filter(!(colony_id == "13" & Light_Value == 825))

pi_rate_plot <- ggplot(pr_plot,
                       aes(x = Light_Value,
                           y = micromol.s.adj,
                           color = colony_id,
                           group = colony_id)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(
    limits = c(0, max(pr_plot$Light_Value, na.rm = TRUE)),
    breaks = sort(unique(pr_plot$Light_Value))
  ) +
  labs(
    x = "Light intensity",
    y = "O2 rate (µmol s⁻¹)",
    title = "PI Curve (Outlier removed)"
  ) +
  theme_bw()

print(pi_rate_plot)



## ---------------------------
## Save plot + table
## ---------------------------
ggsave(
  filename = file.path(output_path, "PI_curve_volume_blank_corrected.png"),
  plot = pi_rate_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_path, "PI_curve_volume_blank_corrected.pdf"),
  plot = pi_rate_plot,
  width = 8,
  height = 6
)

cat("PI plot successfully saved to:", output_path, "\n")

############################################################
# Define PI curve function as a nonlinear Least Squares regression of a quadratic fit, test nls fit
#Aquatic Photosynthesis, Falkowski   
#Pmax = max photosynthesis (AKA Am from Bayesian script)  
#alpha = quantum yeild (AKA AQY from Bayesian script)  
#I/E = irradiance (AKA PAR from Bayesian script)  
#Rd = dark respiration  

##########

library(broom)
library(purrr)
library(dplyr)
library(tidyr)

############################################################
## Prepare Data for NLS (Oxygen-based PI curve)
############################################################

nls_input <- pr_plot %>%
  mutate(
    Light_Value = as.numeric(Light_Value),
    Rate = as.numeric(micromol.s.adj)   # O2 rate
  ) %>%
  filter(!is.na(Light_Value), !is.na(Rate)) %>%
  arrange(colony_id, Light_Value)

############################################################
## Falkowski PI model (oxygen-based)
## Rate = Pmax * ((alpha * I) / sqrt(Pmax^2 + (alpha * I)^2)) - Rd
############################################################

fit_pi_nls <- function(dat) {
  
  if (nrow(dat) < 6) {
    return(tibble(Pmax = NA_real_,
                  alpha = NA_real_,
                  Rd = NA_real_))
  }
  
  # Data-driven starting values
  Rd_start   <- max(0, -min(dat$Rate, na.rm = TRUE))
  Pmax_start <- max(dat$Rate + Rd_start, na.rm = TRUE)
  
  low_light <- dat %>%
    filter(Light_Value <= quantile(Light_Value, 0.3))
  
  if (nrow(low_light) >= 2) {
    alpha_start <- coef(lm(Rate ~ Light_Value, data = low_light))[["Light_Value"]]
  } else {
    alpha_start <- 0.001
  }
  
  alpha_start <- max(alpha_start, 1e-6)
  
  mod <- try(
    nls(
      Rate ~ Pmax * ((alpha * Light_Value) /
                       sqrt(Pmax^2 + (alpha * Light_Value)^2)) - Rd,
      data = dat,
      start = list(Pmax = Pmax_start,
                   alpha = alpha_start,
                   Rd = Rd_start),
      algorithm = "port",
      lower = c(Pmax = 0, alpha = 0, Rd = 0)
    ),
    silent = TRUE
  )
  
  if (inherits(mod, "try-error")) {
    return(tibble(Pmax = NA_real_,
                  alpha = NA_real_,
                  Rd = NA_real_))
  }
  
  broom::tidy(mod) %>%
    select(term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate)
}

############################################################
## Fit model per colony
############################################################

pi_pars_nls <- nls_input %>%
  group_by(colony_id) %>%
  nest() %>%
  mutate(pars = map(data, fit_pi_nls)) %>%
  select(-data) %>%
  unnest(pars) %>%
  mutate(
    Ik = Pmax / alpha
  )

print(pi_pars_nls)

write.csv(pi_pars_nls,
          file.path(output_path, "PI_curve_parameters_O2_NLS.csv"),
          row.names = FALSE)

############################################################
## Plot fitted curves over data
############################################################

pred_curves <- nls_input %>%
  group_by(colony_id) %>%
  summarise(Iseq = list(seq(0,
                            max(Light_Value, na.rm = TRUE),
                            length.out = 200)),
            .groups = "drop") %>%
  left_join(pi_pars_nls, by = "colony_id") %>%
  unnest(Iseq) %>%
  mutate(
    Light_Value = Iseq,
    Rate_fit = ifelse(
      is.na(Pmax),
      NA_real_,
      Pmax * ((alpha * Light_Value) /
                sqrt(Pmax^2 + (alpha * Light_Value)^2)) - Rd
    )
  )

pi_fit_plot <- ggplot(pr_plot,
                      aes(Light_Value,
                          micromol.s.adj,
                          color = colony_id,
                          group = colony_id)) +
  geom_point(size = 3) +
  geom_line() +
  geom_line(data = pred_curves,
            aes(y = Rate_fit),
            linewidth = 1) +
  scale_x_continuous(
    limits = c(0, max(pr_plot$Light_Value, na.rm = TRUE)),
    breaks = sort(unique(pr_plot$Light_Value))
  ) +
  labs(
    x = "Light intensity",
    y = expression(O[2]~rate~(mu*mol~s^-1)),
    title = "PI Curve with Falkowski NLS Fit (Oxygen-based)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(pi_fit_plot)

ggsave(file.path(output_path, "PI_curve_O2_NLS_fit.png"),
       pi_fit_plot, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_path, "PI_curve_O2_NLS_fit.pdf"),
       pi_fit_plot, width = 8, height = 6)












