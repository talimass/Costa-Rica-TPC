# =========================================================
# POCILLOPORA rTPC SITE-MEAN SCRIPT
# Mean by site with quadratic_2008
# White background, no log scale
# =========================================================

rm(list = ls())

# ----------------------------
# Packages
# ----------------------------
pkgs <- c("tidyverse", "readxl", "writexl", "rTPC", "nls.multstart", "broom")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(tidyverse)
library(readxl)
library(writexl)
library(rTPC)
library(nls.multstart)
library(broom)

# ----------------------------
# PATHS
# ----------------------------
in_dir  <- "C:\\Costa rica-2026\\TPC\\Out_R"
out_dir <- file.path(in_dir, "rTPC_SITE_MEAN_quadratic_no_log")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

xlsx_file <- file.path(in_dir, "POCILLOPORA_Photo.T.xlsx")
csv_file  <- file.path(in_dir, "POCILLOPORA_Photo.T.csv")

# ----------------------------
# COLORS
# ----------------------------
site_colors <- c(
  Papagayo = "#0072B2",
  Samara   = "#E69F00"
)

chosen_model <- "quadratic_2008"

# ----------------------------
# READ DATA
# ----------------------------
if (file.exists(xlsx_file)) {
  dat0 <- read_excel(xlsx_file)
} else if (file.exists(csv_file)) {
  dat0 <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  stop("Could not find POCILLOPORA_Photo.T.xlsx or POCILLOPORA_Photo.T.csv")
}

# ----------------------------
# CHECK COLUMNS
# ----------------------------
req_cols <- c("Site", "Temp.C", "Temp.Cat", "umol.cm2.hr", "rate.type")
miss <- setdiff(req_cols, names(dat0))
if (length(miss) > 0) {
  stop("Missing required columns: ", paste(miss, collapse = ", "))
}

# ----------------------------
# CLEAN DATA
# ----------------------------
dat <- dat0 %>%
  mutate(
    Site = factor(trimws(as.character(Site)), levels = c("Papagayo", "Samara")),
    Temp.C = suppressWarnings(as.numeric(Temp.C)),
    Temp.Cat = suppressWarnings(as.numeric(Temp.Cat)),
    umol.cm2.hr = suppressWarnings(as.numeric(umol.cm2.hr)),
    rate.type = trimws(as.character(rate.type))
  ) %>%
  filter(rate.type %in% c("NP", "GP", "R")) %>%
  filter(!is.na(Site), !is.na(Temp.C), !is.na(Temp.Cat), !is.na(umol.cm2.hr)) %>%
  filter(is.finite(umol.cm2.hr)) %>%
  mutate(
    rate_for_fit = case_when(
      rate.type == "R" ~ abs(umol.cm2.hr),
      TRUE ~ umol.cm2.hr
    )
  ) %>%
  filter(!is.na(rate_for_fit), is.finite(rate_for_fit))

if (nrow(dat) == 0) {
  stop("No valid rows found after filtering.")
}

# ----------------------------
# SITE MEAN TABLE
# ----------------------------
mean_dat <- dat %>%
  group_by(Site, rate.type, Temp.Cat) %>%
  summarise(
    Temp.C = mean(Temp.C, na.rm = TRUE),
    mean_rate = mean(rate_for_fit, na.rm = TRUE),
    sd_rate = sd(rate_for_fit, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    curve_id = paste(Site, rate.type, sep = "__")
  )

# ----------------------------
# PREPARE CURVES
# ----------------------------
curve_tbl <- mean_dat %>%
  select(curve_id, Site, rate.type, Temp.C, mean_rate) %>%
  rename(
    temp = Temp.C,
    rate = mean_rate
  ) %>%
  group_by(curve_id, Site, rate.type) %>%
  nest() %>%
  ungroup()

# ----------------------------
# HELPER: FIT ONE CURVE
# ----------------------------
fit_one_model <- function(d) {
  if (nrow(d) < 4) return(NULL)
  if (dplyr::n_distinct(d$temp) < 4) return(NULL)
  
  start_vals <- tryCatch(rTPC::get_start_vals(d$temp, d$rate, chosen_model), error = function(e) NULL)
  lower_lims <- tryCatch(rTPC::get_lower_lims(d$temp, d$rate, chosen_model), error = function(e) NULL)
  upper_lims <- tryCatch(rTPC::get_upper_lims(d$temp, d$rate, chosen_model), error = function(e) NULL)
  
  if (is.null(start_vals) || is.null(lower_lims) || is.null(upper_lims)) return(NULL)
  
  tryCatch(
    nls.multstart::nls_multstart(
      rate ~ quadratic_2008(temp = temp, a, b, c),
      data = d,
      iter = 4,
      start_lower = start_vals - 10,
      start_upper = start_vals + 10,
      lower = lower_lims,
      upper = upper_lims,
      supp_errors = "Y",
      convergence_count = FALSE
    ),
    error = function(e) NULL
  )
}

safe_glance <- function(fit_obj) {
  if (is.null(fit_obj)) return(tibble(AIC = NA_real_, BIC = NA_real_, sigma = NA_real_, nobs = NA_real_))
  out <- tryCatch(
    broom::glance(fit_obj),
    error = function(e) tibble(AIC = NA_real_, BIC = NA_real_, sigma = NA_real_, nobs = NA_real_)
  )
  needed <- c("AIC", "BIC", "sigma", "nobs")
  for (nm in needed) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  out %>% select(AIC, BIC, sigma, nobs)
}

# ----------------------------
# FIT CURVES
# ----------------------------
fit_tbl <- curve_tbl %>%
  mutate(
    fitted_model = chosen_model,
    fit = purrr::map(data, fit_one_model),
    glance = purrr::map(fit, safe_glance)
  ) %>%
  tidyr::unnest_wider(glance, names_sep = "_") %>%
  rename(
    AIC = glance_AIC,
    BIC = glance_BIC,
    sigma = glance_sigma,
    nobs = glance_nobs
  )

best_tbl <- fit_tbl %>%
  filter(!is.na(AIC))

if (nrow(best_tbl) == 0) {
  stop("No site-mean curves were successfully fitted.")
}

# ----------------------------
# PREDICTIONS
# ----------------------------
pred_tbl <- best_tbl %>%
  mutate(
    predictions = purrr::map2(
      fit, data,
      ~{
        if (is.null(.x)) return(tibble())
        new_data <- tibble(
          temp = seq(min(.y$temp, na.rm = TRUE), max(.y$temp, na.rm = TRUE), length.out = 200)
        )
        tryCatch(broom::augment(.x, newdata = new_data), error = function(e) tibble())
      }
    )
  ) %>%
  select(curve_id, Site, rate.type, fitted_model, predictions) %>%
  unnest(predictions) %>%
  rename(
    Temp.C = temp,
    fitted_rate = .fitted
  )

pred_tbl_export <- pred_tbl %>%
  select(curve_id, Site, rate.type, fitted_model, Temp.C, fitted_rate)

# ----------------------------
# COEFFICIENTS
# ----------------------------
coef_tbl <- best_tbl %>%
  mutate(coefficients = purrr::map(fit, ~ if (is.null(.x)) tibble() else broom::tidy(.x))) %>%
  select(curve_id, Site, rate.type, fitted_model, coefficients) %>%
  unnest(coefficients)

# ----------------------------
# PARAMETERS
# ----------------------------
params_tbl <- best_tbl %>%
  mutate(
    params = purrr::map(
      fit,
      ~{
        if (is.null(.x)) tibble()
        else tryCatch(suppressWarnings(rTPC::calc_params(.x)), error = function(e) tibble())
      }
    )
  ) %>%
  select(curve_id, Site, rate.type, fitted_model, params) %>%
  unnest(params)

# ----------------------------
# EXPORT TABLES
# ----------------------------
write.csv(mean_dat, file.path(out_dir, "POCILLOPORA_site_mean_raw.csv"), row.names = FALSE)
write.csv(pred_tbl_export, file.path(out_dir, "POCILLOPORA_site_mean_predictions.csv"), row.names = FALSE)
write.csv(coef_tbl, file.path(out_dir, "POCILLOPORA_site_mean_coefficients.csv"), row.names = FALSE)
write.csv(params_tbl, file.path(out_dir, "POCILLOPORA_site_mean_calc_params.csv"), row.names = FALSE)

write_xlsx(
  list(
    site_mean_raw = mean_dat,
    successful_fits = best_tbl %>% select(curve_id, Site, rate.type, fitted_model, AIC, BIC, sigma, nobs),
    coefficients = coef_tbl,
    calc_params = params_tbl,
    predictions = pred_tbl_export
  ),
  file.path(out_dir, "POCILLOPORA_site_mean_rTPC_outputs.xlsx")
)

# ----------------------------
# PLOT HELPER
# ----------------------------
make_site_plot <- function(rate_name, plot_title, out_name) {
  raw_sub <- mean_dat %>% filter(rate.type == rate_name)
  pred_sub <- pred_tbl_export %>% filter(rate.type == rate_name)
  
  if (nrow(raw_sub) == 0 || nrow(pred_sub) == 0) {
    warning(paste0("No data/predictions for: ", rate_name))
    return(NULL)
  }
  
  p <- ggplot() +
    geom_line(
      data = pred_sub,
      aes(x = Temp.C, y = fitted_rate, color = Site),
      linewidth = 1.4
    ) +
    geom_point(
      data = raw_sub,
      aes(x = Temp.C, y = mean_rate, color = Site),
      size = 2.5,
      alpha = 0.9
    ) +
    geom_errorbar(
      data = raw_sub,
      aes(x = Temp.C, ymin = mean_rate - sd_rate, ymax = mean_rate + sd_rate, color = Site),
      width = 0.25,
      linewidth = 0.7
    ) +
    scale_color_manual(values = site_colors, drop = FALSE) +
    labs(
      title = plot_title,
      x = "Temperature °C",
      y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
      color = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = file.path(out_dir, out_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  p
}

# ----------------------------
# MAKE PLOTS
# ----------------------------
make_site_plot("NP", "Pocillopora NP site mean", "POCILLOPORA_site_mean_NP.png")
make_site_plot("R",  "Pocillopora R site mean",  "POCILLOPORA_site_mean_R.png")
make_site_plot("GP", "Pocillopora GP site mean", "POCILLOPORA_site_mean_GP.png")

message("Done. Outputs written to: ", out_dir)