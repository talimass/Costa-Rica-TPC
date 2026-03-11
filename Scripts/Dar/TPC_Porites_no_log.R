# =========================================================
# PORITES rTPC FITTING SCRIPT
# Single-model fitting with rTPC + nls.multstart
# Model used for all curves: quadratic_2008
# Styled plots with white background and no log scale
# =========================================================
# This script reads the PORITES Photo.T file, reformats the data for rTPC,
# fits the same TPC model to each curve, extracts fitted parameters with
# calc_params(), creates predictions, and exports tables and plots.

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
in_dir  <- "C:\\Costa rica-2026\\TPC\\OUTPUT-PORITES"
out_dir <- file.path(in_dir, "rTPC_FITS_quadratic_no_log")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

xlsx_file <- file.path(in_dir, "PORITES_Photo.T.xlsx")
csv_file  <- file.path(in_dir, "PORITES_Photo.T.csv")

# ----------------------------
# COLORS
# ----------------------------
site_colors <- c(
  Papagayo = "#0072B2",
  Samara   = "#E69F00"
)

# ----------------------------
# MODEL TO USE
# ----------------------------
chosen_model <- "quadratic_2008"

# ----------------------------
# READ INPUT
# ----------------------------
if (file.exists(xlsx_file)) {
  dat0 <- read_excel(xlsx_file)
} else if (file.exists(csv_file)) {
  dat0 <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  stop("Could not find PORITES_Photo.T.xlsx or PORITES_Photo.T.csv in: ", in_dir)
}

# ----------------------------
# CHECK COLUMNS
# ----------------------------
req_cols <- c("Site", "Temp.C", "Temp.Cat", "umol.cm2.hr", "rate.type", "fragment_ID")
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
    rate.type = trimws(as.character(rate.type)),
    fragment_ID = trimws(as.character(fragment_ID))
  ) %>%
  filter(rate.type %in% c("NP", "GP", "R")) %>%
  filter(!is.na(Site), !is.na(Temp.C), !is.na(Temp.Cat), !is.na(umol.cm2.hr), !is.na(fragment_ID)) %>%
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
# DEFINE CURVES
# ----------------------------
dat <- dat %>%
  mutate(
    curve_id = paste(Site, fragment_ID, rate.type, sep = "__")
  )

curve_tbl <- dat %>%
  select(curve_id, Site, fragment_ID, rate.type, Temp.C, Temp.Cat, rate_for_fit) %>%
  rename(
    temp = Temp.C,
    rate = rate_for_fit
  ) %>%
  group_by(curve_id, Site, fragment_ID, rate.type) %>%
  nest() %>%
  ungroup()

# ----------------------------
# HELPER: build model formula
# ----------------------------
build_model_formula <- function(model_name) {
  fmls <- formals(get(model_name, envir = asNamespace("rTPC")))
  arg_names <- names(fmls)
  
  est_params <- setdiff(arg_names, "temp")
  fixed_parts <- character(0)
  
  if ("tref" %in% est_params) {
    est_params <- setdiff(est_params, "tref")
    fixed_parts <- c(fixed_parts, "tref = 20")
  }
  
  rhs_parts <- c("temp = temp", est_params, fixed_parts)
  formula_txt <- paste0("rate ~ ", model_name, "(", paste(rhs_parts, collapse = ", "), ")")
  
  list(
    formula = as.formula(formula_txt),
    est_params = est_params
  )
}

# ----------------------------
# HELPER: fit one model to one curve
# ----------------------------
fit_one_model <- function(d, model_name) {
  
  if (nrow(d) < 4) return(NULL)
  if (dplyr::n_distinct(d$temp) < 4) return(NULL)
  
  model_bits <- build_model_formula(model_name)
  est_params <- model_bits$est_params
  model_formula <- model_bits$formula
  
  if (length(est_params) == 0) return(NULL)
  
  start_vals <- tryCatch(
    rTPC::get_start_vals(d$temp, d$rate, model_name = model_name),
    error = function(e) NULL
  )
  lower_lims <- tryCatch(
    rTPC::get_lower_lims(d$temp, d$rate, model_name = model_name),
    error = function(e) NULL
  )
  upper_lims <- tryCatch(
    rTPC::get_upper_lims(d$temp, d$rate, model_name = model_name),
    error = function(e) NULL
  )
  
  if (is.null(start_vals) || is.null(lower_lims) || is.null(upper_lims)) return(NULL)
  
  start_vals <- start_vals[est_params]
  lower_lims <- lower_lims[est_params]
  upper_lims <- upper_lims[est_params]
  
  if (any(is.na(start_vals)) || any(is.na(lower_lims)) || any(is.na(upper_lims))) return(NULL)
  if (any(!is.finite(start_vals)) || any(!is.finite(lower_lims)) || any(!is.finite(upper_lims))) return(NULL)
  
  fit <- tryCatch(
    nls.multstart::nls_multstart(
      formula = model_formula,
      data = d,
      iter = rep(4, length(est_params)),
      start_lower = start_vals - 10,
      start_upper = start_vals + 10,
      lower = lower_lims,
      upper = upper_lims,
      supp_errors = "Y",
      convergence_count = FALSE
    ),
    error = function(e) NULL
  )
  
  fit
}

safe_fit_one_model <- purrr::possibly(fit_one_model, otherwise = NULL)

# ----------------------------
# HELPER: safe glance for fitted model
# ----------------------------
safe_glance <- function(fit_obj) {
  if (is.null(fit_obj)) {
    return(tibble(AIC = NA_real_, BIC = NA_real_, sigma = NA_real_, nobs = NA_real_))
  }
  
  out <- tryCatch(
    broom::glance(fit_obj),
    error = function(e) tibble(AIC = NA_real_, BIC = NA_real_, sigma = NA_real_, nobs = NA_real_)
  )
  
  needed <- c("AIC", "BIC", "sigma", "nobs")
  for (nm in needed) {
    if (!nm %in% names(out)) out[[nm]] <- NA_real_
  }
  
  out %>% select(AIC, BIC, sigma, nobs)
}

# ----------------------------
# FIT THE SINGLE MODEL TO ALL CURVES
# ----------------------------
fit_tbl <- curve_tbl %>%
  mutate(
    fitted_model = chosen_model,
    n_points = purrr::map_int(data, nrow),
    n_temps  = purrr::map_int(data, ~dplyr::n_distinct(.x$temp)),
    fit = purrr::map(data, ~{
      if (nrow(.x) < 4 || dplyr::n_distinct(.x$temp) < 4) {
        return(NULL)
      } else {
        safe_fit_one_model(.x, chosen_model)
      }
    }),
    glance = purrr::map(fit, safe_glance)
  ) %>%
  tidyr::unnest_wider(glance, names_sep = "_") %>%
  rename(
    AIC = glance_AIC,
    BIC = glance_BIC,
    sigma = glance_sigma,
    nobs = glance_nobs
  )

# ----------------------------
# TABLES SAFE FOR CSV
# ----------------------------
fit_tbl_export <- fit_tbl %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, n_points, n_temps, AIC, BIC, sigma, nobs)

write.csv(
  fit_tbl_export,
  file.path(out_dir, "PORITES_quadratic_all_fits.csv"),
  row.names = FALSE
)

# ----------------------------
# KEEP ONLY SUCCESSFUL FITS
# ----------------------------
best_tbl <- fit_tbl %>%
  filter(!is.na(AIC))

if (nrow(best_tbl) == 0) {
  stop("No curves were successfully fitted with quadratic_2008.")
}

best_tbl_export <- best_tbl %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, n_points, n_temps, AIC, BIC, sigma, nobs)

write.csv(
  best_tbl_export,
  file.path(out_dir, "PORITES_quadratic_successful_fits.csv"),
  row.names = FALSE
)

failed_tbl <- fit_tbl %>%
  filter(is.na(AIC)) %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, n_points, n_temps)

write.csv(
  failed_tbl,
  file.path(out_dir, "PORITES_quadratic_failed_fits.csv"),
  row.names = FALSE
)

# ----------------------------
# EXTRACT MODEL COEFFICIENTS
# ----------------------------
coef_tbl <- best_tbl %>%
  mutate(
    coefficients = purrr::map(
      fit,
      ~{
        if (is.null(.x)) tibble()
        else broom::tidy(.x)
      }
    )
  ) %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, coefficients) %>%
  unnest(coefficients)

write.csv(
  coef_tbl,
  file.path(out_dir, "PORITES_quadratic_coefficients.csv"),
  row.names = FALSE
)

# ----------------------------
# EXTRACT EXTRA TPC PARAMETERS
# ----------------------------
params_tbl <- best_tbl %>%
  mutate(
    params = purrr::map(
      fit,
      ~{
        if (is.null(.x)) {
          tibble()
        } else {
          tryCatch(
            suppressWarnings(rTPC::calc_params(.x)),
            error = function(e) tibble()
          )
        }
      }
    )
  ) %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, params) %>%
  unnest(params)

write.csv(
  params_tbl,
  file.path(out_dir, "PORITES_quadratic_calc_params.csv"),
  row.names = FALSE
)

# ----------------------------
# CREATE PREDICTIONS
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
        tryCatch(
          broom::augment(.x, newdata = new_data),
          error = function(e) tibble()
        )
      }
    )
  ) %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, predictions) %>%
  unnest(predictions) %>%
  rename(
    Temp.C = temp,
    fitted_rate = .fitted
  )

pred_tbl_export <- pred_tbl %>%
  select(curve_id, Site, fragment_ID, rate.type, fitted_model, Temp.C, fitted_rate)

write.csv(
  pred_tbl_export,
  file.path(out_dir, "PORITES_quadratic_predictions.csv"),
  row.names = FALSE
)

# ----------------------------
# EXPORT TO EXCEL
# ----------------------------
write_xlsx(
  list(
    raw_data = dat,
    successful_fits = best_tbl_export,
    failed_fits = failed_tbl,
    coefficients = coef_tbl,
    calc_params = params_tbl,
    predictions = pred_tbl_export
  ),
  file.path(out_dir, "PORITES_quadratic_rTPC_outputs.xlsx")
)

# ----------------------------
# PLOT HELPER
# ----------------------------
make_rtpc_plot <- function(rate_name, plot_title, out_name) {
  
  raw_sub <- dat %>%
    filter(rate.type == rate_name)
  
  pred_sub <- pred_tbl_export %>%
    filter(rate.type == rate_name)
  
  if (nrow(raw_sub) == 0 || nrow(pred_sub) == 0) {
    warning(paste0("No data/predictions found for rate type: ", rate_name))
    return(NULL)
  }
  
  ribbon_tbl <- raw_sub %>%
    group_by(curve_id) %>%
    summarise(
      curve_sd = sd(rate_for_fit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      curve_sd = ifelse(is.na(curve_sd), 0, curve_sd)
    )
  
  pred_sub <- pred_sub %>%
    left_join(ribbon_tbl, by = "curve_id") %>%
    mutate(
      curve_sd = ifelse(is.na(curve_sd), 0, curve_sd),
      y_low  = fitted_rate - curve_sd,
      y_high = fitted_rate + curve_sd,
      y_low2  = fitted_rate - 1.96 * curve_sd,
      y_high2 = fitted_rate + 1.96 * curve_sd
    )
  
  raw_sub <- raw_sub %>%
    mutate(fragment_panel = paste0(as.character(Site), " | ", fragment_ID))
  
  pred_sub <- pred_sub %>%
    mutate(fragment_panel = paste0(as.character(Site), " | ", fragment_ID))
  
  panel_levels <- raw_sub %>%
    distinct(Site, fragment_ID, fragment_panel) %>%
    arrange(Site, fragment_ID) %>%
    pull(fragment_panel)
  
  raw_sub$fragment_panel  <- factor(raw_sub$fragment_panel, levels = panel_levels)
  pred_sub$fragment_panel <- factor(pred_sub$fragment_panel, levels = panel_levels)
  
  p <- ggplot() +
    geom_ribbon(
      data = pred_sub,
      aes(x = Temp.C, ymin = y_low, ymax = y_high, fill = Site, group = curve_id),
      alpha = 0.22,
      color = NA
    ) +
    geom_line(
      data = pred_sub,
      aes(x = Temp.C, y = y_low2, color = Site, group = curve_id),
      linewidth = 0.8,
      linetype = "dashed",
      alpha = 0.7
    ) +
    geom_line(
      data = pred_sub,
      aes(x = Temp.C, y = y_high2, color = Site, group = curve_id),
      linewidth = 0.8,
      linetype = "dashed",
      alpha = 0.7
    ) +
    geom_line(
      data = pred_sub,
      aes(x = Temp.C, y = fitted_rate, color = Site, group = curve_id),
      linewidth = 1.35
    ) +
    geom_point(
      data = raw_sub,
      aes(x = Temp.C, y = rate_for_fit, color = Site),
      alpha = 0.75,
      size = 2.2
    ) +
    scale_color_manual(values = site_colors, drop = FALSE) +
    scale_fill_manual(values = site_colors, drop = FALSE) +
    facet_wrap(~ fragment_panel, scales = "free_y") +
    labs(
      title = plot_title,
      x = "Temperature °C",
      y = expression(paste(mu, "mol O"[2], " ", cm^{-2}, " ", h^{-1})),
      color = NULL,
      fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = 9, face = "bold"),
      axis.line = element_line(color = "black", linewidth = 0.7),
      axis.ticks = element_line(color = "black"),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white", color = NA)
    )
  
  n_frag <- dplyr::n_distinct(raw_sub$fragment_panel)
  ncol_plot <- ifelse(n_frag <= 4, 2, ifelse(n_frag <= 9, 3, 4))
  nrow_plot <- ceiling(n_frag / ncol_plot)
  plot_height <- max(5.5, nrow_plot * 3.2)
  
  ggsave(
    filename = file.path(out_dir, out_name),
    plot = p,
    width = 14,
    height = plot_height,
    dpi = 300
  )
  
  p
}

# ----------------------------
# MAKE PLOTS
# ----------------------------
p_np <- make_rtpc_plot(
  rate_name = "NP",
  plot_title = "Porites NP fitted with quadratic_2008",
  out_name = "PORITES_quadratic_NP_by_fragment.png"
)

p_r <- make_rtpc_plot(
  rate_name = "R",
  plot_title = "Porites R fitted with quadratic_2008",
  out_name = "PORITES_quadratic_R_by_fragment.png"
)

p_gp <- make_rtpc_plot(
  rate_name = "GP",
  plot_title = "Porites GP fitted with quadratic_2008",
  out_name = "PORITES_quadratic_GP_by_fragment.png"
)

# ----------------------------
# OPTIONAL CONSOLE OUTPUT
# ----------------------------
print(
  best_tbl_export %>%
    select(curve_id, Site, fragment_ID, rate.type, fitted_model, AIC, BIC)
)

message("Done. Outputs written to: ", out_dir)