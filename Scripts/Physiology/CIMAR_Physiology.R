# ==============================================================================
# CIMAR Coral Physiology — Costa Rica, Feb 2026  (PhD Chapter 2, TPC experiment)
# Endosymbiont density & chlorophyll a (protein to be added when finalized)
# Pocillopora & Porites | Samara (non-upwelling) & Papagayo (upwelling)
#
# Design: each fragment was sampled before (T0) and after (END) an acute
# ascending thermal ramp (~45 min per step, up to 34 °C) -> repeated measures.
#
# Statistics (one model per response variable):
#   Generalized linear mixed model, Gamma distribution, log link
#     Value ~ Timepoint * Site * Species + (1 | Fragment)
#   - Gamma/log: response is strictly positive and right-skewed, with variance
#     increasing with the mean (normality/homogeneity of an LMM were violated).
#   - Random intercept for Fragment accounts for the paired (T0/END) structure.
#   - Heteroscedasticity: a second model lets the Gamma dispersion differ among
#     Site x Species groups (dispformula = ~ Site * Species). It is retained only
#     if a likelihood-ratio test shows a better fit (p < 0.05); otherwise the
#     constant-dispersion model is used.
#   - Fixed effects: Type III Wald chi-square tests (sum-to-zero contrasts).
#   - Planned post-hoc contrasts (emmeans):
#       (1) END vs T0 within each Site x Species          -> ratio END/T0
#       (2) Papagayo vs Samara in the END/T0 ratio, per species
#     p-values adjusted with Benjamini–Hochberg within each response.
#   - Model validation: simulation-based (DHARMa) residuals — uniformity,
#     dispersion, outliers, and residuals vs. groups.
#
# Input : Data/Physiology/Physiology_CIMAR_clean.xlsx  (sheets: Endosym, Chl, [Protein])
# Output: Output/Physiology/
#   Fig1_T0_vs_END.png/.pdf          main figure
#   Fig2_Response_by_site.png/.pdf   site comparison of the thermal response
#   Results_statistics.xlsx          all tables
#   Model_diagnostics.pdf            residual checks (for supplementary)
# ==============================================================================

# ---- 0. PACKAGES -------------------------------------------------------------
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
          "glmmTMB", "car", "emmeans", "DHARMa", "openxlsx")
missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing)) install.packages(missing)
for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
select <- dplyr::select

# ---- 1. CONFIGURATION --------------------------------------------------------
# Paths are relative to the repository root (Costa-Rica-TPC).
# If the script is run from Scripts/Physiology, move up to the root automatically.
DATA_FILE <- "Data/Physiology/Physiology_CIMAR_clean.xlsx"
OUT_DIR   <- "Output/Physiology"
if (!file.exists(DATA_FILE) && file.exists(file.path("../..", DATA_FILE))) setwd("../..")
stopifnot("Data file not found - set the working directory to the Costa-Rica-TPC folder" = file.exists(DATA_FILE))
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

ALPHA          <- 0.05
EXCLUDE_CHL_QC <- character(0)   # e.g. c("TURBID") to drop flagged chlorophyll
set.seed(2026)                   # reproducible simulation-based diagnostics

METRICS <- c(                    # column -> panel title
  Cells_cm2    = "Symbiont density",
  Chla_ug_cm2  = "Chlorophyll a per area",
  Chla_pg_cell = "Chlorophyll a per cell"
  # Protein_ug_cm2 = "Protein density"   # <- activate once protein is final
)
YLABS <- list(                   # plotmath axis labels
  Cells_cm2      = "Symbionts~(10^6~cells~cm^{-2})",
  Chla_ug_cm2    = "Chl~italic(a)~(mu*g~cm^{-2})",
  Chla_pg_cell   = "Chl~italic(a)~(pg~cell^{-1})",
  Protein_ug_cm2 = "Protein~(mu*g~cm^{-2})"
)
YSCALE <- c(Cells_cm2 = 1e-6)    # display scaling only (statistics use raw values)

TIME_LEVELS    <- c("T0", "END")
SITE_LEVELS    <- c("Samara", "Papagayo")
SPECIES_LEVELS <- c("Pocillopora", "Porites")
SITE_COLORS    <- c(Samara = "#E69F00", Papagayo = "#0072B2")   # as in all manuscript figures
# T0 = light tint of the site colour, END = full site colour
tint <- function(col, f = 0.35) colorRampPalette(c("white", col))(100)[round(f * 100)]
GROUP_COLORS <- c("Samara.T0"   = tint(SITE_COLORS[["Samara"]]),   "Samara.END"   = SITE_COLORS[["Samara"]],
                  "Papagayo.T0" = tint(SITE_COLORS[["Papagayo"]]), "Papagayo.END" = SITE_COLORS[["Papagayo"]])
GROUP_LABELS <- c("Samara.T0" = "Samara T0", "Samara.END" = "Samara END",
                  "Papagayo.T0" = "Papagayo T0", "Papagayo.END" = "Papagayo END")

theme_set(
  theme_classic(base_size = 13, base_family = "serif") +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.text  = element_text(colour = "black"),
          legend.title = element_blank(), legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold.italic", size = 12))
)
p_to_star <- function(p) ifelse(is.na(p) | p >= ALPHA, "",
                         ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", "*")))

# ==============================================================================
# 2. DATA
# ==============================================================================
endo <- read_excel(DATA_FILE, sheet = "Endosym")
chl  <- read_excel(DATA_FILE, sheet = "Chl")
if (length(EXCLUDE_CHL_QC)) {
  bad <- grepl(paste(EXCLUDE_CHL_QC, collapse = "|"), chl$QC_flag)
  chl$Chla_ug_cm2[bad] <- NA; chl$Chla_pg_cell[bad] <- NA
}
dat <- endo %>%
  select(Fragment_ID, Site, Species, Timepoint, Cells_cm2) %>%
  left_join(chl %>% select(Fragment_ID, Timepoint, Chla_ug_cm2, Chla_pg_cell),
            by = c("Fragment_ID", "Timepoint"))
if ("Protein" %in% excel_sheets(DATA_FILE))
  dat <- dat %>% left_join(read_excel(DATA_FILE, sheet = "Protein") %>%
                             select(Fragment_ID, Timepoint, any_of("Protein_ug_cm2")),
                           by = c("Fragment_ID", "Timepoint"))

long <- dat %>%
  pivot_longer(all_of(names(METRICS)), names_to = "Metric", values_to = "Value") %>%
  filter(!is.na(Value), Value > 0) %>%
  mutate(Metric = factor(Metric, names(METRICS)),
         Timepoint = factor(Timepoint, TIME_LEVELS),
         Site = factor(Site, SITE_LEVELS),
         Species = factor(Species, SPECIES_LEVELS),
         Fragment_ID = factor(Fragment_ID))

# ==============================================================================
# 3. MODELS, TESTS, CONTRASTS, DIAGNOSTICS
# ==============================================================================
options(contrasts = c("contr.sum", "contr.poly"))
res <- list(desc = list(), anova = list(), time = list(), site = list(), diag = list())
pdf(file.path(OUT_DIR, "Model_diagnostics.pdf"), width = 10, height = 5, family = "serif")

for (m in names(METRICS)) {
  d <- long %>% filter(Metric == m)
  fit0 <- glmmTMB(Value ~ Timepoint * Site * Species + (1 | Fragment_ID),
                  family = Gamma(link = "log"), data = d)
  fit1 <- glmmTMB(Value ~ Timepoint * Site * Species + (1 | Fragment_ID),
                  dispformula = ~ Site * Species, family = Gamma(link = "log"), data = d)
  lrt <- anova(fit0, fit1)
  lrt_p <- lrt$`Pr(>Chisq)`[2]
  use_disp <- isTRUE(fit1$sdr$pdHess) && !is.na(lrt_p) && lrt_p < ALPHA
  fit <- if (use_disp) fit1 else fit0
  cat("\n", m, ": dispersion model LRT p =", signif(lrt_p, 3),
      "->", if (use_disp) "group-specific dispersion" else "constant dispersion", "\n")

  # (a) fixed effects — Type III Wald chi-square
  at <- car::Anova(fit, type = 3)
  res$anova[[m]] <- data.frame(Response = METRICS[[m]], Term = rownames(at),
                               Chisq = at$Chisq, Df = at$Df, p_value = at$`Pr(>Chisq)`) %>%
    filter(Term != "(Intercept)")

  # (b) END vs T0 within each Site x Species (ratio END/T0)
  emm <- emmeans(fit, ~ Timepoint | Site * Species, type = "response")
  tc <- summary(contrast(emm, method = "revpairwise"), infer = TRUE, adjust = "none") %>%
    as.data.frame()
  res$time[[m]] <- tc %>%
    transmute(Response = METRICS[[m]], Site, Species, Contrast = "END / T0",
              Ratio = ratio, CI_low = asymp.LCL, CI_high = asymp.UCL,
              Change_pct = 100 * (ratio - 1), z = z.ratio, p_value = p.value) %>%
    mutate(p_adj_BH = p.adjust(p_value, "BH"), Sig = p_to_star(p_adj_BH))

  # (c) does the thermal response differ between sites? (ratio of ratios)
  emm2 <- emmeans(fit, ~ Timepoint * Site | Species, type = "response")
  ic <- summary(contrast(emm2, interaction = c("revpairwise", "revpairwise")),
                infer = TRUE, adjust = "none") %>% as.data.frame()
  res$site[[m]] <- ic %>%
    transmute(Response = METRICS[[m]], Species,
              Contrast = "(END/T0 Papagayo) / (END/T0 Samara)",
              Ratio = ratio, CI_low = asymp.LCL, CI_high = asymp.UCL, z = z.ratio, p_value = p.value) %>%
    mutate(p_adj_BH = p.adjust(p_value, "BH"), Sig = p_to_star(p_adj_BH))

  # (d) simulation-based residual diagnostics (DHARMa)
  sim <- simulateResiduals(fit, n = 1000)
  plot(sim, title = paste("DHARMa residuals —", METRICS[[m]]))
  par(mfrow = c(1, 2))
  plotResiduals(sim, form = interaction(d$Timepoint, d$Site, d$Species), main = "Residuals vs group")
  plotResiduals(sim, form = d$Fragment_ID, main = "Residuals vs fragment", xlab = "Fragment")
  par(mfrow = c(1, 1))
  res$diag[[m]] <- data.frame(
    Response = METRICS[[m]], N = nobs(fit), Fragments = nlevels(droplevels(d$Fragment_ID)),
    KS_uniformity_p = testUniformity(sim, plot = FALSE)$p.value,
    Dispersion_p    = testDispersion(sim, plot = FALSE)$p.value,
    Outlier_p       = testOutliers(sim, type = "bootstrap", plot = FALSE)$p.value,
    Homogeneity_p   = testCategorical(sim, catPred = interaction(d$Timepoint, d$Site, d$Species),
                                      plot = FALSE)$homogeneity$`Pr(>F)`[1],
    Converged = fit$sdr$pdHess,
    Dispersion_model = if (use_disp) "~ Site * Species" else "~ 1",
    Dispersion_LRT_p = lrt_p)

  # (e) descriptive statistics
  res$desc[[m]] <- d %>% group_by(Site, Species, Timepoint) %>%
    summarise(N = n(), Mean = mean(Value), SD = sd(Value), SE = SD / sqrt(N),
              Median = median(Value), .groups = "drop") %>%
    mutate(Response = METRICS[[m]], .before = 1)
}
dev.off()
options(contrasts = c("contr.treatment", "contr.poly"))

tabs <- lapply(res, bind_rows)
tabs$diag <- tabs$diag %>%
  mutate(Assumptions_OK = KS_uniformity_p > ALPHA & Dispersion_p > ALPHA &
                          Outlier_p > ALPHA & Homogeneity_p > ALPHA & Converged)

cat("\n=== Fixed effects (Type III Wald chi-square) ===\n");  print(tabs$anova, digits = 3)
cat("\n=== END vs T0 within groups ===\n");                   print(tabs$time,  digits = 3)
cat("\n=== Site difference in thermal response ===\n");       print(tabs$site,  digits = 3)
cat("\n=== Model diagnostics (DHARMa) ===\n");                print(tabs$diag,  digits = 3)

wb <- createWorkbook()
for (s in list(c("Descriptives", "desc"), c("Model_tests", "anova"),
               c("END_vs_T0", "time"), c("Site_response", "site"), c("Diagnostics", "diag"))) {
  addWorksheet(wb, s[1]); writeData(wb, s[1], tabs[[s[2]]], headerStyle = createStyle(textDecoration = "bold"))
  setColWidths(wb, s[1], cols = 1:ncol(tabs[[s[2]]]), widths = "auto")
}
addWorksheet(wb, "Methods")
writeData(wb, "Methods", data.frame(Note = c(
  "Model: glmmTMB, Gamma(log), Value ~ Timepoint * Site * Species + (1 | Fragment).",
  "Dispersion: group-specific (~ Site * Species) if LRT vs constant dispersion p < 0.05 (see Diagnostics sheet).",
  "Fixed effects: Type III Wald chi-square (car::Anova), sum-to-zero contrasts.",
  "END_vs_T0: emmeans ratio END/T0 within each Site x Species; BH-adjusted within response.",
  "Site_response: interaction contrast = ratio of (END/T0) Papagayo vs Samara, per species; BH-adjusted.",
  "Diagnostics: DHARMa simulated residuals (1000 sims): KS uniformity, dispersion, outliers, homogeneity across groups.",
  paste("Run:", Sys.time(), "| R", getRversion()))))
saveWorkbook(wb, file.path(OUT_DIR, "Results_statistics.xlsx"), overwrite = TRUE)

# ==============================================================================
# 4. FIGURES
# ==============================================================================
scale_of <- function(m) if (m %in% names(YSCALE)) YSCALE[[m]] else 1

# Fig 1 — T0 vs END for each site and species (stars: END vs T0, BH-adjusted)
fig1_panel <- function(m) {
  k  <- scale_of(m)
  d  <- long %>% filter(Metric == m) %>%
    mutate(Y = Value * k, Group = factor(paste(Site, Timepoint, sep = "."), names(GROUP_COLORS)))
  st <- tabs$time %>% filter(Response == METRICS[[m]], Sig != "") %>%
    left_join(d %>% group_by(Site, Species) %>% summarise(ymax = max(Y), .groups = "drop"),
              by = c("Site", "Species"))
  ggplot(d, aes(Site, Y, fill = Group, group = interaction(Site, Timepoint))) +
    geom_boxplot(width = 0.7, outlier.shape = NA, colour = "grey15", position = position_dodge(0.8)) +
    geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
               shape = 21, size = 1.8, colour = "black", stroke = 0.3, show.legend = FALSE) +
    geom_text(data = st, aes(Site, ymax * 1.07, label = Sig), inherit.aes = FALSE,
              size = 6, fontface = "bold") +
    facet_wrap(~ Species) +
    scale_fill_manual(values = GROUP_COLORS, labels = GROUP_LABELS, drop = FALSE) +
    guides(fill = guide_legend(nrow = 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
    labs(x = NULL, y = parse(text = YLABS[[m]]), title = METRICS[[m]])
}
fig1 <- wrap_plots(lapply(names(METRICS), fig1_panel), ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "a",
                  caption = expression("Light boxes: T0 (before the thermal ramp); dark boxes: END (after 34"*degree*"C).")) & theme(plot.tag = element_text(face = "bold", size = 16),
                                            legend.position = "bottom")
h1 <- 3.6 * length(METRICS) + 0.6
ggsave(file.path(OUT_DIR, "Fig1_T0_vs_END.png"), fig1, width = 7, height = h1, dpi = 600)
ggsave(file.path(OUT_DIR, "Fig1_T0_vs_END.pdf"), fig1, width = 7, height = h1)

# Fig 2 — thermal response (END/T0) by site; model estimate ± 95% CI over fragment values
fc <- long %>% select(Metric, Fragment_ID, Site, Species, Timepoint, Value) %>%
  pivot_wider(names_from = Timepoint, values_from = Value) %>%
  filter(!is.na(T0), !is.na(END)) %>% mutate(log2FC = log2(END / T0))
est <- tabs$time %>% mutate(Metric = names(METRICS)[match(Response, METRICS)],
                            across(c(Ratio, CI_low, CI_high), log2, .names = "{.col}_l2"))
fig2_panel <- function(m) {
  d  <- fc %>% filter(Metric == m)
  e  <- est %>% filter(Metric == m)
  st <- tabs$site %>% filter(Response == METRICS[[m]], Sig != "") %>%
    left_join(d %>% group_by(Species) %>% summarise(ymax = max(log2FC), .groups = "drop"), by = "Species")
  ggplot(d, aes(Site, log2FC, colour = Site)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_jitter(width = 0.12, size = 1.8, alpha = 0.5) +
    geom_pointrange(data = e, aes(Site, Ratio_l2, ymin = CI_low_l2, ymax = CI_high_l2),
                    inherit.aes = FALSE, size = 0.6, linewidth = 0.9, shape = 23, fill = "white") +
    geom_text(data = st, aes(x = 1.5, y = ymax + 0.5, label = Sig), inherit.aes = FALSE,
              size = 6, fontface = "bold") +
    facet_wrap(~ Species) +
    scale_colour_manual(values = SITE_COLORS, guide = "none") +
    labs(x = NULL, y = expression(log[2]("END / T0")), title = METRICS[[m]])
}
fig2 <- wrap_plots(lapply(names(METRICS), fig2_panel), ncol = 1) +
  plot_annotation(tag_levels = "a",
                  caption = "Points: individual fragments; diamonds: model estimate with 95% CI.\nDashed line: no change between T0 and END.") &
  theme(plot.tag = element_text(face = "bold", size = 16),
        plot.caption = element_text(face = "italic", hjust = 0.5))
ggsave(file.path(OUT_DIR, "Fig2_Response_by_site.png"), fig2, width = 6.5, height = h1, dpi = 600)
ggsave(file.path(OUT_DIR, "Fig2_Response_by_site.pdf"), fig2, width = 6.5, height = h1)

writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"))
cat("\nDone. Outputs in:", normalizePath(OUT_DIR), "\n")
