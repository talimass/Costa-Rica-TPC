This project experimentally quantifies species-specific thermal performance curves (TPCs) for dominant coral taxa collected from both upwelling and non-upwelling reef sites. Thermal performance is assessed using the photosynthesis-to-respiration (P:R) ratio measured across a controlled temperature gradient as a proxy for the coral holobiont’s thermal threshold. This approach tests whether chronic exposure to the naturally variable thermal regime of upwelling environments confers broader thermal performance breadth or enhanced thermal tolerance relative to non-upwelling conspecifics. To complement physiological measurements, high-resolution image analysis is performed on corals exposed to the thermal treatments to quantify tissue coloration changes as a visual indicator of coral health and stress. Additionally, chlorophyll concentration and symbiont (zooxanthellae) density are measured to assess photophysiological condition and bleaching-related responses across temperature treatments.

---

## Physiology: symbiont density and chlorophyll *a* (T0 vs END)

*Pocillopora* sp. and *Porites* sp., Papagayo (upwelling) vs Sámara (non-upwelling); 10 fragments per species per site (n = 40). Each fragment was sampled before (T0) and after (END) the acute thermal ramp to 34 °C, so the design is repeated measures.

Full dated record of field work, lab processing, data QC, statistical decisions and results: **[Physiology_Lab_Notebook.md](Physiology_Lab_Notebook.md)**

| Folder | Content |
|---|---|
| `Data/Physiology/` | `Physiology_CIMAR_clean.xlsx` (analysis-ready; sheets `Endosym`, `Chl`, `README`), `Raw/` original lab sheets incl. protein, `Env_Data/` YSI field files, `Protocol/` lab protocol |
| `Scripts/Physiology/` | `CIMAR_Physiology.R` (full analysis); `Archive/` earlier Python notebook |
| `Output/Physiology/` | `Fig1_T0_vs_END`, `Fig2_Response_by_site` (PNG + PDF), `Results_statistics.xlsx`, `Model_diagnostics.pdf`, `sessionInfo.txt` |

**Model (per response):** `glmmTMB(Value ~ Timepoint * Site * Species + (1 | Fragment), family = Gamma(log))`, group-specific dispersion (`~ Site * Species`) kept if LRT p < 0.05; Type III Wald χ²; emmeans contrasts (END/T0 within groups; Papagayo vs Sámara difference in END/T0), BH-adjusted; DHARMa diagnostics (all pass).

**Key results:**
- *Porites* symbiont density: thermal response differed between sites (ratio of ratios 3.45, 95 % CI 1.32–9.05, p_adj = 0.023): Sámara ×0.49, Papagayo ×1.67.
- *Porites* Chl *a* per area: same direction (p_adj = 0.07).
- *Pocillopora* Papagayo Chl *a* per cell increased ×2.2 (p_adj = 0.02); *Pocillopora* symbiont density stable at both sites.

**Reproduce:** open R in the `Costa-Rica-TPC` folder and run `source("Scripts/Physiology/CIMAR_Physiology.R")` (R 4.6.1; missing packages install automatically).

Protein analysis is pending (see the notebook's open issues).
