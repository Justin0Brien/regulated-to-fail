# ==============================================================================
# UK Higher Education Financial Sustainability Model 2026-2035
# ==============================================================================
#
# This script generates print-ready figures for the consolidated financial
# sustainability model, combining income and cost projections:
# 1. Income Composition (stacked area)
# 2. Cost Composition (stacked area)
# 3. Financial Gap Analysis (income vs costs)
# 4. Risk Scenario Analysis (central, high costs, "perfect storm")
#
# Data Sources:
# - Consolidated from "University Projections" and "University Costs" models
# - Assumes balanced budget baseline in 2026 (Income=100, Cost=100)
# - Income weights: Domestic (35%), International (25%), Research (20%), 
#                   Other (15%), TNE (5%)
# - Cost weights: Staff (55%), Estates (12%), Digital (18%), Finance (15%)
#
# ==============================================================================

# --- Load Required Packages ---
packages <- c("ggplot2", "dplyr", "tidyr", "scales", "svglite", "patchwork", "ggtext")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# --- Create Output Directory ---
output_dir <- "fig"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 1. DATA GENERATION ENGINE
# ==============================================================================

years <- 2026:2035

# --- Inflation Vector (RPI 2.8% compounding) ---
rpi <- 0.028
inflation_vector <- (1 + rpi)^(0:9)

# --- INCOME DRIVERS ---
# Weights (sum to 100)
w_dom <- 35   # Domestic Tuition
w_int <- 25   # International Tuition
w_res <- 20   # Research Grants
w_tne <- 5    # TNE
w_oth <- 15   # Other

# Raw Indices (Volume/Real Terms)
idx_dom_vol <- c(1.00, 1.015, 1.030, 1.042, 1.050, 1.040, 1.025, 1.005, 0.985, 0.970)
idx_int_real <- c(1.00, 1.005, 1.010, 1.015, 1.020, 1.025, 1.030, 1.035, 1.040, 1.045)
idx_res_real <- c(1.00, 1.008, 1.015, 1.022, 1.030, 1.025, 1.020, 1.015, 1.010, 1.005)
idx_tne_real <- c(1.00, 1.04, 1.082, 1.125, 1.170, 1.217, 1.265, 1.316, 1.369, 1.423)

# Calculated Nominal Components
inc_dom <- idx_dom_vol * inflation_vector * w_dom
inc_int <- idx_int_real * inflation_vector * w_int
inc_res <- idx_res_real * inflation_vector * w_res
inc_tne <- idx_tne_real * inflation_vector * w_tne
inc_oth <- inflation_vector * w_oth

# Total Income (Central)
total_income <- inc_dom + inc_int + inc_res + inc_tne + inc_oth

# --- COST DRIVERS ---
# Weights (sum to 100)
wc_staff <- 55
wc_est <- 12
wc_dig <- 18
wc_fin <- 15

# Raw Indices (Nominal)
idx_staff <- c(1.00, 1.03, 1.06, 1.09, 1.12, 1.15, 1.18, 1.21, 1.24, 1.28)
idx_est <- c(1.00, 1.02, 1.04, 1.07, 1.11, 1.15, 1.19, 1.23, 1.27, 1.31)
idx_dig <- c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.24, 1.28, 1.32, 1.36)
idx_fin <- c(1.00, 1.00, 1.00, 1.01, 1.02, 1.03, 1.04, 1.05, 1.06, 1.07)

# Calculated Nominal Components
cost_staff <- idx_staff * wc_staff
cost_est <- idx_est * wc_est
cost_dig <- idx_dig * wc_dig
cost_fin <- idx_fin * wc_fin

# Total Cost (Central)
total_cost <- cost_staff + cost_est + cost_dig + cost_fin

# --- SCENARIO CALCULATIONS ---
# Scenario A: Higher Costs (+0.5% drift per year)
cost_drift_a <- 1 + (0.005 * (0:9))
total_cost_a <- total_cost * cost_drift_a

# Scenario B: Higher Costs + Lower Income (-0.5% drift per year)
income_drift_b <- 1 - (0.005 * (0:9))
total_income_b <- total_income * income_drift_b

# Net Positions
net_central <- total_income - total_cost
net_scenario_a <- total_income - total_cost_a  # Central Income - High Cost
net_scenario_b <- total_income_b - total_cost_a  # Low Income - High Cost

# ==============================================================================
# 2. CREATE DATA FRAMES
# ==============================================================================

# Income stack
income_stack_df <- data.frame(
  year = years,
  Domestic = inc_dom,
  International = inc_int,
  Research = inc_res,
  TNE = inc_tne,
  Other = inc_oth
) %>%
  pivot_longer(cols = -year, names_to = "category", values_to = "value")

income_stack_df$category <- factor(income_stack_df$category,
                                   levels = c("Other", "Domestic", "International", "Research", "TNE"))

# Cost stack
cost_stack_df <- data.frame(
  year = years,
  Staff = cost_staff,
  Estates = cost_est,
  Digital = cost_dig,
  Finance = cost_fin
) %>%
  pivot_longer(cols = -year, names_to = "category", values_to = "value")

cost_stack_df$category <- factor(cost_stack_df$category,
                                 levels = c("Finance", "Digital", "Estates", "Staff"))

# Gap analysis
gap_df <- data.frame(
  year = years,
  income = total_income,
  cost = total_cost
)

# Scenario comparison
scenario_df <- data.frame(
  year = rep(years, 3),
  scenario = rep(c("Central Forecast", "Scenario A: High Costs", "Scenario B: Combined Squeeze"), each = 10),
  net = c(net_central, net_scenario_a, net_scenario_b)
)

scenario_df$scenario <- factor(scenario_df$scenario,
                               levels = c("Central Forecast", "Scenario A: High Costs", "Scenario B: Combined Squeeze"))

# ==============================================================================
# 3. DEFINE THEME AND COLOUR PALETTE
# ==============================================================================

theme_projection <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0, 
                                margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0,
                                   margin = margin(b = 15)),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0,
                                  margin = margin(t = 10)),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Income colours
col_domestic <- "#3b82f6"
col_international <- "#4338ca"
col_research <- "#c2410c"
col_tne <- "#f59e0b"
col_other <- "#94a3b8"

# Cost colours
col_staff <- "#e11d48"
col_estates <- "#d97706"
col_digital <- "#0891b2"
col_finance <- "#6366f1"

# Scenario colours
col_central <- "#10b981"
col_scenario_a <- "#d97706"
col_scenario_b <- "#7c3aed"

# ==============================================================================
# 4. FIGURE 1: INCOME COMPOSITION (STACKED AREA)
# ==============================================================================

p1_income_stack <- ggplot(income_stack_df, aes(x = year, y = value, fill = category)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("Other" = col_other, 
                                "Domestic" = col_domestic, 
                                "International" = col_international,
                                "Research" = col_research, 
                                "TNE" = col_tne),
                    labels = c("Other (15%)", "Domestic Fees (35%)", 
                               "International (25%)", "Research (20%)", "TNE (5%)")) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Projected Income Composition",
    subtitle = "Stacked breakdown of Nominal Income. Assumes domestic fees track RPI (2.8%)\nand TNE grows at 4% annually. Note shrinking Domestic share post-2030.",
    x = "Year",
    y = "Nominal Income Index",
    caption = "Sources: HEPI, OBR, DfE, British Council, HESA/UUKi"
  ) +
  theme_projection() +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2))

# Save
ggsave(file.path(output_dir, "uk-he-sustainability-income-stack.png"), p1_income_stack,
       width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-sustainability-income-stack.pdf"), p1_income_stack,
       width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-sustainability-income-stack.svg"), p1_income_stack,
       width = 10, height = 7)

# ==============================================================================
# 5. FIGURE 2: COST COMPOSITION (STACKED AREA)
# ==============================================================================

p2_cost_stack <- ggplot(cost_stack_df, aes(x = year, y = value, fill = category)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("Finance" = col_finance, 
                                "Digital" = col_digital, 
                                "Estates" = col_estates,
                                "Staff" = col_staff),
                    labels = c("Finance (15%)", "Digital (18%)", 
                               "Estates (12%)", "Staff (55%)")) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Projected Cost Composition",
    subtitle = "Stacked breakdown of Nominal Costs. Staff (red) remains dominant, compounding ~3%.\nEstates widens after 2028 due to Net Zero capital requirements.",
    x = "Year",
    y = "Nominal Cost Index",
    caption = "Sources: UCEA, USS/TPS, AUDE, Jisc, OfS"
  ) +
  theme_projection() +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2))

# Save
ggsave(file.path(output_dir, "uk-he-sustainability-cost-stack.png"), p2_cost_stack,
       width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-sustainability-cost-stack.pdf"), p2_cost_stack,
       width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-sustainability-cost-stack.svg"), p2_cost_stack,
       width = 10, height = 7)

# ==============================================================================
# 6. FIGURE 3: FINANCIAL GAP ANALYSIS
# ==============================================================================

p3_gap <- ggplot(gap_df, aes(x = year)) +
  # Surplus/deficit fill between lines
  geom_ribbon(aes(ymin = cost, ymax = income), 
              fill = ifelse(gap_df$income >= gap_df$cost, 
                            alpha("#10b981", 0.2), 
                            alpha("#ef4444", 0.2))) +
  # Income line
  geom_line(aes(y = income), color = col_central, linewidth = 1.5) +
  geom_point(aes(y = income), color = col_central, size = 3, fill = "white", 
             shape = 21, stroke = 1.5) +
  # Cost line (dashed)
  geom_line(aes(y = cost), color = col_staff, linewidth = 1.5, linetype = "dashed") +
  geom_point(aes(y = cost), color = col_staff, size = 3, fill = "white", 
             shape = 21, stroke = 1.5) +
  # Labels for lines
  annotate("text", x = 2035.3, y = tail(gap_df$income, 1), 
           label = "Total Income", size = 3.5, hjust = 0, color = col_central, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(gap_df$cost, 1), 
           label = "Total Costs", size = 3.5, hjust = 0, color = col_staff, fontface = "bold") +
  # Squeeze annotation
  annotate("label", x = 2031.5, y = 112, 
           label = "The 'Squeeze'\n2030-2032",
           size = 2.8, fill = "#fef2f2", color = "#991b1b", 
           fontface = "bold", label.size = 0.3, label.r = unit(3, "pt")) +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(limits = c(95, 135), 
                     breaks = seq(100, 135, 5)) +
  # Labels
  labs(
    title = "Sector Financial Health (Central Forecast)",
    subtitle = "The delta between Total Income (solid) and Total Costs (dashed).\nGreen = surplus; Red = deficit. A 'squeeze' emerges 2030–32 as demographics fall.",
    x = "Year",
    y = "Index (2026 = 100)",
    caption = "Model: Balanced budget assumed for 2026. Real deficits would shift cost line upward."
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-sustainability-gap.png"), p3_gap,
       width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-sustainability-gap.pdf"), p3_gap,
       width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-sustainability-gap.svg"), p3_gap,
       width = 10, height = 7)

# ==============================================================================
# 7. FIGURE 4: RISK SCENARIO ANALYSIS
# ==============================================================================

p4_scenarios <- ggplot(scenario_df, aes(x = year, y = net, color = scenario, linetype = scenario)) +
  # Deficit zone shading
  annotate("rect", xmin = 2025.5, xmax = 2035.5, ymin = -15, ymax = 0,
           fill = alpha("#fecaca", 0.3), color = NA) +
  annotate("text", x = 2026, y = -7, label = "DEFICIT ZONE", 
           size = 3, hjust = 0, color = "#dc2626", fontface = "bold", alpha = 0.6) +
  # Break-even line
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8) +
  # Scenario lines
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, fill = "white", shape = 21, stroke = 1.2) +
  # Colours and linetypes
  scale_color_manual(values = c("Central Forecast" = col_central, 
                                 "Scenario A: High Costs" = col_scenario_a,
                                 "Scenario B: Combined Squeeze" = col_scenario_b)) +
  scale_linetype_manual(values = c("Central Forecast" = "solid",
                                    "Scenario A: High Costs" = "dashed",
                                    "Scenario B: Combined Squeeze" = "dotted")) +
  # End labels
  annotate("text", x = 2035.3, y = tail(net_central, 1), 
           label = paste0("Central\n", round(tail(net_central, 1), 1)), 
           size = 2.5, hjust = 0, color = col_central, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(net_scenario_a, 1), 
           label = paste0("Scenario A\n", round(tail(net_scenario_a, 1), 1)), 
           size = 2.5, hjust = 0, color = col_scenario_a, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(net_scenario_b, 1) - 1, 
           label = paste0("Scenario B\n", round(tail(net_scenario_b, 1), 1)), 
           size = 2.5, hjust = 0, color = col_scenario_b, fontface = "bold") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(limits = c(-15, 8), 
                     breaks = seq(-15, 5, 5)) +
  # Labels
  labs(
    title = "Risk Sensitivity Analysis: The 'Double Squeeze'",
    subtitle = "Net Operating Position (Surplus/Deficit) under three scenarios:\nCentral | Scenario A (+0.5% costs) | Scenario B (+0.5% costs AND −0.5% income)",
    x = "Year",
    y = "Net Surplus / Deficit (Index Points)",
    caption = "Scenarios model cumulative annual drift from central forecast."
  ) +
  theme_projection() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1), linetype = "none")

# Save
ggsave(file.path(output_dir, "uk-he-sustainability-scenarios.png"), p4_scenarios,
       width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-sustainability-scenarios.pdf"), p4_scenarios,
       width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-sustainability-scenarios.svg"), p4_scenarios,
       width = 10, height = 7)

# ==============================================================================
# 8. COMBINED DASHBOARD PANEL
# ==============================================================================

# Create compact versions
p1_compact <- p1_income_stack + 
  labs(subtitle = NULL, caption = NULL, title = "1. Income Composition") +
  theme(plot.title = element_text(size = 11), legend.position = "right",
        legend.text = element_text(size = 7))

p2_compact <- p2_cost_stack + 
  labs(subtitle = NULL, caption = NULL, title = "2. Cost Composition") +
  theme(plot.title = element_text(size = 11), legend.position = "right",
        legend.text = element_text(size = 7))

p3_compact <- p3_gap + 
  labs(subtitle = NULL, caption = NULL, title = "3. Financial Gap (Central)") +
  theme(plot.title = element_text(size = 11))

p4_compact <- p4_scenarios + 
  labs(subtitle = NULL, caption = NULL, title = "4. Risk Scenarios") +
  theme(plot.title = element_text(size = 11), legend.position = "top",
        legend.text = element_text(size = 7))

# Combine
combined_sustainability <- (p1_compact | p2_compact) / 
                           (p3_compact | p4_compact) +
  plot_annotation(
    title = "UK HE Financial Sustainability Model 2026–2035",
    subtitle = "Consolidated projections. Base: 2026 = 100 (Balanced Budget Assumption).",
    caption = "Income Weights: Dom 35%, Intl 25%, Research 20%, Other 15%, TNE 5% | Cost Weights: Staff 55%, Estates 12%, Digital 18%, Finance 15%",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Save
ggsave(file.path(output_dir, "uk-he-sustainability-combined.png"), combined_sustainability,
       width = 16, height = 14, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-sustainability-combined.pdf"), combined_sustainability,
       width = 16, height = 14, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-sustainability-combined.svg"), combined_sustainability,
       width = 16, height = 14)

# ==============================================================================
# 9. KPI SUMMARY TABLE
# ==============================================================================

kpi_2035 <- data.frame(
  Metric = c("Projected 2035 Income", "Projected 2035 Costs", 
             "Net Position (Central)", "Net Position (Scenario A)",
             "Net Position (Scenario B)"),
  Value = c(round(tail(total_income, 1), 1),
            round(tail(total_cost, 1), 1),
            round(tail(net_central, 1), 1),
            round(tail(net_scenario_a, 1), 1),
            round(tail(net_scenario_b, 1), 1)),
  Status = c("Index", "Index", 
             ifelse(tail(net_central, 1) >= 0, "Surplus", "Deficit"),
             ifelse(tail(net_scenario_a, 1) >= 0, "Surplus", "Deficit"),
             ifelse(tail(net_scenario_b, 1) >= 0, "Surplus", "Deficit"))
)

# ==============================================================================
# 10. OUTPUT SUMMARY
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("UK HE SUSTAINABILITY MODEL - Figure Generation Complete\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\n")
cat("Individual figures generated (PNG, PDF, SVG):\n")
cat("  1. uk-he-sustainability-income-stack  - Income composition\n")
cat("  2. uk-he-sustainability-cost-stack    - Cost composition\n")
cat("  3. uk-he-sustainability-gap           - Financial gap analysis\n")
cat("  4. uk-he-sustainability-scenarios     - Risk scenarios\n")
cat("\n")
cat("Combined dashboard:\n")
cat("  5. uk-he-sustainability-combined      - 4-panel dashboard\n")
cat("\n")
cat("2035 KPI Summary:\n")
print(kpi_2035)
cat("\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
