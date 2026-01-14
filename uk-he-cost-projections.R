# ==============================================================================
# UK Higher Education Cost Projections 2026-2035
# ==============================================================================
#
# This script generates print-ready figures for UK university expenditure
# projections across four key cost drivers:
# 1. Workforce (Staff & Pensions) - ~55-60% of total
# 2. Estates & Energy - Net Zero retrofit burden
# 3. Digital & Administration - "Techflation"
# 4. Financing & Debt Servicing - Interest rate sensitivity
#
# Data Sources:
# - Workforce: UCEA financial health reports 2024, USS/TPS pension data
# - Estates: AUDE estates management 2024/25, Net Zero targets
# - Digital: Jisc digital infrastructure analysis 2024
# - Financing: OfS financial sustainability data
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
# 1. DATA PREPARATION
# ==============================================================================

years <- 2026:2035

# --- 1. Workforce (Staff & Pensions) ---
# ~55-60% of total expenditure
# High: Inflation spirals + Pension deficit contributions return
# Central: CPI (~2.5%) + NI drift
# Low: Austerity pay awards (sub-inflation)
staff_high <- c(100, 105, 110, 116, 122, 129, 136, 144, 153, 162)
staff_central <- c(100, 103, 106, 109, 112, 115, 118, 121, 124, 128)
staff_low <- c(100, 102, 103, 104, 105, 106, 107, 108, 109, 110)

staff_df <- data.frame(
  year = years,
  central = staff_central,
  low = staff_low,
  high = staff_high
)

# --- 2. Estates & Energy ---
# High: Energy spike + Aggressive Retrofit costs
# Central: Energy stable, steady retrofit
# Low: Min maintenance only (kicking the can)
estates_high <- c(100, 104, 110, 118, 128, 138, 148, 155, 162, 170)
estates_central <- c(100, 102, 104, 107, 111, 115, 119, 123, 127, 131)
estates_low <- c(100, 98, 97, 98, 99, 100, 101, 102, 103, 104)

estates_df <- data.frame(
  year = years,
  central = estates_central,
  low = estates_low,
  high = estates_high
)

# --- 3. Digital & Administration ---
# "Techflation": SaaS licensing (USD-pegged), cybersecurity, AI costs
# High: FX crisis (weak pound) + AI licensing boom
# Central: Standard SaaS inflation (5-7%)
# Low: Open source / efficiency savings
digital_high <- c(100, 107, 114, 122, 130, 139, 148, 158, 168, 179)
digital_central <- c(100, 104, 108, 112, 116, 120, 124, 128, 132, 136)
digital_low <- c(100, 102, 103, 104, 105, 106, 106, 107, 107, 108)

digital_df <- data.frame(
  year = years,
  central = digital_central,
  low = digital_low,
  high = digital_high
)

# --- 4. Financing & Debt Servicing ---
# High: Refinancing at 6-7% rates (bond maturity wall in 2031)
# Central: Rates settle at ~3.5-4%
# Low: Rates drop to ~2.5%
finance_high <- c(100, 105, 110, 115, 120, 130, 140, 145, 150, 155)
finance_central <- c(100, 100, 100, 101, 102, 103, 104, 105, 106, 107)
finance_low <- c(100, 98, 96, 94, 92, 90, 89, 88, 87, 86)

finance_df <- data.frame(
  year = years,
  central = finance_central,
  low = finance_low,
  high = finance_high
)

# ==============================================================================
# 2. DEFINE THEME AND COLOUR PALETTE
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
      legend.key.width = unit(1.5, "cm"),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Colour palette for costs (warmer/warning tones)
col_rose <- "#e11d48"
col_amber <- "#d97706"
col_cyan <- "#0891b2"
col_indigo <- "#4f46e5"
col_red <- "#ef4444"

# ==============================================================================
# 3. FIGURE 1: WORKFORCE COSTS
# ==============================================================================

p1_staff <- ggplot(staff_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_rose, 0.15)) +
  # Central line
  geom_line(aes(y = central), color = col_rose, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_rose, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(staff_df$high, 1) + 2, 
           label = "High: Inflation +\npension spike", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(staff_df$central, 1), 
           label = "Central", size = 2.5, hjust = 0, color = col_rose, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(staff_df$low, 1) - 2, 
           label = "Low: Pay restraint", size = 2.5, hjust = 0, color = "grey40") +
  # Risk badge annotation
  annotate("label", x = 2027, y = 160, 
           label = "HIGH IMPACT\n55–60% of total costs",
           size = 2.5, fill = "#fef2f2", color = "#991b1b", 
           fontface = "bold", label.size = 0.3, label.r = unit(3, "pt")) +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(95, 170), 
                     breaks = seq(100, 170, 10),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Workforce Costs (Staff & Pensions)",
    subtitle = "Driven by wage settlements, employer NI increases, and pension valuations (USS/TPS).\nCentral forecast assumes CPI+1% pay growth.",
    x = "Year",
    y = "Cost Index (2026 = 100)",
    caption = "Source: UCEA Financial Health Reports 2024; USS/TPS actuarial valuations"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-cost-workforce.png"), p1_staff,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-workforce.pdf"), p1_staff,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-workforce.svg"), p1_staff,
       width = 8, height = 6)

# ==============================================================================
# 4. FIGURE 2: ESTATES & ENERGY
# ==============================================================================

p2_estates <- ggplot(estates_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_amber, 0.15)) +
  # Central line
  geom_line(aes(y = central), color = col_amber, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_amber, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Net Zero annotation
  geom_vline(xintercept = 2028, linetype = "dotted", color = col_amber, linewidth = 0.7) +
  annotate("text", x = 2028.2, y = 98, label = "Net Zero\nRetrofit Start", 
           size = 2.5, hjust = 0, color = "#b45309", fontface = "bold") +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(estates_df$high, 1) + 2, 
           label = "High: Aggressive\nNet Zero", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(estates_df$central, 1), 
           label = "Central", size = 2.5, hjust = 0, color = col_amber, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(estates_df$low, 1), 
           label = "Low: Minimal\nmaintenance", size = 2.5, hjust = 0, color = "grey40") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(92, 180), 
                     breaks = seq(100, 180, 20),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Estates & Energy Costs",
    subtitle = "Energy prices stabilised but remain high. Major driver post-2028 is\ncapital cost of decarbonising ageing campuses (Net Zero 2035 targets).",
    x = "Year",
    y = "Cost Index (2026 = 100)",
    caption = "Source: AUDE Estates Management Reports 2024/25; Net Zero 2035 commitments"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-cost-estates.png"), p2_estates,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-estates.pdf"), p2_estates,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-estates.svg"), p2_estates,
       width = 8, height = 6)

# ==============================================================================
# 5. FIGURE 3: DIGITAL & ADMINISTRATION
# ==============================================================================

p3_digital <- ggplot(digital_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_cyan, 0.15)) +
  # Central line
  geom_line(aes(y = central), color = col_cyan, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_cyan, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(digital_df$high, 1) + 3, 
           label = "High: Techflation\n+ FX risk", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(digital_df$central, 1), 
           label = "Central", size = 2.5, hjust = 0, color = col_cyan, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(digital_df$low, 1) - 2, 
           label = "Low: Efficiency gains", size = 2.5, hjust = 0, color = "grey40") +
  # Techflation callout
  annotate("label", x = 2028, y = 175, 
           label = "TECHFLATION\nSaaS licensing (USD-pegged)\nCybersecurity insurance\nAI integration costs",
           size = 2.3, fill = "#ecfeff", color = "#0e7490", 
           fontface = "italic", label.size = 0.3, label.r = unit(3, "pt"),
           hjust = 0) +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(95, 190), 
                     breaks = seq(100, 180, 20),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Digital & Administration Costs",
    subtitle = "'Techflation': Software licensing, cybersecurity, and AI costs outpace CPI.\nMany contracts are USD-pegged, creating FX exposure.",
    x = "Year",
    y = "Cost Index (2026 = 100)",
    caption = "Source: Jisc Digital Infrastructure Analysis 2024; vendor pricing trends"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-cost-digital.png"), p3_digital,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-digital.pdf"), p3_digital,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-digital.svg"), p3_digital,
       width = 8, height = 6)

# ==============================================================================
# 6. FIGURE 4: FINANCING & DEBT SERVICING
# ==============================================================================

p4_finance <- ggplot(finance_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_indigo, 0.15)) +
  # Central line
  geom_line(aes(y = central), color = col_indigo, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_indigo, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Bond maturity wall annotation
  geom_vline(xintercept = 2031, linetype = "dotted", color = col_indigo, linewidth = 0.7) +
  annotate("text", x = 2031.2, y = 155, label = "Bond Maturity\nWall", 
           size = 2.5, hjust = 0, color = "#4338ca", fontface = "bold") +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(finance_df$high, 1) + 3, 
           label = "High: Refinancing\nat 6-7%", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(finance_df$central, 1), 
           label = "Central: Rates\nsettle 3.5-4%", size = 2.5, hjust = 0, color = col_indigo, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(finance_df$low, 1) - 3, 
           label = "Low: Rate cuts\nto ~2.5%", size = 2.5, hjust = 0, color = "grey40") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(80, 165), 
                     breaks = seq(80, 160, 20),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Financing & Capital Costs",
    subtitle = "Cost of servicing debt. High scenario assumes refinancing 'cliffs'\nfor bonds/loans issued during low-rate era.",
    x = "Year",
    y = "Cost Index (2026 = 100)",
    caption = "Source: OfS Financial Sustainability Data; institutional bond prospectuses"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-cost-finance.png"), p4_finance,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-finance.pdf"), p4_finance,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-finance.svg"), p4_finance,
       width = 8, height = 6)

# ==============================================================================
# 7. COMBINED PANEL: ALL COST PROJECTIONS
# ==============================================================================

# Create smaller versions without subtitles for panel
p1_small <- p1_staff + 
  labs(subtitle = NULL, caption = NULL, title = "1. Workforce (Staff & Pensions)") +
  theme(plot.title = element_text(size = 11))

p2_small <- p2_estates + 
  labs(subtitle = NULL, caption = NULL, title = "2. Estates & Energy") +
  theme(plot.title = element_text(size = 11))

p3_small <- p3_digital + 
  labs(subtitle = NULL, caption = NULL, title = "3. Digital & Admin") +
  theme(plot.title = element_text(size = 11))

p4_small <- p4_finance + 
  labs(subtitle = NULL, caption = NULL, title = "4. Financing & Debt") +
  theme(plot.title = element_text(size = 11))

# Combine using patchwork
combined_costs <- (p1_small | p2_small) / 
                  (p3_small | p4_small) +
  plot_annotation(
    title = "UK University Cost Pressures 2026–2035",
    subtitle = "Four key expenditure drivers. Shaded areas show uncertainty between high and low scenarios.",
    caption = "Sources: UCEA, USS/TPS, AUDE, Jisc, OfS",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Save combined panel
ggsave(file.path(output_dir, "uk-he-cost-projections-combined.png"), combined_costs,
       width = 14, height = 12, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-projections-combined.pdf"), combined_costs,
       width = 14, height = 12, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-projections-combined.svg"), combined_costs,
       width = 14, height = 12)

# ==============================================================================
# 8. SUMMARY COMPARISON CHART
# ==============================================================================

# Create a comparison chart showing all central forecasts together
comparison_df <- data.frame(
  year = rep(years, 4),
  category = rep(c("Workforce", "Estates", "Digital", "Financing"), each = 10),
  index = c(staff_central, estates_central, digital_central, finance_central)
)

comparison_df$category <- factor(comparison_df$category, 
                                 levels = c("Workforce", "Estates", "Digital", "Financing"))

p_comparison <- ggplot(comparison_df, aes(x = year, y = index, color = category)) +
  # Lines
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, fill = "white", shape = 21, stroke = 1.2) +
  # Reference line
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Colours
  scale_color_manual(values = c("Workforce" = col_rose, "Estates" = col_amber, 
                                 "Digital" = col_cyan, "Financing" = col_indigo)) +
  # Scales
  scale_x_continuous(breaks = years) +
  scale_y_continuous(limits = c(95, 140), 
                     breaks = seq(100, 140, 10)) +
  # Labels
  labs(
    title = "Comparative Cost Growth (Central Forecasts)",
    subtitle = "All four cost drivers indexed to 2026 = 100. Workforce dominates\nboth in weight (~55%) and growth trajectory.",
    x = "Year",
    y = "Cost Index (2026 = 100)",
    caption = "Source: Consolidated from UCEA, AUDE, Jisc, OfS data"
  ) +
  theme_projection() +
  theme(legend.position = "right")

# Save
ggsave(file.path(output_dir, "uk-he-cost-comparison.png"), p_comparison,
       width = 10, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-cost-comparison.pdf"), p_comparison,
       width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-cost-comparison.svg"), p_comparison,
       width = 10, height = 6)

# ==============================================================================
# 9. OUTPUT SUMMARY
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("UK HE COST PROJECTIONS - Figure Generation Complete\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\n")
cat("Individual figures generated (PNG, PDF, SVG):\n")
cat("  1. uk-he-cost-workforce    - Staff & pensions\n")
cat("  2. uk-he-cost-estates      - Estates & energy\n")
cat("  3. uk-he-cost-digital      - Digital & admin\n")
cat("  4. uk-he-cost-finance      - Financing & debt\n")
cat("\n")
cat("Comparison and combined:\n")
cat("  5. uk-he-cost-comparison         - All 4 central forecasts\n")
cat("  6. uk-he-cost-projections-combined - 2x2 panel\n")
cat("\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
