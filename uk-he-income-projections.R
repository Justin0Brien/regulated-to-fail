# ==============================================================================
# UK Higher Education Income Projections 2026-2035
# ==============================================================================
#
# This script generates print-ready figures for UK university income projections
# across five key areas:
# 1. Domestic Demographics (18-year-old population index)
# 2. Teaching Income (nominal, RPI-linked fees)
# 3. Research Income (real terms)
# 4. International Student Income (real terms, onshore)
# 5. Transnational Education Income (real terms, offshore)
#
# Data Sources:
# - Demographics: ONS population projections, HEPI Report 179 (Bekhradnia 2024)
# - Teaching: OBR/DfE fee policy, assumes RPI-linked increases
# - Research: DSIT R&D plans 2029-30, UKRI budgets, HESA research data
# - International: British Council mobility outlook, QS Global Student Flows 2025
# - TNE: HESA Aggregate Offshore Record, UUKi Scale Report 2024
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

# --- 1. Domestic Demographics ---
# Source: ONS/HEPI - 18-year-old population curve
# Peak in 2030, then ~7% decline to 2035
student_volume <- c(100, 101.5, 103.0, 104.2, 105.0, 104.0, 102.5, 100.5, 98.5, 97.0)

demographics_df <- data.frame(
  year = years,
  index = student_volume
)

# --- 2. Teaching Income (Nominal) ---
# Source: OBR/DfE - fees track RPI
# Uncertainty wedge: 1.5% (low) to 4.5% (high) inflation
rpi_low <- 0.015
rpi_central <- 0.028  
rpi_high <- 0.045

income_central <- numeric(10)
income_low <- numeric(10)
income_high <- numeric(10)

cf_c <- 1.0; cf_l <- 1.0; cf_h <- 1.0

for (i in 1:10) {
  volume <- student_volume[i] / 100
  income_central[i] <- volume * cf_c * 100
  income_low[i] <- volume * cf_l * 100
  income_high[i] <- volume * cf_h * 100
  cf_c <- cf_c * (1 + rpi_central)
  cf_l <- cf_l * (1 + rpi_low)
  cf_h <- cf_h * (1 + rpi_high)
}

teaching_df <- data.frame(
  year = years,
  central = income_central,
  low = income_low,
  high = income_high
)

# --- 3. Research Income (Real Terms) ---
# Source: DSIT/UKRI - govt commitments to 2030, then flatline
# High: Private diversification + Horizon Europe boost
# Low: R&D inflation erodes settlements
research_central <- c(100, 100.8, 101.5, 102.2, 103.0, 102.5, 102.0, 101.5, 101.0, 100.5)
research_low <- c(100, 99.5, 99.0, 98.0, 97.0, 96.0, 95.0, 94.0, 93.0, 92.0)
research_high <- c(100, 102.0, 104.0, 106.0, 108.0, 109.0, 110.0, 111.0, 112.0, 113.0)

research_df <- data.frame(
  year = years,
  central = research_central,
  low = research_low,
  high = research_high
)

# --- 4. International Income (Real Terms) ---
# Source: British Council/QS
# High: Displacement dividend from Canada/Australia caps
# Central: Status quo stagnation
# Low: Visa levy + restrictions impact
intl_high <- c(100, 103.0, 106.1, 109.3, 112.6, 115.9, 119.4, 123.0, 126.7, 130.5)
intl_central <- c(100, 100.5, 101.0, 101.5, 102.0, 102.5, 103.0, 103.5, 104.0, 104.5)
intl_low <- c(100, 98.0, 96.0, 90.0, 88.2, 86.4, 84.7, 83.0, 81.3, 79.7)

international_df <- data.frame(
  year = years,
  central = intl_central,
  low = intl_low,
  high = intl_high
)

# --- 5. TNE Income (Real Terms) ---
# Source: HESA/UUKi Scale Report
# Base: ~£3.5bn (smaller than onshore £12bn)
# High: Strategic pivot, 8% CAGR (bypass visa restrictions)
# Central: Organic growth, 4% CAGR
# Low: Host country regulation, 1% CAGR
tne_high <- 100 * (1.08^(0:9))
tne_central <- 100 * (1.04^(0:9))
tne_low <- 100 * (1.01^(0:9))

tne_df <- data.frame(
  year = years,
  central = tne_central,
  low = tne_low,
  high = tne_high
)

# ==============================================================================
# 2. DEFINE THEME AND COLOUR PALETTE
# ==============================================================================

# Professional theme for print publication
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

# Colour palettes
col_blue <- "#3b82f6"
col_teal <- "#0f766e"
col_orange <- "#d97706"
col_indigo <- "#4338ca"
col_amber <- "#f59e0b"
col_red <- "#e11d48"
col_purple <- "#7c3aed"

# ==============================================================================
# 3. FIGURE 1: DOMESTIC DEMOGRAPHICS
# ==============================================================================

p1_demographics <- ggplot(demographics_df, aes(x = year, y = index)) +
  # Filled area under curve
  geom_area(fill = alpha(col_blue, 0.15), color = NA) +
  # Main line
  geom_line(color = col_blue, linewidth = 1.2) +
  # Points
  geom_point(color = col_blue, size = 2.5, fill = "white", shape = 21, stroke = 1.2) +
  # Peak annotation
  geom_vline(xintercept = 2030, linetype = "dashed", color = col_red, linewidth = 0.5) +
  annotate("text", x = 2030.3, y = 105.5, label = "Peak\n2030", 
           size = 3, color = col_red, hjust = 0, fontface = "bold") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(limits = c(95, 107), 
                     breaks = seq(95, 107, 2),
                     expand = expansion(mult = c(0, 0.02))) +
  # Labels
  labs(
    title = "Domestic Student Demographics",
    subtitle = "Projected 18-year-old population index. A 'demographic bulge' peaks in 2030\nbefore a sharp ~7% decline.",
    x = "Year",
    y = "Volume Index (2026 = 100)",
    caption = "Source: ONS population projections; HEPI Report 179 (Bekhradnia, 2024)"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-demographics.png"), p1_demographics,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-demographics.pdf"), p1_demographics,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-demographics.svg"), p1_demographics,
       width = 8, height = 6)

# ==============================================================================
# 4. FIGURE 2: TEACHING INCOME (NOMINAL)
# ==============================================================================

teaching_long <- teaching_df %>%
  pivot_longer(cols = c(central, low, high), names_to = "scenario", values_to = "index")

p2_teaching <- ggplot(teaching_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_teal, 0.2)) +
  # Central line
  geom_line(aes(y = central), color = col_teal, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_teal, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Boundary lines (thin)
  geom_line(aes(y = high), color = col_teal, linewidth = 0.4, linetype = "dotted") +
  geom_line(aes(y = low), color = col_teal, linewidth = 0.4, linetype = "dotted") +
  # Labels for scenarios
  annotate("text", x = 2035.3, y = tail(teaching_df$high, 1), 
           label = "High RPI (4.5%)", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(teaching_df$central, 1), 
           label = "Central (2.8%)", size = 2.5, hjust = 0, color = col_teal, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(teaching_df$low, 1), 
           label = "Low RPI (1.5%)", size = 2.5, hjust = 0, color = "grey40") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(limits = c(90, 170), 
                     breaks = seq(90, 170, 20),
                     expand = expansion(mult = c(0, 0.02))) +
  # Labels
  labs(
    title = "Teaching Income Projections (Nominal)",
    subtitle = "Projected tuition fee income assuming RPI-linked increases.\nShaded area shows uncertainty range from inflation volatility.",
    x = "Year",
    y = "Nominal Income Index (2026 = 100)",
    caption = "Source: OBR/DfE fee policy assumptions; House of Commons Library CBP-10155"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-teaching-income.png"), p2_teaching,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-teaching-income.pdf"), p2_teaching,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-teaching-income.svg"), p2_teaching,
       width = 8, height = 6)

# ==============================================================================
# 5. FIGURE 3: RESEARCH INCOME (REAL TERMS)
# ==============================================================================

p3_research <- ggplot(research_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_orange, 0.2)) +
  # Central line
  geom_line(aes(y = central), color = col_orange, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_orange, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Peak annotation
  geom_vline(xintercept = 2030, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  annotate("text", x = 2030.3, y = 114, label = "Spending Review\ncommitments end", 
           size = 2.5, hjust = 0, color = "grey40") +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(research_df$high, 1), 
           label = "High (Diversification)", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(research_df$central, 1), 
           label = "Central", size = 2.5, hjust = 0, color = col_orange, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(research_df$low, 1), 
           label = "Low (R&D inflation)", size = 2.5, hjust = 0, color = "grey40") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.2))) +
  scale_y_continuous(limits = c(88, 118), 
                     breaks = seq(88, 118, 5),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Research Income Projections (Real Terms)",
    subtitle = "Government grants rise slowly to 2030 under DSIT commitments,\nthen risk stagnation without private sector diversification.",
    x = "Year",
    y = "Real Terms Index (2026 = 100)",
    caption = "Source: DSIT R&D Plans 2029-30; UKRI budgets; Russell Group sustainability analysis"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-research-income.png"), p3_research,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-research-income.pdf"), p3_research,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-research-income.svg"), p3_research,
       width = 8, height = 6)

# ==============================================================================
# 6. FIGURE 4: INTERNATIONAL STUDENT INCOME (REAL TERMS)
# ==============================================================================

p4_international <- ggplot(international_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_indigo, 0.2)) +
  # Central line
  geom_line(aes(y = central), color = col_indigo, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_indigo, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Risk annotation (visa levy)
  annotate("label", x = 2029, y = 88, 
           label = "Risk: Proposed\nIntl Student Levy",
           size = 2.5, fill = "#fee2e2", color = "#991b1b", 
           fontface = "bold", label.size = 0.3, label.r = unit(3, "pt")) +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(international_df$high, 1) + 2, 
           label = "High: Displacement\nfrom Canada/Aus", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(international_df$central, 1), 
           label = "Central: Status quo", size = 2.5, hjust = 0, color = col_indigo, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(international_df$low, 1) - 2, 
           label = "Low: Visa levy\n+ restrictions", size = 2.5, hjust = 0, color = "grey40") +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(75, 140), 
                     breaks = seq(75, 140, 10),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "International Student Income (Real Terms)",
    subtitle = "High volatility: Sensitive to visa levies and global competition.\nShaded area shows wide uncertainty between policy scenarios.",
    x = "Year",
    y = "Real Terms Index (2026 = 100)",
    caption = "Source: British Council mobility outlook; QS Global Student Flows 2025; ApplyBoard 2025"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-international-income.png"), p4_international,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-international-income.pdf"), p4_international,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-international-income.svg"), p4_international,
       width = 8, height = 6)

# ==============================================================================
# 7. FIGURE 5: TRANSNATIONAL EDUCATION INCOME (REAL TERMS)
# ==============================================================================

p5_tne <- ggplot(tne_df, aes(x = year)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = low, ymax = high), fill = alpha(col_amber, 0.25)) +
  # Central line
  geom_line(aes(y = central), color = col_amber, linewidth = 1.2) +
  geom_point(aes(y = central), color = col_amber, size = 2.5, fill = "white", 
             shape = 21, stroke = 1.2) +
  # Reference line at 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  # Scenario labels
  annotate("text", x = 2035.3, y = tail(tne_df$high, 1) + 5, 
           label = "High: Strategic Pivot\n(8% CAGR)", size = 2.5, hjust = 0, color = "grey40") +
  annotate("text", x = 2035.3, y = tail(tne_df$central, 1), 
           label = "Central: Organic\n(4% CAGR)", size = 2.5, hjust = 0, color = col_amber, fontface = "bold") +
  annotate("text", x = 2035.3, y = tail(tne_df$low, 1) - 3, 
           label = "Low: Regulatory\nheadwinds (1%)", size = 2.5, hjust = 0, color = "grey40") +
  # Strategic insight box
  annotate("label", x = 2028, y = 175, 
           label = "The Strategic Hedge:\nUniversities bypass UK visa\ncaps by taking degrees to\nstudents' home countries",
           size = 2.3, fill = "#fff7ed", color = "#c2410c", 
           fontface = "italic", label.size = 0.3, label.r = unit(3, "pt"),
           hjust = 0) +
  # Scales
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(limits = c(95, 230), 
                     breaks = seq(100, 220, 20),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Labels
  labs(
    title = "Transnational Education (TNE) Income (Real Terms)",
    subtitle = "Offshore education revenue from franchises, branch campuses, and distance learning.\nSmaller base (~£3.5bn) but fastest-growing stream—a hedge against migration policy.",
    x = "Year",
    y = "Real Terms Index (2026 = 100)",
    caption = "Source: HESA Aggregate Offshore Record; UUKi Scale of TNE Report 2024; QAA Global Framework"
  ) +
  theme_projection()

# Save
ggsave(file.path(output_dir, "uk-he-tne-income.png"), p5_tne,
       width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-tne-income.pdf"), p5_tne,
       width = 8, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-tne-income.svg"), p5_tne,
       width = 8, height = 6)

# ==============================================================================
# 8. COMBINED PANEL: ALL INCOME PROJECTIONS
# ==============================================================================

# Create smaller versions of each plot without subtitles for panel
p1_small <- p1_demographics + 
  labs(subtitle = NULL, caption = NULL, title = "1. Demographics") +
  theme(plot.title = element_text(size = 11))

p2_small <- p2_teaching + 
  labs(subtitle = NULL, caption = NULL, title = "2. Teaching Income (Nominal)") +
  theme(plot.title = element_text(size = 11))

p3_small <- p3_research + 
  labs(subtitle = NULL, caption = NULL, title = "3. Research Income (Real)") +
  theme(plot.title = element_text(size = 11))

p4_small <- p4_international + 
  labs(subtitle = NULL, caption = NULL, title = "4. International (Onshore)") +
  theme(plot.title = element_text(size = 11))

p5_small <- p5_tne + 
  labs(subtitle = NULL, caption = NULL, title = "5. TNE (Offshore)") +
  theme(plot.title = element_text(size = 11))

# Combine using patchwork
combined_income <- (p1_small | p2_small) / 
                   (p3_small | p4_small) / 
                   p5_small +
  plot_annotation(
    title = "UK Higher Education Income Projections 2026–2035",
    subtitle = "Five key income streams with scenario uncertainty. Shaded areas show high/low projections.",
    caption = "Sources: ONS, HEPI, OBR, DfE, DSIT, UKRI, British Council, QS, HESA, UUKi",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Save combined panel
ggsave(file.path(output_dir, "uk-he-income-projections-combined.png"), combined_income,
       width = 14, height = 16, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "uk-he-income-projections-combined.pdf"), combined_income,
       width = 14, height = 16, device = cairo_pdf)
ggsave(file.path(output_dir, "uk-he-income-projections-combined.svg"), combined_income,
       width = 14, height = 16)

# ==============================================================================
# 9. OUTPUT SUMMARY
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("UK HE INCOME PROJECTIONS - Figure Generation Complete\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\n")
cat("Individual figures generated (PNG, PDF, SVG):\n")
cat("  1. uk-he-demographics          - 18yo population curve\n")
cat("  2. uk-he-teaching-income       - Nominal fee income\n")
cat("  3. uk-he-research-income       - Real terms research\n")
cat("  4. uk-he-international-income  - Onshore international\n")
cat("  5. uk-he-tne-income            - Transnational education\n")
cat("\n")
cat("Combined panel:\n")
cat("  6. uk-he-income-projections-combined - All 5 figures\n")
cat("\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
