# ============================================================================
# Global Visa Difficulty Index (2026)
# Publication-Ready Visualisations for Print
# ============================================================================
# 
# This script produces high-quality, print-ready figures comparing the 
# barriers to entry for international students across major destination 
# countries. Multiple visualisation types are generated to effectively 
# communicate the multi-dimensional data.
#
# Output: PNG (600 DPI), PDF, and SVG files ready for publication
# 
# Data Sources:
#   - Official government immigration websites as of January 2026
#   - Home Affairs Australia, IRCC Canada, UKVI, US State Dept,
#     German Federal Foreign Office, Japanese Immigration Bureau
#
# Dimensions Measured (scored 1-10, higher = more difficult):
#   - Financial Cost: Visa fees + mandatory financial proof requirements
#   - Refusal Risk: Based on recent rejection/refusal rates
#   - Policy Hostility: Caps, bans on dependents, interview strictness
#   - Bureaucracy: Paperwork complexity, processing times, appointments
# ============================================================================

# Load required packages (install if necessary)
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales", "svglite", 
                       "RColorBrewer", "ggtext", "patchwork", "forcats")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ============================================================================
# OUTPUT DIRECTORY
# ============================================================================

output_dir <- file.path(getwd(), "fig")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Visa difficulty scores (1-10 scale, higher = more difficult)
# Based on data from the HTML interactive visualisation
visa_data <- data.frame(
  country = c("Australia", "Canada", "United Kingdom", "Germany", "United States"),
  flag = c("🇦🇺", "🇨🇦", "🇬🇧", "🇩🇪", "🇺🇸"),
  difficulty_level = c("Extreme", "High", "High", "Moderate", "Medium-High"),
  financial_cost = c(10, 7, 8, 6, 6),
  refusal_risk = c(8, 9, 4, 3, 8),
  policy_hostility = c(9, 8, 7, 2, 5),
  bureaucracy = c(7, 9, 5, 8, 6),
  stringsAsFactors = FALSE
)

# Add total difficulty score
visa_data <- visa_data %>%
  mutate(
    total_score = financial_cost + refusal_risk + policy_hostility + bureaucracy,
    country_label = paste0(country, " ", flag)
  )

# Order countries by total difficulty (descending)
visa_data$country <- factor(visa_data$country, 
                             levels = visa_data$country[order(-visa_data$total_score)])
visa_data$country_label <- factor(visa_data$country_label,
                                   levels = visa_data$country_label[order(-visa_data$total_score)])

# Convert to long format for plotting
visa_long <- visa_data %>%
  select(country, country_label, financial_cost, refusal_risk, 
         policy_hostility, bureaucracy) %>%
  pivot_longer(
    cols = c(financial_cost, refusal_risk, policy_hostility, bureaucracy),
    names_to = "dimension",
    values_to = "score"
  ) %>%
  mutate(
    dimension = case_when(
      dimension == "financial_cost" ~ "Financial Cost",
      dimension == "refusal_risk" ~ "Refusal Risk",
      dimension == "policy_hostility" ~ "Policy Hostility",
      dimension == "bureaucracy" ~ "Bureaucracy"
    ),
    dimension = factor(dimension, levels = c("Financial Cost", "Refusal Risk", 
                                              "Policy Hostility", "Bureaucracy"))
  )

# ============================================================================
# COLOUR PALETTES
# ============================================================================

# Country colours - distinctive and print-friendly
country_colours <- c(
  "Australia"      = "#c0392b",  # Deep red (extreme)
  "Canada"         = "#e67e22",  # Orange (high)
  "United Kingdom" = "#f39c12",  # Gold (high)
  "United States"  = "#2980b9",  # Blue (medium-high)
  "Germany"        = "#27ae60"   # Green (moderate)
)

# Difficulty level colours for heatmap (diverging scale)
difficulty_gradient <- c(
  "1" = "#f7fcf5", "2" = "#e5f5e0", "3" = "#c7e9c0", "4" = "#a1d99b",
  "5" = "#74c476", "6" = "#41ab5d", "7" = "#fec44f", "8" = "#fe9929",
  "9" = "#ec7014", "10" = "#cc4c02"
)

# Sequential gradient for heatmap
heatmap_low <- "#f0f9e8"
heatmap_mid <- "#fdae61"
heatmap_high <- "#d7191c"

# Dimension colours for stacked bars
dimension_colours <- c(
  "Financial Cost"    = "#2c3e50",
  "Refusal Risk"      = "#8e44ad",
  "Policy Hostility"  = "#c0392b",
  "Bureaucracy"       = "#16a085"
)

# ============================================================================
# COMMON THEME FOR ALL PLOTS
# ============================================================================

theme_visa <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      # Text styling
      plot.title = element_text(face = "bold", size = rel(1.3), hjust = 0,
                                margin = margin(b = 8), colour = "#2c3e50"),
      plot.subtitle = element_text(size = rel(0.95), hjust = 0, colour = "grey40",
                                    margin = margin(b = 15), lineheight = 1.2),
      plot.caption = element_text(size = rel(0.75), hjust = 0, colour = "grey50",
                                   margin = margin(t = 15), lineheight = 1.2),
      # Panel styling
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      # Axis styling
      axis.title = element_text(face = "bold", size = rel(0.9), colour = "#2c3e50"),
      axis.text = element_text(size = rel(0.85), colour = "#333333"),
      axis.ticks = element_line(colour = "grey70", linewidth = 0.3),
      # Legend styling
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = rel(0.85)),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key.size = unit(0.6, "cm"),
      # Margins
      plot.margin = margin(t = 20, r = 20, b = 15, l = 15, unit = "pt")
    )
}

# ============================================================================
# FIGURE 1: HEATMAP
# A clear matrix showing difficulty scores across all dimensions and countries
# ============================================================================

p_heatmap <- ggplot(visa_long, 
                    aes(x = dimension, y = fct_rev(country), fill = score)) +
  geom_tile(colour = "white", linewidth = 2) +
  geom_text(aes(label = score), 
            size = 5, fontface = "bold", colour = "white") +
  scale_fill_gradientn(
    colours = c("#2ecc71", "#f1c40f", "#e74c3c", "#8e1a0f"),
    values = scales::rescale(c(1, 4, 7, 10)),
    limits = c(1, 10),
    breaks = c(2, 5, 8),
    labels = c("Low", "Medium", "High"),
    name = "Difficulty Score"
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_visa(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  labs(
    title = "International Student Visa Difficulty Index (2026)",
    subtitle = "Barrier to entry scores by country and dimension (1 = easy, 10 = very difficult)",
    caption = paste0(
      "Sources: Home Affairs Australia, IRCC Canada, UKVI, US State Department, ",
      "German Federal Foreign Office, Japanese Immigration Bureau (January 2026).\n",
      "Financial Cost includes visa fees and mandatory financial proof requirements. ",
      "Refusal Risk based on published rejection rates.\n",
      "Policy Hostility reflects caps, dependent restrictions, and interview requirements. ",
      "Bureaucracy measures paperwork complexity and processing times."
    )
  )

# ============================================================================
# FIGURE 2: HORIZONTAL BAR CHART WITH TOTAL SCORES
# Overview comparison showing total difficulty with rank
# ============================================================================

p_total_bar <- visa_data %>%
  mutate(country = fct_reorder(country, total_score)) %>%
  ggplot(aes(x = total_score, y = country, fill = country)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = total_score), 
            hjust = -0.3, fontface = "bold", size = 4.5, colour = "#2c3e50") +
  geom_text(aes(x = 1, label = difficulty_level),
            hjust = 0, fontface = "italic", size = 3.5, colour = "white") +
  scale_fill_manual(values = country_colours) +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    expand = c(0, 0)
  ) +
  theme_visa(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 12)
  ) +
  labs(
    x = "Total Difficulty Score (max 40)",
    y = NULL,
    title = "Overall Visa Difficulty Ranking (2026)",
    subtitle = "Combined score across all four barrier dimensions",
    caption = paste0(
      "Total score is the sum of Financial Cost, Refusal Risk, Policy Hostility, and Bureaucracy (each scored 1-10).\n",
      "Sources: Official government immigration sources, January 2026."
    )
  )

# ============================================================================
# FIGURE 3: STACKED BAR CHART
# Shows breakdown of difficulty by component for each country
# ============================================================================

p_stacked <- visa_long %>%
  mutate(country = fct_reorder(country, -as.numeric(country))) %>%
  ggplot(aes(x = score, y = fct_rev(country), fill = fct_rev(dimension))) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = rev(dimension_colours),
    name = "Difficulty Dimension",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    expand = c(0, 0)
  ) +
  theme_visa(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    x = "Cumulative Difficulty Score (max 40)",
    y = NULL,
    title = "Visa Difficulty Breakdown by Component",
    subtitle = "Contribution of each barrier dimension to overall difficulty",
    caption = paste0(
      "Each dimension scored 1-10. Higher scores indicate greater difficulty.\n",
      "Sources: Official government immigration sources, January 2026."
    )
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

# ============================================================================
# FIGURE 4: FACETED LOLLIPOP/DOT CHART
# Individual country profiles showing strengths/weaknesses
# ============================================================================

p_lollipop <- visa_long %>%
  ggplot(aes(x = score, y = fct_rev(dimension), colour = country)) +
  geom_segment(aes(x = 0, xend = score, yend = fct_rev(dimension)),
               linewidth = 1.2, show.legend = FALSE) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_text(aes(label = score), 
            hjust = -0.8, fontface = "bold", size = 3.2, show.legend = FALSE) +
  facet_wrap(~ country, nrow = 1) +
  scale_colour_manual(values = country_colours) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = c(0, 5, 10),
    expand = c(0, 0.1)
  ) +
  theme_visa(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11, colour = "#2c3e50"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Difficulty Score (1-10)",
    y = NULL,
    title = "Visa Difficulty Profiles by Country",
    subtitle = "Each country's barrier scores across the four key dimensions",
    caption = paste0(
      "Scores out of 10 for each dimension. Higher = more difficult.\n",
      "Financial Cost: fees + proof of funds. Refusal Risk: rejection rates. ",
      "Policy Hostility: caps, dependent bans. Bureaucracy: paperwork complexity.\n",
      "Sources: Official government immigration sources, January 2026."
    )
  )

# ============================================================================
# FIGURE 5: COMBINED PANEL FOR PUBLICATION
# A comprehensive 2-panel figure for print
# ============================================================================

p_combined <- (p_heatmap | p_stacked) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Global Visa Difficulty Index for International Students (2026)",
    subtitle = "Comparative analysis of entry barriers across major destination countries",
    caption = paste0(
      "Left: Individual dimension scores (1-10). Right: Cumulative barrier profile.\n",
      "Dimensions: Financial Cost (visa fees + funds), Refusal Risk (rejection rates), ",
      "Policy Hostility (caps, dependent bans), Bureaucracy (paperwork complexity).\n",
      "Sources: Home Affairs Australia, IRCC Canada, UKVI, US State Dept, ",
      "German Federal Foreign Office (January 2026)."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, 
                                colour = "#2c3e50", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "grey40",
                                    margin = margin(b = 15)),
      plot.caption = element_text(size = 9, hjust = 0, colour = "grey50",
                                   margin = margin(t = 15), lineheight = 1.2),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

# --- Export Heatmap ---

# PNG (600 DPI)
png(
  filename = file.path(output_dir, "visa-difficulty-heatmap.png"),
  width = 10, height = 6, units = "in", res = 600, bg = "white"
)
print(p_heatmap)
dev.off()

# PDF
pdf(
  file = file.path(output_dir, "visa-difficulty-heatmap.pdf"),
  width = 10, height = 6, paper = "special", useDingbats = FALSE
)
print(p_heatmap)
dev.off()

# SVG
svglite::svglite(
  filename = file.path(output_dir, "visa-difficulty-heatmap.svg"),
  width = 10, height = 6, bg = "white"
)
print(p_heatmap)
dev.off()

# --- Export Total Bar Chart ---

png(
  filename = file.path(output_dir, "visa-difficulty-ranking.png"),
  width = 9, height = 5, units = "in", res = 600, bg = "white"
)
print(p_total_bar)
dev.off()

pdf(
  file = file.path(output_dir, "visa-difficulty-ranking.pdf"),
  width = 9, height = 5, paper = "special", useDingbats = FALSE
)
print(p_total_bar)
dev.off()

# --- Export Stacked Bar Chart ---

png(
  filename = file.path(output_dir, "visa-difficulty-breakdown.png"),
  width = 10, height = 5, units = "in", res = 600, bg = "white"
)
print(p_stacked)
dev.off()

pdf(
  file = file.path(output_dir, "visa-difficulty-breakdown.pdf"),
  width = 10, height = 5, paper = "special", useDingbats = FALSE
)
print(p_stacked)
dev.off()

# --- Export Lollipop/Profile Chart ---

png(
  filename = file.path(output_dir, "visa-difficulty-profiles.png"),
  width = 14, height = 5, units = "in", res = 600, bg = "white"
)
print(p_lollipop)
dev.off()

pdf(
  file = file.path(output_dir, "visa-difficulty-profiles.pdf"),
  width = 14, height = 5, paper = "special", useDingbats = FALSE
)
print(p_lollipop)
dev.off()

svglite::svglite(
  filename = file.path(output_dir, "visa-difficulty-profiles.svg"),
  width = 14, height = 5, bg = "white"
)
print(p_lollipop)
dev.off()

# --- Export Combined Panel ---

png(
  filename = file.path(output_dir, "visa-difficulty-combined.png"),
  width = 16, height = 7, units = "in", res = 600, bg = "white"
)
print(p_combined)
dev.off()

pdf(
  file = file.path(output_dir, "visa-difficulty-combined.pdf"),
  width = 16, height = 7, paper = "special", useDingbats = FALSE
)
print(p_combined)
dev.off()

svglite::svglite(
  filename = file.path(output_dir, "visa-difficulty-combined.svg"),
  width = 16, height = 7, bg = "white"
)
print(p_combined)
dev.off()

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  GLOBAL VISA DIFFICULTY INDEX - EXPORT COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("Files created in fig/ folder:\n")
cat("\n")
cat("  Heatmap (overview of all dimensions):\n")
cat("    • visa-difficulty-heatmap.png (600 DPI)\n")
cat("    • visa-difficulty-heatmap.pdf\n")
cat("    • visa-difficulty-heatmap.svg\n")
cat("\n")
cat("  Ranking Bar Chart (total scores):\n")
cat("    • visa-difficulty-ranking.png (600 DPI)\n")
cat("    • visa-difficulty-ranking.pdf\n")
cat("\n")
cat("  Stacked Breakdown (component contributions):\n")
cat("    • visa-difficulty-breakdown.png (600 DPI)\n")
cat("    • visa-difficulty-breakdown.pdf\n")
cat("\n")
cat("  Profile Charts (individual country profiles):\n")
cat("    • visa-difficulty-profiles.png (600 DPI)\n")
cat("    • visa-difficulty-profiles.pdf\n")
cat("    • visa-difficulty-profiles.svg\n")
cat("\n")
cat("  Combined Panel (publication-ready composite):\n")
cat("    • visa-difficulty-combined.png (600 DPI)\n")
cat("    • visa-difficulty-combined.pdf\n")
cat("    • visa-difficulty-combined.svg\n")
cat("\n")
cat("Output directory:", output_dir, "\n")
cat("\n")
cat("Data Summary:\n")
cat("  Countries compared:", nrow(visa_data), "\n")
cat("  Dimensions measured:", 4, "\n")
cat("\n")
cat("Difficulty Ranking (highest to lowest):\n")
print(visa_data %>% 
        arrange(desc(total_score)) %>% 
        select(country, difficulty_level, total_score) %>%
        rename(Country = country, 
               `Difficulty Level` = difficulty_level,
               `Total Score` = total_score))
cat("\n")
cat("============================================================\n")
cat("  Ready for publication!\n")
cat("============================================================\n")
