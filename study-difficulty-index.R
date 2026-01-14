# ============================================================================
# Study Difficulty Index 2026
# Publication-Ready Visualisations for Print
# ============================================================================
# 
# This script produces high-quality, print-ready figures comparing the 
# barriers to entry for international students across major destination 
# countries. Expanded from the original 5 countries to 12 destinations.
#
# Output: PNG (600 DPI), PDF, and SVG files ready for publication
# 
# Data Sources:
#   - Australia: Home Affairs (2025/26)
#   - Canada: IRCC (2025/26)
#   - United Kingdom: UKVI (2025/26)
#   - Netherlands: IND + ICEF Monitor (2025)
#   - New Zealand: Immigration NZ (2025)
#   - United States: State Department (2025/26)
#   - France: Campus France (2025)
#   - Germany: Federal Foreign Office (2025/26)
#   - South Korea: Ministry of Education/StudyKorea (2025)
#   - Japan: JASSO / Immigration Services Agency (2025)
#   - Ireland: INIS (2025)
#   - Sweden: Migrationsverket (2025)
#
# Dimensions Measured (scored 1-10, higher = more difficult):
#   - Financial Cost: Visa fees + mandatory financial proof requirements
#   - Refusal Risk: Based on recent rejection/refusal rates
#   - Policy Hostility: Caps, bans on dependents, political climate
#   - Bureaucracy: Paperwork complexity, processing times, appointments
# ============================================================================

# Load required packages (install if necessary)
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales", "svglite", 
                       "RColorBrewer", "ggtext", "patchwork", "forcats",
                       "ggrepel", "grid", "gridExtra")

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
# DATA PREPARATION - EXPANDED TO 12 COUNTRIES
# ============================================================================

# Study difficulty scores (1-10 scale, higher = more difficult)
# Expanded dataset with 12 major destination countries
study_data <- data.frame(
  country = c(
    "Australia", "Canada", "United Kingdom", "Netherlands", "New Zealand",
    "United States", "France", "Germany", "South Korea", "Japan",
    "Ireland", "Sweden"
  ),
  flag = c(
    "🇦🇺", "🇨🇦", "🇬🇧", "🇳🇱", "🇳🇿",
    "🇺🇸", "🇫🇷", "🇩🇪", "🇰🇷", "🇯🇵",
    "🇮🇪", "🇸🇪"
  ),
  region = c(
    "Oceania", "North America", "Europe", "Europe", "Oceania",
    "North America", "Europe", "Europe", "Asia", "Asia",
    "Europe", "Europe"
  ),
  # Scores: Financial Cost, Refusal Risk, Policy Hostility, Bureaucracy
  financial_cost = c(10, 7, 8, 7, 8, 6, 4, 6, 5, 4, 7, 5),
  refusal_risk   = c(8, 9, 4, 4, 7, 8, 5, 3, 3, 3, 5, 3),
  policy_hostility = c(9, 8, 7, 9, 6, 5, 3, 2, 2, 2, 4, 3),
  bureaucracy    = c(7, 9, 5, 6, 7, 6, 8, 8, 5, 6, 5, 7),
  # Visa fee info (for annotation)
  visa_fee = c(
    "AUD 1,600", "CAD 150", "£490+IHS", "€228", "NZD 375",
    "USD 185", "€50-99", "€75", "~USD 60", "~JPY 3,000",
    "€150", "SEK 1,500"
  ),
  # Financial proof requirements
  funds_required = c(
    "AUD 29,710/yr", "CAD 20,635/yr", "£1,334/mo", "€13,104/yr", "NZD 20,000/yr",
    "Varies (I-20)", "€7,380/yr", "€11,904", "USD 20,000", "~¥2M/yr",
    "€10,000/yr", "SEK 10,314/mo"
  ),
  # Key barrier summary
  key_barrier = c(
    "Caps & GS Test", "PAL Process", "Dependents Ban", "Housing Crisis",
    "AU Alignment", "Interviews", "Campus France", "Blocked Account",
    "Growing Sector", "COE Process", "Cost Increase", "Residence Permit"
  ),
  # Verdict for each country
  verdict = c(
    "Highest fees globally. Hard cap on enrolments via NPL. Genuine Student Test creates uncertainty.",
    "Complex Provincial Attestation Letter (PAL) requirement. Approval rates dropped ~50% in 2024/25.",
    "High upfront cost (visa + Health Surcharge). Dependents ban deters family-oriented students.",
    "Severe housing shortage causing universities to pause recruitment. Political pushback on English courses.",
    "Aligning policies with Australia. High rejection rates for non-university sectors.",
    "Mandatory in-person interview with high subjective refusal rate (~36%). Rules stable vs competitors.",
    "Low cost destination, but Campus France paperwork is notoriously complex and time-consuming.",
    "Welcoming policy and often free tuition, but Blocked Account and appointment bottlenecks cause delays.",
    "Actively recruiting to address demographic decline. Fast-tracking visa approvals.",
    "Target of 400k students by 2033. COE process thorough but predictable. Stable and welcoming.",
    "Growing as alternative to UK. Recent fee increases but still competitive within Europe.",
    "Free tuition at public universities. Residence permit process can be slow but straightforward."
  ),
  stringsAsFactors = FALSE
)

# Calculate total and average difficulty scores
study_data <- study_data %>%
  mutate(
    total_score = financial_cost + refusal_risk + policy_hostility + bureaucracy,
    avg_score = total_score / 4,
    # Difficulty level categories
    difficulty_level = case_when(
      avg_score >= 8 ~ "Extreme",
      avg_score >= 6.5 ~ "High",
      avg_score >= 5 ~ "Medium-High",
      avg_score >= 4 ~ "Moderate",
      TRUE ~ "Low"
    ),
    difficulty_level = factor(difficulty_level, 
                               levels = c("Low", "Moderate", "Medium-High", "High", "Extreme"))
  )

# Order countries by total difficulty (descending)
study_data <- study_data %>%
  arrange(desc(total_score)) %>%
  mutate(
    country = factor(country, levels = country),
    rank = row_number()
  )

# Convert to long format for plotting
study_long <- study_data %>%
  select(country, region, financial_cost, refusal_risk, 
         policy_hostility, bureaucracy, total_score, difficulty_level) %>%
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

# Country colours by difficulty tier
country_colours <- c(
  "Australia"      = "#8e1a0f",
  "Canada"         = "#c0392b",
  "Netherlands"    = "#d35400",
  "New Zealand"    = "#e67e22",
  "United Kingdom" = "#f39c12",
  "United States"  = "#3498db",
  "Ireland"        = "#1abc9c",
  "France"         = "#27ae60",
  "Germany"        = "#2ecc71",
  "Sweden"         = "#9b59b6",
  "South Korea"    = "#16a085",
  "Japan"          = "#1e8449"
)

# Region colours
region_colours <- c(
  "Oceania"       = "#c0392b",
  "North America" = "#2980b9",
  "Europe"        = "#27ae60",
  "Asia"          = "#8e44ad"
)

# Difficulty level colours
level_colours <- c(
  "Low"         = "#27ae60",
  "Moderate"    = "#2ecc71",
  "Medium-High" = "#f1c40f",
  "High"        = "#e67e22",
  "Extreme"     = "#c0392b"
)

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

theme_study <- function(base_size = 11) {
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
# FIGURE 1: COMPREHENSIVE HEATMAP (All 12 Countries)
# ============================================================================

p_heatmap <- ggplot(study_long, 
                    aes(x = dimension, y = fct_rev(country), fill = score)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = score), 
            size = 4, fontface = "bold", colour = "white") +
  scale_fill_gradientn(
    colours = c("#27ae60", "#f1c40f", "#e67e22", "#c0392b"),
    values = scales::rescale(c(1, 4, 7, 10)),
    limits = c(1, 10),
    breaks = c(2, 5, 8),
    labels = c("Low", "Medium", "High"),
    name = "Difficulty"
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_study(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm")
  ) +
  labs(
    title = "Study Difficulty Index 2026: Comparative Heatmap",
    subtitle = "Barrier scores by country and dimension (1 = easy, 10 = very difficult) — 12 major destinations",
    caption = paste0(
      "Sources: Home Affairs Australia, IRCC Canada, UKVI, IND Netherlands, INZ, US State Dept, Campus France,\n",
      "German Foreign Office, StudyKorea, JASSO Japan, INIS Ireland, Migrationsverket Sweden (2025/26)."
    )
  )

# ============================================================================
# FIGURE 2: HORIZONTAL RANKING BAR CHART
# ============================================================================

p_ranking <- study_data %>%
  ggplot(aes(x = total_score, y = fct_reorder(country, total_score), 
             fill = difficulty_level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = total_score), 
            hjust = -0.3, fontface = "bold", size = 3.5, colour = "#2c3e50") +
  geom_text(aes(x = 1.5, label = difficulty_level),
            hjust = 0, size = 2.8, colour = "white", fontface = "italic") +
  scale_fill_manual(values = level_colours, name = "Difficulty Level") +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    expand = c(0, 0)
  ) +
  theme_study(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  ) +
  labs(
    x = "Total Difficulty Score (max 40)",
    y = NULL,
    title = "Overall Study Visa Difficulty Ranking (2026)",
    subtitle = "Combined score across Financial Cost, Refusal Risk, Policy Hostility, and Bureaucracy",
    caption = "Each dimension scored 1-10. Total represents cumulative barrier to entry."
  ) +
  guides(fill = guide_legend(nrow = 1))

# ============================================================================
# FIGURE 3: STACKED BAR CHART WITH COMPONENT BREAKDOWN
# ============================================================================

p_stacked <- study_long %>%
  ggplot(aes(x = score, y = fct_reorder(country, total_score), 
             fill = fct_rev(dimension))) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = rev(dimension_colours),
    name = "Barrier Dimension",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(
    limits = c(0, 42),
    breaks = seq(0, 40, 10),
    expand = c(0, 0)
  ) +
  theme_study(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    x = "Cumulative Difficulty Score",
    y = NULL,
    title = "Study Visa Difficulty: Component Breakdown",
    subtitle = "Contribution of each barrier dimension to overall difficulty",
    caption = "Financial Cost: fees + funds. Refusal Risk: rejection rates. Policy Hostility: caps, bans. Bureaucracy: paperwork."
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

# ============================================================================
# FIGURE 4: FACETED LOLLIPOP PROFILES
# ============================================================================

p_profiles <- study_long %>%
  ggplot(aes(x = score, y = fct_rev(dimension))) +
  geom_segment(aes(x = 0, xend = score, yend = fct_rev(dimension), 
                   colour = difficulty_level),
               linewidth = 1.2, show.legend = FALSE) +
  geom_point(aes(colour = difficulty_level), size = 3.5, show.legend = FALSE) +
  geom_text(aes(label = score), 
            hjust = -0.6, fontface = "bold", size = 2.8, colour = "#2c3e50") +
  facet_wrap(~ country, nrow = 2) +
  scale_colour_manual(values = level_colours) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = c(0, 5, 10),
    expand = c(0.02, 0.1)
  ) +
  theme_study(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 9, colour = "#2c3e50"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    panel.spacing = unit(0.8, "lines")
  ) +
  labs(
    x = "Difficulty Score (1-10)",
    y = NULL,
    title = "Individual Country Difficulty Profiles",
    subtitle = "Barrier patterns across the four key dimensions for each destination",
    caption = "Scores out of 10. Higher = more difficult. Countries ordered by total difficulty score."
  )

# ============================================================================
# FIGURE 5: DOT PLOT BY REGION
# ============================================================================

p_dots <- study_data %>%
  ggplot(aes(x = total_score, y = fct_reorder(country, total_score), 
             colour = region)) +
  geom_segment(aes(x = 0, xend = total_score, yend = fct_reorder(country, total_score)),
               colour = "grey80", linewidth = 0.5) +
  geom_point(size = 5) +
  geom_text(aes(label = total_score), 
            colour = "white", size = 2.5, fontface = "bold") +
  scale_colour_manual(values = region_colours, name = "Region") +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    expand = c(0, 0.05)
  ) +
  theme_study(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  ) +
  labs(
    x = "Total Difficulty Score",
    y = NULL,
    title = "Study Visa Difficulty by Region",
    subtitle = "Oceania (AU/NZ) leads in barriers; Asia (JP/KR) most welcoming",
    caption = paste0(
      "Total score = sum of Financial Cost, Refusal Risk, Policy Hostility, and Bureaucracy.\n",
      "Oceania countries increasingly aligned in restrictive policies."
    )
  ) +
  guides(colour = guide_legend(nrow = 1))

# ============================================================================
# FIGURE 6: SCATTER PLOT - COST vs REFUSAL RISK
# ============================================================================

p_scatter <- study_data %>%
  ggplot(aes(x = financial_cost, y = refusal_risk, 
             colour = region, size = total_score)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(aes(label = country), 
                   size = 3, fontface = "bold",
                   box.padding = 0.5, point.padding = 0.3,
                   max.overlaps = 20) +
  scale_colour_manual(values = region_colours, name = "Region") +
  scale_size_continuous(range = c(3, 10), name = "Total Score") +
  scale_x_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "grey60", alpha = 0.5) +
  geom_vline(xintercept = 5, linetype = "dashed", colour = "grey60", alpha = 0.5) +
  annotate("text", x = 2.5, y = 2.5, label = "Low Cost\nLow Risk", 
           colour = "grey50", size = 3, fontface = "italic") +
  annotate("text", x = 8, y = 8, label = "High Cost\nHigh Risk", 
           colour = "grey50", size = 3, fontface = "italic") +
  theme_study(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Financial Cost Score",
    y = "Refusal Risk Score",
    title = "Cost vs Refusal Risk Trade-off",
    subtitle = "Identifying destinations with favourable cost-risk profiles",
    caption = "Dashed lines at score 5 indicate medium difficulty threshold. Bubble size = total difficulty score."
  )

# ============================================================================
# FIGURE 7: SUMMARY TABLE AS FIGURE (for print)
# ============================================================================

# Create a summary table
summary_table <- study_data %>%
  select(country, difficulty_level, financial_cost, refusal_risk, 
         policy_hostility, bureaucracy, total_score) %>%
  arrange(desc(total_score))

# Table as grob for export
table_theme <- gridExtra::ttheme_minimal(
  base_size = 9,
  core = list(
    fg_params = list(fontface = "plain", col = "#2c3e50"),
    bg_params = list(fill = c("grey95", "white"))
  ),
  colhead = list(
    fg_params = list(fontface = "bold", col = "#2c3e50"),
    bg_params = list(fill = "grey85")
  )
)

names(summary_table) <- c("Country", "Level", "Financial", "Refusal", 
                           "Policy", "Bureaucracy", "Total")

p_table <- gridExtra::tableGrob(summary_table, rows = NULL, theme = table_theme)

# ============================================================================
# FIGURE 8: COMBINED PANEL FOR PUBLICATION
# ============================================================================

p_combined <- (p_heatmap | p_stacked) +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = "Study Difficulty Index 2026: International Student Visa Barriers",
    subtitle = "Comparative analysis of entry barriers across 12 major destination countries",
    caption = paste0(
      "Left: Dimension scores (1-10) for each country. Right: Cumulative contribution of each barrier type.\n",
      "Sources: Official immigration authorities of each country (January 2026). ",
      "See methodology notes for full scoring criteria."
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
# FIGURE 9: ALTERNATIVE COMBINED - RANKING + SCATTER
# ============================================================================

p_combined_alt <- (p_ranking / p_scatter) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Study Visa Difficulty Analysis: Rankings and Trade-offs",
    subtitle = "Overview of barriers and cost-risk positioning for international student destinations",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, 
                                colour = "#2c3e50", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40",
                                    margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# ============================================================================
# EXPORT ALL FIGURES
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  EXPORTING FIGURES...\n")
cat("============================================================\n")

# --- 1. Heatmap ---
cat("  → Heatmap...\n")
png(file.path(output_dir, "study-difficulty-heatmap.png"),
    width = 11, height = 8, units = "in", res = 600, bg = "white")
print(p_heatmap)
dev.off()

pdf(file.path(output_dir, "study-difficulty-heatmap.pdf"),
    width = 11, height = 8, paper = "special", useDingbats = FALSE)
print(p_heatmap)
dev.off()

svglite::svglite(file.path(output_dir, "study-difficulty-heatmap.svg"),
                  width = 11, height = 8, bg = "white")
print(p_heatmap)
dev.off()

# --- 2. Ranking ---
cat("  → Ranking bar chart...\n")
png(file.path(output_dir, "study-difficulty-ranking.png"),
    width = 10, height = 7, units = "in", res = 600, bg = "white")
print(p_ranking)
dev.off()

pdf(file.path(output_dir, "study-difficulty-ranking.pdf"),
    width = 10, height = 7, paper = "special", useDingbats = FALSE)
print(p_ranking)
dev.off()

# --- 3. Stacked ---
cat("  → Stacked breakdown...\n")
png(file.path(output_dir, "study-difficulty-stacked.png"),
    width = 11, height = 7, units = "in", res = 600, bg = "white")
print(p_stacked)
dev.off()

pdf(file.path(output_dir, "study-difficulty-stacked.pdf"),
    width = 11, height = 7, paper = "special", useDingbats = FALSE)
print(p_stacked)
dev.off()

# --- 4. Profiles ---
cat("  → Country profiles...\n")
png(file.path(output_dir, "study-difficulty-profiles.png"),
    width = 14, height = 7, units = "in", res = 600, bg = "white")
print(p_profiles)
dev.off()

pdf(file.path(output_dir, "study-difficulty-profiles.pdf"),
    width = 14, height = 7, paper = "special", useDingbats = FALSE)
print(p_profiles)
dev.off()

svglite::svglite(file.path(output_dir, "study-difficulty-profiles.svg"),
                  width = 14, height = 7, bg = "white")
print(p_profiles)
dev.off()

# --- 5. Regional Dots ---
cat("  → Regional dot plot...\n")
png(file.path(output_dir, "study-difficulty-regional.png"),
    width = 10, height = 7, units = "in", res = 600, bg = "white")
print(p_dots)
dev.off()

pdf(file.path(output_dir, "study-difficulty-regional.pdf"),
    width = 10, height = 7, paper = "special", useDingbats = FALSE)
print(p_dots)
dev.off()

# --- 6. Scatter ---
cat("  → Cost-Risk scatter...\n")
png(file.path(output_dir, "study-difficulty-scatter.png"),
    width = 10, height = 8, units = "in", res = 600, bg = "white")
print(p_scatter)
dev.off()

pdf(file.path(output_dir, "study-difficulty-scatter.pdf"),
    width = 10, height = 8, paper = "special", useDingbats = FALSE)
print(p_scatter)
dev.off()

svglite::svglite(file.path(output_dir, "study-difficulty-scatter.svg"),
                  width = 10, height = 8, bg = "white")
print(p_scatter)
dev.off()

# --- 7. Summary Table ---
cat("  → Summary table...\n")
png(file.path(output_dir, "study-difficulty-table.png"),
    width = 8, height = 5, units = "in", res = 600, bg = "white")
grid::grid.draw(p_table)
dev.off()

pdf(file.path(output_dir, "study-difficulty-table.pdf"),
    width = 8, height = 5, paper = "special", useDingbats = FALSE)
grid::grid.draw(p_table)
dev.off()

# --- 8. Combined Panel ---
cat("  → Combined panel...\n")
png(file.path(output_dir, "study-difficulty-combined.png"),
    width = 18, height = 9, units = "in", res = 600, bg = "white")
print(p_combined)
dev.off()

pdf(file.path(output_dir, "study-difficulty-combined.pdf"),
    width = 18, height = 9, paper = "special", useDingbats = FALSE)
print(p_combined)
dev.off()

svglite::svglite(file.path(output_dir, "study-difficulty-combined.svg"),
                  width = 18, height = 9, bg = "white")
print(p_combined)
dev.off()

# --- 9. Alternative Combined ---
cat("  → Alternative combined panel...\n")
png(file.path(output_dir, "study-difficulty-combined-alt.png"),
    width = 11, height = 14, units = "in", res = 600, bg = "white")
print(p_combined_alt)
dev.off()

pdf(file.path(output_dir, "study-difficulty-combined-alt.pdf"),
    width = 11, height = 14, paper = "special", useDingbats = FALSE)
print(p_combined_alt)
dev.off()

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  STUDY DIFFICULTY INDEX 2026 - EXPORT COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("Files created in fig/ folder:\n")
cat("\n")
cat("  1. Heatmap (comprehensive overview):\n")
cat("     • study-difficulty-heatmap.png (600 DPI)\n")
cat("     • study-difficulty-heatmap.pdf\n")
cat("     • study-difficulty-heatmap.svg\n")
cat("\n")
cat("  2. Ranking Bar Chart (total scores):\n")
cat("     • study-difficulty-ranking.png (600 DPI)\n")
cat("     • study-difficulty-ranking.pdf\n")
cat("\n")
cat("  3. Stacked Breakdown (component contributions):\n")
cat("     • study-difficulty-stacked.png (600 DPI)\n")
cat("     • study-difficulty-stacked.pdf\n")
cat("\n")
cat("  4. Country Profiles (faceted lollipop):\n")
cat("     • study-difficulty-profiles.png (600 DPI)\n")
cat("     • study-difficulty-profiles.pdf\n")
cat("     • study-difficulty-profiles.svg\n")
cat("\n")
cat("  5. Regional Dot Plot:\n")
cat("     • study-difficulty-regional.png (600 DPI)\n")
cat("     • study-difficulty-regional.pdf\n")
cat("\n")
cat("  6. Cost-Risk Scatter Plot:\n")
cat("     • study-difficulty-scatter.png (600 DPI)\n")
cat("     • study-difficulty-scatter.pdf\n")
cat("     • study-difficulty-scatter.svg\n")
cat("\n")
cat("  7. Summary Table:\n")
cat("     • study-difficulty-table.png (600 DPI)\n")
cat("     • study-difficulty-table.pdf\n")
cat("\n")
cat("  8. Combined Panel (publication composite):\n")
cat("     • study-difficulty-combined.png (600 DPI)\n")
cat("     • study-difficulty-combined.pdf\n")
cat("     • study-difficulty-combined.svg\n")
cat("\n")
cat("  9. Alternative Combined (ranking + scatter):\n")
cat("     • study-difficulty-combined-alt.png (600 DPI)\n")
cat("     • study-difficulty-combined-alt.pdf\n")
cat("\n")
cat("Output directory:", output_dir, "\n")
cat("\n")
cat("============================================================\n")
cat("  DATA SUMMARY\n")
cat("============================================================\n")
cat("\n")
cat("Countries analysed:", nrow(study_data), "\n")
cat("Dimensions measured: 4\n")
cat("  - Financial Cost (visa fees + proof of funds)\n")
cat("  - Refusal Risk (rejection rates)\n")
cat("  - Policy Hostility (caps, bans, restrictions)\n")
cat("  - Bureaucracy (paperwork, processing)\n")
cat("\n")
cat("Difficulty Ranking (highest to lowest):\n")
cat("============================================================\n")
ranking_df <- study_data %>% 
  select(rank, country, difficulty_level, total_score, region) %>%
  as.data.frame()
names(ranking_df) <- c("Rank", "Country", "Level", "Total", "Region")
print(ranking_df)
cat("\n")
cat("Regional Summary:\n")
cat("============================================================\n")
regional_df <- study_data %>%
  group_by(region) %>%
  summarise(
    Countries = n(),
    `Avg Score` = round(mean(total_score), 1),
    `Min` = min(total_score),
    `Max` = max(total_score)
  ) %>%
  arrange(desc(`Avg Score`)) %>%
  as.data.frame()
print(regional_df)
cat("\n")
cat("============================================================\n")
cat("  Ready for publication!\n")
cat("============================================================\n")
