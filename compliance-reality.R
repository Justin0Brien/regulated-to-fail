# ============================================================================
# The Compliance Reality: International Student Visa Compliance
# Publication-Ready Visualisations for Print
# ============================================================================
# 
# This script produces high-quality figures illustrating international student
# visa compliance rates across major destination countries. The data contradicts
# the common "overstay myth" narrative with evidence from official sources.
#
# Output: PNG (600 DPI), PDF, and SVG files ready for publication
# 
# Data Sources:
#   - UK: ONS/Home Office Exit Checks (2024)
#   - USA: DHS Fiscal Year 2023 Entry/Exit Overstay Report
#   - Australia: Department of Home Affairs; Universities Australia (2024)
#   - Canada: IRCC Annual Report (2024/25)
#   - Germany: DAAD Wissenschaft weltoffen (2024)
#   - Schengen: Eurostat estimates
#
# Key Findings:
#   - UK compliance: 97.5%
#   - USA compliance: 96.4%
#   - Australia compliance: ~98%
#   - The "overstay problem" is statistically tiny vs political narrative
# ============================================================================

# Load required packages
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales", "svglite", 
                       "RColorBrewer", "ggtext", "patchwork", "forcats",
                       "grid", "gridExtra")

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
# COLOUR PALETTE
# ============================================================================

# Consistent colour scheme
colours <- list(
  compliance  = "#00b894",  # Green - compliant
  overstay    = "#d63031",  # Red - overstay
  contribute  = "#0984e3",  # Blue - economic contribution
  cost        = "#b2bec3",  # Grey - public cost
  retention   = "#6c5ce7",  # Purple - intentional retention
  neutral     = "#636e72",  # Dark grey - neutral
  background  = "#f0f4f8",  # Light grey background
  highlight   = "#fdcb6e"   # Gold - highlight/warning
)

# Country colours
country_colours <- c(
  "United Kingdom" = "#c0392b",
  "United States"  = "#2980b9",
  "Australia"      = "#27ae60",
  "Canada"         = "#e67e22",
  "Germany"        = "#8e44ad",
  "Schengen Area"  = "#16a085"
)

# ============================================================================
# DATA PREPARATION
# ============================================================================

# 1. COMPLIANCE RATES DATA
compliance_data <- data.frame(
  country = c("United Kingdom", "United States", "Australia", "Schengen Area"),
  compliance_rate = c(97.5, 96.4, 98.0, 95.0),
  overstay_rate = c(2.5, 3.6, 2.0, 5.0),
  source = c("ONS/Home Office 2024", "DHS FY2023", "Home Affairs Est.", "Eurostat Est."),
  stringsAsFactors = FALSE
) %>%
  mutate(
    country = factor(country, levels = country[order(-compliance_rate)])
  )

# 2. DETAILED STUDENT OUTCOMES (UK EXAMPLE)
uk_outcomes <- data.frame(
  outcome = c("Left on Time", "Legal Extension", "No Departure Record", "Data Matching Errors"),
  percentage = c(85, 12.5, 1.5, 1.0),
  category = c("Compliant", "Compliant", "Potential Overstay", "Unknown"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcome)),
    category = factor(category, levels = c("Compliant", "Potential Overstay", "Unknown"))
  )

# 3. ECONOMIC CONTRIBUTION DATA
economic_data <- data.frame(
  country = c("United Kingdom", "United Kingdom", "Australia", "Australia"),
  metric = c("Net Contribution", "Public Cost", "Export Value", "Public Cost Est."),
  value = c(41.9, 4.4, 48.0, 5.2),
  currency = c("£bn", "£bn", "A$bn", "A$bn"),
  type = c("benefit", "cost", "benefit", "cost"),
  stringsAsFactors = FALSE
)

# UK specific for donut
uk_economic <- data.frame(
  category = c("Net Economic Contribution", "Estimated Public Cost"),
  value = c(41.9, 4.4),
  stringsAsFactors = FALSE
) %>%
  mutate(
    percentage = value / sum(value) * 100,
    label = paste0(category, "\n£", value, "bn")
  )

# 4. COUNTRY APPROACH COMPARISON
approach_data <- data.frame(
  country = c("United Kingdom", "United States", "Australia", "Canada", "Germany"),
  compliance_rate = c(97.5, 96.4, 98.0, 98.5, NA),
  retention_rate = c(NA, NA, NA, NA, 44),  # Germany focuses on retention
  approach = c("Restrictive", "Restrictive", "Very Restrictive", "Tightening", "Welcoming"),
  policy_goal = c("Departure", "Departure", "Departure", "Departure", "Retention"),
  health_funding = c("£776/yr IHS", "Private OSHC", "Private OSHC", "Provincial", "Public/Free"),
  stringsAsFactors = FALSE
)

# 5. PERCEPTION VS REALITY DATA
perception_data <- data.frame(
  country = c("United Kingdom", "United States", "Australia"),
  perceived_overstay = c(15, 20, 12),  # Estimated public perception

  actual_overstay = c(2.5, 3.6, 2.0),
  stringsAsFactors = FALSE
) %>%
  pivot_longer(
    cols = c(perceived_overstay, actual_overstay),
    names_to = "type",
    values_to = "rate"
  ) %>%
  mutate(
    type = case_when(
      type == "perceived_overstay" ~ "Public Perception",
      type == "actual_overstay" ~ "Actual Data"
    ),
    type = factor(type, levels = c("Public Perception", "Actual Data"))
  )

# 6. CANADA CONTEXT DATA
canada_data <- data.frame(
  category = c("Legal Pathway (PGWP)", "Departed on Time", "Asylum Claims", "Other/Unknown"),
  percentage = c(35, 62.5, 1.5, 1.0),
  stringsAsFactors = FALSE
) %>%
  mutate(
    category = factor(category, levels = category),
    compliance = ifelse(category %in% c("Legal Pathway (PGWP)", "Departed on Time"), 
                        "Legal", "Other")
  )

# ============================================================================
# COMMON THEME
# ============================================================================

theme_compliance <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.3), hjust = 0,
                                margin = margin(b = 8), colour = "#2d3436"),
      plot.subtitle = element_text(size = rel(0.95), hjust = 0, colour = "#636e72",
                                    margin = margin(b = 15), lineheight = 1.2),
      plot.caption = element_text(size = rel(0.75), hjust = 0, colour = "#636e72",
                                   margin = margin(t = 15), lineheight = 1.2),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.title = element_text(face = "bold", size = rel(0.9), colour = "#2d3436"),
      axis.text = element_text(size = rel(0.85), colour = "#333333"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = rel(0.85)),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(t = 20, r = 20, b = 15, l = 15, unit = "pt")
    )
}

# ============================================================================
# FIGURE 1: THE 97% RULE - COMPLIANCE RATES BAR CHART
# ============================================================================

p_compliance <- compliance_data %>%
  ggplot(aes(x = fct_reorder(country, compliance_rate), y = compliance_rate)) +
  geom_col(fill = colours$compliance, width = 0.7) +
  geom_text(aes(label = paste0(compliance_rate, "%")), 
            hjust = -0.15, fontface = "bold", size = 5, colour = colours$compliance) +
  geom_hline(yintercept = 95, linetype = "dashed", colour = "grey60", alpha = 0.7) +
  annotate("text", x = 0.7, y = 95.3, label = "95% threshold", 
           colour = "grey50", size = 3, fontface = "italic", hjust = 0) +
  coord_flip(ylim = c(90, 102)) +
  scale_y_continuous(
    breaks = seq(90, 100, 2),
    labels = function(x) paste0(x, "%")
  ) +
  theme_compliance(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11)
  ) +
  labs(
    x = NULL,
    y = "Visa Compliance Rate",
    title = "The '97% Rule': International Student Visa Compliance",
    subtitle = "Percentage of students who departed on time or obtained legal extension",
    caption = paste0(
      "Sources: UK – ONS/Home Office Exit Checks (2024); USA – DHS FY2023 Entry/Exit Report;\n",
      "Australia – Dept. of Home Affairs estimates; Schengen – Eurostat estimates.\n",
      "Note: Y-axis starts at 90% to show variation. All countries exceed 95% compliance."
    )
  )

# ============================================================================
# FIGURE 2: STACKED BAR - COMPLIANCE VS OVERSTAY
# ============================================================================

compliance_stacked <- compliance_data %>%
  pivot_longer(
    cols = c(compliance_rate, overstay_rate),
    names_to = "status",
    values_to = "rate"
  ) %>%
  mutate(
    status = case_when(
      status == "compliance_rate" ~ "Compliant (Left/Extended)",
      status == "overstay_rate" ~ "No Departure Record"
    ),
    status = factor(status, levels = c("No Departure Record", "Compliant (Left/Extended)"))
  )

p_stacked <- ggplot(compliance_stacked, 
                    aes(x = fct_reorder(country, -rate), y = rate, fill = status)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(
    data = compliance_stacked %>% filter(status == "No Departure Record"),
    aes(label = paste0(rate, "%")),
    position = position_stack(vjust = 0.5),
    colour = "white", fontface = "bold", size = 3.5
  ) +
  scale_fill_manual(
    values = c("Compliant (Left/Extended)" = colours$compliance,
               "No Departure Record" = colours$overstay),
    name = "Visa Status"
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(80, 101)) +  # Zoom to show detail
  theme_compliance(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    legend.position = "top"
  ) +
  labs(
    x = NULL,
    y = "Percentage of Students",
    title = "Visa Compliance: The Overstay Reality",
    subtitle = "Zoomed view (80-100%) showing the tiny fraction of non-compliance",
    caption = "Red segments represent students with no departure record, not confirmed illegal overstays."
  )

# ============================================================================
# FIGURE 3: UK STUDENT OUTCOMES BREAKDOWN
# ============================================================================

outcome_colours <- c(
  "Left on Time" = colours$compliance,
  "Legal Extension" = "#55efc4",
  "No Departure Record" = colours$overstay,
  "Data Matching Errors" = colours$highlight
)

p_uk_outcomes <- uk_outcomes %>%
  ggplot(aes(x = percentage, y = outcome, fill = outcome)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), 
            hjust = -0.1, fontface = "bold", size = 4) +
  scale_fill_manual(values = outcome_colours, guide = "none") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  theme_compliance(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)
  ) +
  labs(
    x = "Percentage of Visa Holders",
    y = NULL,
    title = "UK Student Visa Outcomes: The Full Picture",
    subtitle = "What actually happens when international student visas expire",
    caption = paste0(
      "Source: ONS/Home Office Exit Checks (2024).\n",
      "97.5% of students are fully compliant (departed on time or granted legal extension).\n",
      "'No Departure Record' includes both genuine overstays and data matching issues."
    )
  )

# ============================================================================
# FIGURE 4: ECONOMIC CONTRIBUTION PIE/DONUT CHART
# ============================================================================

# Calculate positions for donut chart
uk_economic <- uk_economic %>%
  arrange(desc(category)) %>%
  mutate(
    ymax = cumsum(value),
    ymin = lag(ymax, default = 0),
    labelPosition = (ymax + ymin) / 2,
    ratio = round(value / sum(value) * 100, 1)
  )

p_economic_donut <- ggplot(uk_economic, aes(ymax = ymax, ymin = ymin, 
                                             xmax = 4, xmin = 2.5, fill = category)) +
  geom_rect(colour = "white", linewidth = 1) +
  geom_text(aes(x = 3.25, y = labelPosition, 
                label = paste0("£", value, "bn\n(", ratio, "%)")),
            size = 4, fontface = "bold", colour = "white") +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  scale_fill_manual(
    values = c("Net Economic Contribution" = colours$contribute,
               "Estimated Public Cost" = colours$cost),
    name = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40",
                                  margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0.5, colour = "grey50",
                                 margin = margin(t = 15)),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "UK International Students: Net Contributors",
    subtitle = "Economic impact of the 2021/22 student intake over their study period",
    caption = "Source: Universities UK Economic Impact Report (2023). Cost includes NHS, education, public services."
  )

# ============================================================================
# FIGURE 5: PERCEPTION VS REALITY GAP
# ============================================================================

p_perception <- perception_data %>%
  ggplot(aes(x = country, y = rate, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(rate, "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(
    values = c("Public Perception" = colours$overstay,
               "Actual Data" = colours$compliance),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  theme_compliance(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    legend.position = "top"
  ) +
  labs(
    x = NULL,
    y = "Estimated Overstay Rate",
    title = "The Perception Gap: Overstay Fears vs Facts",
    subtitle = "Public perception of student visa overstays far exceeds reality",
    caption = paste0(
      "Public perception estimates based on polling and media narrative analysis.\n",
      "Actual data from official government sources. The gap fuels anti-immigration policy."
    )
  )

# ============================================================================
# FIGURE 6: HORIZONTAL COMPARISON - BENEFITS VS COSTS
# ============================================================================

benefit_cost_data <- data.frame(
  country = c("United Kingdom", "United Kingdom", "Australia", "Australia"),
  type = c("Economic Contribution", "Public Cost", "Economic Contribution", "Public Cost"),
  value = c(41.9, 4.4, 48.0, 5.2),
  stringsAsFactors = FALSE
) %>%
  mutate(
    display_value = ifelse(type == "Public Cost", -value, value)
  )

p_benefit_cost <- ggplot(benefit_cost_data, 
                          aes(x = display_value, y = country, fill = type)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
  geom_text(aes(label = paste0(ifelse(type == "Public Cost", "-", "+"),
                                abs(value), "bn")),
            hjust = ifelse(benefit_cost_data$type == "Public Cost", 1.1, -0.1),
            fontface = "bold", size = 3.5, colour = "#2d3436") +
  scale_fill_manual(
    values = c("Economic Contribution" = colours$contribute,
               "Public Cost" = colours$cost),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(-10, 55),
    breaks = seq(-10, 50, 10),
    labels = function(x) ifelse(x < 0, paste0("-", abs(x)), x)
  ) +
  theme_compliance(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11),
    legend.position = "top"
  ) +
  labs(
    x = "Value (£bn / A$bn)",
    y = NULL,
    title = "The Economic Balance Sheet",
    subtitle = "International students are massive net contributors to host economies",
    caption = paste0(
      "Sources: UK – Universities UK (2023); Australia – Universities Australia (2024).\n",
      "Economic contribution includes tuition fees, living expenses, tourism, and post-study taxes.\n",
      "Public cost estimates include education subsidy, healthcare, and public service usage."
    )
  )

# ============================================================================
# FIGURE 7: CANADA STUDENT PATHWAYS
# ============================================================================

canada_colours <- c(
  "Legal Pathway (PGWP)" = colours$retention,
  "Departed on Time" = colours$compliance,
  "Asylum Claims" = colours$highlight,
  "Other/Unknown" = colours$neutral
)

p_canada <- canada_data %>%
  ggplot(aes(x = percentage, y = fct_rev(category), fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")),
            hjust = -0.1, fontface = "bold", size = 4) +
  scale_fill_manual(values = canada_colours, guide = "none") +
  scale_x_continuous(
    limits = c(0, 75),
    breaks = seq(0, 75, 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  theme_compliance(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)
  ) +
  labs(
    x = "Percentage of Students",
    y = NULL,
    title = "Canada: Student Visa Pathways",
    subtitle = "The PGWP route is designed policy, not a 'loophole'",
    caption = paste0(
      "Source: IRCC Annual Report (2024/25).\n",
      "97.5% of students either depart legally or use the Post-Graduation Work Permit.\n",
      "Asylum claims (~1.5%) remain a small fraction despite media focus."
    )
  )

# ============================================================================
# FIGURE 8: KEY STATISTICS SUMMARY
# ============================================================================

# Create a visually striking summary of key numbers
key_stats <- data.frame(
  stat = c("97.5%", "96.4%", "98%", "£41.9bn", "A$48bn", "1.5%"),
  description = c(
    "UK Compliance",
    "USA Compliance",
    "Australia Compliance",
    "UK Net Contribution",
    "Australia Export Value",
    "Canada Asylum Claims"
  ),
  country = c("UK", "USA", "AUS", "UK", "AUS", "CAN"),
  colour = c(
    colours$compliance, colours$compliance, colours$compliance,
    colours$contribute, colours$contribute, colours$highlight
  ),
  stringsAsFactors = FALSE
)

p_summary <- ggplot(key_stats, aes(x = 1, y = fct_rev(description))) +
  geom_tile(aes(fill = colour), width = 0.9, height = 0.85, alpha = 0.9) +
  geom_text(aes(label = stat), fontface = "bold", size = 8, colour = "white") +
  geom_text(aes(x = 2.2, label = description), hjust = 0, size = 4, colour = "#2d3436") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0.5, 4)) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0, colour = "grey40",
                                  margin = margin(b = 20)),
    plot.caption = element_text(size = 9, hjust = 0, colour = "grey50",
                                 margin = margin(t = 20)),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  labs(
    title = "The Compliance Reality: Key Statistics",
    subtitle = "Data that contradicts the 'overstay crisis' narrative",
    caption = "Sources: ONS/Home Office (UK), DHS (USA), Universities Australia, IRCC (Canada)."
  )

# ============================================================================
# FIGURE 9: COMBINED PANEL - MAIN PUBLICATION FIGURE
# ============================================================================

p_combined <- (p_compliance + p_stacked) / 
              (p_uk_outcomes + p_benefit_cost) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "The Compliance Reality: International Student Visa Data",
    subtitle = "Evidence shows the 'overstay crisis' is a myth — students are overwhelmingly compliant and economically beneficial",
    caption = paste0(
      "Sources: UK – ONS/Home Office Exit Checks (2024), Universities UK (2023); USA – DHS FY2023 Entry/Exit Report;\n",
      "Australia – Dept. of Home Affairs, Universities Australia (2024). All data represents latest available reporting periods."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5,
                                colour = "#2d3436", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "#636e72",
                                    margin = margin(b = 15)),
      plot.caption = element_text(size = 9, hjust = 0, colour = "#636e72",
                                   margin = margin(t = 15), lineheight = 1.2),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# ============================================================================
# FIGURE 10: ALTERNATIVE COMBINED - PERCEPTION FOCUS
# ============================================================================

p_combined_perception <- (p_perception | p_economic_donut) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Myth vs Reality: The Compliance Gap",
    subtitle = "Public fears about student visa overstays vastly exceed the evidence",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                                colour = "#2d3436", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "#636e72",
                                    margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# ============================================================================
# EXPORT ALL FIGURES
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  EXPORTING COMPLIANCE FIGURES...\n")
cat("============================================================\n")

# --- 1. Compliance Rates ---
cat("  → Compliance rates bar chart...\n")
png(file.path(output_dir, "compliance-rates.png"),
    width = 10, height = 6, units = "in", res = 600, bg = "white")
print(p_compliance)
dev.off()

pdf(file.path(output_dir, "compliance-rates.pdf"),
    width = 10, height = 6, paper = "special", useDingbats = FALSE)
print(p_compliance)
dev.off()

svglite::svglite(file.path(output_dir, "compliance-rates.svg"),
                  width = 10, height = 6, bg = "white")
print(p_compliance)
dev.off()

# --- 2. Stacked Compliance ---
cat("  → Stacked compliance chart...\n")
png(file.path(output_dir, "compliance-stacked.png"),
    width = 9, height = 6, units = "in", res = 600, bg = "white")
print(p_stacked)
dev.off()

pdf(file.path(output_dir, "compliance-stacked.pdf"),
    width = 9, height = 6, paper = "special", useDingbats = FALSE)
print(p_stacked)
dev.off()

# --- 3. UK Outcomes ---
cat("  → UK outcomes breakdown...\n")
png(file.path(output_dir, "compliance-uk-outcomes.png"),
    width = 10, height = 5, units = "in", res = 600, bg = "white")
print(p_uk_outcomes)
dev.off()

pdf(file.path(output_dir, "compliance-uk-outcomes.pdf"),
    width = 10, height = 5, paper = "special", useDingbats = FALSE)
print(p_uk_outcomes)
dev.off()

# --- 4. Economic Donut ---
cat("  → Economic contribution donut...\n")
png(file.path(output_dir, "compliance-economic-donut.png"),
    width = 8, height = 7, units = "in", res = 600, bg = "white")
print(p_economic_donut)
dev.off()

pdf(file.path(output_dir, "compliance-economic-donut.pdf"),
    width = 8, height = 7, paper = "special", useDingbats = FALSE)
print(p_economic_donut)
dev.off()

# --- 5. Perception Gap ---
cat("  → Perception vs reality...\n")
png(file.path(output_dir, "compliance-perception-gap.png"),
    width = 10, height = 6, units = "in", res = 600, bg = "white")
print(p_perception)
dev.off()

pdf(file.path(output_dir, "compliance-perception-gap.pdf"),
    width = 10, height = 6, paper = "special", useDingbats = FALSE)
print(p_perception)
dev.off()

svglite::svglite(file.path(output_dir, "compliance-perception-gap.svg"),
                  width = 10, height = 6, bg = "white")
print(p_perception)
dev.off()

# --- 6. Benefit vs Cost ---
cat("  → Economic balance sheet...\n")
png(file.path(output_dir, "compliance-benefit-cost.png"),
    width = 10, height = 5, units = "in", res = 600, bg = "white")
print(p_benefit_cost)
dev.off()

pdf(file.path(output_dir, "compliance-benefit-cost.pdf"),
    width = 10, height = 5, paper = "special", useDingbats = FALSE)
print(p_benefit_cost)
dev.off()

# --- 7. Canada Pathways ---
cat("  → Canada student pathways...\n")
png(file.path(output_dir, "compliance-canada-pathways.png"),
    width = 10, height = 5, units = "in", res = 600, bg = "white")
print(p_canada)
dev.off()

pdf(file.path(output_dir, "compliance-canada-pathways.pdf"),
    width = 10, height = 5, paper = "special", useDingbats = FALSE)
print(p_canada)
dev.off()

# --- 8. Key Stats Summary ---
cat("  → Key statistics summary...\n")
png(file.path(output_dir, "compliance-key-stats.png"),
    width = 9, height = 6, units = "in", res = 600, bg = "white")
print(p_summary)
dev.off()

pdf(file.path(output_dir, "compliance-key-stats.pdf"),
    width = 9, height = 6, paper = "special", useDingbats = FALSE)
print(p_summary)
dev.off()

# --- 9. Combined Main Panel ---
cat("  → Combined main panel...\n")
png(file.path(output_dir, "compliance-combined.png"),
    width = 16, height = 12, units = "in", res = 600, bg = "white")
print(p_combined)
dev.off()

pdf(file.path(output_dir, "compliance-combined.pdf"),
    width = 16, height = 12, paper = "special", useDingbats = FALSE)
print(p_combined)
dev.off()

svglite::svglite(file.path(output_dir, "compliance-combined.svg"),
                  width = 16, height = 12, bg = "white")
print(p_combined)
dev.off()

# --- 10. Combined Perception ---
cat("  → Combined perception panel...\n")
png(file.path(output_dir, "compliance-perception-combined.png"),
    width = 14, height = 7, units = "in", res = 600, bg = "white")
print(p_combined_perception)
dev.off()

pdf(file.path(output_dir, "compliance-perception-combined.pdf"),
    width = 14, height = 7, paper = "special", useDingbats = FALSE)
print(p_combined_perception)
dev.off()

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  THE COMPLIANCE REALITY - EXPORT COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("Files created in fig/ folder:\n")
cat("\n")
cat("  1. Compliance Rates (97% Rule):\n")
cat("     • compliance-rates.png (600 DPI)\n")
cat("     • compliance-rates.pdf\n")
cat("     • compliance-rates.svg\n")
cat("\n")
cat("  2. Stacked Compliance/Overstay:\n")
cat("     • compliance-stacked.png (600 DPI)\n")
cat("     • compliance-stacked.pdf\n")
cat("\n")
cat("  3. UK Outcomes Breakdown:\n")
cat("     • compliance-uk-outcomes.png (600 DPI)\n")
cat("     • compliance-uk-outcomes.pdf\n")
cat("\n")
cat("  4. Economic Contribution Donut:\n")
cat("     • compliance-economic-donut.png (600 DPI)\n")
cat("     • compliance-economic-donut.pdf\n")
cat("\n")
cat("  5. Perception vs Reality Gap:\n")
cat("     • compliance-perception-gap.png (600 DPI)\n")
cat("     • compliance-perception-gap.pdf\n")
cat("     • compliance-perception-gap.svg\n")
cat("\n")
cat("  6. Economic Balance Sheet:\n")
cat("     • compliance-benefit-cost.png (600 DPI)\n")
cat("     • compliance-benefit-cost.pdf\n")
cat("\n")
cat("  7. Canada Student Pathways:\n")
cat("     • compliance-canada-pathways.png (600 DPI)\n")
cat("     • compliance-canada-pathways.pdf\n")
cat("\n")
cat("  8. Key Statistics Summary:\n")
cat("     • compliance-key-stats.png (600 DPI)\n")
cat("     • compliance-key-stats.pdf\n")
cat("\n")
cat("  9. Combined Main Panel:\n")
cat("     • compliance-combined.png (600 DPI)\n")
cat("     • compliance-combined.pdf\n")
cat("     • compliance-combined.svg\n")
cat("\n")
cat("  10. Combined Perception Panel:\n")
cat("      • compliance-perception-combined.png (600 DPI)\n")
cat("      • compliance-perception-combined.pdf\n")
cat("\n")
cat("Output directory:", output_dir, "\n")
cat("\n")
cat("============================================================\n")
cat("  KEY DATA SUMMARY\n")
cat("============================================================\n")
cat("\n")
cat("Compliance Rates:\n")
cat("  • United Kingdom:  97.5%\n")
cat("  • United States:   96.4%\n")
cat("  • Australia:       ~98%\n")
cat("  • Schengen Area:   ~95%\n")
cat("\n")
cat("Economic Impact (UK):\n")
cat("  • Net Contribution: £41.9bn\n")
cat("  • Public Cost:      £4.4bn\n")
cat("  • Net Benefit:      £37.5bn\n")
cat("\n")
cat("Economic Impact (Australia):\n")
cat("  • Export Value:     A$48bn\n")
cat("  • (4th largest export sector)\n")
cat("\n")
cat("============================================================\n")
cat("  Ready for publication!\n")
cat("============================================================\n")
