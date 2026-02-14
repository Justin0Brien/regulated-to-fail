# ==============================================================================
# UK University Fee Cap Analysis: Market vs Capped Fees
# ==============================================================================
#
# This script generates print-ready figures illustrating the gap between:
# - The current regulated domestic fee cap (~£9,535)
# - The break-even cost of teaching (cost of delivery)
# - The "market rate" as proxied by international student fees
#
# Key insight: If the cap were lifted, fees would diverge dramatically based on
# reputation and demand, with elite institutions potentially tripling fees.
#
# Data Sources:
# - Russell Group (2024): Analysis of teaching deficits (~£4,000/student loss)
# - Complete University Guide (2024/25): International tuition fees
# - Higher Education Policy Institute (HEPI): Differential fee research
#
# ==============================================================================

# --- Load Required Packages ---
packages <- c("ggplot2", "dplyr", "tidyr", "scales", "svglite", "patchwork", 
              "forcats", "ggtext")
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

# University tiers
tiers <- c("Global Elite\n(Oxbridge, Imperial, LSE)", 
           "Russell Group\n(Manchester, Bristol, Leeds)", 
           "Modern / Post-92\n(Teesside, Coventry, UWE)")

tier_short <- c("Global Elite", "Russell Group", "Modern / Post-92")

# Fee data (annual, in GBP)
# Current domestic cap (2025/26 estimate, indexed from £9,250)
current_cap <- c(9535, 9535, 9535)

# Break-even cost: actual cost to teach a student (Russell Group deficit analysis)
# Elite: higher research intensity, smaller class sizes
# Russell: typical research-intensive
# Modern: teaching-focused, lower overheads
break_even <- c(16000, 13500, 11500)

# Market rate: international fee proxy (unregulated, demand-driven)
# Elite: global brand premium, some science courses reach £45k
# Russell: standardised around this band
# Modern: closer to cost of delivery
market_rate <- c(35000, 24500, 14500)

# Create data frame for grouped bar chart
fee_data <- data.frame(
  tier = rep(tiers, 3),
  tier_short = rep(tier_short, 3),
  fee_type = rep(c("Current Cap", "Break-Even Cost", "Market Rate"), each = 3),
  fee = c(current_cap, break_even, market_rate)
)

fee_data$tier <- factor(fee_data$tier, levels = tiers)
fee_data$tier_short <- factor(fee_data$tier_short, levels = tier_short)
fee_data$fee_type <- factor(fee_data$fee_type, 
                            levels = c("Current Cap", "Break-Even Cost", "Market Rate"))

# Create data frame for lollipop/dumbbell chart
lollipop_data <- data.frame(
  tier = tiers,
  tier_short = tier_short,
  current = current_cap,
  break_even = break_even,
  market = market_rate,
  deficit_per_student = break_even - current_cap,
  market_premium = market_rate - current_cap
)

lollipop_data$tier <- factor(lollipop_data$tier, levels = rev(tiers))
lollipop_data$tier_short <- factor(lollipop_data$tier_short, levels = rev(tier_short))

# ==============================================================================
# 2. DEFINE THEME AND COLOUR PALETTE
# ==============================================================================

theme_fee_cap <- function() {
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

# Colour palette
col_cap <- "#95a5a6"      # Grey - current cap
col_breakeven <- "#e67e22" # Orange - break-even
col_market <- "#8e44ad"    # Purple - market rate
col_elite <- "#8e44ad"     # Purple for elite
col_russell <- "#2980b9"   # Blue for Russell
col_modern <- "#95a5a6"    # Grey for Modern

# ==============================================================================
# 3. FIGURE 1: GROUPED BAR CHART (MAIN COMPARISON)
# ==============================================================================

p1_bars <- ggplot(fee_data, aes(x = tier_short, y = fee, fill = fee_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Value labels on bars
  geom_text(aes(label = paste0("£", formatC(fee, format = "d", big.mark = ","))),
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, fontface = "bold") +
  # Colour scale
  scale_fill_manual(values = c("Current Cap" = col_cap, 
                                "Break-Even Cost" = col_breakeven,
                                "Market Rate" = col_market)) +
  # Y axis formatting
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(0, 42000),
                     breaks = seq(0, 40000, 10000),
                     expand = expansion(mult = c(0, 0.05))) +
  # Labels
  labs(
    title = "The Great Divergence: What Universities Could Charge",
    subtitle = "Comparing current capped fee against break-even cost and market rate\n(international fee proxy) across university tiers",
    x = NULL,
    y = "Annual Tuition Fee (£)",
    caption = "Sources: Russell Group deficit analysis (2024); Complete University Guide International Fees (2024/25); HEPI"
  ) +
  theme_fee_cap() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, face = "bold")
  )

# Save
ggsave(file.path(output_dir, "fee-cap-comparison-bars.png"), p1_bars,
       width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-comparison-bars.pdf"), p1_bars,
       width = 10, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-comparison-bars.svg"), p1_bars,
       width = 10, height = 7)

# ==============================================================================
# 4. FIGURE 2: DUMBBELL/RANGE CHART (GAP VISUALISATION)
# ==============================================================================

p2_dumbbell <- ggplot(lollipop_data) +
  # Segment from cap to market
  geom_segment(aes(x = current, xend = market, y = tier_short, yend = tier_short),
               color = "grey70", linewidth = 3, alpha = 0.3) +
  # Segment from cap to break-even (the "deficit zone")
  geom_segment(aes(x = current, xend = break_even, y = tier_short, yend = tier_short),
               color = col_breakeven, linewidth = 3, alpha = 0.5) +
  # Current cap point
  geom_point(aes(x = current, y = tier_short), 
             color = col_cap, size = 6, shape = 19) +
  # Break-even point
  geom_point(aes(x = break_even, y = tier_short), 
             color = col_breakeven, size = 6, shape = 19) +
  # Market rate point
  geom_point(aes(x = market, y = tier_short), 
             color = col_market, size = 6, shape = 19) +
  # Labels for points
  geom_text(aes(x = current, y = tier_short, label = paste0("£", formatC(current, format = "d", big.mark = ","))),
            vjust = -1.5, size = 3, color = col_cap, fontface = "bold") +
  geom_text(aes(x = break_even, y = tier_short, label = paste0("£", formatC(break_even, format = "d", big.mark = ","))),
            vjust = -1.5, size = 3, color = col_breakeven, fontface = "bold") +
  geom_text(aes(x = market, y = tier_short, label = paste0("£", formatC(market, format = "d", big.mark = ","))),
            vjust = -1.5, size = 3, color = col_market, fontface = "bold") +
  # Reference line at current cap
  geom_vline(xintercept = 9535, linetype = "dashed", color = col_cap, linewidth = 0.5) +
  annotate("text", x = 9535, y = 0.5, label = "Current\nCap", 
           size = 2.5, hjust = 1.1, color = col_cap) +
  # X axis
  scale_x_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(0, 40000),
                     breaks = seq(0, 40000, 10000)) +
  # Labels
  labs(
    title = "The Fee Gap: From Capped to Market Price",
    subtitle = "Orange zone = deficit (cost exceeds cap). Grey zone = market premium.",
    x = "Annual Tuition Fee (£)",
    y = NULL,
    caption = "Sources: Russell Group (2024); Complete University Guide (2024/25)"
  ) +
  theme_fee_cap() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )

# Add legend manually
legend_df <- data.frame(
  x = c(6000, 16000, 28000),
  y = c(3.5, 3.5, 3.5),
  label = c("Current Cap", "Break-Even", "Market Rate"),
  color = c(col_cap, col_breakeven, col_market)
)

p2_dumbbell <- p2_dumbbell +
  geom_point(data = legend_df, aes(x = x, y = y), 
             color = c(col_cap, col_breakeven, col_market), size = 4) +
  geom_text(data = legend_df, aes(x = x, y = y, label = label),
            hjust = -0.2, size = 3, color = c(col_cap, col_breakeven, col_market))

# Save
ggsave(file.path(output_dir, "fee-cap-dumbbell.png"), p2_dumbbell,
       width = 10, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-dumbbell.pdf"), p2_dumbbell,
       width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-dumbbell.svg"), p2_dumbbell,
       width = 10, height = 6)

# ==============================================================================
# 5. FIGURE 3: DEFICIT WATERFALL (PER-STUDENT LOSS)
# ==============================================================================

deficit_data <- data.frame(
  tier = factor(tier_short, levels = tier_short),
  cap = current_cap,
  deficit = break_even - current_cap,
  break_even_cost = break_even
)

p3_deficit <- ggplot(deficit_data, aes(x = tier)) +
  # Base cap bar
  geom_col(aes(y = cap), fill = col_cap, width = 0.6) +
  # Deficit bar (stacked on top)
  geom_col(aes(y = break_even_cost), fill = col_breakeven, width = 0.6, alpha = 0.7) +
  geom_col(aes(y = cap), fill = col_cap, width = 0.6) +
  # Deficit annotation
  geom_segment(aes(x = as.numeric(tier) + 0.35, xend = as.numeric(tier) + 0.35,
                   y = cap, yend = break_even_cost),
               arrow = arrow(length = unit(0.15, "cm"), ends = "both"),
               color = "#c0392b", linewidth = 0.8) +
  geom_label(aes(x = as.numeric(tier) + 0.35, y = (cap + break_even_cost) / 2,
                 label = paste0("-£", formatC(deficit, format = "d", big.mark = ","))),
             fill = "#fadbd8", color = "#c0392b", size = 3.5, fontface = "bold",
             label.size = 0.3) +
  # Value labels
  geom_text(aes(y = cap / 2, label = paste0("Cap: £", formatC(cap, format = "d", big.mark = ","))),
            color = "white", size = 3.5, fontface = "bold") +
  # Y axis
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(0, 18000),
                     breaks = seq(0, 18000, 3000),
                     expand = expansion(mult = c(0, 0.02))) +
  # Labels
  labs(
    title = "The Deficit Reality: Teaching at a Loss",
    subtitle = "Universities lose money on every domestic student. The gap between the\nfee cap (grey) and break-even cost (orange) represents the per-student deficit.",
    x = NULL,
    y = "Annual Fee / Cost (£)",
    caption = "Source: Russell Group analysis (2024) estimates ~£4,000 deficit per student at research-intensive universities"
  ) +
  theme_fee_cap() +
  theme(axis.text.x = element_text(size = 10, face = "bold"))

# Save
ggsave(file.path(output_dir, "fee-cap-deficit.png"), p3_deficit,
       width = 9, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-deficit.pdf"), p3_deficit,
       width = 9, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-deficit.svg"), p3_deficit,
       width = 9, height = 6)

# ==============================================================================
# 6. FIGURE 4: MARKET PREMIUM LOLLIPOP
# ==============================================================================

premium_data <- data.frame(
  tier = factor(tier_short, levels = rev(tier_short)),
  premium = market_rate - current_cap,
  multiplier = market_rate / current_cap
)

p4_premium <- ggplot(premium_data, aes(x = premium, y = tier)) +
  # Lollipop stem
  geom_segment(aes(x = 0, xend = premium, y = tier, yend = tier),
               color = col_market, linewidth = 2, alpha = 0.7) +
  # Lollipop head
  geom_point(aes(size = multiplier), color = col_market, alpha = 0.8) +
  # Value labels
  geom_text(aes(label = paste0("+£", formatC(premium, format = "d", big.mark = ","),
                                " (", round(multiplier, 1), "x)")),
            hjust = -0.15, size = 4, fontface = "bold", color = col_market) +
  # Reference line
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.5) +
  # Size scale (hidden)
  scale_size_continuous(range = c(8, 16), guide = "none") +
  # X axis
  scale_x_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(-1000, 35000),
                     breaks = seq(0, 30000, 10000)) +
  # Labels
  labs(
    title = "The Reputation Premium: What Top Universities Could Charge",
    subtitle = "Market premium over current cap. Multiplier shows how many times\nhigher fees would be in a deregulated market.",
    x = "Premium Over Current Cap (£)",
    y = NULL,
    caption = "Market rate based on international fee proxy (Complete University Guide, 2024/25)"
  ) +
  theme_fee_cap() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold")
  )

# Save
ggsave(file.path(output_dir, "fee-cap-premium.png"), p4_premium,
       width = 10, height = 5, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-premium.pdf"), p4_premium,
       width = 10, height = 5, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-premium.svg"), p4_premium,
       width = 10, height = 5)

# ==============================================================================
# 7. FIGURE 5: TIER CARDS (INFOGRAPHIC STYLE)
# ==============================================================================

# Create a more stylized single figure showing the three tiers
tier_card_data <- data.frame(
  tier = factor(c("Global Elite", "Russell Group", "Modern / Post-92"),
                levels = c("Global Elite", "Russell Group", "Modern / Post-92")),
  examples = c("Oxbridge, Imperial, LSE", "Manchester, Bristol, Leeds", "Teesside, Coventry, UWE"),
  market_rate = c(33000, 24500, 14500),
  current_cap = c(9535, 9535, 9535),
  tier_color = c(col_elite, col_russell, col_modern),
  note = c("Science courses reach £45k", "High demand = pricing power", "Closer to cost of delivery")
)

p5_cards <- ggplot(tier_card_data, aes(x = tier, y = market_rate)) +
  # Base columns
  geom_col(aes(fill = tier), width = 0.7, alpha = 0.9) +
  # Current cap reference line
  geom_hline(yintercept = 9535, linetype = "dashed", color = "grey40", linewidth = 1) +
  annotate("text", x = 0.5, y = 10500, label = "Current Cap £9,535", 
           hjust = 0, size = 3, color = "grey40", fontface = "italic") +
  # Market rate labels
  geom_text(aes(label = paste0("£", formatC(market_rate, format = "d", big.mark = ","))),
            vjust = -0.5, size = 5, fontface = "bold", color = "white") +
  geom_text(aes(label = paste0("£", formatC(market_rate, format = "d", big.mark = ",")),
                y = market_rate + 1500),
            size = 5, fontface = "bold") +
  # Example universities
  geom_text(aes(y = market_rate / 2, label = examples),
            color = "white", size = 3.5, fontface = "italic") +
  # Colour scale
  scale_fill_manual(values = c("Global Elite" = col_elite, 
                                "Russell Group" = col_russell, 
                                "Modern / Post-92" = col_modern),
                    guide = "none") +
  # Y axis
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(0, 40000),
                     breaks = seq(0, 40000, 10000),
                     expand = expansion(mult = c(0, 0.08))) +
  # Labels
  labs(
    title = "Estimated Market Rate by University Tier",
    subtitle = "What fees might look like in a deregulated market, based on international fee proxies",
    x = NULL,
    y = "Annual Tuition Fee (£)",
    caption = "Sources: Complete University Guide (2024/25); Russell Group; HEPI"
  ) +
  theme_fee_cap() +
  theme(axis.text.x = element_text(size = 11, face = "bold"))

# Save
ggsave(file.path(output_dir, "fee-cap-tier-cards.png"), p5_cards,
       width = 9, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-tier-cards.pdf"), p5_cards,
       width = 9, height = 7, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-tier-cards.svg"), p5_cards,
       width = 9, height = 7)

# ==============================================================================
# 8. FIGURE 6: FACETED BREAKDOWN (INDIVIDUAL TIER DETAIL)
# ==============================================================================

# Create detailed breakdown for each tier
breakdown_data <- fee_data %>%
  mutate(
    fee_label = paste0("£", formatC(fee, format = "d", big.mark = ",")),
    fee_type_ordered = factor(fee_type, levels = c("Market Rate", "Break-Even Cost", "Current Cap"))
  )

p6_facet <- ggplot(breakdown_data, aes(x = fee_type_ordered, y = fee, fill = fee_type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = fee_label), vjust = -0.5, size = 3.5, fontface = "bold") +
  facet_wrap(~tier_short, nrow = 1) +
  scale_fill_manual(values = c("Current Cap" = col_cap, 
                                "Break-Even Cost" = col_breakeven,
                                "Market Rate" = col_market),
                    guide = "none") +
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ","),
                     limits = c(0, 42000),
                     breaks = seq(0, 40000, 10000),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Fee Breakdown by University Tier",
    subtitle = "Each tier faces a different gap between regulated cap and market reality",
    x = NULL,
    y = "Annual Fee (£)",
    caption = "Sources: Russell Group (2024); Complete University Guide (2024/25)"
  ) +
  theme_fee_cap() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

# Save
ggsave(file.path(output_dir, "fee-cap-facet-breakdown.png"), p6_facet,
       width = 12, height = 6, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-facet-breakdown.pdf"), p6_facet,
       width = 12, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-facet-breakdown.svg"), p6_facet,
       width = 12, height = 6)

# ==============================================================================
# 9. COMBINED PANEL
# ==============================================================================

# Create compact versions for combined view
p1_compact <- p1_bars + 
  labs(subtitle = NULL, caption = NULL, title = "1. The Great Divergence") +
  theme(plot.title = element_text(size = 11), legend.position = "bottom",
        legend.text = element_text(size = 7))

p3_compact <- p3_deficit + 
  labs(subtitle = NULL, caption = NULL, title = "2. Per-Student Deficit") +
  theme(plot.title = element_text(size = 11))

p4_compact <- p4_premium + 
  labs(subtitle = NULL, caption = NULL, title = "3. Market Premium") +
  theme(plot.title = element_text(size = 11))

p5_compact <- p5_cards + 
  labs(subtitle = NULL, caption = NULL, title = "4. Tier Market Rates") +
  theme(plot.title = element_text(size = 11))

# Combine
combined_fees <- (p1_compact | p3_compact) / 
                 (p4_compact | p5_compact) +
  plot_annotation(
    title = "The Uncapped Market: What UK Universities Could Charge",
    subtitle = "Analysis of fee cap constraints vs market rates (international fee proxy)",
    caption = "Sources: Russell Group (2024); Complete University Guide International Fees (2024/25); HEPI",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Save
ggsave(file.path(output_dir, "fee-cap-combined.png"), combined_fees,
       width = 14, height = 12, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "fee-cap-combined.pdf"), combined_fees,
       width = 14, height = 12, device = cairo_pdf)
ggsave(file.path(output_dir, "fee-cap-combined.svg"), combined_fees,
       width = 14, height = 12)

# ==============================================================================
# 10. KEY STATISTICS SUMMARY
# ==============================================================================

key_stats <- data.frame(
  Metric = c(
    "Current Domestic Cap (2025/26)",
    "Average Break-Even Cost",
    "Average Market Rate",
    "Average Per-Student Deficit",
    "Elite Market Premium",
    "Elite Fee Multiplier"
  ),
  Value = c(
    "£9,535",
    paste0("£", formatC(mean(break_even), format = "d", big.mark = ",")),
    paste0("£", formatC(mean(market_rate), format = "d", big.mark = ",")),
    paste0("-£", formatC(mean(break_even - current_cap), format = "d", big.mark = ",")),
    paste0("+£", formatC(market_rate[1] - current_cap[1], format = "d", big.mark = ",")),
    paste0(round(market_rate[1] / current_cap[1], 1), "x")
  )
)

# ==============================================================================
# 11. OUTPUT SUMMARY
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("UK UNIVERSITY FEE CAP ANALYSIS - Figure Generation Complete\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\n")
cat("Individual figures generated (PNG, PDF, SVG):\n")
cat("  1. fee-cap-comparison-bars   - Grouped bar chart comparison\n")
cat("  2. fee-cap-dumbbell          - Range/dumbbell chart\n")
cat("  3. fee-cap-deficit           - Per-student deficit waterfall\n")
cat("  4. fee-cap-premium           - Market premium lollipop\n")
cat("  5. fee-cap-tier-cards        - Tier infographic bars\n")
cat("  6. fee-cap-facet-breakdown   - Faceted tier breakdown\n")
cat("\n")
cat("Combined panel:\n")
cat("  7. fee-cap-combined          - 4-panel overview\n")
cat("\n")
cat("Key Statistics:\n")
print(key_stats, row.names = FALSE)
cat("\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
