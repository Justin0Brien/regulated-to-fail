# ============================================================================
# International Student Mobility Sankey Diagram
# Publication-Ready Visualisation for Print
# ============================================================================
# 
# This script produces a high-quality Sankey diagram showing the flow of 
# international students from origin countries to major destination countries.
#
# Output: SVG and PDF files ready for publication
# 
# Data Sources:
#   - USA: IIE Open Doors Report (2023/24)
#   - UK: HESA Higher Education Statistics (2022/23)
#   - Australia: Department of Education (2023)
#   - Canada: CBIE/IRCC (2023)
#   - Japan: JASSO (2024)
#   - EU: DAAD/Campus France estimates (2024)
# ============================================================================

# Load required packages (install if necessary)
required_packages <- c("ggplot2", "ggalluvial", "dplyr", "scales", "svglite", 
                       "extrafont", "RColorBrewer", "tidyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Create the student flow dataset
student_flows <- data.frame(
  origin = c(
    # To United States
    "India", "China", "South Korea", "Vietnam", "EU Nationals", "Canada",
    # To United Kingdom
    "India", "China", "Nigeria", "Pakistan", "EU Nationals",
    # To Australia
    "China", "India", "Nepal", "Vietnam", "Colombia",
    # To Canada
    "India", "China", "Philippines", "Nigeria", "Vietnam",
    # To Japan
    "China", "Vietnam", "Nepal", "South Korea",
    # To South Korea
    "Vietnam", "China",
    # To European Union
    "China", "India", "North Africa", "South Korea"
  ),
  destination = c(
    # United States
    rep("United States", 6),
    # United Kingdom
    rep("United Kingdom", 5),
    # Australia
    rep("Australia", 5),
    # Canada
    rep("Canada", 5),
    # Japan
    rep("Japan", 4),
    # South Korea
    rep("South Korea", 2),
    # European Union
    rep("European Union", 4)
  ),
  students = c(
    # To United States (IIE Open Doors 2023/24)
    331602, 277398, 43149, 22000, 50000, 28998,
    # To United Kingdom (HESA 2022/23)
    173190, 154260, 72355, 34690, 95505,
    # To Australia (Dept of Education 2023)
    166420, 126487, 62379, 33000, 39724,
    # To Canada (CBIE 2023)
    319130, 100000, 32455, 21660, 17100,
    # To Japan (JASSO 2024)
    123485, 40323, 64816, 14500,
    # To South Korea
    43300, 60000,
    # To European Union (estimates)
    85000, 65000, 80000, 12000
  ),
  stringsAsFactors = FALSE
)

# Calculate totals for ordering
origin_totals <- student_flows %>%
  group_by(origin) %>%
  summarise(total = sum(students)) %>%
  arrange(desc(total))

dest_totals <- student_flows %>%
  group_by(destination) %>%
  summarise(total = sum(students)) %>%
  arrange(desc(total))

# Set factor levels for proper ordering (largest flows at top)
student_flows$origin <- factor(student_flows$origin, 
                                levels = rev(origin_totals$origin))
student_flows$destination <- factor(student_flows$destination, 
                                     levels = rev(dest_totals$destination))

# ============================================================================
# COLOUR PALETTES
# ============================================================================

# Elegant, print-friendly colour palette for origins
origin_colours <- c(
  "India"        = "#E63946",  # Warm red

"China"        = "#457B9D",  # Steel blue
  "Vietnam"      = "#2A9D8F",  # Teal
  "Nepal"        = "#F4A261",  # Sandy orange
  "Nigeria"      = "#264653",  # Dark slate
  "Pakistan"     = "#8338EC",  # Purple
  "South Korea"  = "#06D6A0",  # Mint green
  "Philippines"  = "#FB8500",  # Orange
  "Colombia"     = "#FFBE0B",  # Golden yellow
  "EU Nationals" = "#3A86FF",  # Bright blue
  "Canada"       = "#FF006E",  # Magenta
  "North Africa" = "#BC6C25"   # Sienna brown
)

# Destination colours (muted, professional)
dest_colours <- c(
  "United States"  = "#1D3557",
  "United Kingdom" = "#14213D",
  "Canada"         = "#540B0E",
  "Australia"      = "#023047",
  "European Union" = "#003049",
  "Japan"          = "#2B2D42",
  "South Korea"    = "#3D405B"
)

# ============================================================================
# CREATE THE SANKEY DIAGRAM
# ============================================================================

# Convert to alluvial format
alluvial_data <- student_flows %>%
  mutate(
    flow_id = row_number(),
    students_k = students / 1000  # Convert to thousands for cleaner labels
  )

# Create the plot
sankey_plot <- ggplot(alluvial_data,
                      aes(axis1 = origin, 
                          axis2 = destination, 
                          y = students)) +
  # Draw the flows (stratum connections)
  geom_alluvium(aes(fill = origin),
                width = 1/6,
                alpha = 0.7,
                decreasing = FALSE) +
  # Draw the nodes (strata)
  geom_stratum(width = 1/6, 
               fill = "grey95", 
               colour = "grey40",
               linewidth = 0.3) +
  # Add labels to the strata
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3.2,
            fontface = "bold",
            colour = "#2c3e50") +
  # Apply the colour palette
  scale_fill_manual(values = origin_colours,
                    name = "Origin Country") +
  # Scale for y-axis (student numbers)
  scale_y_continuous(labels = scales::label_comma(),
                     expand = expansion(mult = c(0.02, 0.02))) +
  # Minimal, elegant theme for publication
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    # Remove grid for clean look
    panel.grid = element_blank(),
    # Axis styling
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.box = "horizontal",
    # Plot margins for print
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20, unit = "pt"),
    # Title styling
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40",
                                  margin = margin(b = 15)),
    plot.caption = element_text(size = 8, hjust = 0, colour = "grey50",
                                 margin = margin(t = 15), lineheight = 1.2)
  ) +
  # Add titles and caption
  labs(
    title = "Global International Student Mobility",
    subtitle = "Flow of enrolled international students from origin countries to major destination countries",
    caption = paste0(
      "Sources: USA – IIE Open Doors (2023/24); UK – HESA (2022/23); ",
      "Australia – Dept. of Education (2023);\n",
      "Canada – CBIE/IRCC (2023); Japan – JASSO (2024); EU – DAAD/Campus France estimates.\n",
      "Student numbers represent total enrolled (stock) for the most recent available year."
    )
  ) +
  # Guides for legend layout
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# ============================================================================
# ALTERNATIVE: HORIZONTAL SANKEY (Often better for print)
# ============================================================================

sankey_horizontal <- sankey_plot +
  coord_flip() +
  theme(
    legend.position = "right",
    plot.caption = element_text(hjust = 1)
  ) +
  guides(fill = guide_legend(ncol = 1))

# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

# Output directory (same as script location)
output_dir <- getwd()

# --- Export Vertical Sankey (Default) ---

# SVG Export (vector, scalable)
svglite::svglite(
  filename = file.path(output_dir, "international-student-sankey.svg"),
  width = 12,
  height = 10,
  bg = "white"
)
print(sankey_plot)
dev.off()

# PDF Export (print-ready, CMYK-compatible)
pdf(
  file = file.path(output_dir, "international-student-sankey.pdf"),
  width = 12,
  height = 10,
  paper = "special",
  useDingbats = FALSE,  # Better font embedding
  compress = FALSE       # Maintain quality
)
print(sankey_plot)
dev.off()

# --- Export Horizontal Sankey (Alternative) ---

# SVG Export
svglite::svglite(
  filename = file.path(output_dir, "international-student-sankey-horizontal.svg"),
  width = 14,
  height = 9,
  bg = "white"
)
print(sankey_horizontal)
dev.off()

# PDF Export
pdf(
  file = file.path(output_dir, "international-student-sankey-horizontal.pdf"),
  width = 14,
  height = 9,
  paper = "special",
  useDingbats = FALSE,
  compress = FALSE
)
print(sankey_horizontal)
dev.off()

# ============================================================================
# HIGH-RESOLUTION PNG (For presentations/web)
# ============================================================================

png(
  filename = file.path(output_dir, "international-student-sankey.png"),
  width = 3600,
  height = 3000,
  res = 300,
  bg = "white"
)
print(sankey_plot)
dev.off()

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  INTERNATIONAL STUDENT SANKEY DIAGRAM - EXPORT COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("Files created:\n")
cat("  • international-student-sankey.svg (vector)\n")
cat("  • international-student-sankey.pdf (print-ready)\n")
cat("  • international-student-sankey-horizontal.svg (alternative)\n")
cat("  • international-student-sankey-horizontal.pdf (alternative)\n")
cat("  • international-student-sankey.png (300 DPI)\n")
cat("\n")
cat("Output directory:", output_dir, "\n")
cat("\n")
cat("Data Summary:\n")
cat("  Total student flows visualised:", nrow(student_flows), "\n")
cat("  Total students represented:", format(sum(student_flows$students), big.mark = ","), "\n")
cat("  Origin countries:", length(unique(student_flows$origin)), "\n")
cat("  Destination countries:", length(unique(student_flows$destination)), "\n")
cat("\n")
cat("Top 5 Origin Countries by Total Outbound Students:\n")
print(head(origin_totals, 5))
cat("\n")
cat("Top 5 Destination Countries by Total Inbound Students:\n")
print(head(dest_totals, 5))
cat("\n")
cat("============================================================\n")
cat("  Ready for publication!\n")
cat("============================================================\n")
