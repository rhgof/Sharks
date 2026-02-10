library(tidyverse)
library(paletteer)

library(RUtils)

#' Generate a softened IGV palette for n species
#'
#' Uses ggsci IGV colors with reduced alpha for a less loud appearance.
#' @param n Number of colors needed
#' @param alpha Alpha transparency (0-1). Default 0.75.
#' @return A character vector of n hex colors with alpha baked in
igvPalette <- function(n, alpha = 0.75) {
  cols <- as.character(paletteer_d("ggsci::default_igv", n))
  adjustcolor(cols, alpha.f = alpha)
}

# Target shark species palette: distinct colors for White, Tiger, Bull + grey for Other
TARGET_SPECIES_PALETTE <- c(
  "White Shark" = "#E41A1C",
  "Tiger Shark" = "#377EB8",
  "Bull Shark"  = "#FF7F00",
  "Other"       = "#999999"
)

# ----- shared period-prep helper -----
#' Prepare data with period grouping column
#'
#' Uses floor_date() to produce real Date values so the x-axis naturally
#' orders chronologically (Jul '23 → Jun '24) rather than calendar order.
#'
#' @param data A tibble with a Date column
#' @param period "week" or "month"
#' @return A list with `data` (mutated with Period column) and `x_scale`
#'   (a ggplot scale to add to the chart)
preparePeriodData <- function(data, period) {
  # Ensure Date is a Date type
  if (is.character(data$Date)) {
    data <- data |> mutate(Date = dmy(Date))
  }

  if (period == "week") {
    data <- data |> mutate(Period = floor_date(Date, "week"))
    x_scale <- scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b '%y"
    )
  } else {
    data <- data |> mutate(Period = floor_date(Date, "month"))
    x_scale <- scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b '%y"
    )
  }

  list(data = data, x_scale = x_scale)
}

# ----- shared theme for drumline charts -----
drumlineChartTheme <- function() {
  list(
    labs(caption = makeCaption("Source: NSW DPI SMART Drumline Program 2023-24")),
    customTheme(),
    theme(
      plot.title.position = "plot",
      plot.title = element_text(size = 12, face = "bold", hjust = 0,
                                margin = margin(0, 0, 0, 0)),
      legend.title = element_text(size = 9),
      axis.text.x = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  )
}

# ----- chart drumline captures by period -----
chartDrumLineCaptureByPeriod <- function(data, period = c("week", "month"),
                                         title, subtitle) {
  period <- match.arg(period)
  prep <- preparePeriodData(data, period)
  data <- prep$data

  # Count captures per period and species
  counts <- data |>
    count(Period, Species)

  # Order species factor by total count (most common first) for legend readability
  species_order <- data |>
    count(Species, sort = TRUE) |>
    pull(Species)
  counts <- counts |>
    mutate(Species = factor(Species, levels = rev(species_order)))

  chart <- ggplot(counts, aes(x = Period, y = n, fill = Species)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = setNames(igvPalette(length(species_order)), species_order)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    prep$x_scale +
    ggtitle(title, subtitle) +
    xlab("") +
    ylab("Number of Captures") +
    drumlineChartTheme()

  return(chart)
}

# ----- chart target sharks vs other -----
chartDrumLineTargetVsOther <- function(data, period = c("week", "month"),
                                        title, subtitle) {
  period <- match.arg(period)
  prep <- preparePeriodData(data, period)
  data <- prep$data

  # Recode species: keep White/Tiger/Bull, everything else → "Other"
  target_sharks <- c("White Shark", "Tiger Shark", "Bull Shark")
  data <- data |>
    mutate(Species = if_else(Species %in% target_sharks, Species, "Other"))

  # Count captures per period and species
  counts <- data |>
    count(Period, Species)

  # Fixed factor order: White, Tiger, Bull, Other (Other stacks on top)
  species_levels <- c("White Shark", "Tiger Shark", "Bull Shark", "Other")
  counts <- counts |>
    mutate(Species = factor(Species, levels = rev(species_levels)))

  chart <- ggplot(counts, aes(x = Period, y = n, fill = Species)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = TARGET_SPECIES_PALETTE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    prep$x_scale +
    ggtitle(title, subtitle) +
    xlab("") +
    ylab("Number of Captures") +
    drumlineChartTheme()

  return(chart)
}
