library(tidyverse)

library(RUtils)

source(codeFile("chartDrumLineCaptures.R"))

# Named vector mapping long-form SD_region names to canonical short names
SD_REGION_LONG_TO_SHORT <- c(
  "Ballina to Lennox Head" = "Ballina",
  "Barrenjoey - Barrenjoey to Warriewood" = "Barrenjoey",
  "Bondi - Bondi to Little Bay" = "Bondi",
  "Central Coast South - Forresters to MacMasters" = "Central Coast South",
  "Coffs Harbour - Coffs Harbour to Sawtell" = "Coffs Harbour",
  "Eurobodalla - Surf Beach to Barlings Beach" = "Eurobodalla",
  "Kiama - Jones Beach to Gerringong" = "Kiama",
  "Lake Macquarie -Blacksmiths to Frazer Park" = "Lake Macquarie",
  "Manly - Manly to Turimetta" = "Manly",
  "Merimbula - Pambula to Tura Head" = "Merimbula",
  "Newcastle - Stockton to Redhead" = "Newcastle",
  "Port Macquarie \u2013 Port Macquarie to Tacking Point" = "Port Macquarie",
  "Shellharbour - Windang to Minnamurra" = "Shellharbour",
  "Yamba - Iluka Bluff to Angourie Point" = "Yamba"
)

#' Normalize SD_region names to canonical short form
#'
#' Maps long-form region names (e.g. "Ballina to Lennox Head") to their
#' canonical short form ("Ballina"). Names already in short form pass through.
#'
#' @param sd_region Character vector of SD_region names
#' @return Character vector with long names replaced by short canonical names
normalizeSdRegion <- function(sd_region) {
  matched <- unname(SD_REGION_LONG_TO_SHORT[sd_region])
  if_else(is.na(matched), sd_region, matched)
}

#' Chart drumline captures by SD region (north to south)
#'
#' Horizontal bar chart of total captures per region, ordered from
#' northernmost (top) to southernmost (bottom).
#'
#' @param data Tibble with SD_region column (from drumline CSVs)
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @return A ggplot object
chartDrumLineCaptureByRegion <- function(data, title, subtitle) {
  coords <- read_csv(inputFile("sd_region_coordinates.csv"),
                     show_col_types = FALSE)

  counts <- data |>
    mutate(SD_region = normalizeSdRegion(SD_region)) |>
    count(SD_region, name = "Captures") |>
    left_join(coords |> select(SD_region, Latitude), by = "SD_region")

  # Order regions north (top) to south (bottom): most negative lat at bottom
  region_order <- counts |>
    arrange(desc(Latitude)) |>
    pull(SD_region)
  counts <- counts |>
    mutate(SD_region = factor(SD_region, levels = rev(region_order)))

  ggplot(counts, aes(x = SD_region, y = Captures)) +
    geom_col(fill = "#4E79A7") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    ggtitle(title, subtitle) +
    xlab("") +
    ylab("Number of Captures") +
    drumlineChartTheme()
}

#' Chart drumline target sharks by SD region
#'
#' Stacked horizontal bar chart per region showing White, Tiger, Bull sharks
#' vs all other species, ordered north (top) to south (bottom).
#'
#' @param data Tibble with SD_region and Species columns
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @return A ggplot object
chartDrumLineRegionTargetVsOther <- function(data, title, subtitle) {
  coords <- read_csv(inputFile("sd_region_coordinates.csv"),
                     show_col_types = FALSE)

  target_sharks <- c("White Shark", "Tiger Shark", "Bull Shark")

  counts <- data |>
    mutate(
      SD_region = normalizeSdRegion(SD_region),
      Species = if_else(Species %in% target_sharks, Species, "Other")
    ) |>
    count(SD_region, Species, name = "Captures") |>
    left_join(coords |> select(SD_region, Latitude), by = "SD_region")

  # Order regions north (top) to south (bottom)
  region_order <- counts |>
    distinct(SD_region, Latitude) |>
    arrange(desc(Latitude)) |>
    pull(SD_region)
  # Order target species by total captures (largest first), Other always last
  target_order <- counts |>
    filter(Species != "Other") |>
    summarise(Total = sum(Captures), .by = Species) |>
    arrange(desc(Total)) |>
    pull(Species)
  species_levels <- c("Other", rev(target_order))

  counts <- counts |>
    mutate(
      SD_region = factor(SD_region, levels = rev(region_order)),
      Species = factor(Species, levels = species_levels)
    )

  ggplot(counts, aes(x = SD_region, y = Captures, fill = Species)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = TARGET_SPECIES_PALETTE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    ggtitle(title, subtitle) +
    xlab("") +
    ylab("Number of Captures") +
    drumlineChartTheme() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = alpha("white", 0.8),
                                       colour = NA)
    )
}
