#' @title runScraper
#' @return totRes
#' @export
#'
#' @importFrom magrittr %>%
#' @import 
#'
runScrapeHemnet <- function(){
  # library(rvest)
  # library(xml2)
  # library(tidyverse)
  # library(data.table)
  # library(purrr)
  # library(lubridate)



kungsholmen <- "898748"
södermalm <- "898472"
vasastan <- "925970"
östermalm <- "473448"


söder <- scrapeHemnet(södermalm)
kungsholmen <- scrapeHemnet(kungsholmen)
vasastan <- scrapeHemnet(vasastan)
östermalm <- scrapeHemnet(östermalm)

söder <-
  söder %>%
  mutate(area = "södermalm")

kungsholmen <-
  kungsholmen %>%
  mutate(area = "kungsholmen")

vasastan <-
  vasastan %>%
  mutate(area = "vasastan")

östermalm <-
  östermalm %>%
  mutate(area = "östermalm")

totRes <- bind_rows(söder, kungsholmen, vasastan, östermalm)


}

