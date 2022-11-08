#' @title runScraper
#' @return totRes
#' @export
#'
#' @importFrom magrittr %>%

#'
runScrapeHemnet <- function(){


kungsholmen <- "898748"
sodermalm <- "898472"
vasastan <- "925970"
ostermalm <- "473448"


soder <- scrapeHemnet(sodermalm)
kungsholmen <- scrapeHemnet(kungsholmen)
vasastan <- scrapeHemnet(vasastan)
ostermalm <- scrapeHemnet(ostermalm)

soder <-
  soder %>%
  mutate(area = "södermalm")

kungsholmen <-
  kungsholmen %>%
  mutate(area = "kungsholmen")

vasastan <-
  vasastan %>%
  mutate(area = "vasastan")

ostermalm <-
  ostermalm %>%
  mutate(area = "östermalm")

totRes <- bind_rows(soder, kungsholmen, vasastan, ostermalm)


}

