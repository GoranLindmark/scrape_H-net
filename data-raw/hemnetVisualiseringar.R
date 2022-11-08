library(ScrapeHemnet)
library(tidyverse)

data <- runScrapeHemnet()

data <-
  data %>%
  mutate(period = paste0(str_sub(ar,3,4), manad = ifelse(manad<10, paste0("0",manad), manad))) %>%
  mutate(period = as.numeric(period))


data %>%
  filter( !is.na(kvmPris)) %>%
  filter( !is.na(ar)) %>%
  group_by(period) %>%
  summarize(antal = n(),
            snitt = mean(kvmPris)) %>% view()

corData <-
  data %>%
  select(rum, hyra, pris, kvmPris, area, kvm) %>%
  filter( str_detect( kvm, " ", negate = T) ) %>% 
  mutate(area = ifelse(area == "södermalm",1,
                       ifelse(area == "kungsholmen",2,
                              ifelse(area == "vasastan",3,
                                     ifelse(area == "östermalm",4,0))))) %>% 
  drop_na() %>% 
  mutate( kvm = as.numeric(kvm))

cor(corData[, 1:6])


data %>%
  filter( !is.na(kvmPris)) %>%
  filter( !is.na(period)) %>%
  filter( !is.na(area)) %>%
  filter(period > 2000) %>%
  group_by(area, period) %>%
  summarize(antal = n(),
            snitt = mean(kvmPris)) %>%
  ggplot()+
  geom_line (aes(x=factor(period), y=snitt), group = 1)+
  facet_grid(cols = vars(area))


data %>%
  filter( !is.na(kvmPris)) %>%
  filter( !is.na(ar)) %>%
  group_by(ar, manad) %>%
  summarize(antal = n()) %>%
  ggplot()+
  geom_line (aes(x= manad, y=antal))+
  facet_grid(cols = vars(ar))


data %>%
  filter(!is.na(kvmPris)) %>%
  filter(!is.na(rum)) %>%
  group_by(area, rum) %>%
  summarize(
    antal = n(),
    snittPris = mean(kvmPris)) %>%
  ggplot()+
  geom_col(aes(x=factor(rum), y=antal)) +
  facet_grid( cols = vars(area))

data %>%
  filter(!is.na(kvmPris)) %>%
  filter(!is.na(rum)) %>%
  group_by(area, rum) %>%
  summarize(
    maxPris = max(kvmPris),
    minPris = min(kvmPris),
    diffPris = maxPris - minPris) %>%
  ggplot()+
  geom_col(aes(x=factor(rum), y=diffPris)) +
  facet_grid( cols = vars(area))+



data %>%
  filter(!is.na(kvmPris)) %>%
  filter(!is.na(rum)) %>%
  filter(!is.na(hyra)) %>%
  mutate(hyra_bin = ntile(hyra, n=5)) %>%
  group_by(area, hyra_bin) %>%
  summarize(
    snittPris  = mean(kvmPris),
    .groups = "drop") %>%
  ggplot( aes( x=hyra_bin, y=snittPris))+
  geom_col( )+
  facet_grid( cols = vars(area))
