library(tidyverse)

landsdel_storkreds <- tribble(
  ~landsdel, ~storkreds,
  "hovedstaden", "københavn",
  "hovedstaden", "københavns omegn",
  "hovedstaden", "nordsjælland",
  "hovedstaden", "bornholm",
  "sjælland-syddanmark", "sjælland",
  "sjælland-syddanmark", "fyn",
  "sjælland-syddanmark", "sydjylland",
  "midtjylland-nordjylland", "østjylland",
  "midtjylland-nordjylland", "vestjylland",
  "midtjylland-nordjylland", "nordjylland"
)

# kredsmandater ----
kredsmandater <- tribble(
  ~storkreds, ~mandater,
  "københavn", 16,
  "københavns omegn", 11,
  "nordsjælland", 10,
  "bornholm", 2,
  "sjælland", 20,
  "fyn", 12,
  "sydjylland", 18,
  "østjylland", 18,
  "vestjylland", 13,
  "nordjylland", 15
)

# tillægsmandater ----
tillægsmandater <- tribble(
  ~landsdel, ~tillægsmandater,
  "hovedstaden", 11,
  "sjælland-syddanmark", 15,
  "midtjylland-nordjylland", 14
)

# valres import dst ----
valres <- read_csv2("https://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=*&OMR%C3%85DE=*&Tid=*")
names(valres) <- tolower(names(valres))
valres <- valres %>% select(-tid)
partierne <- c("A. Socialdemokratiet", "B. Radikale Venstre", "C. Det Konservative Folkeparti",
               "F. SF - Socialistisk Folkeparti", "I. Liberal Alliance", "K. Kristendemokraterne",
               "O. Dansk Folkeparti", "V. Venstre, Danmarks Liberale Parti", "Ø. Enhedslisten - De Rød-Grønne",
               "Å. Alternativet")

# landsdels_res ----
landsdels_res <- valres %>%
  filter(str_detect(område, "^LANDSDEL")) %>%
  mutate(landsdel = str_remove_all(område, "LANDSDEL ") %>% tolower()) %>%
  select(landsdel, parti = valres,  stemmer = indhold) %>%
  filter(parti %in% partierne) %>%
  mutate(parti = str_sub(parti, 1, 1))

# storkreds_res ----
storkreds_res <- valres %>%
  filter(str_detect(område, "STORKREDS$")) %>%
  mutate(storkreds = str_remove_all(område, "S STORKREDS") %>% tolower()) %>%
  select(storkreds, parti = valres,  stemmer = indhold) %>%
  filter(parti %in% partierne) %>%
  mutate(parti = str_sub(parti, 1, 1))

# clean up ----
rm(valres)

# Save data to package

devtools::use_data(kredsmandater, tillægsmandater, landsdel_storkreds, landsdels_res, storkreds_res, overwrite = TRUE)
