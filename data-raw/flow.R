# https://www.altinget.dk/artikel/saadan-fordeles-folketingets-mandater
# https://www.dst.dk/valg/Valg1487635/valgopg/valgopgStor10.htm

library(mandatfordeling)
library(magrittr)

ft15 <- beregn_mandater(storkreds_res)

b <- pollsDK::get_berlingske() %>%
  filter(datetime == max(datetime)) %>%
  group_by(parti = letter) %>%
  summarise(procent_now = mean(percent))

res <- landsdels_res %>%
  group_by(parti) %>%
  summarise(stemmer = sum(stemmer)) %>%
  mutate(procent_then = stemmer / sum(stemmer) * 100)

data <- full_join(b, res, by = "parti") %>%
  filter(parti != "D")

now <- data %>%
  pull(procent_now) %>%
  sum()

data <- data %>%
  mutate(procent_now = procent_now * (100 / now))

dif <- data %>%
  mutate(dif = procent_now / procent_then) %>%
  select(parti, dif)

storkredse <- storkreds_res$storkreds %>% unique()

storkreds_new <- map_dfr(storkredse, function(my_storkreds){
  storkreds_res %>%
    filter(storkreds == my_storkreds) %>%
    left_join(dif, by = "parti") %>%
    transmute(storkreds = storkreds,
              parti = parti,
              stemmer = stemmer * dif)
})

sum(storkreds_res$stemmer) == sum(storkreds_new$stemmer)

storkreds_new$to_D <- storkreds_new$stemmer - (storkreds_new$stemmer * (100-2.2) / 100)
storkreds_new$stemmer <- storkreds_new$stemmer * (100-2.2) / 100

D <- storkreds_new %>%
  group_by(storkreds) %>%
  summarise(to_D = sum(to_D)) %>%
  mutate(parti = "D", stemmer = to_D) %>%
  select(-to_D)

all <- storkreds_new %>% select(-to_D )

res_new <- bind_rows(all, D) %>% arrange(storkreds, parti)

sum(res_new$stemmer) == sum(storkreds_res$stemmer)

storkreds_votes <- res_new

