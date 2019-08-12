# Download WOJ data as "WoJ.sav" from
# https://worldsofjournalism.org/research/2012-2016-study/data-and-key-tables

library(dplyr, warn.conflicts = FALSE)

WoJ_full <- haven::read_sav("data-raw/WoJ.sav")

# Select variables

countries <- c("UK", "Austria", "Switzerland", "Germany", "Denmark")

WoJ <- WoJ_full %>% select(
    country = COUNTRY,
    reach = T8,
    employment = C2,
    temp_contract = O1,
    autonomy_selection = C9,
    autonomy_emphasis = C10,
    ethics_1 = C13A,
    ethics_2 = C13B,
    ethics_3 = C13C,
    ethics_4 = C13D,
    work_experience = C17,
    trust_parliament = O4A,
    trust_government = O4B,
    trust_parties = O4C,
    trust_politicians = O4D
  ) %>%
  mutate_at(vars(country, employment, temp_contract, reach),
            labelled::to_factor) %>%
  mutate_all(labelled::user_na_to_na) %>%
  mutate_all(labelled::remove_val_labels) %>%
  mutate(ethics_1 = 6 - ethics_1, # reverse item
         employment = stringr::str_extract(employment, "([^\\s]+)")) %>%
  filter(country %in% c(countries),
         emplyoment != "Other")


## Take Sample

set.seed(85582)

WoJ <- WoJ %>%
  tidyr::drop_na(starts_with("ethics"), starts_with("trust"), reach) %>%
  sample_n(1200) %>%
  droplevels()


readr::write_csv(WoJ, "data-raw/WoJ.csv")

usethis::use_data(WoJ, overwrite = TRUE)
