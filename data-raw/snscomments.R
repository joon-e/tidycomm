# SNS comments
# This dataset is created from the OSF project: https://osf.io/r867v/,
# corresponding to the paper:
# Kümpel, A. S., & Unkel, J. (2020). Negativity wins at last: How presentation
# order and valence of user comments affect perceptions of journalistic quality.
# Journal of Media Psychology: Theories, Methods, and Applications, 32(2), 89–99.
# https://doi.org/10.1027/1864-1105/a000261

# Load data
snscomments <- readr::read_csv("data-raw/snscomments.csv")

# Data cleaning
snscomments <- snscomments %>%
  mutate(
    time = ifelse(TIME005 >= 10, TRUE, FALSE),
    time = ifelse(GR01 %in% c(1,2,3,4,6,7,8,9) & TIME004 < 5, FALSE, time)
  ) %>%
  filter(time)

# Rename and mutate variables
snscomments <- snscomments %>%
  dplyr::rename(
    age = SD02_01,
    comments_quality = TC02_01,
    comments_valence = TC02_02,
    article_elaboration = TC01_01
  ) %>%
  dplyr::mutate(
    gender = case_when(
      SD01 == 1 ~ 'female',
      SD01 == 2 | is.na(SD01) ~ 'not female',
      TRUE ~ as.character(SD01)
    ),
    education = ifelse(SD03 %in% c(1, 2, 3, 4, 5, 6, 9), "low formal education",
                       "high formal education"),
    group = GR01,
    issue = recode_factor(GR02_01,
                          `1` = "Copyright directive",
                          `2` = "Social housing"),
    order = recode_factor(GR02_02,
                          `0` = "Control group",
                          `-1` = "Comments after",
                          `1` = "Comments before"),
    valence = recode_factor(GR02_03,
                            `0` = "Control group",
                            `-1` = "Negative",
                            `1` = "Positive"),
    control_group = ifelse(order == "Control group" & valence == "Control group", "Control group", "Experimental group")
  )

# Create indices
snscomments <- snscomments %>%
  tidycomm::add_index(medium_evaluation, KV03_01, KV03_02, KV03_03, KV03_04, na.rm = TRUE) %>%
  tidycomm::add_index(article_evaluation, QE01_01, QE01_02, QE01_03, QE01_04, QE01_05, QE01_06, QE01_07, na.rm = TRUE) %>%
  tidycomm::add_index(need_cognition, KV01_01, KV01_02, KV01_03, KV01_04, KV01_05, KV01_06, KV01_07, na.rm = TRUE) %>%
  tidycomm::add_index(prior_knowledge, KV02_01, KV02_02, KV02_03, KV02_04, na.rm = TRUE) %>%
  mutate(across(everything(), ~replace_na(., NA)))

# Select variables of interest
snscomments <- snscomments %>%
  select(
    age, gender, education, need_cognition, prior_knowledge,
    group, issue, order, valence, control_group,
    medium_evaluation, article_evaluation,
    comments_quality, comments_valence,
    article_elaboration
  )

# Save data
usethis::use_data(snscomments, overwrite = TRUE)
