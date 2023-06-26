# Incivil comments
# This dataset is created from the OSF project: https://osf.io/w92vj,
# corresponding to the paper:
# Kümpel, A. S., Unkel, J (2023). Differential perceptions of and reactions to
# incivil and intolerant user comments, Journal of Computer-Mediated
# Communication,Volume 28, Issue 4,, https://doi.org/10.1093/jcmc/zmad018

# Load data
incvlcomments <- readr::read_csv("data-raw/incvlcomments.csv")

# Data cleaning
incvlcomments <- incvlcomments %>%
  dplyr::filter(speeder == FALSE)

# Rename and mutate variables
incvlcomments <- incvlcomments %>%
  dplyr::select(-c(EID, speeder, attitude, block, political_satisfaction,
            starts_with("bfi"), starts_with("use"))) %>%
  dplyr::rename(
    participant_num = PID,
    comment_num = iteration
  ) %>%
  dplyr::select(participant_num, age, male, high_education, comment_num, issue,
         profanity, attacks_argument, offensive_stereotyping, violent_threats,
         offensiveness, adequacy, harm_to_society, deletion_intention,
         similarity_poster, similarity_group, attitude_gender, attitude_abortion,
         attitude_migration, attitude_climate, left_right_placement,
         freedom_of_speech)

# Save data
usethis::use_data(incvlcomments, overwrite = TRUE)
