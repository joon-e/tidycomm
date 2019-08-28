# FB Posts

fbposts <- readr::read_csv("data-raw/fbposts.csv",
                           col_types = readr::cols(
                             post_id = readr::col_integer(),
                             coder_id = readr::col_integer(),
                             type = readr::col_character(),
                             n_pictures = readr::col_integer(),
                             pop_elite = readr::col_integer(),
                             pop_people = readr::col_integer(),
                             pop_othering = readr::col_integer()
                           ))

usethis::use_data(fbposts, overwrite = TRUE)
