#' Worlds of Journalism sample data
#'
#' A subset of data from the [Worlds of Journalism](https://www.worldsofjournalism.org/)
#' 2012-16 study containing survey data of 1,200 journalists from five European
#' countries.
#'
#' @format A data frame with 1200 rows and 15 variables:
#' \describe{
#'   \item{country}{Country of residence}
#'   \item{reach}{Reach of medium}
#'   \item{employment}{Current employment situation}
#'   \item{temp_contract}{Type of contract (if current employment situation is
#'     either full-time or part-time}
#'   \item{autonomy_selection}{Autonomy in news story selection, scale from 1
#'     (*no freedom at all*) to 5 (*complete freedom*)}
#'   \item{autonomy_emphasis}{Autonomy in news story emphasis, scale from 1
#'     (*no freedom at all*) to 5 (*complete freedom*)}
#'   \item{ethics_1}{Agreement with statement "Journalists should always adhere
#'     to codes of professional ethics, regardless of situation and context",
#'     scale from 1 (*strongly disagree*) to 5 (*strongly agree*)
#'     (*reverse-coded!*)}
#'   \item{ethics_2}{Agreement with statement "What is ethical in journalism
#'     depends on the specific situation.",
#'     scale from 1 (*strongly disagree*) to 5 (*strongly agree*)}
#'   \item{ethics_3}{Agreement with statement "What is ethical in journalism
#'     is a matter of personal judgment.",
#'     scale from 1 (*strongly disagree*) to 5 (*strongly agree*)}
#'   \item{ethics_4}{Agreement with statement "It is acceptable to set aside
#'     moral standards if extraordinary circumstances require it.",
#'     scale from 1 (*strongly disagree*) to 5 (*strongly agree*)}
#'   \item{work_experience}{Work experience as a journalist in years}
#'   \item{trust_parliament}{Trust placed in parliament,
#'     scale from 1 (*no trust at all*) to 5 (*complete trust*)}
#'   \item{trust_government}{Trust placed in government,
#'     scale from 1 (*no trust at all*) to 5 (*complete trust*)}
#'   \item{trust_parties}{Trust placed in parties,
#'     scale from 1 (*no trust at all*) to 5 (*complete trust*)}
#'   \item{trust_politicians}{Trust placed in politicians in general,
#'     scale from 1 (*no trust at all*) to 5 (*complete trust*)}
#' }
#' @source \url{https://worldsofjournalism.org/data/data-and-key-tables-2012-2016}
"WoJ"

#' Facebook posts reliability test
#'
#' 45 political facebook posts coded by 6 coders for an intercoder reliability test,
#' focused on populist messages.
#'
#' @format A data frame with 270 rows and 7 variables
#' \describe{
#'   \item{post_id}{Numeric id of the coded Facebook post}
#'   \item{coder_id}{Numeric id of the coder}
#'   \item{type}{Type of Facebook post, one of "link", "photo", "status", or "video}
#'   \item{n_pictures}{Amount of pictures attached to the post, ranges from 0 to 6}
#'   \item{pop_elite}{Populism indicator: Does the Facebook post attack elites?,
#'     0 = "no attacks on elites",
#'     1 = "attacks political actors",
#'     2 = "attacks public administration actors",
#'     3 = "attacks economical actors",
#'     4 = "attacks media actors/journalists",
#'     9 = "attacks other elites"}
#'   \item{pop_people}{Populism indicator: Does the Facebook refer to 'the people'?,
#'     0 = "does not refer to 'the people'",
#'     1 = "refers to 'the people'"}
#'   \item{pop_othering}{Populism indicator: Does the Facebook attack 'others'?,
#'     0 = "no attacks on 'others'",
#'     1 = "attacks other cultures",
#'     2 = "attacks other political stances",
#'     3 = "attacks other 'others'"}
#' }
"fbposts"
