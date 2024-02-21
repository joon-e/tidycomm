#' Worlds of Journalism sample data
#'
#' A subset of data from the [Worlds of Journalism](https://worldsofjournalism.org/)
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
#' @source \samp{https://worldsofjournalism.org/data/data-and-key-tables-2012-2016}
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

#' SNS Comments data
#'
#' A dataset of 630 German participants in an online experiment. The experiment
#' investigated the effects of user comments on social network sites (SNS) on
#' individuals' perceptions of journalistic quality. The researchers varied the
#' subject of the article (factor 1: 'Copyright directive' or 'Social housing'),
#' the order of comment presentation (factor 2: before or after the
#' article) and the valence of the comments (factor 3: positive or negative).
#'
#' This dataset was created from the OSF project: \url{https://osf.io/r867v/},
#' corresponding to the paper:
#' Kümpel, A. S., & Unkel, J. (2020). Negativity wins at last: How presentation
#' order and valence of user comments affect perceptions of journalistic quality.
#' Journal of Media Psychology: Theories, Methods, and Applications, 32(2), 89–99.
#' \doi{10.1027/1864-1105/a000261}
#'
#' @format A data frame of 630 observations and 15 variables:
#' \describe{
#'   \item{age}{Age of the participant}
#'   \item{gender}{Gender of the participant, either 'female' or 'not female'}
#'   \item{education}{Level of formal education of the participant, either 'low formal education' or 'high formal education'}
#'   \item{need_cognition}{Index measuring the psychological trait of a person to enjoy thinking, calculated from several survey items}
#'   \item{prior_knowledge}{Index measuring a person's prior knowledge of the presented subject of the article, calculated from several survey items}
#'   \item{group}{Numeric id of the group that the participant was in during the experiment}
#'   \item{issue}{Subject of the article that the participant was given to read, either 'Copyright directive' or 'Social housing'}
#'   \item{order}{Order of the comments that the participant was exposed to, either 'Comments after', 'Comments before', or 'Control group'}
#'   \item{valence}{Valence of the comments that the participant was exposed to, either 'Negative', 'Positive', or 'Control group'}
#'   \item{control_group}{Indicates whether the participant was in the 'Control group' or 'Experimental group'}
#'   \item{medium_evaluation}{Index measuring participant's evaluation of the medium's quality, calculated from several survey items}
#'   \item{article_evaluation}{Index measuring participant's evaluation of the article's quality, calculated from several survey items}
#'   \item{comments_quality}{Participant's perception of the quality of the comments}
#'   \item{comments_valence}{Participant's perception of the valence of the comments}
#'   \item{article_elaboration}{Participant's measure of how much attention they put in reading the article}
#' }
#' @source \url{https://osf.io/r867v/}
"snscomments"

#' Incivil Comments Data
#'
#' A dataset of a preregistered factorial survey experiment with a nationally
#' representative sample of 964 German online users. Participants were presented
#' with manipulated user comments that included statements associated with
#' incivil discourse (such as profanity and attacks on arguments) and
#' intolerant discourse (such as offensive stereotyping and violent threats).
#' Participants rated the comments, e.g. offensiveness, harm to society, and
#' their intention to delete the comment containing the statement.
#'
#' The dataset was created from the OSF project: [Differential perceptions of and reactions to incivil and intolerant user comments](https://osf.io/w92vj),
#' corresponding to the paper:
#' Kümpel, A. S., Unkel, J (2023). Differential perceptions of and reactions to
#' incivil and intolerant user comments, Journal of Computer-Mediated
#' Communication, Volume 28, Issue 4, https://doi.org/10.1093/jcmc/zmad018
#'
#' @format A data frame of 3856 observations nested in 964 participants and 22 variables:
#' \describe{
#'   \item{participant_num}{Numeric id of the participant}
#'   \item{age}{Age of the participant}
#'   \item{male}{Gender of the participant, either 'male' or 'not male'}
#'   \item{high_education}{Level of formal education of the participant, either 'high formal education' or 'low formal education'}
#'   \item{comment_num}{Numeric id of the comment that the participant was exposed to}
#'   \item{issue}{The subject of the comment that the participant was exposed to, either 'Gender', 'Abortion', 'Climate', or 'Migration'}
#'   \item{profanity}{Whether the comment contained profanities as an indicator of incivility}
#'   \item{attacks_argument}{Whether the comment contained attacks towards arguments as an indicator of incivility}
#'   \item{offensive_stereotyping}{Whether the comment contained offensive stereotypes as an indicator of intolerant discourse}
#'   \item{violent_threats}{Whether the comment contained violent threats as an indicator of intolerant discourse}
#'   \item{offensiveness}{Rate statement whether the comment is being perceived as offensive & hostile (Scale from 1 to 7)}
#'   \item{adequacy}{Rate statement whether the comment is being perceived as necessary & accurate (Scale from 1 to 7)}
#'   \item{harm_to_society}{Rate statement whether the comment is being perceived as harmful to society (Scale from 1 to 7)}
#'   \item{deletion_intention}{Whether the participant wants to delete the comment}
#'   \item{similarity_poster}{How similar the participant feels to the person who created the post (Scale from 1 to 7)}
#'   \item{similarity_group}{How similar the participant feels to the group of people criticized in the post (Scale from 1 to 7)}
#'   \item{attitude_gender}{Rate agreement with statements on gender policies (Scale from 1 to 7)}
#'   \item{attitude_abortion}{Rate agreement with statements on abortion (Scale from 1 to 7)}
#'   \item{attitude_migration}{Rate agreement with statements on migration (Scale from 1 to 7)}
#'   \item{attitude_climate}{Rate agreement with statements on climate change (Scale from 1 to 7)}
#'   \item{left_right_placement}{Placement on a political spectrum from left to right (Scale from 1 to 9)}
#'   \item{freedom_of_speech}{Rate agreement with statements about the freedom of speech and expression (Scale from 1 to 7)}
#' }
#' @source \url{https://osf.io/w92vj}
"incvlcomments"
