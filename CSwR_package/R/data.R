#' Nuuk temperature data
#'
#' Annual summaries of monthly mean temperatures in Nuuk from 1867 to 2013.
#'
#' @format
#' A data frame with 147 rows and 6 columns:
#' \describe{
#'   \item{Year}{Year (numeric)}
#'   \item{Temperature}{Annual mean temperature}
#'   \item{Median}{Annual median of montly mean temperatures}
#'   \item{High}{Annual maximum of montly mean temperatures}
#'   \item{Low}{Annual minimum of montly mean temperatures}
#'   \item{Range}{High - Low}
#' }
#'
#' @source <https://crudata.uea.ac.uk/cru/data/greenland>
"nuuk"

#' South west Greenland temperature data
#'
#' Monthly average temperatures in degree Celsius in Nuuk and Qaqortoq from 1873 to 2013.
#'
#' @format
#' A data frame with 147 rows and 5 columns:
#' \describe{
#'   \item{Year}{Year (numeric)}
#'   \item{Month}{Month (encoded as a factor with levels 1 to 12)}
#'   \item{Temp_Nuuk}{Montly mean temperature in Nuuk}
#'   \item{Temp_Qaqortoq}{Montly mean temperature in Qaqortoq}
#'   \item{Temp_diff}{Temperature difference, `Temp_Nuuk - Temp_Qaqortoq`}
#' }
#'
#' @source <https://crudata.uea.ac.uk/cru/data/greenland>
"greenland"

#' Social media news sharing
#'
#' A dataset containing features about articles published by Mashable in a period
#' of two years from January 7, 2013, to January 7, 2015. The purpose of collecting
#' the data was to predict the number of shares of the news articles on social networks.
#' Compared to the source, seven features are excluded. Two (url and a timestamp)
#' are not relevant predictors and five are redundant or almost redundant, leading
#' to collinearity.
#'
#' @format
#' A data frame with 39,644 rows and 53 columns:
#' \describe{
#' \item{n_tokens_title}{Number of words in the title}
#' \item{n_tokens_content}{Number of words in the content}
#' \item{n_unique_tokens}{Rate of unique words in the content}
#' \item{num_hrefs}{Number of links}
#' \item{num_self_hrefs}{Number of links to other articles published by Mashable}
#' \item{num_imgs}{Number of images}
#' \item{num_videos}{Number of videos}
#' \item{average_token_length}{Average length of the words in the content}
#' \item{num_keywords}{Number of keywords in the metadata}
#' \item{data_channel_is_lifestyle}{Is data channel 'Lifestyle'?}
#' \item{data_channel_is_entertainment}{Is data channel 'Entertainment'?}
#' \item{data_channel_is_bus}{Is data channel 'Business'?}
#' \item{data_channel_is_socmed}{Is data channel 'Social Media'?}
#' \item{data_channel_is_tech}{Is data channel 'Tech'?}
#' \item{data_channel_is_world}{Is data channel 'World'?}
#' \item{kw_min_min}{Worst keyword (min. shares)}
#' \item{kw_max_min}{Worst keyword (max. shares)}
#' \item{kw_avg_min}{Worst keyword (avg. shares)}
#' \item{kw_max_max}{Best keyword (max. shares)}
#' \item{kw_avg_max}{Best keyword (avg. shares)}
#' \item{kw_min_avg}{Avg. keyword (min. shares)}
#' \item{kw_max_avg}{Avg. keyword (max. shares)}
#' \item{kw_avg_avg}{Avg. keyword (avg. shares)}
#' \item{self_reference_min_shares}{Min. shares of referenced articles in Mashable}
#' \item{self_reference_avg_sharess}{Avg. shares of referenced articles in Mashable}
#' \item{weekday_is_monday}{Was the article published on a Monday?}
#' \item{weekday_is_tuesday}{Was the article published on a Tuesday?}
#' \item{weekday_is_wednesday}{Was the article published on a Wednesday?}
#' \item{weekday_is_thursday}{Was the article published on a Thursday?}
#' \item{weekday_is_friday}{Was the article published on a Friday?}
#' \item{weekday_is_saturday}{Was the article published on a Saturday?}
#' \item{weekday_is_sunday}{Was the article published on a Sunday?}
#' \item{LDA_00}{Closeness to LDA topic 0}
#' \item{LDA_01}{Closeness to LDA topic 1}
#' \item{LDA_02}{Closeness to LDA topic 2}
#' \item{LDA_03}{Closeness to LDA topic 3}
#' \item{LDA_04}{Closeness to LDA topic 4}
#' \item{global_subjectivity}{Text subjectivity}
#' \item{global_sentiment_polarity}{Text sentiment polarity}
#' \item{global_rate_positive_words}{Rate of positive words in the content}
#' \item{global_rate_negative_words}{Rate of negative words in the content}
#' \item{rate_positive_words}{Rate of positive words among non-neutral tokens}
#' \item{rate_negative_words}{Rate of negative words among non-neutral tokens}
#' \item{avg_positive_polarity}{Avg. polarity of positive words}
#' \item{min_positive_polarity}{Min. polarity of positive words}
#' \item{max_positive_polarity}{Max. polarity of positive words}
#' \item{avg_negative_polarity}{Avg. polarity of negative  words}
#' \item{min_negative_polarity}{Min. polarity of negative  words}
#' \item{max_negative_polarity}{Max. polarity of negative  words}
#' \item{title_subjectivity}{Title subjectivity}
#' \item{title_sentiment_polarity}{Title polarity}
#' \item{abs_title_subjectivity}{Absolute subjectivity level}
#' \item{abs_title_sentiment_polarity}{Absolute polarity level}
#' \item{shares}{Number of shares (target)}
#' }
#'
#' @source <https://archive.ics.uci.edu/dataset/332/online+news+popularity>
"news"

#' Vegetable sale
#'
#' Weekly sale of frozen vegetables in different stores.
#'
#' @format
#' A data frame with 1056 rows and 3 columns:
#' \describe{
#'  \item{\code{sale}}{a numeric vector. Number of items sold in a week}
#'  \item{\code{normalSale}}{a numeric vector. Estimated normal sale that week}
#'  \item{\code{store}}{a factor. Id number of the different stores}
#'  }
#'
#' @source Obtained by package author.
"vegetables"

#' Torsion angles
#'
#' The phi and psi torsion angles between peptide planes for the human protein
#' 1HMP.
#'
#' @format
#' A data frame with 419 rows and 5 columns:
#' \describe{
#'  \item{\code{chain}}{a character. The chain (either A or B) of the protein}
#'  \item{\code{AA}}{a character. The three-letter amino acid name}
#'  \item{\code{pos}}{an integer. Position in the amino acid sequence}
#'  \item{\code{phi}}{numeric. The phi angle}
#'  \item{\code{psi}}{numeric. The psi angle}
#'  }
#'
#' @source <https://warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/ramachandran>
"angle"
