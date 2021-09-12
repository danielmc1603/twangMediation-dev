# #' Substance use data among women
#'
#' A dataset containing the substance use condition and sexual orientation 
#' of 40293 women respondents to the 2017 & 2018 National Survey of Drug Use 
#' and Health.
#'
#' @format A data frame with 40293 rows and 24 variables:
#' \describe{
#'   \item{cigmon}{indiidual smoked any cigarettes within the past month, yes or no}
#'   \item{educ}{education level, 1 = less than high school diploma, 
#'           2 = high school diploma, 3 = some college/associates degree, 
#'           4 = college degree or higher}
#'   \item{income}{income level, 1 <= $20,000, 2 = $20,000 - $49,999,
#'           3 = $50,000 - 70,000, 4 = $75,000+}
#'   \item{NSDUHwt}{NSDUH sampling weight}
#'   \item{vestr}{NSDUH strata variable}
#'   \item{verep}{NSDUH replicate within stratum}
#'   \item{employ}{employment status, 1 = full-time employment, 
#'           2 = part-time employment, 3 = student, 4 = unemployed, 5 = other}
#'   \item{race}{1 = non-Hispanic white, 2 = non-Hispanic Black, 
#'           3 = student, 4 = multiracial/other race}
#'   \item{alc15}{iniciated alcohol use prior to 15 years old}
#'   \item{cig15}{iniciated smoking prior to 15 years old, yes or no}
#'   \item{lgb_flag}{1 = lesbian, gay or sexual, 0 = heterosexual}
#'   \item{alc_cig_depend}{individual meets criteria for 
#'           either past-year alcohol use disorder or nicotine dependence}
#'   \item{weight2y}{NSDUH sampling weights(scaled for
#'    pooling 2017 and 2018 survey years)}
#'   \item{age}{age, 1 = 18-25, 2 = 26-34, 3 = 35-49, 4 = 50+}
#' }
#' @return \item{NSDUH_female}{A sample data for demonstration}
#' 
#' @source \url{https://nsduhweb.rti.org/respweb/homepage.cfm}
#' @seealso \code{\link{wgtmed}}
#' @examples
#' \dontrun{
#' data(NSDUH_female)
#' }
"NSDUH_female"


#' Simulated data for twangMediation
#'
#' A simulate dataset for demonstrating the functions in the twangMediation 
#' package.
#'
#' @format A data frame with 500 rows and 7 variables:
#' \describe{
#'   \item{w1}{Simulated continuous covariate}
#'   \item{w2}{Simulated continuous covariate}
#'   \item{w3}{Simulated continuous covariate}
#'   \item{A}{Simulated dichotomous exposure indicator}
#'   \item{Y}{Simulated continuous outcome}
#'   \item{M}{Simulated mediator that has 11 unique values}
#'   \item{te.wgt}{Estimated inverse probability weight, estimated using GBM via the twang ps function}
#' }
#' @return \item{tMdat}{A sample of simulated data for demonstration}
#' 
#' @seealso \code{\link{wgtmed}}
#' @examples
#' \dontrun{
#' data(tMdat)
#' }
"tMdat"
