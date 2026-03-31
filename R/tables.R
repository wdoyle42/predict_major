# R/tables.R
# Table construction functions
# Each returns a tibble; paper.Rmd formats via kableExtra


#' Table 1: Analytic sample descriptive statistics
#'
#' Reports means/SDs for continuous variables and
#' proportions for categorical variables.
#'
#' @param data Analytic sample
#' @return Tibble formatted for kableExtra
make_descriptives_table <- function(data) {
  continuous_vars <- c(
    "bytxmstd", "bytxrstd", "bymathse", "byenglse",
    "byconexp", "byinstmo", "byactctl", "bystprep",
    "byses2", "byincome", "byhmwrk", "bywrkhrs",
    "f3tzyr1gpa", "f3tzyr2gpa", "f3tzyr1ern", "f3tzyr2ern"
  )

  data %>%
    dplyr::select(dplyr::any_of(continuous_vars)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      n       = dplyr::n() - sum(is.na(value)),
      mean    = mean(value, na.rm = TRUE),
      sd      = sd(value, na.rm = TRUE),
      missing = mean(is.na(value))
    ) %>%
    dplyr::arrange(variable)
}


#' Table 2: Overall and per-class performance metrics
#'
#' @param performance_metrics Output of compute_metrics()
#' @param per_class_metrics Output of compute_per_class_metrics()
#' @return Tibble formatted for kableExtra
make_performance_table <- function(performance_metrics, per_class_metrics) {
  performance_metrics %>%
    dplyr::select(.metric, .estimate) %>%
    dplyr::rename(metric = .metric, value = .estimate)
}


#' Table A1 (appendix): Full variable list with sources and coding
#'
#' Hard-coded documentation table — update when variables change.
#'
#' @return Tibble of variable metadata
make_variable_list_table <- function() {
  tibble::tribble(
    ~variable,      ~label,                                    ~source,  ~wave,   ~type,
    "bytxmstd",     "Math standardized T score",               "Test",   "BY",    "Continuous",
    "bytxrstd",     "Reading standardized T score",            "Test",   "BY",    "Continuous",
    "bymathse",     "Math self-efficacy scale",                "Survey", "BY",    "Continuous",
    "byenglse",     "English self-efficacy scale",             "Survey", "BY",    "Continuous",
    "byconexp",     "Control expectation scale",               "Survey", "BY",    "Continuous",
    "byinstmo",     "Instrumental motivation scale",           "Survey", "BY",    "Continuous",
    "byactctl",     "Effort and persistence scale",            "Survey", "BY",    "Continuous",
    "bystprep",     "Class preparation scale",                 "Survey", "BY",    "Continuous",
    "byacclim",     "School academic climate scale",           "Admin",  "BY",    "Continuous",
    "bystexp",      "Student educational expectations",        "Survey", "BY",    "Ordinal",
    "byparasp",     "Parent educational aspirations",          "Survey", "BY",    "Ordinal",
    "byocc30",      "Expected occupation at age 30",           "Survey", "BY",    "Nominal",
    "byocchs",      "Expected occupation after HS",            "Survey", "BY",    "Nominal",
    "bypared",      "Parents' highest education",              "Survey", "BY",    "Ordinal",
    "byses2",       "SES composite v.2",                       "Admin",  "BY",    "Continuous",
    "byincome",     "Family income 2001",                      "Survey", "BY",    "Ordinal",
    "bygrdrpt",     "Grades repeated K-10",                    "Survey", "BY",    "Continuous",
    "byriskfc",     "Academic risk factor count",              "Admin",  "BY",    "Continuous",
    "byhmwrk",      "Hours/week on homework",                  "Survey", "BY",    "Continuous",
    "bynsport",     "Interscholastic sports count",            "Survey", "BY",    "Continuous",
    "byxtracu",     "School-sponsored activities count",       "Survey", "BY",    "Continuous",
    "f1stexp",      "Student educational expectations (F1)",   "Survey", "F1",    "Ordinal",
    "f1byedex",     "BY-to-F1 BA expectation change",         "Admin",  "F1",    "Nominal",
    "f1mathse",     "Math self-efficacy (F1)",                 "Survey", "F1",    "Continuous",
    "f1psepln",     "Postsecondary plans after HS",            "Survey", "F1",    "Nominal",
    "f1himath",     "Highest math course taken",               "Transcript", "F1", "Ordinal",
    "f1ragp",       "Academic GPA (transcript)",               "Transcript", "F1", "Continuous",
    "f1rapib",      "Total AP/IB courses",                     "Transcript", "F1", "Continuous",
    "f1rmapip",     "Math coursetaking pipeline",              "Transcript", "F1", "Ordinal",
    "f1rscpip",     "Science coursetaking pipeline",           "Transcript", "F1", "Ordinal",
    "f2ps1lvl",     "Level of first PS institution",           "Survey", "F2",    "Nominal",
    "f2ps1slc",     "Selectivity of first PS institution",     "Survey", "F2",    "Ordinal",
    "f2stexp",      "Educational expectations (F2)",           "Survey", "F2",    "Ordinal",
    "f2f1edex",     "F1-to-F2 BA expectation change",         "Admin",  "F2",    "Nominal",
    "f2switch",     "Transferred institutions",                "Survey", "F2",    "Binary",
    "f3tzyr1gpa",   "GPA in year 1 (transcript)",             "Transcript", "F3", "Continuous",
    "f3tzyr2gpa",   "GPA in year 2 (transcript)",             "Transcript", "F3", "Continuous",
    "f3tzyr1ern",   "Credits earned year 1 (transcript)",     "Transcript", "F3", "Continuous",
    "f3tzyr2ern",   "Credits earned year 2 (transcript)",     "Transcript", "F3", "Continuous",
    "f3tzremtot",   "Remedial courses taken (transcript)",    "Transcript", "F3", "Continuous"
  )
}
