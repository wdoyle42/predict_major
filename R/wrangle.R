# R/wrangle.R
# Data loading, recoding, and analytic sample construction
# All functions are called by _targets.R — no side effects

# Variable list: all columns selected from the raw ELS file
# Organized by domain; edit here to add/remove predictors
els_vars <- c(
  "stu_id",

  # Outcome
  "f3tzbch1cip2",

  # Student background
  "bygnstat", "bystlang", "byfcomp", "bysibhom",
  "bypared", "bymothed", "byfathed", "bygpared",
  "byoccum", "byoccuf", "byincome", "byses2",
  "bygrdrpt", "byriskfc",
  # "byiepflg",  # >50% missing on public-use file

  # Cognitive achievement (BY)
  "bytxmstd", "bytxrstd",

  # Attitudinal / psychosocial scales (BY)
  "bymathse", "byenglse", "byconexp", "byinstmo",
  "byactctl", "bystprep", "byacclim",

  # Aspirations & plans (BY)
  "bystexp", "byparasp", "byocc30",
  # "byocchs",   # >50% missing on public-use file

  # Behavioral engagement (BY)
  "byhmwrk", "bynsport", "byxtracu", "byworksy", "bywrkhrs",

  # School context (BY)
  "bysctrl", "byurban", "byregion",

  # HS transcript: restricted-use only — excluded from public-use analysis
  # Uncomment if working with restricted-use file:
  # "f1rhtun",  "f1rhtac",  "f1rhtvo",  "f1rhtco",
  # "f1rmat_c", "f1rsci_c", "f1reng_c", "f1rsoc_c",
  # "f1rnon_c", "f1rfin_c", "f1rfam_c", "f1rsla_c",
  # "f1rcal_c", "f1rpre_c", "f1ral2_c",
  # "f1rbio_c", "f1rche_c", "f1rphy_c", "f1rear_c",
  # "f1rapib",  "f1rapma",  "f1rapca",  "f1rapsc",
  # "f1rapen",  "f1rapso",  "f1rapne",  "f1rapcs",
  # "f1rgp",    "f1ragp",   "f1ragph",  "f1rgpa",
  # "f1rmapip", "f1rscpip", "f1rtrcc",
  # "f1racadc", "f1roccuc",

  # F1 wave (senior year 2004)
  "f1stexp",  "f1byedex", "f1mathse",
  "f1occ30",
  # "f1occhs",   # >50% missing on public-use file
  "f1psepln",
  "f1ses2",   "f1xtracu", "f1wrkhrs",
  "f1colinf", "f1himath", "f1pared",

  # F2 postsecondary context (2006)
  "f2evratt",  "f2ps1lvl", "f2ps1sec",
  # "f2ps1slc",  # all-NA on public-use file
  "f2ps1ftp",  "f2ps1rem", "f2ps1aid",
  "f2psstrt",  "f2enrgap", "f2switch",
  "f2stexp",   "f2f1edex", "f2pseexm", "f2psepln",
  "f2psapsl",  "f2ptn3ps",
  # "f2nattnd", "f2hs2ps1",  # all-NA on public-use file

  # F3TZ transcript — early enrollment only (years 1-2; safe predictors)
  # "f3tzps1ctr", "f3tzps1lvl",  # all-NA on public-use file
  "f3tzps1sec", "f3tzps1slc",
  "f3tzhs2ps1",
  "f3tzyr1ern", "f3tzyr2ern", "f3tzyr12ern",
  "f3tzyr1gpa", "f3tzyr2gpa",
  "f3tzremtot", "f3tzremmttot", "f3tzrementot",
  "f3tznumcrswd",

  # Subgroup analysis variables (not model predictors)
  "bysex", "byrace"
)

# Nominal variables to be cast as factors before recipe
# (Unordered; will receive one-hot encoding downstream)
nominal_vars <- c(
  "bygnstat", "bystlang", "byfcomp",
  "byoccum",  "byoccuf",
  "byocc30",
  # "byocchs",   # >50% missing on public-use file
  "bysctrl",  "byurban",  "byregion",
  # "byiepflg",  # >50% missing on public-use file
  "byworksy",
  # "f1rtrcc",  "f1racadc", "f1roccuc",  # restricted-use only
  "f1psepln", "f1colinf", "f1himath",
  "f1occ30",
  # "f1occhs",   # >50% missing on public-use file
  "f2ps1lvl", "f2ps1sec",
  # "f2ps1slc",  # all-NA on public-use file
  "f2ps1ftp", "f2ps1rem", "f2ps1aid",
  "f2enrgap", "f2switch", "f2pseexm",
  "f2ptn3ps",
  # "f3tzps1ctr", "f3tzps1lvl",  # all-NA on public-use file
  "f3tzps1sec", "f3tzps1slc"
)


#' Load raw ELS Stata file and select analysis variables
#'
#' @param path Path to .dta file (tracked by targets as a file target)
#' @return Tibble with selected variables; NCES negative codes recoded to NA
load_els <- function(path) {
  haven::read_dta(path) %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::any_of(els_vars)) %>%
    dplyr::mutate(across(everything(), haven::zap_labels)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ replace(.x, .x %in% -9:-1, NA)
      )
    )
}


#' Load hand-curated CIP-2 labels and collapse decisions
#'
#' @param path Path to cip2_labels.csv
#' @return Tibble with columns: cip2_code, cip2_label, cip2_collapsed_code,
#'   cip2_collapsed_label
load_cip_labels <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}


#' Full wrangling pipeline: factor encoding, outcome construction
#'
#' @param els_raw Output of load_els()
#' @param cip_labels Output of load_cip_labels()
#' @return Cleaned tibble ready for analytic sample filtering
wrangle_els <- function(els_raw, cip_labels) {

  # Build CIP collapse lookup from labels file
  cip_collapse <- cip_labels %>%
    dplyr::select(cip2_code, cip2_collapsed_code, cip2_collapsed_label) %>%
    dplyr::distinct()

  els_raw %>%
    # Encode nominal predictors as factors
    dplyr::mutate(
      dplyr::across(dplyr::any_of(nominal_vars), as.factor)
    ) %>%
    # Construct collapsed outcome factor
    dplyr::left_join(
      cip_collapse,
      by = dplyr::join_by(f3tzbch1cip2 == cip2_code)
    ) %>%
    dplyr::mutate(
      cip2 = factor(cip2_collapsed_code, levels = sort(unique(cip_collapse$cip2_collapsed_code)))
    )
}


#' Filter to analytic sample: ever-enrolled PS students with observed outcome
#'
#' @param els_clean Output of wrangle_els()
#' @return Analytic sample tibble
make_analytic_sample <- function(els_clean) {
  els_clean %>%
    dplyr::filter(
      f2evratt == 1,      # ever enrolled in postsecondary
      !is.na(cip2)        # observed bachelor's degree major
    )
}


#' Summarize missingness rates by variable for appendix table
#'
#' @param data Analytic sample
#' @return Tibble with variable, n_missing, pct_missing, sorted descending
summarize_missingness <- function(data) {
  data %>%
    dplyr::select(-stu_id, -bysex, -byrace, -f3tzbch1cip2,
                  -cip2_collapsed_code, -cip2_collapsed_label) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          n_missing  = ~ sum(is.na(.x)),
          pct_missing = ~ mean(is.na(.x))
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to    = c("variable", ".value"),
      names_pattern = "^(.+)_(n_missing|pct_missing)$"
    ) %>%
    dplyr::arrange(dplyr::desc(pct_missing))
}
