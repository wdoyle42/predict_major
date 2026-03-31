# R/recipe.R
# Recipe construction for the keras MLP pipeline
# Called by _targets.R; depends on wrangle.R for nominal_vars


#' Build tidymodels recipe for ELS major prediction
#'
#' Preprocessing steps (in order):
#'   1. Assign non-predictor roles (ID, subgroup, original outcome)
#'   2. Add missingness indicators for key transcript variables
#'   3. Median imputation for numeric predictors
#'   4. Mode imputation for nominal predictors
#'   5. novel() safety valve before dummy encoding
#'   6. One-hot encoding of all nominal predictors
#'   7. Zero-variance filter
#'   8. Normalize all numeric predictors (critical for keras)
#'
#' @param train_data Training split from make_split()
#' @return Unprepped recipe object
build_recipe <- function(train_data) {

  recipes::recipe(cip2 ~ ., data = train_data) %>%

    # ── Non-predictor roles ────────────────────────────────────────────────
    recipes::update_role(stu_id,             new_role = "id") %>%
    recipes::update_role(bysex,              new_role = "subgroup") %>%
    recipes::update_role(byrace,             new_role = "subgroup") %>%
    recipes::update_role(f3tzbch1cip2,       new_role = "original_outcome") %>%
    recipes::update_role(cip2_collapsed_code,  new_role = "id") %>%
    recipes::update_role(cip2_collapsed_label, new_role = "id") %>%

    # ── Missingness indicators for transcript variables ────────────────────
    # The *fact* of missing transcript data is informative about PS pathways
    recipes::step_indicate_na(
      f3tzyr1ern, f3tzyr2ern, f3tzyr1gpa, f3tzyr2gpa
    ) %>%

    # ── Imputation ────────────────────────────────────────────────────────
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_impute_mode(recipes::all_nominal_predictors()) %>%

    # ── Dummy encoding ────────────────────────────────────────────────────
    # step_novel() catches factor levels in test/CV folds not seen in training
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    # one_hot = TRUE: no reference category dropped; keras needs dense numeric input
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%

    # ── Housekeeping ──────────────────────────────────────────────────────
    # Drop zero-variance columns (emerge from sparse one-hot cells)
    recipes::step_zv(recipes::all_predictors()) %>%

    # Normalize to mean=0, sd=1 — required for stable keras training
    recipes::step_normalize(recipes::all_numeric_predictors())
}


#' Face validity check: prep and bake recipe, report dimensions
#'
#' @param rec Unprepped recipe from build_recipe()
#' @param train_data Training data
#' @return List with prepped recipe and baked training dimensions
check_recipe <- function(rec, train_data) {
  prepped <- recipes::prep(rec, training = train_data)
  baked   <- recipes::bake(prepped, new_data = NULL)

  list(
    prepped         = prepped,
    n_rows          = nrow(baked),
    n_predictors    = ncol(baked) - 1,  # exclude outcome
    predictor_names = setdiff(names(baked), "cip2")
  )
}
