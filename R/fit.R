# R/fit.R
# Model tuning, parameter selection, and final fit
# All functions called by _targets.R


#' Build Latin hypercube tuning grid
#'
#' @param size Number of parameter combinations to evaluate
#' @param seed Random seed for reproducibility
#' @return Tibble of hyperparameter combinations
build_tuning_grid <- function(size = 20, seed = 2002) {
  set.seed(seed)
  dials::grid_latin_hypercube(
    dials::hidden_units(range = c(32L, 256L)),
    dials::epochs(range       = c(50L,  300L)),
    dials::penalty(range      = c(-5, -1), trans = scales::log10_trans()),
    dials::dropout(range      = c(0, 0.5)),
    dials::learn_rate(range   = c(-4, -2), trans = scales::log10_trans()),
    size = size
  )
}


#' Tune model across CV folds
#'
#' Uses tune_grid() with ROC AUC (hand) as primary metric.
#' Saves predictions for post-hoc analysis.
#'
#' @param wf Workflow from build_workflow()
#' @param folds CV folds from make_folds()
#' @param grid Tuning grid from build_tuning_grid()
#' @return tune_results object
tune_model <- function(wf, folds, grid) {
  tune::tune_grid(
    wf,
    resamples = folds,
    grid      = grid,
    metrics   = yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::mn_log_loss
    ),
    control = tune::control_grid(
      save_pred     = TRUE,
      verbose       = TRUE,
      allow_par     = FALSE   # keras manages its own parallelism
    )
  )
}


#' Select best hyperparameter combination
#'
#' @param tuning_results Output of tune_model()
#' @param metric Metric to optimize; default "roc_auc"
#' @return One-row tibble of best parameters
select_best_params <- function(tuning_results, metric = "roc_auc") {
  tune::select_best(tuning_results, metric = metric)
}


#' Finalize workflow with best parameters and fit on full training data
#'
#' last_fit() fits on the full training set and evaluates on the test set.
#' The fitted workflow object is accessible via extract_workflow(final_fit).
#'
#' @param wf Workflow from build_workflow()
#' @param best_params Output of select_best_params()
#' @param split rsplit object from make_split()
#' @return last_fit object
fit_final_model <- function(wf, split) {
  tune::last_fit(
    wf,
    split   = split,
    metrics = yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::mn_log_loss
    )
  )
}


# ── Split and fold constructors ───────────────────────────────────────────────
# Kept here rather than wrangle.R because they depend on modeling decisions
# (stratification variable = outcome, prop, v)

#' Stratified train/test split
#'
#' @param data Analytic sample from make_analytic_sample()
#' @param prop Proportion for training set
#' @param seed Random seed
#' @return rsplit object
make_split <- function(data, prop = 0.80, seed = 2002) {
  set.seed(seed)
  rsample::initial_split(data, prop = prop, strata = cip2)
}


#' V-fold cross-validation with stratification
#'
#' @param train_data Training split
#' @param v Number of folds
#' @param seed Random seed
#' @return rset object
make_folds <- function(train_data, v = 5, seed = 2002) {
  set.seed(seed)
  rsample::vfold_cv(train_data, v = v, strata = cip2)
}
