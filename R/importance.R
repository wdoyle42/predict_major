# R/importance.R
# Variable importance via permutation (vip package)
# Note: keras models don't support model-based importance (no coef() method)
# so permutation importance is the appropriate approach here.
#
# Caveats to surface in paper/appendix:
#   - Correlated features lead to underestimation of individual importance
#   - Importance reflects model reliance, not causal effect
#   - Scores are post-hoc and specific to this fitted model


#' Compute permutation-based variable importance
#'
#' @param final_fit last_fit object from fit_final_model()
#' @param train_data Training data (used as reference for permutation)
#' @param n_features Number of top features to return
#' @return Tibble of variable / importance score pairs, sorted descending
compute_vip <- function(final_fit, train_data, n_features = 30) {

  # Extract fitted workflow
  fitted_wf <- tune::extract_workflow(final_fit)

  # Prediction wrapper for vip::vi_permute()
  pred_fun <- function(object, newdata) {
    predict(object, newdata, type = "prob") %>%
      # Return matrix of class probabilities for multiclass log loss
      as.matrix()
  }

  vip::vi_permute(
    object     = fitted_wf,
    train      = train_data,
    target     = "cip2",
    metric     = "mauc",          # multiclass AUC
    pred_wrapper = pred_fun,
    nsim       = 5,               # average over 5 permutations per feature
    sample_frac = 0.75            # use 75% of training data per permutation
  ) %>%
    dplyr::slice_max(Importance, n = n_features) %>%
    dplyr::arrange(dplyr::desc(Importance))
}
