# R/evaluate.R
# Model evaluation: predictions, metrics, confusion matrix, per-class stats
# All functions called by _targets.R


#' Extract test set predictions from last_fit object
#'
#' @param final_fit Output of fit_final_model()
#' @param test_data Test split (for joining labels)
#' @return Tibble of predicted class, predicted probabilities, truth
get_test_predictions <- function(final_fit, test_data) {
  tune::collect_predictions(final_fit) %>%
    dplyr::bind_cols(
      test_data %>%
        dplyr::select(stu_id, bysex, byrace, cip2_collapsed_label)
    )
}


#' Compute overall performance metrics on test set
#'
#' @param predictions Output of get_test_predictions()
#' @return Tibble of metric name / estimate pairs
compute_metrics <- function(predictions) {
  metrics <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::roc_auc,
    yardstick::mn_log_loss
  )

  metrics(
    predictions,
    truth    = cip2,
    estimate = .pred_class,
    # probability columns for multiclass ROC AUC
    dplyr::starts_with(".pred_"),
    estimator = "macro_weighted"
  )
}


#' Compute confusion matrix
#'
#' @param predictions Output of get_test_predictions()
#' @param cip_labels CIP label crosswalk from load_cip_labels()
#' @return conf_mat object with human-readable labels
compute_confusion_matrix <- function(predictions, cip_labels) {
  # Join readable labels onto predictions
  label_lookup <- cip_labels %>%
    dplyr::select(cip2_collapsed_code, cip2_collapsed_label) %>%
    dplyr::distinct()

  predictions %>%
    yardstick::conf_mat(truth = cip2, estimate = .pred_class)
}


#' Compute per-class precision, recall, F1
#'
#' @param predictions Output of get_test_predictions()
#' @param cip_labels CIP label crosswalk
#' @return Tibble with one row per CIP class
compute_per_class_metrics <- function(predictions, cip_labels) {
  dplyr::bind_rows(
    yardstick::precision(predictions, truth = cip2,
                         estimate = .pred_class, estimator = "macro") %>%
      dplyr::mutate(metric = "precision"),
    yardstick::recall(predictions, truth = cip2,
                      estimate = .pred_class, estimator = "macro") %>%
      dplyr::mutate(metric = "recall"),
    yardstick::f_meas(predictions, truth = cip2,
                      estimate = .pred_class, estimator = "macro") %>%
      dplyr::mutate(metric = "f1")
  ) %>%
    dplyr::left_join(
      cip_labels %>% dplyr::select(cip2_collapsed_code, cip2_collapsed_label) %>% dplyr::distinct(),
      by = dplyr::join_by(.level == cip2_collapsed_code)
    ) %>%
    dplyr::arrange(metric, dplyr::desc(.estimate))
}
