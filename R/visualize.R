# R/visualize.R
# All ggplot figure-generating functions
# Each function saves to outputs/figures/ and returns the file path
# (enabling targets to track figures as file targets)

# Shared theme applied to all figures
theme_predict_major <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 10),
      axis.title    = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_figure <- function(plot, filename, width = 7, height = 5) {
  path_pdf <- here::here("outputs", "figures", paste0(filename, ".pdf"))
  path_png <- here::here("outputs", "figures", paste0(filename, ".png"))
  ggplot2::ggsave(path_pdf, plot, width = width, height = height, device = cairo_pdf)
  ggplot2::ggsave(path_png, plot, width = width, height = height, dpi = 300)
  path_png  # return path for targets file tracking
}


#' Figure 1: Distribution of bachelor's degree majors in analytic sample
#'
#' @param data Analytic sample
#' @param cip_labels CIP label crosswalk
#' @return File path to saved figure
plot_outcome_dist <- function(data, cip_labels) {
  label_lookup <- cip_labels %>%
    dplyr::select(cip2_collapsed_code, cip2_collapsed_label) %>%
    dplyr::distinct()

  p <- data %>%
    dplyr::count(cip2) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::left_join(label_lookup, by = dplyr::join_by(cip2 == cip2_collapsed_code)) %>%
    dplyr::mutate(
      cip2_collapsed_label = forcats::fct_reorder(cip2_collapsed_label, pct)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = pct, y = cip2_collapsed_label)) +
    ggplot2::geom_col(fill = "#2C7BB6", alpha = 0.85) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title    = "Distribution of Bachelor's Degree Major Fields",
      subtitle = "Analytic sample: ELS:2002 bachelor's degree completers",
      x        = "Percent of sample",
      y        = NULL
    ) +
    theme_predict_major()

  save_figure(p, "fig1_outcome_dist", width = 7, height = 6)
}


#' Figure 2: Tuning results — ROC AUC across hyperparameter combinations
#'
#' @param tuning_results Output of tune_model()
#' @return File path to saved figure
plot_tuning_curves <- function(tuning_results) {
  p <- tune::collect_metrics(tuning_results) %>%
    dplyr::filter(.metric == "roc_auc") %>%
    dplyr::arrange(dplyr::desc(mean)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    ggplot2::ggplot(ggplot2::aes(x = rank, y = mean)) +
    ggplot2::geom_point(alpha = 0.7, color = "#2C7BB6") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - std_err, ymax = mean + std_err),
      width = 0.3, alpha = 0.4
    ) +
    ggplot2::labs(
      title    = "Hyperparameter Tuning Results",
      subtitle = "ROC AUC (macro-weighted) across Latin hypercube grid",
      x        = "Rank (best to worst)",
      y        = "Mean ROC AUC (5-fold CV)"
    ) +
    theme_predict_major()

  save_figure(p, "fig2_tuning_curves")
}


#' Figure 3: Confusion matrix heatmap
#'
#' @param conf_mat conf_mat object from compute_confusion_matrix()
#' @return File path to saved figure
plot_confusion_matrix <- function(conf_mat) {
  p <- conf_mat %>%
    ggplot2::autoplot(type = "heatmap") +
    ggplot2::scale_fill_gradient(low = "white", high = "#2C7BB6") +
    ggplot2::labs(
      title    = "Confusion Matrix — Test Set Predictions",
      subtitle = "Rows: true class  |  Columns: predicted class"
    ) +
    theme_predict_major() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  save_figure(p, "fig3_confusion_matrix", width = 8, height = 7)
}


#' Figure 4: Variable importance (top N features)
#'
#' @param vip_scores Output of compute_vip()
#' @return File path to saved figure
plot_vip <- function(vip_scores) {
  p <- vip_scores %>%
    dplyr::mutate(Variable = forcats::fct_reorder(Variable, Importance)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Importance, y = Variable)) +
    ggplot2::geom_col(fill = "#2C7BB6", alpha = 0.85) +
    ggplot2::labs(
      title    = "Permutation-Based Variable Importance",
      subtitle = "Change in macro-weighted AUC from permuting each feature",
      x        = "Importance (mean decrease in AUC)",
      y        = NULL
    ) +
    theme_predict_major()

  save_figure(p, "fig4_vip", width = 7, height = 7)
}


#' Figure A1 (appendix): Missingness rates by variable
#'
#' @param missingness_table Output of summarize_missingness()
#' @return File path to saved figure
plot_missingness <- function(missingness_table) {
  p <- missingness_table %>%
    dplyr::filter(pct_missing > 0) %>%
    dplyr::mutate(variable = forcats::fct_reorder(variable, pct_missing)) %>%
    ggplot2::ggplot(ggplot2::aes(x = pct_missing, y = variable)) +
    ggplot2::geom_col(fill = "#D7191C", alpha = 0.75) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title    = "Missing Data by Predictor Variable",
      subtitle = "Analytic sample prior to imputation",
      x        = "Percent missing",
      y        = NULL
    ) +
    theme_predict_major()

  save_figure(p, "figA1_missingness", width = 7, height = 8)
}
