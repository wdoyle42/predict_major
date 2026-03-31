# _targets.R
# Master pipeline definition for predict_major
# Run with: targets::tar_make()
# Visualize with: targets::tar_visnetwork()

library(targets)
library(tarchetypes)

# Source all function definitions from R/
tar_source("R/")

# Global pipeline options
tar_option_set(
  packages = c(
    "tidyverse", "tidymodels", "haven", "here",
    "keras", "vip", "probably", "janitor",
    "kableExtra", "patchwork", "scales", "glue"
  ),
  format = "rds"   # default storage format for all targets
)

# ── Pipeline ─────────────────────────────────────────────────────────────────
list(

  # ── 1. Data ingestion ──────────────────────────────────────────────────────

  tar_target(
    raw_data_path,
    here::here("data", "raw", "els_02_12_byf3pststu_v1_0.dta"),
    format = "file"   # targets tracks file changes
  ),

  tar_target(
    cip_labels_path,
    here::here("data", "codebook", "cip2_labels.csv"),
    format = "file"
  ),

  # ── 2. Wrangling ───────────────────────────────────────────────────────────

  tar_target(
    els_raw,
    load_els(raw_data_path)
  ),

  tar_target(
    cip_labels,
    load_cip_labels(cip_labels_path)
  ),

  tar_target(
    els_clean,
    wrangle_els(els_raw, cip_labels)
  ),

  tar_target(
    analytic_sample,
    make_analytic_sample(els_clean)
  ),

  # ── 3. Missingness diagnostics (feeds appendix) ────────────────────────────

  tar_target(
    missingness_table,
    summarize_missingness(analytic_sample)
  ),

  # ── 4. Train / test split ──────────────────────────────────────────────────

  tar_target(
    els_split,
    make_split(analytic_sample, prop = 0.80, seed = 2002)
  ),

  tar_target(
    els_train,
    training(els_split)
  ),

  tar_target(
    els_test,
    testing(els_split)
  ),

  # ── 5. Cross-validation folds ──────────────────────────────────────────────

  tar_target(
    els_folds,
    make_folds(els_train, v = 5, seed = 2002)
  ),

  # ── 6. Recipe ──────────────────────────────────────────────────────────────

  tar_target(
    els_recipe,
    build_recipe(els_train)
  ),

  # Face validity check: baked training data dimensions
  tar_target(
    recipe_check,
    check_recipe(els_recipe, els_train)
  ),

  # ── 7. Model specification ─────────────────────────────────────────────────

  tar_target(
    model_spec,
    build_model_spec()
  ),

  # ── 8. Workflow ────────────────────────────────────────────────────────────

  tar_target(
    els_workflow,
    build_workflow(els_recipe, model_spec)
  ),

  # ── 9. Tuning grid ─────────────────────────────────────────────────────────

  tar_target(
    tuning_grid,
    build_tuning_grid(size = 20, seed = 2002)
  ),

  # ── 10. Tuning ─────────────────────────────────────────────────────────────

  tar_target(
    tuning_results,
    tune_model(els_workflow, els_folds, tuning_grid)
  ),

  tar_target(
    best_params,
    select_best_params(tuning_results, metric = "roc_auc")
  ),

  # ── 11. Final fit ──────────────────────────────────────────────────────────

  tar_target(
    final_workflow,
    finalize_workflow(els_workflow, best_params)
  ),

  tar_target(
    final_fit,
    fit_final_model(final_workflow, els_split)
  ),

  # ── 12. Evaluation ─────────────────────────────────────────────────────────

  tar_target(
    test_predictions,
    get_test_predictions(final_fit, els_test)
  ),

  tar_target(
    performance_metrics,
    compute_metrics(test_predictions)
  ),

  tar_target(
    confusion_matrix,
    compute_confusion_matrix(test_predictions, cip_labels)
  ),

  tar_target(
    per_class_metrics,
    compute_per_class_metrics(test_predictions, cip_labels)
  ),

  # ── 13. Variable importance ────────────────────────────────────────────────

  tar_target(
    vip_scores,
    compute_vip(final_fit, els_train, n_features = 30)
  ),

  # ── 14. Figures ────────────────────────────────────────────────────────────

  tar_target(
    fig_outcome_dist,
    plot_outcome_dist(analytic_sample, cip_labels),
    format = "file"
  ),

  tar_target(
    fig_tuning_curves,
    plot_tuning_curves(tuning_results),
    format = "file"
  ),

  tar_target(
    fig_confusion_matrix,
    plot_confusion_matrix(confusion_matrix),
    format = "file"
  ),

  tar_target(
    fig_vip,
    plot_vip(vip_scores),
    format = "file"
  ),

  tar_target(
    fig_missingness,
    plot_missingness(missingness_table),
    format = "file"
  ),

  # ── 15. Tables ─────────────────────────────────────────────────────────────

  tar_target(
    tbl_sample_descriptives,
    make_descriptives_table(analytic_sample)
  ),

  tar_target(
    tbl_performance,
    make_performance_table(performance_metrics, per_class_metrics)
  ),

  tar_target(
    tbl_variable_list,
    make_variable_list_table()
  ),

  # ── 16. Paper ──────────────────────────────────────────────────────────────

  tar_render(
    paper,
    here::here("paper", "paper.Rmd"),
    output_dir = here::here("paper")
  ),

  tar_render(
    appendix,
    here::here("paper", "appendix.Rmd"),
    output_dir = here::here("paper")
  )

)
