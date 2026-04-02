# R/model.R
# Model specification and workflow construction
# Keras MLP via parsnip; see:
# https://parsnip.tidymodels.org/reference/details_mlp_keras.html


#' Build keras MLP model specification with tunable hyperparameters
#'
#' Tunable parameters:
#'   - hidden_units: nodes per layer (two equal-width hidden layers)
#'   - epochs: training epochs
#'   - dropout: dropout rate applied after each hidden layer
#'
#' Fixed parameters:
#'   - penalty: L2 regularization fixed at 0 (cannot specify alongside
#'     dropout in the keras parsnip engine)
#'   - learn_rate: NOT exposed by the keras parsnip engine; Adam optimizer
#'     uses its default learning rate (0.001). To tune learn_rate, use
#'     set_engine("keras", optimizer = keras::optimizer_adam(lr = ...))
#'     and pass via engine args instead.
#'
#' Architecture: two hidden layers (parsnip keras engine default).
#' Activation: relu. Output: softmax (multiclass).
#'
#' @return Unfit parsnip model spec
build_model_spec <- function() {
  parsnip::mlp(
    hidden_units = tune::tune(),
    epochs       = tune::tune(),
    penalty      = 0,            # fixed — cannot specify with dropout
    dropout      = tune::tune()
    # learn_rate not supported by keras engine — use Adam default (0.001)
  ) %>%
    parsnip::set_engine("keras") %>%
    parsnip::set_mode("classification")
}


#' Combine recipe and model spec into a workflow
#'
#' @param rec Unprepped recipe from build_recipe()
#' @param spec Model spec from build_model_spec()
#' @return workflows::workflow object
build_workflow <- function(rec, spec) {
  workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(spec)
}
