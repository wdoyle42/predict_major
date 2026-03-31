# R/model.R
# Model specification and workflow construction
# Keras MLP via parsnip; see:
# https://parsnip.tidymodels.org/reference/details_mlp_keras.html


#' Build keras MLP model specification with tunable hyperparameters
#'
#' Tunable parameters:
#'   - hidden_units: nodes per layer
#'   - num_comp (epochs): training epochs (aliased via tune())
#'   - penalty: L2 regularization
#'   - dropout: dropout rate
#'   - learn_rate: Adam optimizer learning rate
#'
#' Architecture: two hidden layers (parsnip keras engine default).
#' Activation: relu (default). Output: softmax (multiclass).
#'
#' @return Unfit parsnip model spec
build_model_spec <- function() {
  parsnip::mlp(
    hidden_units = tune::tune(),
    epochs       = tune::tune(),
    penalty      = tune::tune(),
    dropout      = tune::tune(),
    learn_rate   = tune::tune()
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
