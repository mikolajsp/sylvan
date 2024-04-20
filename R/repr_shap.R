#' @export
repr_shap <- function(model, data) {
    if (!inherits(model, "model_unified")) {
        stop("`model` needs to be a unified model from the `treeshap` package")
    }

    if (!inherits(data, "data.frame")) {
        stop("`data` needs to be a 'data.frame'")
    }

    result <- treeshap::treeshap(model, data)
    result$shaps
}
