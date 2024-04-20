#' @export
repr_tree <- function(model, data, depth = 8) {
    if (!inherits(model, "model_unified")) {
        stop("`model` needs to be a unified model from the `treeshap` package")
    }

    if (!inherits(data, "data.frame")) {
        stop("`data` needs to be a 'data.frame'")
    }

    data <- data[, colnames(data) %in% c(model$feature_names)]

    model <- model$model
    roots <- which(model$Node == 0) - 1
    yes <- model$Yes - 1
    no <- model$No - 1
    missing <- model$Missing - 1
    is_leaf <- is.na(model$Feature)
    feature <- match(model$Feature, colnames(data)) - 1
    split <- model$Split
    decision_type <- unclass(model$Decision.type)
    trees <- unique(model$Tree)

    n <- nrow(data)
    data <- as.data.frame(sapply(data, as.numeric))
    if (n > 1) data <- t(data)

    is_na <- is.na(data)
    repr_tree_cpp(
        data, is_na, roots, yes, no, missing, is_leaf,
        feature, split, decision_type, trees, depth
    )
}


#' @export
repr_tree_slow <- function(model, data) {
    if (!inherits(model, "model_unified")) {
        stop("`model` needs to be a unified model from the `treeshap` package")
    }

    if (!inherits(data, "data.frame")) {
        stop("`data` needs to be a 'data.frame'")
    }
    model <- model$model
    roots <- which(model$Node == 0)
    yes <- model$Yes
    no <- model$No
    missing <- model$Missing
    is_leaf <- is.na(model$Feature)
    feature <- model$Feature
    split <- model$Split
    decision_type <- unclass(model$Decision.type)
    trees <- unique(model$Tree) + 1


    result <- data.frame(matrix(0, nrow = nrow(data), ncol = length(roots)))

    for (i in seq_len(nrow(data))) {
        print(i)
        observation <- data[i, ]

        for (tree in trees) {
            node <- roots[tree]

            while (!is_leaf[node]) {
                if (((decision_type[node] == 1) && (observation[feature[node]] <= split[node])) ||
                    ((decision_type[node] == 2) && (observation[feature[node]] < split[node])) ||
                    (is.na(observation[feature[node]]) && (missing[node] == yes[node]))) {
                    result[i, tree] <- result[i, tree] * 2
                    node <- yes[node]
                } else {
                    result[i, tree] <- result[i, tree] * 2 + 1
                    node <- no[node]
                }
            }
        }
    }

    result
}
