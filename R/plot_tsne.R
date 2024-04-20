
#'@importFrom plotly plot_ly layout
#'@export
plot_tsne <- function(repr, df, target_variable = NULL, color_variable = NULL, tooltip_variable = NULL, title = "PCA", size = FALSE, repr_orig = NULL) {
    tsne <- Rtsne::Rtsne(repr, perplexity = 30, dims = 2, verbose = TRUE, max_iter = 500, pca = FALSE)
    # if (length(unique(df[, color_variable])) < 20) {
    # df[, color_variable] <- as.factor(df[, color_variable])

        # }
    if (size) {
        dists_orig <- as.numeric(dist(repr_orig))
        dists_new <- as.numeric(dist(repr))
        sizes <- abs(dists_orig - dists_new)
    }

    target_text <-  df[, target_variable]
    info_text <- df[, tooltip_variable]
    coloring <- df[, color_variable]

    tooltip_text <- paste("target:", target_text, "<br>info: ", info_text, "<br>")

    plotly::plot_ly(
        df,
        x = tsne$Y[, 1],
        y = tsne$Y[, 2],
        color = coloring,
        type = "scatter",
        mode = "markers",
        marker = list(size = 8),
        text = tooltip_text
    ) |>
        layout(title = title)
}
