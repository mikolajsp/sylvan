
#'@export
plot_shapard_distances <- function(repr_orig, repr_new, repr_names = NULL) {
    if (is.null(repr_names)) repr_names <- c("frac{d_{i,j} + d_{i,j}^*}{2}", "d_{i,j} - d_{i,j}^*")

    repr1 <- as.matrix(repr_orig)
    repr2 <- as.matrix(repr_new)

    dists_orig <- as.numeric(dist(repr1))
    dists_orig <- dists_orig/mean(dists_orig)
    dists_new <- as.numeric(dist(repr2))
    dists_new <- dists_new/mean(dists_new)


    df <- data.frame(average_dist = (dists_orig + dists_new) / 2, difference = dists_orig - dists_new)

    ggplot(data = df, aes(average_dist, difference)) +
        geom_point(size = 0.1) +
        xlab(TeX(repr_names[1])) +
        ylab(TeX(repr_names[2])) +
        theme_minimal()
}

