#'@import ggplot2
#'@export
plot_mds_distances <- function(repr_orig, repr_new, repr_names = NULL) {

    if (is.null(repr_names)) repr_names <- c("Distance in original representation", "Distance in new representation")

    repr1 <- as.matrix(repr_orig)
    repr2 <- as.matrix(repr_new)

    dists1 <- dist(repr1)
    dists2 <- dist(repr2)

    mds1 <- cmdscale(dists1, k = 1)
    mds2 <- cmdscale(dists2, k = 1)

    mds_df <- data.frame(mds1 = mds1, mds2 = mds2)

    ggplot(data = mds_df, aes(mds1, mds2)) +
        geom_point() +
        xlab(repr_names[1]) +
        ylab(repr_names[2]) +
        theme_minimal()

}
