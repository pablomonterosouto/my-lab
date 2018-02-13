# Data
n_groups <- 2
n_indiv_group <- 3
n_obs <- 10
n_indiv <- n_indiv_group * n_groups
group_mean <- sample(n_groups * 2, n_groups)
indiv_group <- data.frame(
  indiv_id = seq_len(n_indiv),
  group_id = sort(rep(seq_len(n_groups), n_indiv_group)),
  group_mean = c(t(replicate(
    n_indiv_group, group_mean
  )))
)
df <- data.frame()
for (i in seq_len(dim(indiv_group)[1])) {
  temp_df <-
    replicate(2, rnorm(n_obs, indiv_group[i, "group_mean"] * indiv_group[i, "group_id"]))
  df <- rbind(df,
              cbind(temp_df,
                    indiv_id = indiv_group[i, 1],
                    group_id = indiv_group[i, 2]))
}
# Plot
n_colors <- length(rownames(RColorBrewer::brewer.pal.info))
p <- list()
ggplot2::ggplot(df, ggplot2::aes(x = V1, y = V2)) +
  ggplot2::geom_point(ggplot2::aes(colour = indiv_id), size = 2) +
  ggplot2::scale_colour_distiller(palette = "Set1") +
  ggplot2::stat_ellipse(ggplot2::aes(
    x = V1,
    y = V2,
    color = indiv_id,
    group = indiv_id
  ),
  type = "norm") +
  ggplot2::theme(legend.position = 'none')

