library(ggplot2)
library(ggExtra)

#####################################
###          Grid Search          ###
#####################################
df <- data.frame(x = seq(0, 1, .25),
                 y = seq(-1, 1, .5))

ggplot(df, aes(x, y)) +
  geom_vline(aes(xintercept = x), color = 'grey50') +
  geom_hline(aes(yintercept = y), color = 'grey50') +
  geom_point(data = expand(df, x, y), color = 'red', size = 3) +
  labs(x = 'hyperparameter 1', y = 'hyperparameter 2') +
  theme_minimal()

ggsave('grid_search.png', width = 4, height = 3, dpi = 200)

#####################################
###        Random Guessing        ###
#####################################

set.seed(42)
df <- data.frame(x = rnorm(mean = .5, sd = .25, n = 100000),
                 y = rnorm(mean =  0, sd =  .5, n = 100000)) %>%
  mutate(y = exp(y))

df_clip <- df %>% slice_sample(n = 20)

p <- ggplot() +
  geom_point(data = df, aes(x, y), color = NA) +
  geom_vline(aes(xintercept = seq(-.25, 1.25, .25)), color = 'grey50') +
  geom_hline(aes(yintercept = seq(0, 3, .5)), color = 'grey50') +
  geom_point(data = df_clip, aes(x, y), color = 'red', size = 3) +
  ylim(0, 3) +
  xlim(-.25, 1.25) +
  labs(x = 'hyperparameter 1', y = 'hyperparameter 2') +
  theme_minimal()

p2 <- ggMarginal(p, col = 'grey50', fill = '#ffeeee')

ggsave(plot = p2, filename = 'randomized_search.png', width = 5, height = 3, dpi = 200)

#####################################
###     Bayesian Optimization     ###
#####################################
# ---- 1. "True" function being optimized ----
true_f <- function(x) {
  0.4 + 0.35 * sin(2 * pi * (x + 0.05)) + 0.15 * sin(2 * pi * x * 3 - 1)
}

# ---- 2. Observed points (hyperparameter, value) ----
obs_x <- c(0.08, 0.36, 0.40, 0.77, 0.95)
obs_y <- true_f(obs_x)

# ---- 3. Squared-exponential (RBF) kernel ----
rbf_kernel <- function(x1, x2, length_scale = 0.15, sigma_f = 0.4) {
  sigma_f^2 * exp(-outer(x1, x2, "-")^2 / (2 * length_scale^2))
}

# ---- 4. GP posterior mean & covariance ----
x_grid <- seq(0, 1, length.out = 300)
noise  <- 1e-6

K        <- rbf_kernel(obs_x, obs_x) + diag(noise, length(obs_x))
K_s      <- rbf_kernel(x_grid, obs_x)
K_ss     <- rbf_kernel(x_grid, x_grid)

K_inv    <- solve(K)
mu       <- as.numeric(K_s %*% K_inv %*% obs_y)
cov_post <- K_ss - K_s %*% K_inv %*% t(K_s)
sd_post  <- sqrt(pmax(diag(cov_post), 0))

# ---- 5. Data frames for ggplot ----
gp_df <- data.frame(
  x     = x_grid,
  mean  = mu,
  lower = mu - 1.96 * sd_post,
  upper = mu + 1.96 * sd_post
)

true_df <- data.frame(x = x_grid, y = true_f(x_grid))
obs_df  <- data.frame(x = obs_x, y = obs_y)
best    <- obs_df[which.max(obs_df$y), ]

# ---- 6. Plot ----
p <- ggplot() +
  geom_ribbon(
    data = gp_df,
    aes(x = x, ymin = lower, ymax = upper, fill = "confidence interval"),
    alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_line(
    data = true_df,
    aes(x = x, y = y, color = "true function", linetype = "true function"),
    linewidth = 0.9
  ) +
  geom_line(
    data = gp_df,
    aes(x = x, y = mean, color = "GP mean", linetype = "GP mean"),
    linewidth = 0.9
  ) +
  geom_point(
    data = obs_df,
    aes(x = x, y = y, color = "observed values"),
    size = 3
  ) +
  annotate(
    "segment", x = best$x, xend = best$x,
    y = best$y + 0.22, yend = best$y + 0.03,
    arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7
  ) +
  annotate(
    "text", x = best$x, y = best$y + 0.27, vjust = 1,
    label = "best observed value", size = 4
  ) +
  ### NEXT GUESS
  geom_vline(
    xintercept = 0.18,
    color = 'red', linetype = 'dashed',
    size = 0.5
  ) +
  annotate(
    "segment", x = .26, xend = .19,
    y = best$y + 0.22, yend = .90,
    arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7
  ) +
  annotate(
    "text", x = .27, y = best$y + 0.27, hjust = 0, vjust = 1,
    label = "NEXT HYPERPARAMETER GUESS:\nhigh uncertainty, high mean", size = 4, color = 'red'
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c("observed values", "true function", "GP mean"),
    values = c(
      "observed values" = "#1f3864",
      "true function"   = "#ff7f0e",
      "GP mean"         = "#1f4e9c"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("true function" = "dashed", "GP mean" = "solid"),
    guide = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c("confidence interval" = "#aec6e8")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "hyperparameter", y = "value") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.78, 0.78),
    legend.background = element_rect(fill = "white", color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

p

ggsave("bayesian_optimization.png", p, width = 8, height = 5, dpi = 200)
