library(simrel)
library(tidyverse)
library(pls)
sim_list <- list(
  n = 500,
  p = 20,
  m = 3,
  q = 10,
  R2 = 0.8,
  relpos = list(1:5),
  ypos = list(1:3),
  gamma = 0.6,
  eta = 0.4,
  type = "multivariate"
)
set.seed(777)
sim_obj <- do.call(simrel, sim_list)
plt <- ggsimrelplot(sim_obj, which = 3) +
  labs(subtitle = )

mdl <- with(sim_obj, plsr(Y ~ X, validation = "CV", segments = 10))
rmsep <- RMSEP(mdl, estimate = "all")[["val"]] %>% 
  as.data.frame() %>%
  rownames_to_column("Estimate") %>% 
  gather("key", "RMSEP", -Estimate) %>% 
  separate(key, c("Response", "Components"), sep = "\\.") %>%
  mutate(Components = gsub("[()[:alpha:]+]", "", Components),
         Components = ifelse(Components == "", 0, Components)) %>% 
  mutate_at("Components", as.numeric) %>% 
  as.tibble()

err_plt <- ggplot(rmsep, aes(Components, RMSEP, color = Estimate, 
                             linetype = Estimate, group = Estimate)) +
  geom_line() + geom_point(shape = 1) +
  facet_grid(. ~ Response, labeller = label_both) +
  coord_cartesian(ylim = c(0.5, 1.1)) +
  theme(legend.position = "bottom")

normalize <- function(x) (x - max(x)) / diff(range(x))
est_err <- apply(mdl$coefficients, 3, function(x) {
  mat <- t(x - sim_obj$beta) %*% (x - sim_obj$beta)
  as.data.frame(mat) %>% 
    rownames_to_column("Response1") %>% 
    gather(Response2, Est_Err, -1)
}) %>% 
  map_df(identity, .id = "Components") %>% 
  mutate(Components = as.numeric(gsub(" comps", "", Components))) %>% 
  filter(Response1 == Response2) %>% 
  group_by(Components) %>%
  # mutate_at("Est_Err", normalize) %>%
  as_tibble()
est_err_plt <- ggplot(est_err, aes(Components, Est_Err, color = (Response2))) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = function(a) seq.int(floor(a[1]), ceiling(a[2]))) +
  # scale_y_continuous(labels = function(x) paste(x/1000, "K")) +
  labs(y = "Estimation Error") +
  coord_cartesian(ylim = c(0, 2)) +
  theme(legend.position = "bottom")

gridExtra::grid.arrange(plt, err_plt, est_err_plt, ncol = 1)
