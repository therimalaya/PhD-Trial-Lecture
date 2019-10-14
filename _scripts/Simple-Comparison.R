library(simrel)
library(tidyverse)
library(pls)

sim_obj <- crossing(
  gamma = seq(0.1, 1.3, length.out = 4),
  relpos = c("1:5", "6:9")
) %>% mutate(sobj = map2(gamma, relpos, ~simrel(
  n = 100,
  p = 10,
  q = 10,
  relpos = eval(parse(text = ..2)),
  gamma = ..1,
  R2 = 0.8,
  ntest = 200,
  type = "univariate"
))
) %>% rownames_to_column(var = "Design")
common_params <- as.list(sim_obj$sobj[[1]]$call)[c(2:4, 7, 9)]
params_lbl <- paste0(paste(names(common_params), common_params, sep = ":"), collapse = ", ")

design_plot <- ggplot(sim_obj, aes(gamma, relpos)) +
  # geom_point(shape = 4, size = 3) +
  geom_text(aes(label = paste("Design", Design))) +
  scale_x_continuous(breaks = unique(sim_obj$gamma)) +
  theme_minimal() +
  labs(x = paste0("Level of multicollinearity\n",
                  "(Higher value gives higher multicollinearity"),
       y = paste0("Position index of principal components\n",
                  "relevant for response")) +
  ggtitle("Experimental Design for Accesing Prediction Methods",
          subtitle = params_lbl) +
  theme(plot.subtitle = element_text(family = "mono"),
        axis.text = element_text(size = rel(1.3)))

design_plot_near <- design_plot +
  # geom_point(shape = 4, size = 3, color = "firebrick", 
  #            data = sim_obj %>% filter(relpos == "1:5")) +
  geom_text(aes(label = paste("Design", Design)),
            color = "firebrick", 
            data = sim_obj %>% filter(relpos == "1:5"))
ggsave(design_plot_near, filename = "_images/design-plot-near.png", width = 6, height = 3, dpi = 180, scale = 1.2)

design_plot_far <- design_plot +
  # geom_point(shape = 4, size = 3, color = "firebrick", 
  #            data = sim_obj %>% filter(relpos == "6:9")) +
  geom_text(aes(label = paste("Design", Design)),
            color = "firebrick", 
            data = sim_obj %>% filter(relpos == "6:9"))
ggsave(design_plot_far, filename = "_images/design-plot-far.png", width = 6, height = 3, dpi = 180, scale = 1.2)

 fit <- sim_obj %>% 
  group_by(gamma, relpos) %>% 
  mutate(error = map(sobj, function(obj) {
    train <- with(obj, {
      colnames(X) <- paste0("x", 1:ncol(X))
      colnames(Y) <- "y"
      as_tibble(cbind(Y, X))
    })
    test <- with(obj, {
      colnames(TESTX) <- paste0("x", 1:ncol(X))
      colnames(TESTY) <- "y"
      as_tibble(cbind(TESTY, TESTX))
    })
    err <- map(c("pcr", "plsr"), function(fn) {
      mdl <- get(fn)(y ~ ., data = train, validation = "CV", segments = 10)
      err <- RMSEP(mdl, newdata = test, estimate = "all")
    })
    names(err) <- c("PCR", "PLSR")
    out <- map_df(err, function(x){
      reshape2::melt(x$val)
    }, .id = "Method") %>% select(-response) %>% 
      rename(ErrorType = estimate,
             Model = model,
             Error = value)
    as_tibble(out)
  })) %>% unnest(error) %>% 
  mutate(Model = str_remove_all(Model, "[()a-zA-Z ]")) %>% 
  mutate(Model = as.integer(Model)) %>% 
  mutate(Model = replace_na(Model, 0))

err_plot_near <- fit %>% filter(ErrorType != "adjCV", relpos == "1:5") %>% 
  ggplot(aes(Model, Error, color = Method, group = Method)) +
  geom_line() +
  geom_point(shape = 21, fill = "whitesmoke", size = 1) +
  facet_grid(ErrorType ~ gamma, labeller = label_both) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  theme(legend.position = "bottom") +
  labs(y = "Prediction error", x = "Number of components")
ggsave(err_plot_near, filename = "_images/error-plot-near.png", width = 5, height = 5, dpi = 150)

err_plot_far <- fit %>% filter(ErrorType != "adjCV", relpos == "6:9") %>% 
  ggplot(aes(Model, Error, color = Method, group = Method)) +
  geom_line() +
  geom_point(shape = 21, fill = "whitesmoke", size = 1) +
  facet_grid(ErrorType ~ gamma, labeller = label_both) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  theme(legend.position = "bottom") +
  labs(y = "Prediction error", x = "Number of components")
ggsave(err_plot_far, filename = "_images/error-plot-far.png", width = 5, height = 5, dpi = 150)
