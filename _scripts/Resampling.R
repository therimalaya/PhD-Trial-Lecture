library(tidyverse)

dta <- tibble(
  x = 1:4,
  y = 1:4,
  Sample = factor(1:4)
)

## permutation --------
set.seed(1)
dta_perm <- dta %>% mutate(
  Resample1 = sample(1:4, 4, replace = FALSE),
  Resample2 = sample(1:4, 4, replace = FALSE),
  Resample3 = sample(1:4, 4, replace = FALSE),
  Resample4 = sample(1:4, 4, replace = FALSE)
) %>% 
  gather(key, value, x:y) %>% 
  mutate_at(vars(contains("ample")), as.character) %>% 
  gather(fill, fill_value, Sample:Resample4) %>% 
  group_by(key, fill) %>% 
  mutate(fill_value = sample(fill_value)) %>% 
  ungroup() %>% 
  mutate(fill = factor(fill, levels = c('Sample', paste0("Resample", 1:4))))

## Bootstrapping --------
set.seed(1)
dta_boot <- dta %>% mutate(
  Resample1 = sample(1:4, 4, replace = TRUE),
  Resample2 = sample(1:4, 4, replace = TRUE),
  Resample3 = sample(1:4, 4, replace = TRUE),
  Resample4 = sample(1:4, 4, replace = TRUE)
) %>% 
  gather(key, value, x:y) %>% 
  mutate_at(vars(contains("ample")), as.character) %>% 
  gather(fill, fill_value, Sample:Resample4) %>% 
  mutate(fill = factor(fill, levels = c('Sample', paste0("Resample", 1:4))))

## Jackkniffing --------
set.seed(1)
dta_jk <- dta %>% mutate(
  Resample1 = sample(c(NA, 1:4), 4, replace = FALSE),
  Resample2 = sample(c(NA, 1:4), 4, replace = FALSE),
  Resample3 = sample(c(NA, 1:4), 4, replace = FALSE),
  Resample4 = sample(c(NA, 1:4), 4, replace = FALSE)
) %>% 
  gather(key, value, x:y) %>% 
  mutate_at(vars(contains("ample")), as.character) %>% 
  gather(fill, fill_value, Sample:Resample4) %>% 
  mutate(fill = factor(fill, levels = c('Sample', paste0("Resample", 1:4))))

## Cross-Validation --------
set.seed(1)
dta_cv <- dta %>% mutate(
  Resample1 = sample(c(NA, NA, 1:4), 4, replace = FALSE),
  Resample2 = sample(c(NA, NA, 1:4), 4, replace = FALSE),
  Resample3 = sample(c(NA, NA, 1:4), 4, replace = FALSE),
  Resample4 = sample(c(NA, NA, 1:4), 4, replace = FALSE)
) %>% 
  gather(key, value, x:y) %>% 
  mutate_at(vars(contains("ample")), as.character) %>% 
  gather(fill, fill_value, Sample:Resample4) %>% 
  mutate(fill = factor(fill, levels = c('Sample', paste0("Resample", 1:4))))

dta <- bind_rows(
  Permutation = dta_perm,
  Bootstrap = dta_boot,
  Jacknife = dta_jk,
  CrossValidation = dta_cv,
  .id = "Method"
) %>% 
  mutate(Method = factor(
    Method, 
    levels = c("Permutation", "Bootstrap",
               "Jacknife", "CrossValidation")))
walk(unique(dta$Method), function(mthd) {
  plt <- dta %>% filter(Method == mthd) %>% 
    ggplot(aes(key, value, fill = fill_value)) +
    geom_tile(color = "black", show.legend = FALSE) +
    facet_grid(Method ~ fill) +
    scale_fill_brewer(palette = "Blues") +
    labs(x = "Variable", y = "Observation") +
    ggtitle(paste0("Resampling Method: ", mthd))
  ggsave(plt, filename = paste0("_images/", mthd, ".png"), width = 5, height = 2, dpi = 150)
})


