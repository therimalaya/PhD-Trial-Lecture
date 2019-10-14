library(tidyverse)

theta <-seq(pi/6, 2*pi, pi/6)
r <- 5
h_r <- 2
m_r <- 3
s_r <- 4

# get_xy_point <- function(radius = 1, time = NULL,  unit = c("hour", "min", "sec")) {
#   if (is.null(time)) time <- Sys.time()
#   browser()
#   h <- as.numeric(format(time, "%H"))
#   m <- as.numeric(format(time, "%M"))
#   s <- as.numeric(format(time, "%S"))
#   return(out)
# }

df <- tibble(x = r * sin(theta),
       y = r * cos(theta)) %>% 
  rownames_to_column("label") %>% 
  mutate_at("label", parse_integer)

df %>% 
  ggplot(aes(x, y)) +
  geom_text(aes(label = label),
            size = ifelse(df$label %% 3, 6, 12)) +
  geom_point(x = 0, y = 0, size = 9) +
  coord_equal() +
  theme_minimal() +
  annotate(geom = "rect", xmin = -r/3, xmax = r/3, ymin = -r/1.5, ymax = -r/2,
           fill = "lightgrey") +
  annotate(geom = "text", x = 0, y = -r/1.5, label = format(Sys.time(), "%H:%M:%S"),
           vjust = -1, family = "mono", size = 6, fontface = "bold")


# this_period <- Sys.time() %>% 
#   seconds() %>% 
#   seconds_to_period()