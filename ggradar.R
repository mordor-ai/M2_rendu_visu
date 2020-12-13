library(ggradar)
library(dplyr)
library(scales)
library(tibble)
head(mtcars)
mtcars_radar <- mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10)

mtcars_radar
ggradar(mtcars_radar)
