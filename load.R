rm(list=ls())
NUM_SAMPLES = 100
TRAIN_PATH <- "data/train.json"
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

data <- fromJSON(TRAIN_PATH)
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)