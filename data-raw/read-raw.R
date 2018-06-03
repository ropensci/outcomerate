############################################################
## A script to read-in formula matrix data from a csv file
############################################################

# load packages
library(dplyr)
library(readr)
library(tidyr)
library(here)
library(reshape2)

# read-in csv data that defines the formulae
df <- read_csv(here::here("data-raw/fmat.csv"))

# define a preferred ordering for dimension levels
order1 <- c("I", "P", "R", "NC", "O", "UH", "UO", "NE", "eUH", "eUO")
order2 <- unique(df$rate)
order3 <- c("NUM", "DEN")

# reshape into a matrix
fmat <- df %>%
  select(-name, -aapor) %>%
  tidyr::gather(outcome, value, -rate, -side) %>%
  acast(outcome ~ rate ~ side, value.var = "value", fill = 0L)

# rename and reorder dimensions
names(dimnames(fmat)) <- c("outcome", "rate", "side")
fmat <- fmat[order1, order2, order3]

# save to package
use_data(fmat, internal = TRUE)
