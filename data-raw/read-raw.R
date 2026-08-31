############################################################
## A script to read-in formula matrix data from a csv file
############################################################

# load packages
library(readr)
library(here)

# read-in csv data that defines the formulae
df <- read_csv(here::here("data-raw/fmat.csv"), show_col_types = FALSE)

# define a preferred ordering for dimension levels
order1 <- c(
  "I", "P", "R", "NC", "O", "UH", "UR", "UO", "NE",
  "eUH", "eUR", "eUO"
)
order2 <- unique(df$rate)
order3 <- c("NUM", "DEN")

# reshape into a matrix
fmat <- array(
  0L,
  dim = c(length(order1), length(order2), length(order3)),
  dimnames = list(outcome = order1, rate = order2, side = order3)
)
for (i in seq_len(nrow(df))) {
  fmat[, df$rate[[i]], df$side[[i]]] <- as.integer(
    unlist(df[i, order1], use.names = FALSE)
  )
}

# save to package
usethis::use_data(
  fmat,
  internal = TRUE,
  overwrite = TRUE,
  compress = "bzip2",
  version = 2
)
