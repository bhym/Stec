library("data.table")
library("tidyverse")
library("patchwork")
source("functions.R")
popu <- make_inds(id = 1:100, string1 = 1, string2 = 1,
 thickness = 5, cell_length = sample(c(5, 4), 100, replace = T))

j <- 90
dyn <- list()
mutrat <- 1e-7
tarlen <- 5


for (i in seq_len(j)) {
  popu <- growth(popu)
  popu <- reproduce_inds(popu, mutrat, tarlen)
  dyn[[i]] <- popu
}

names(dyn) <- paste("t", seq_len(j), sep = "")

imp1 <- function(x) {
  res <- x %>%
         count(string1, string2) %>%
         unite(genotype, string1, string2)
  return(res)
}

dat_prep <-  enframe(dyn) %>%
  mutate(name = gsub("t", "", name) %>% as.integer)

d_atini <- dat_prep %>%
 mutate(conto = map(value, ~imp1(.x))) %>%
  select(-value) %>%
  unnest(conto)

a_atini <- dat_prep %>%
 mutate(conto = map(value, ~count(.x, cell_length))) %>%
  select(-value) %>%
  unnest(conto)

aplo <- d_atini %>% ggplot() +
  aes(x = name, y = n, group = genotype, col = genotype) +
  geom_line(show.legend = F) +
  scale_y_log10() +
  theme_minimal()

bplo <- a_atini %>% ggplot() +
  aes(x = cell_length, group = name, fill = name, col = NULL) +
  geom_density(show.legend = F) +
  theme_minimal()

aplo | bplo
