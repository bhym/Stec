make_inds <- function(id = NaN, string1= NaN, string2= NaN, thickness = NaN,
                      cell_length = NaN) {
  inds <- data.table(id = id, string1 = string1,
                     string2 = string2, thickness = thickness,
                     cell_length = cell_length)
  return(inds)
}

growth <- function(inds) {
  inds[, 4] <- inds[, 4] + 1
  return(inds)
}


reproduce_inds <- function(inds, mu, base_thick) {
  ready_to_split <- which(inds[, 4] >= 2 * base_thick)
  n_ready_to_split <- length(ready_to_split)
  new_thick <- inds$thickness[1] / 2
  string1 <- inds$string1
  string2 <- inds$string2
  runi1 <- runif(n_ready_to_split)
  runi2 <- runif(n_ready_to_split)
  mutated1 <- runi1 <= mu
  mutated2 <- runi2 <= mu
  newstring1 <- ifelse(mutated1, string1 + 1, string1)
  newstring2 <- ifelse(mutated2, string2 + 1, string2)
  if (n_ready_to_split > 0) {
    starting <- make_inds(id             = seq(max(inds[, 1]) + 1,
                                               length.out = n_ready_to_split),
                          string1        = newstring1,
                          string2        = newstring2,
                          thickness      = new_thick,
                          cell_length = 0.8 * inds$cell_length[ready_to_split]
                          )
   inds$thickness[ready_to_split] <- new_thick
   inds <- data.table::rbindlist(list(inds, starting), use.names = T, fill = F)
  }
  return(inds)
}

encounter3 <- function(inds, giug) {
  lele <- nrow(inds)
  indices <- vector("integer", lele)
  indices <- seq(lele - lele %% 2) #This is to avoid R complains if lele is odd
  encounter_split_len <- lele %/% 2 #This is to avoid R complains if lele is odd
  scrambled_idxs <- sample(indices)
  ran_first_group_idxs <- vector("integer", encounter_split_len)
  ran_first_group_idxs <- head(scrambled_idxs, encounter_split_len)
  ran_secon_group_idxs <- vector("integer", encounter_split_len)
  ran_secon_group_idxs <- tail(scrambled_idxs, encounter_split_len)
  a <- array(NA, dim = c(encounter_split_len, 3))
  #GARBAGE PRODUCED HERE BECAUSE OF SUBSETTING
  a[, 1] <- inds[ran_first_group_idxs, ]$cell_length
  a[, 2] <- inds[ran_secon_group_idxs, ]$cell_length
  check <- which(a[, 1] > 4 | a[, 2] > 4)
    kill <- 0 #this is dummy
    inds <- inds[-kill]
    check
    return(inds)
}

encounter_and_sex <- function(inds) {
  lele <- nrow(inds)
  indices <- vector("integer", lele)
  indices <- seq_len(lele - lele %% 2) #This is to avoid R complains if lele is odd
  encounter_split_len <- lele %/% 2 #This is to avoid R complains if lele is odd
  scrambled_idxs <- sample(indices)
  ran_first_group_idxs <- vector("integer", encounter_split_len)
  ran_first_group_idxs <- head(scrambled_idxs, encounter_split_len)
  ran_secon_group_idxs <- vector("integer", encounter_split_len)
  ran_secon_group_idxs <- tail(scrambled_idxs, encounter_split_len)
  a <- array(NA, dim = c(encounter_split_len, 3))
  #GARBAGE PRODUCED HERE BECAUSE OF SUBSETTING
  a[, 1] <- inds[ran_first_group_idxs, ]$cell_length
  a[, 2] <- inds[ran_secon_group_idxs, ]$cell_length
  check <- (a[, 1] > 4 | a[, 2] > 4)
  first_wz_a <- data.table(a = ran_first_group_idxs,
                         b = ran_secon_group_idxs,
                         ci = check)
  first_wz <- first_wz_a[ci == TRUE]
  first <- first_wz
  a_ <- first$a
  b_ <- first$b
  samp1 <- sample(2:3, 2 * length(a_), repl = T)
  samp2 <- sample(2:3, 2 * length(b_), repl = T)
  ch1 <- inds[a_, .(samp1)]
  ch2 <- inds[b_, .(samp2)]
  
  new_inds <- make_inds(id = seq(max(inds$id), length = 2 * sum(check)),
                        string1 = unlist(ch1), string2 = unlist(ch2),
                        thickness = 5,
                        cell_length = 5)
                        kill_because_gametes <- c(a_, b_)
                        inds <- inds[-kill_because_gametes]
                        inds <- data.table::rbindlist(list(inds, new_inds), use.names = T, fill = F)
                        return(inds)
}
remove_tiny <- function(inds, death_thresh) {
  kill <- which(inds$cell_length < death_thresh)
 inds <- inds[-kill]
return(inds)
}
# check death is not working well
