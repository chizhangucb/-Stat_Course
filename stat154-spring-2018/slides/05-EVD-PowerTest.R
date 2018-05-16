S <- t(USArrests) %*% USArrests  # sample dataset
wt <- c(0, 0, 0, 1)  # Random non-zero vector eg. (0, 0, 0, 1)
wt_up <- S %*% wt
wt_up <- wt_up / norm(wt_up)
n <- 0
while (all.equal(as.vector(wt), as.vector(wt_up)) != TRUE) {
  wt <- wt_up
  wt_up <- S %*% wt_up
  wt_up <- wt_up / norm(wt_up)
  n <- n + 1
  print(paste0("Iteratoin ", n))
}
print(paste0("Converge at ", n, " iteration ",
             "with the dominant eigenvalue as ",
             (t(wt) %*% S %*% wt) / (t(wt) %*% wt)))
max(eigen(S)$values)  # Double check
