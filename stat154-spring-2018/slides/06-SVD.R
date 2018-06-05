set.seed(22)
X = matrix(rnorm(20), 5, 4)
# singular value decomposition
SVD = svd(X)
# elements returned by svd()
names(SVD)
(d = SVD$d)
# matrix of left singular vectors
(U = SVD$u)
# matrix of right singular vectors
(V = SVD$v)

t(U) %*% U
t(V) %*% V  
U %*% diag(d) %*% t(V)
X

u1 <- U[, 1]
v1 <- V[, 1]
l1 <- d[1]
X1 <- l1 * u1 %*% t(v1)
qr(X1)$rank  # rank 1
qr(d[2] * U[, 2] %*% t(V[, 2]))$rank
