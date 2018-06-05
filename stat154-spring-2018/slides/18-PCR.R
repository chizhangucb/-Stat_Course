cars2004 <- read.csv('../data/cars2004.csv', stringsAsFactors = F)
cars2004 <- cars2004[, -1]
str(cars2004, vec.len = 1)

# Correlation Matrix
corr_cars2004 <- round(cor(cars2004), digits = 3)
corr_cars2004[lower.tri(corr_cars2004, diag = T)] <- ""
(corr_cars2004 <- as.data.frame(corr_cars2004))

# Circle of correlations
library(factoextra)
library("FactoMineR")
rownames(cars2004) <- cars2004$name
# Remove name and price since price is the response
cars2004.pca <- PCA(cars2004[, -1], ncp = 9, graph = FALSE)
# scores <- cars2004.pca$ind$coord
fviz_pca_var(cars2004.pca, col.var = "black")
# cars2004.pca$svd$vs
cars2004.pca$eig[, 2]

# PCA with princomp() (The same result as above)
pca_princomp <- princomp(cars2004[, -1], cor = TRUE)
names(pca_princomp)
loadings <- pca_princomp$loadings
# scores <- pca_princomp$scores
(eigenvalues <- round((pca_princomp$sdev)^2, digits = 4))
proportion <- round(eigenvalues / ncol(cars2004), digits = 4) * 100
cum_prop <- cumsum(proportion)
(Eigval.Tb <- cbind(eigenvalues, proportion, cum_prop))

ols_reg <- lm(price ~ ., data = cars2004)
(ols_reg_sum <- summary(ols_reg))
price_corr <- cor(cars2004)[1, -1]
corr_coef_mat <- as.data.frame(cbind(price_corr, coef(ols_reg_sum)[-1, 1]))
colnames(corr_coef_mat) <- c("correlation", "coefficient")
corr_coef_mat

# Correlations barplot
library(ggplot2)
library(pryr)
corr_coef_mat$variable <- rownames(corr_coef_mat)
# corr_coef_mat$variable <- factor(corr_coef_mat$variable)
corr_p <- ggplot(corr_coef_mat, aes(x=variable, y=correlation)) + 
  geom_bar(stat="identity", position = "identity", 
           fill=ifelse(corr_coef_mat$correlation > 0, "grey", "red")) + 
                       #rgb(56, 146, 208, maxColorValue = 255),
                       #rgb(227, 111, 30, maxColorValue = 255))) + 
  scale_x_discrete(limits = rownames(corr_coef_mat)) + 
  labs(x = "", y = "", title="correlations") + 
  scale_y_continuous(breaks = round(seq(-0.6, 0.8, by = 0.2), 1)) + 
  theme(plot.title = element_text(hjust = 0.5))
corr_p

# Coefficient barplot
coef_p <- ggplot(corr_coef_mat, aes(x=variable, y=coefficient)) + 
  geom_bar(stat="identity", position = "identity", 
           fill=ifelse(corr_coef_mat$coefficient > 0, "grey", "red")) + 
  #rgb(56, 146, 208, maxColorValue = 255),
  #rgb(227, 111, 30, maxColorValue = 255))) + 
  scale_x_discrete(limits = rownames(corr_coef_mat)) + 
  labs(x = "", y = "", title="coefficients") + 
  scale_y_continuous(breaks = round(seq(-3000, 2000, by = 1000))) + 
  theme(plot.title = element_text(hjust = 0.5))
coef_p

# Linear Regression on scores (with full PCs)
pca_princomp <- princomp(cars2004[, -1], cor = TRUE)
scores <- pca_princomp$scores
new_cars2004 <- as.data.frame(cbind(price = cars2004$price, scores))
ols_pcr <- lm(price ~ ., data = new_cars2004)
(ols_pcr_sum <- summary(ols_pcr))

# Linear Regression on orignal matrix
ols_reg <- lm(price ~ ., data = cars2004)
(ols_reg_sum <- summary(ols_reg))

loadings <- pca_princomp$loadings
# Should be the same since Vt %*% betaOLS = betaPCR
t(as.matrix(loadings)) %*% coef(ols_reg_sum)[-1, 1]
coef(ols_pcr_sum)[-1, 1]

# Linear Regression on scores (only 2 or 3 PCs)
ols_pcr_2PC <- lm(price ~ ., data = new_cars2004[, 1:3])
(ols_pcr_2PC_sum <- summary(ols_pcr_2PC))
ols_pcr_3PC <- lm(price ~ ., data = new_cars2004[, 1:4])
(ols_pcr_3PC_sum <- summary(ols_pcr_3PC))

### PCR Coefficients
# PCR without training-and-test sets
library(pls)
set.seed(2)
pcr_fit <- pcr(price ~ ., data = cars2004, scale = TRUE, validation = "CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")

# PCR with training-and-test sets
# train set
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(cars2004), replace = TRUE)
test <- (!train)
pcr_fit <- pcr(price ~ ., data = cars2004, subset = train,
               scale = TRUE, validation = "CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")
