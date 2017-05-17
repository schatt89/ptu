library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)
library(readr)


geners1 <- read_csv("C:/Users/95sch/Desktop/university/3 курс/курсач/music_ptu/music1-clean.csv")
geners2 <- read_csv("C:/Users/95sch/Desktop/university/3 курс/курсач/music_ptu/music2-clean.csv")


geners1$v1 = as.factor(geners1$v1) 
geners1$v2 = as.factor(geners1$v2)
geners1$v3 = as.factor(geners1$v3)
geners1$v4 = as.factor(geners1$v4)
geners1$v5 = as.factor(geners1$v5)
geners1$v6 = as.factor(geners1$v6)
geners1$v7 = as.factor(geners1$v7)
geners1$v8 = as.factor(geners1$v8)
summary(geners1)


geners2$v1 = as.factor(geners2$v1)
geners2$v2 = as.factor(geners2$v2)
geners2$v3 = as.factor(geners2$v3)
geners2$v4 = as.factor(geners2$v4)
geners2$v5 = as.factor(geners2$v5)
geners2$v6 = as.factor(geners2$v6)
geners2$v7 = as.factor(geners2$v7)
summary(geners2)


geners10 = left_join(geners1, geners2, by = "ID_student")

library(reshape2)
geners10 = dcast(melt(geners1,id.vars = "ID_student"),ID_student ~value,fun.aggregate = length)
geners10 = geners10[,-26]

a = factanal(geners10[,-1], 3)


rownames(geners10) = geners10$ID_student
geners10 = geners10[,-1]

geners10 = t(as.matrix(geners10))

row.sum <- apply(geners10, 1, sum)
head(row.sum)
row.profile <- geners10/row.sum


dist.matrix <- function(data, average.profile){
  mat <- as.matrix(t(data))
  n <- ncol(mat)
  dist.mat<- matrix(NA, n, n)
  diag(dist.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      d2 <- sum(((mat[, i] - mat[, j])^2) / average.profile)
      dist.mat[i, j] <- dist.mat[j, i] <- d2
    }
  }
  colnames(dist.mat) <- rownames(dist.mat) <- colnames(mat)
  dist.mat
}

n <- sum(geners10)

# Column sums
col.sum <- apply(geners10, 2, sum)
# average row profile = Column sums / grand total
average.rp <- col.sum/n 
average.rp


# Distance matrix
dist.mat <- dist.matrix(row.profile, average.rp)
dist.mat <-round(dist.mat, 2)
# Visualize the matrix
library("corrplot")
corrplot(dist.mat, type="upper",  is.corr = FALSE)



chisq <- chisq.test(geners10)
chisq


# Grand total
n <- sum(geners10)
# Standardized residuals
residuals <- chisq$residuals/sqrt(n)
# Number of dimensions
nb.axes <- min(nrow(residuals)-1, ncol(residuals)-1)
# Singular value decomposition
res.svd <- svd(residuals, nu = nb.axes, nv = nb.axes)
res.svd

library(ggplot2)
library(SparseFactorAnalysis)

a = sfa(t(geners10))
