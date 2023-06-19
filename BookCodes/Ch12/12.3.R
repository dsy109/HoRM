library(ggplot2)
library(HoRM)
library(deming)
library(robustbase)
library(car)
library(MASS)
library(RFreak)
library(L1pack)
library(fda)
library(bootstrap)
library(boot)
library(iterpc)

###############################################################################
### Example 12.6.3: Pulp Property Data, cont'd
###############################################################################

data(wood, package = "HoRM")
attach(wood)

perms <- iterpc(7, 7, ordered = TRUE)
all.perm <- getall(perms)

tmp.out <- t(sapply(1:nrow(all.perm), function(i) summary(lm(pulp ~ Kappa, data = data.frame(pulp = pulp[all.perm[i, 
    ]], Kappa)))$coef[, 3]))
tmp.out <- data.frame(tmp.out)
names(tmp.out) <- c("Intercept", "Slope")

mean(abs(tmp.out[, 1]) >= abs(tmp.out[1, 1]))
mean(abs(tmp.out[, 2]) >= abs(tmp.out[1, 2]))

ggplot(tmp.out, aes(x = Intercept)) + geom_histogram(binwidth = 0.5, aes(y = ..density.., 
    fill = ..density..)) + geom_vline(xintercept = tmp.out[1, 1], linetype = "longdash") + 
    theme(text = element_text(size = 20)) + xlab(expression(paste(t^"#", (beta[0])))) + 
    ylab("Density") + ggtitle(expression(paste("Permutation Distribution of ", t^"#", 
    (beta[0]))))

ggplot(tmp.out, aes(x = Slope)) + geom_histogram(binwidth = 0.5, aes(y = ..density.., 
    fill = ..density..)) + geom_vline(xintercept = c(-1, 1) * tmp.out[1, 2], linetype = "longdash") + 
    theme(text = element_text(size = 20)) + xlab(expression(paste(t^"#", (beta[1])))) + 
    ylab("Density") + ggtitle(expression(paste("Permutation Distribution of ", t^"#", 
    (beta[1]))))

