library(hydroGOF)
library(tree)
set.seed(1)
obs <- rnorm(100, 100, 5)
sim <- obs+rnorm(100)

plot(obs, sim)
kge <- KGE(sim = sim, obs = obs, method='2009')
kge_own <- 1-sqrt((cor(obs, sim))^2+(sd(sim)/sd(obs)-1)^2+(mean(sim)/mean(obs)-1)^2)
kge
kge_own
View(cbind(obs, sim))

library(randomForest)
library(MASS)
data("Boston")
set.seed(1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
bag.boston =randomForest(medv∼.,data=Boston ,subset =train ,
                         mtry=13, importance =TRUE)
bag.boston
