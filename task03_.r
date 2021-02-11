task03.r
trueMean1 <- 5 
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

boxplot(Sample1, Sample2)

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
ToMomDad <- length( grep( "grandpa_mom", Focus )) / length( Focus )
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length( intersect( Focus, Sibling_01 )) / length( Focus )
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan) ) ) / length( Focus ) )
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="" , xlab="proportion shared genes")
HWE <- function(p) {
  aa <- p^2  
  ab <- 2 * p * (1-p)
  bb <- (1-p)^2
}
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)