source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum( ( ( Expectation - Observed ) ^ 2 ) / Expectation )
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
## x^2 is 10. When all are in the same they are close to the same.
results<-read.csv("http://jonsmitchell.com/data/bio112labresults.csv", stringsAsFactors=F)
counts <- results[,c("yellow", "red","green", "blue", "black", "tan")]
backgroundCol<-c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
backgrounds<-c("white", "Red", "Yellow", "Green", "Blue", "Black")
calcChi(counts[1,])
Chisqs<-apply(counts, 1, calcChi)
plotChis(counts)
##When Chi squared is high, the bars close to or at 0 Only 1 bar is higher than the rest . When chi^2 is low, the bars are even. The bars indicate the even distribution of the phenotypes. When chi^2 is high, the phenotypes are uneven, when the chi^2 is lower, the phenotypes are evenly distributed. 
Avg<- mean(Chisqs)
##Its higher
##No
backgroundAvg<-tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70) ) / length(Chisqs)
percSig<-round(100 * propSig)
##Yes, its high.
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter<-1
for (i in backgrounds) {
  Data<-Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter<-counter + 1
}
abline(v = 11.70, lty=2, lwd = 2, col='black')
##No. 
Simulation<- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
##
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2<- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit<-c(0.1, 1, 1, 1, 1, 1)
names(Fit)<-1:6
Simulation3<-simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit<-c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit)<- 1:6
Simulation4<-simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit<-c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit)<- 1:6
Simulation5<-simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit<-c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit)<- 1:6
Simulation6<-simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7<-c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
