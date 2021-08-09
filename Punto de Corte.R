predictions1 <- prediction(modlog$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions1, "sens")@x.values), unlist(performance(predictions1, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions1, "spec")@x.values), unlist(performance(predictions1, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.1))
axis(1, at= seq(0,1,0.1))
axis(2,at=seq(0,1,0.1))
mtext("Specificity",side=4, padj=-2, col='red')
abline(h=0.95,lty= 2)
abline(v=0.14,lty = 2)

perf <- performance(predictions1,measure="tpr",x.measure="fpr")
x<-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]

cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

predictions <- prediction(modlog3$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.1))
axis(1, at= seq(0,1,0.1))
axis(2,at=seq(0,1,0.1))
mtext("Specificity",side=4, padj=-2, col='red')
abline(h=0.95,lty= 2)
abline(v=0.14,lty = 2)