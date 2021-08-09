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

rest <- abs(unlist(performance(predictions1, "sens")@y.values)-unlist(performance(predictions1, "spec")@y.values))
opt <- unlist(performance(predictions1, "sens")@x.values)[which(rest==min(rest))]


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