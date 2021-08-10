## relogit   rare event logit

modlog1 <- glm(Vic_Rob_As ~ .,family = binomial(link = "cloglog"), data=CData_CDMX3)
summary(modlog1)

## Hallamos punto de corte gráfica y teóricamente
predictions1 <- prediction(modlog1$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions1, "sens")@x.values), unlist(performance(predictions1, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions1, "spec")@x.values), unlist(performance(predictions1, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest1 <- abs(unlist(performance(predictions1, "sens")@y.values)-unlist(performance(predictions1, "spec")@y.values))
opt1 <- unlist(performance(predictions1, "sens")@x.values)[which(rest1==min(rest1))]


vif(modlog1)

#Matriz de Confusión
r <- ifelse(modlog1$fitted.values>=opt1,1,0)
table(r,CData_CDMX3$Vic_Rob_As)