#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#	analyse de fourier pour identifier une ou des p�riodicit�s dans une s�quence temporelle
#   exemple : une s�rioe temporelle m�langeant deux sinusoides (periode.1 et periode.2) et du bruit
#	auteur : fx.dechaume@u-bourgogne.fr
#	date de derni�re modification : 20/04/2011
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())
close.screen(all = TRUE) 
split.screen(c(2,1))
###
###	g�n�ration d'un s�quence bruit�e
###
screen(1)
t <- 0:2500
periode.1 <- 400
periode.2 <- 80
periodeprincipale <- sin(2*pi*t/(periode.1))
periodesecondaire <-  sin(2*pi*t/(periode.2))/5
bruit <- rnorm(length(t), 0, 0.1)
y <- periodeprincipale + periodesecondaire + bruit
plot(y ~ t, type = "l",
		main = "s�rie temporelle")
segments(0,0,periode.1,0,  col = "red")
segments(0,0.1,periode.2,0.1,  col = "red")
mtext(paste("(en rouge : illustration de p�riode.1 =", periode.1, " et p�riode.2 = ",  periode.2,")"), 3, cex = 0.8)
###
###	periodogramme
###
screen(2)
fourier <- fft(y)
magnitude <- Mod(fourier)
magnitude_firsthalf <- magnitude[1:(length(magnitude)/2)]
x.axis <- 1/((1:length(magnitude_firsthalf)/length(magnitude) ))
plot(magnitude_firsthalf ~ x.axis, type="l",
		main = "p�riodogramme",
		xlab = "t",
		ylab = "magnitude")
points(periode.1, 0, col = "red")
points(periode.2, 0, col = "red")
mtext("(points rouges : pics attendus)", 3, cex  = 0.8)


