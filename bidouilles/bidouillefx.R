nf <- 1000
no_fem <- 1:nf
max_dist_mue <- rep(40,nf)
receptive <- numeric(300)
maxrep <- 1000
for (i in 1:maxrep) {
	dist_mue <- runif(nf, min=0, max=max_dist_mue)
	date_receptivite <-  20
	for (t in 1:300){
		receptive[t] <- receptive[t] + length(dist_mue[dist_mue <= date_receptivite])/maxrep
		dist_mue <- dist_mue - 1
		dist_mue[dist_mue <= 0] <- 40
	}
}
plot(receptive, type="l", ylim=c(450,550))
warnings()

# distribution taille avec croissance :
tmax <- 1200
nf <- 500
f_taille <- round(rnorm(nf,2.75,0.2),3)  
f_max_dist_mue <- round((6.75+14.83*f_taille),2)
f_dist_mue <- round((runif(nf, min=0, max=f_max_dist_mue)),2)
windows()
split.screen(c(2,2))
screen(1)
hist(f_dist_mue,main = "t0")
screen(2)
hist(f_taille,main = "t0")
for (t in 1:tmax ){
	f_dist_mue <- f_dist_mue - 1
	f_taille[f_dist_mue <= 0] <- round(f_taille[f_dist_mue <= 0]*1.1,3)
	f_dist_mue[f_dist_mue <= 0] <- round((6.75+14.83*f_taille[f_dist_mue <= 0]),2)
}
screen(3)
hist(f_dist_mue,main = paste("t =", tmax ))
screen(4)
hist(f_taille,main = paste("t =", tmax ))
sd(f_taille)



n <- 10000
split.screen(c(1, 2))
screen(1)
x <- rnorm(n, 0, 1)
y <- rnorm(n, 0, 1)
plot(y ~ x, asp = 1)
screen(2)
x <- runif(n, 0, 1)
y <- runif(n, 0, 1)
plot(y ~ x, asp = 1)



### estimation de l'espérance de vie
graphics.off()
rm(list=ls())
n <- 10000
no <- 1:n
age <- numeric(n)
mort <- numeric(n)
nb_mort <- 0
tx_mortalite <- 0.025
while (nb_mort < n) {	
	qui_est_encore_en_vie <- no[mort == 0]
	age[qui_est_encore_en_vie] <- age[qui_est_encore_en_vie] + 1 
	nb_mort_aujourdhui <-ceiling((n - nb_mort)*tx_mortalite)
	qui_meurt_aujourdhui <- sample(qui_est_encore_en_vie, nb_mort_aujourdhui, replace = F)
	mort[qui_meurt_aujourdhui] <- 1
	nb_mort <- nb_mort + nb_mort_aujourdhui
}
hist(age)
abline(v = mean(age), col = "red", lwd = 2)
abline(v = median(age), col = "blue", lwd = 2)
mean(age)
median(age)


### estimation de l'espérance de vie
graphics.off()
rm(list=ls())
seqtamortalite <- seq(0.001, 0.05, 0.001)
age_mort_median <- numeric(length(seqtamortalite))
ligne <- 1
for (tx_mortalite in seqtamortalite) {
	n <- 10000
	no <- 1:n
	age <- numeric(n)
	mort <- numeric(n)
	nb_mort <- 0
	while (nb_mort < n) {	
		qui_est_encore_en_vie <- no[mort == 0]
		age[qui_est_encore_en_vie] <- age[qui_est_encore_en_vie] + 1 
		nb_mort_aujourdhui <-ceiling((n - nb_mort)*tx_mortalite)
		qui_meurt_aujourdhui <- sample(qui_est_encore_en_vie, nb_mort_aujourdhui, replace = F)
		mort[qui_meurt_aujourdhui] <- 1
		nb_mort <- nb_mort + nb_mort_aujourdhui
	}
	age_mort_median[ligne] <- median(age)
	ligne <- ligne + 1
}
plot(age_mort_median ~ seqtamortalite)



# nb_amplexus_repro_echantillonne <- tps_max
# duree_amplexus_repro_echantillonne <- numeric(nb_amplexus_repro_echantillonne)
# m_taille_amplexus_repro_echantillonne <- numeric(nb_amplexus_repro_echantillonne)
# f_taille_amplexus_repro_echantillonne <- numeric(nb_amplexus_repro_echantillonne)

	
			# dist_mue_premiere_femelle <- f_max_dist_mue[femelle_en_mue_apparie]
			# taille_premiere_femelle <- f_taille[femelle_en_mue_apparie]
			# taille_premier_male <- m_taille[male_apparie]
			
			# duree_amplexus_repro_echantillonne[tps] <- dist_mue_premiere_femelle[1]
			# f_taille_amplexus_repro_echantillonne[tps] <- taille_premiere_femelle[1]
			# m_taille_amplexus_repro_echantillonne[tps] <- taille_premier_male[1]

	# windows()
	# plot(duree_amplexus_repro_echantillonne~m_taille_amplexus_repro_echantillonne)
	
	# f_taille_amplexus_2 <- f_taille_amplexus_repro_echantillonne
	# f_taille_amplexus_2 <- rep(1,tps_max)
	# temp <-  rep(2,tps_max)
	# temp2 <- 1:tps_max
	# f_taille_amplexus_2[temp2[f_taille_amplexus_repro_echantillonne > median(f_taille_amplexus_repro_echantillonne, na.rm=T)]] <- 2
	# f_taille_amplexus_2 <- as.factor(f_taille_amplexus_2)
	# boxplot(f_taille_amplexus_repro_echantillonne~ f_taille_amplexus_2)
	
	# windows()
	# plot(NULL, xlim=c(0,20),ylim=c(60,83))
	# points(duree_amplexus_repro_echantillonne[f_taille_amplexus_2==2]~m_taille_amplexus_repro_echantillonne[f_taille_amplexus_2==2], col="red")
	# points(duree_amplexus_repro_echantillonne[f_taille_amplexus_2==1]~m_taille_amplexus_repro_echantillonne[f_taille_amplexus_2==1], col="blue")	

