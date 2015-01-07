rm(list=ls())
graphics.off()
debut <- Sys.time()

# Choix des males ? Aléatoire = 0 ; Si mue APRES la femelle = 1 
choix_male <- 1

tx_croissance <- 1.1

txmort_male <- 0.012
txmort_femelle <- 0.012

nm <- 500
nf <- 500

tps_max <- 1000
repetition <- 1

fem_5j_avt_mue_moy <- numeric(tps_max)
fem_10j_avt_mue_moy <- numeric(tps_max)
fem_15j_avt_mue_moy <- numeric(tps_max)
fem_5j_avt_rec_moy <- numeric(tps_max)
pret_fem_5j_avt_mue_moy <- numeric(tps_max)
pret_fem_10j_avt_mue_moy <- numeric(tps_max)
pret_fem_15j_avt_mue_moy <- numeric(tps_max)
nb_fem_dispo_moy <- numeric(tps_max)
nb_male_dispo_moy <- numeric(tps_max)
nb_male_seul_moy <- numeric(tps_max)
nb_couples_moy <- numeric(tps_max)
nb_repro_moy <- numeric(tps_max)
coeff_pearson_moy <- numeric(tps_max)

for (iteration in 1:repetition){


	taille_fem_seule <- numeric(tps_max)
	taille_male_seul <- numeric(tps_max)
	taille_male <- numeric(tps_max)
	taille_femelle <- numeric(tps_max)
	nb_ind_en_mue <- numeric(tps_max)
	fem_5j_avt_mue <- numeric(tps_max)
	fem_10j_avt_mue <- numeric(tps_max)
	fem_15j_avt_mue <- numeric(tps_max)
	fem_5j_avt_rec <- numeric(tps_max)
	pret_fem_5j_avt_mue <- numeric(tps_max)
	pret_fem_10j_avt_mue <- numeric(tps_max)
	pret_fem_15j_avt_mue <- numeric(tps_max)
	nb_fem_dispo <- numeric(tps_max)
	males_dispo <- numeric(tps_max)
	nb_male_seul <- numeric(tps_max)
	nb_couples <- numeric(tps_max)
	nb_repro <- numeric(tps_max)
	coeff_pearson <- numeric(tps_max)
	taille_moy_fem <- numeric(nf)
	taille_moy_mal <- numeric(nm)
	

################################
##### CREATION POPULATION ######
################################

	# Femelles :
	
	f_no <- c(1:nf)
	f_taille <- round(rnorm(nf,2,0.2),3)  
	f_max_dist_mue <- round((6.75+14.83*f_taille),2)
	f_dist_mue <- round((runif(nf, min=0, max=f_max_dist_mue)),2)
	f_age <- numeric(nf)
	f_nb_mue <- numeric(nf)
	f_no_male <- numeric(nf)
	f_taille_male <- numeric(nf)
	f_dist_mue_male <- numeric(nf)

	# Males :

	m_no <- c(1:nm)
	m_taille <- round(rnorm(nm,2.75,0.2),3)
	m_max_dist_mue <- round((6.75+14.83*m_taille),2)	
	m_dist_mue <- round((runif(nm, min=0, max=m_max_dist_mue)),2)
	m_age <- numeric(nm)
	m_nb_mue <- numeric(nm)
	m_no_femelle <- numeric(nm)
	m_taille_femelle <- numeric(nm)
	m_dist_mue_femelle <- numeric(nm)
	
	# x<- seq(1,4,0.1)
	# y <- 6.75+14.83*x
	# plot(f_dist_mue~f_taille)
	# points(y~x, type="l", col="red", lwd=3)
	
	
	# plot(f_date_receptivite~f_max_dist_mue)
	# abline(v=1:60)
	# plot(f_date_receptivite~f_taille)
	
################################
##### DYNAMIQUE POPULATION #####
################################
	
	for (tps in 1:tps_max) {

		########################
		# MORTALITE/NAISSANCES #
		########################

		### Mortalité :
		
	  # combien de males et de femelles vont mourir aujourd'hui ?
		nb_male_mort <- floor(nm*txmort_male)
		nb_femelle_mort <- floor(nf*txmort_femelle)
		
	  # la mort s'abat aléatoirement sur les individus :
		no_male_mort <- sample(m_no, nb_male_mort)
		no_femelle_mort <- sample(f_no, nb_femelle_mort)
				
	  # les individus appariés aux morts se retrouvent seuls :
		veuf <- f_no_male[no_femelle_mort]
		veuve <- m_no_femelle[no_male_mort]
		
		m_no_femelle[veuf] <- 0
		m_taille_femelle[veuf] <- 0
		m_dist_mue_femelle[veuf] <- 0
		f_no_male[veuve] <- 0
		f_taille_male[veuve] <- 0
		f_dist_mue_male[veuve] <- 0

		### Naissances :
	  # pour chaque mort, un individu nait :
  
		f_taille[no_femelle_mort] <- round(rnorm(nb_femelle_mort,2,0.2),3)
		f_max_dist_mue[no_femelle_mort] <- round((6.75 + 14.83 * f_taille[no_femelle_mort]),2)
		f_dist_mue[no_femelle_mort] <- f_max_dist_mue[no_femelle_mort]
		f_age[no_femelle_mort] <- 0
		f_nb_mue[no_femelle_mort] <- 0
		f_no_male[no_femelle_mort] <- 0
		f_taille_male[no_femelle_mort] <- 0
		f_dist_mue_male[no_femelle_mort] <- 0
		
		m_taille[no_male_mort] <- round(rnorm(nb_male_mort,2.75,0.2),3)
		m_max_dist_mue[no_male_mort] <- round((6.75 + 14.83 * m_taille[no_male_mort]),2)
		m_dist_mue[no_male_mort] <- m_max_dist_mue[no_male_mort]
		m_age[no_male_mort]<- 0
		m_nb_mue[no_male_mort] <- 0
		m_no_femelle[no_male_mort] <- 0
		m_taille_femelle[no_male_mort] <- 0
		m_dist_mue_femelle[no_male_mort] <- 0
	
		################################
		######## VIEILLISSEMENT ########
		################################
		
		# tous les individus vieillissent d'un jour :
		f_age <- f_age + 1
		m_age <- m_age + 1	
		# tous les individus se rapprochent de leur date de mue : 
		f_dist_mue <- f_dist_mue - 1
		m_dist_mue <- m_dist_mue - 1		
		# les compagnons se rapprochent aussi de leur mue :
		f_dist_mue_male[f_no_male != 0] <- f_dist_mue_male[f_no_male != 0] - 1
		m_dist_mue_femelle[m_no_femelle != 0] <- m_dist_mue_femelle[m_no_femelle != 0] - 1
		
		# y en a-t-il qui mue aujourd'hui (femelle et male) ?
		nbmuetoday <- length(f_no[f_dist_mue <= 0]) + length(m_no[m_dist_mue <= 0])
		
		###########
		### MUE ###
		###########
	
		# si un des membres de la paire mue, le couple se sépare : 
		if (nbmuetoday > 0) {
			# quelles femelles muent aujourd'hui ?
			femelle_en_mue <- f_no[f_dist_mue <= 0]
			# qui parmi elles sont appariées ?
			femelle_en_mue_apparie <-f_no[(f_dist_mue <= 0) & (f_no_male != 0)]
			# males appariés avec une femelle qui mue aujourd'hui :
			male_apparie <- f_no_male[femelle_en_mue_apparie]
			
			# idem pour les mâles :
			male_en_mue <- m_no[m_dist_mue <= 0]
			male_en_mue_apparie <- m_no[(m_dist_mue <= 0) & (m_no_femelle != 0)]
			femelle_apparie <- m_no_femelle[male_en_mue_apparie]			
			
			# comptage du nombre de reproductions aujourd'hui :
			nbreprotoday <- length(femelle_en_mue_apparie)
			
			# femelle devient libre :
			f_no_male[femelle_en_mue_apparie] <- 0
			f_taille_male[femelle_en_mue_apparie] <- 0
			f_dist_mue_male[femelle_en_mue_apparie] <- 0
			m_no_femelle[male_apparie] <- 0
			m_taille_femelle[male_apparie] <- 0
			m_dist_mue_femelle[male_apparie] <- 0
			
			# male devient libre :
			m_no_femelle[male_en_mue_apparie] <- 0
			m_taille_femelle[male_en_mue_apparie] <- 0
			m_dist_mue_femelle[male_en_mue_apparie] <- 0
			f_no_male[femelle_apparie] <- 0
			f_taille_male[femelle_apparie] <- 0
			f_dist_mue_male[femelle_apparie] <- 0
			
			# les individus grandissent après mue (+10%) :
			f_taille[femelle_en_mue] <- round(f_taille[femelle_en_mue]*tx_croissance ,3)
			m_taille[male_en_mue] <- round(m_taille[male_en_mue]*tx_croissance ,3)
			# ils sont ensuite au maximum de la distance à leur prochaine mue :
			f_max_dist_mue[femelle_en_mue] <- round((6.75+14.83*f_taille[femelle_en_mue]),2)
			f_dist_mue[femelle_en_mue] <- f_max_dist_mue[femelle_en_mue]
			m_max_dist_mue[male_en_mue] <- round((6.75+14.83*m_taille[male_en_mue]),2)
			m_dist_mue[male_en_mue] <- m_max_dist_mue[male_en_mue]
			
			# on compte 1 mue de plus : 
			f_nb_mue[femelle_en_mue] <- f_nb_mue[femelle_en_mue] + 1
			m_nb_mue[male_en_mue] <- m_nb_mue[male_en_mue] + 1
		}
		
		# if(tps == instant){
			# screen(screeninstant)
			# hist(f_taille[f_no_male ==0], ylim=c(0,100), xlim=c(1,5), main=paste("Taille des femelles libres, t=",instant-1))
			# screen(screeninstant+1)
			# hist(m_taille[m_no_femelle == 0],ylim=c(0,100), xlim=c(1,5), main=paste("Taille des males libres, t=",instant-1))
			# instant <- instant + 4999
			# screeninstant <- screeninstant + 2
		# }
		
		taille_fem_seule[tps] <- mean(f_taille[f_no_male == 0])
		taille_male_seul[tps] <- mean(m_taille[m_no_femelle == 0])
	
		################################
		######### APPARIEMENT ##########
		################################		

		# femelles disponibles (qui et combien ?) :
		no_femelle_dispo <- f_no[f_no_male == 0]
		nb_femelle_dispo <- length(f_no[f_no_male == 0])
		
		#mâless disponibles (qui et combien ?)
		no_male_dispo <- m_no[m_no_femelle == 0]
		nb_male_dispo <- length(m_no[m_no_femelle == 0])
		
		# Si des individus sont disponibles :
		if ((nb_femelle_dispo > 0) & (nb_male_dispo > 0)){
		
			# SHAKER :
			if (nb_male_dispo > 1){
				no_male_dispo <- sample(no_male_dispo, nb_male_dispo)
			}
			
			# on interroge chaque mâle disponible :
			for (ligne in no_male_dispo){
			
				##################
				### CHOIX MALE ###
				##################
				
				# quelles femelles sont disponibles pour ce mâle ?
					# choix du mâle pour des femelles qui muent AVANT lui :
				if (choix_male == 1){
					no_femelle_dispo <- f_no[(f_no_male == 0) & (f_dist_mue < m_dist_mue[ligne])]
					 # choix random :
				}else if(choix_male == 0){
					no_femelle_dispo <- f_no[f_no_male == 0]
				}
				
				# combien de femelles sont disponibles pour ce mâle ?
				nb_femelle_dispo <- length(no_femelle_dispo)
				
				# s'il y en a au moins une (sinon, on passe directement à la suite):
				if (nb_femelle_dispo != 0){
					# /!\ éviter le "bug du sample" /!\ :
					if (nb_femelle_dispo == 1) {
						no_femelle_selec <- no_femelle_dispo
						
					} else if (nb_femelle_dispo > 1) {
					
					  # le mâle choisit une femelle au hasard :
						no_femelle_selec <- sample(no_femelle_dispo,1)
					}
					# on attribue l'élue :
					m_no_femelle[ligne] <- no_femelle_selec
					f_no_male[no_femelle_selec] <- m_no[ligne]
					
					m_taille_femelle[ligne] <- f_taille[no_femelle_selec]
					f_taille_male[no_femelle_selec] <- m_taille[ligne]
					
					m_dist_mue_femelle[ligne] <- f_dist_mue[no_femelle_selec]
					f_dist_mue_male[no_femelle_selec] <- m_dist_mue[ligne]
				}
			}
		}
		
		nb_male_celibataire <- length(m_no[m_no_femelle == 0])
		
		# suivi des femelles x jours avant mue :
		
		femelle_5j_avt_mue <- f_no[(f_dist_mue <= 5) & (f_dist_mue >= 1)]
		nb_fem_5j_avt_mue <- length(femelle_5j_avt_mue)
		femelle_10j_avt_mue <- f_no[(f_dist_mue <= 10) & (f_dist_mue >= 1)]
		nb_fem_10j_avt_mue <- length(femelle_10j_avt_mue)
		femelle_15j_avt_mue <- f_no[(f_dist_mue <= 15) & (f_dist_mue >= 1)]
		nb_fem_15j_avt_mue <- length(femelle_15j_avt_mue)
		
		# suivi des mâles pouvant prendre les femelles ci-dessus :
		
		# pretendant_fem_5j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 5)]
		# nb_pretendant_fem_5j_avt_mue <- length(pretendant_fem_5j_avt_mue)
		# pretendant_fem_10j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 10)]
		# nb_pretendant_fem_10j_avt_mue <- length(pretendant_fem_10j_avt_mue)
		# pretendant_fem_15j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 15)]
		# nb_pretendant_fem_15j_avt_mue <- length(pretendant_fem_15j_avt_mue)
		
		pretendant_fem_5j_avt_mue <- m_no[(m_dist_mue > 5)]
		nb_pretendant_fem_5j_avt_mue <- length(pretendant_fem_5j_avt_mue)
		pretendant_fem_10j_avt_mue <- m_no[(m_dist_mue > 10)]
		nb_pretendant_fem_10j_avt_mue <- length(pretendant_fem_10j_avt_mue)
		pretendant_fem_15j_avt_mue <- m_no[(m_dist_mue > 15)]
		nb_pretendant_fem_15j_avt_mue <- length(pretendant_fem_15j_avt_mue)
		
		# compilation des valeurs (en vue d'un plot) :
		coeff <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")$estimate
		coeff_pearson[tps] <- coeff
		nb_fem_dispo[tps] <- nb_femelle_dispo
		fem_5j_avt_mue[tps] <- nb_fem_5j_avt_mue
		fem_10j_avt_mue[tps] <- nb_fem_10j_avt_mue
		fem_15j_avt_mue[tps] <- nb_fem_15j_avt_mue
		pret_fem_5j_avt_mue[tps] <- nb_pretendant_fem_5j_avt_mue
		pret_fem_10j_avt_mue[tps] <- nb_pretendant_fem_10j_avt_mue
		pret_fem_15j_avt_mue[tps] <- nb_pretendant_fem_15j_avt_mue
		males_dispo[tps] <- nb_male_dispo
		nb_male_seul[tps] <- nb_male_celibataire
		nb_couples[tps] <- length(f_no[f_no_male != 0])
		nb_repro[tps] <- nbreprotoday
		taille_male[tps] <- mean(m_taille)
		taille_femelle[tps] <- mean(f_taille)
	
		# if(tps == instant & tps < 120 ){
			# screen(screeninstant)
			# hist(length(f_no[f_no_male !=0]), xlab="distance à la mue(jours)", main=paste("Fréquence de distance à la mue des femelles, t=",instant-1))
			# plot(f_dist_mue~f_taille, xlab="Taille femelle", ylab="Distance à la mue", main=paste("Jour",instant))
			# instant <- instant + 2000
			# screeninstant <- screeninstant + 1
		# }
	}

	# moyennes des valeurs sur tous les réplicats :
	coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/repetition 
	nb_fem_dispo_moy <- nb_fem_dispo_moy + nb_fem_dispo/repetition
	fem_5j_avt_mue_moy <- fem_5j_avt_mue_moy + fem_5j_avt_mue/repetition
	fem_10j_avt_mue_moy <- fem_10j_avt_mue_moy + fem_10j_avt_mue/repetition
	fem_15j_avt_mue_moy <- fem_15j_avt_mue_moy + fem_15j_avt_mue/repetition
	pret_fem_5j_avt_mue_moy <- pret_fem_5j_avt_mue_moy + pret_fem_5j_avt_mue/repetition
	pret_fem_10j_avt_mue_moy <- pret_fem_10j_avt_mue_moy + pret_fem_10j_avt_mue/repetition
	pret_fem_15j_avt_mue_moy <- pret_fem_15j_avt_mue_moy + pret_fem_15j_avt_mue/repetition
	nb_male_dispo_moy <- nb_male_dispo_moy + males_dispo/repetition
	nb_male_seul_moy <- nb_male_seul_moy + nb_male_seul/repetition
	nb_couples_moy <- nb_couples_moy + nb_couples/repetition
	nb_repro_moy <- nb_repro_moy + nb_repro/repetition
}
	femelle <- data.frame(f_no, f_taille, f_dist_mue, f_age, f_nb_mue, f_no_male, f_taille_male, f_dist_mue_male)
	male <- data.frame(m_no, m_taille, m_dist_mue, m_age, m_nb_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)

	# graphs :
	
	# suivi des individus :
	windows()
	plot(nb_couples_moy, type="l", lwd=3, xlim=c(0,tps_max), ylim=c(0,max(nf,nm)), xlab='Jours', ylab="Effectifs", col="red", main='Suivi des femelles')
	abline(h=0)
	points(nb_fem_dispo_moy, type="l", col="orange")
	points(nb_couples_moy + nb_male_seul_moy, type="l", col="black")
	points(nb_male_dispo_moy, type="l", col="blue")
	
	windows()
	plot(fem_5j_avt_mue_moy, type="l", lwd=3,xlim=c(0,300), ylim=c(0,max(nf,nm)), xlab="jours", ylab="effectif", main="Suivi des individus x jours avant mue des femelles", col="green")
	points(fem_10j_avt_mue_moy, type="l", lwd=3, col="blue")
	points(fem_15j_avt_mue_moy, type="l", lwd=3, col="brown")
	points(pret_fem_5j_avt_mue_moy, type="l", col="green")
	points(pret_fem_10j_avt_mue_moy, type="l", col="blue")
	points(pret_fem_15j_avt_mue_moy, type="l", col="brown")
	points(nb_couples_moy, type="l", lty=3, lwd=3, col="red")
	mtext("traits épais : femelles ; traits fins : mâles prétendants ; pointillés : couples", 3, cex  = 0.8)
	
	windows()
	plot(nb_repro_moy, type='l', xlab='Jours', main='Nombre de reproductions par jour')
	
	# homogamie :
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : réplicat", iteration), asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	
	windows()
	plot(f_taille~f_age)
	
	windows()
	hist(f_taille)
	
	windows()
	hist(m_taille)
	
	windows()
	plot(taille_fem_seule, type ="l", col="red")
	lines(taille_femelle, col="pink")
	abline(h=mean(taille_fem_seule), lty =3, lwd =3)
	
	windows()
	plot(taille_male_seul, type="l", col="blue")
	lines(taille_male, col="grey")
	abline(h=mean(taille_male_seul), lty =3, lwd =3)
	
	
	
fin <- Sys.time()
warnings()
fin - debut