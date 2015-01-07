rm(list=ls())

debut <- Sys.time()

tps_max <- 1200
repetition <- 1

nb_receptive_moy <- numeric(tps_max)
nb_fem_libre_moy <- numeric(tps_max)
nb_couples_moy <- numeric(tps_max)
nb_repro_moy <- numeric(tps_max)
coeff_pearson_moy <- numeric(tps_max)

for (iteration in 1:repetition){

	temps <- numeric(tps_max)
	nb_ind_en_mue <- numeric(tps_max)
	nb_fem_libre <- numeric(tps_max)
	nb_mal_dispo <- numeric(tps_max)
	nb_couples <- numeric(tps_max)
	nb_repro <- numeric(tps_max)
	nb_fem_receptive <- numeric(tps_max)
	coeff_pearson <- numeric(tps_max)
	nm <- 500
	nf <- 500
	taille_moy_fem <- numeric(nf)
taille_moy_mal <- numeric(nm)
	txmort_male <- 0.005
	txmort_femelle <- 0.005
	instant <- 1
	screeninstant <- 1

	split.screen(c(2,3))

	###################
	#TRAME DISTRIBUTION
	###################

	# Femelles :

	no <- c(1:nf)
	taille <- rnorm(nf,2,0.2)
	max_dist_mue <- ceiling((6.75+14.83*taille))
	dist_mue <- ceiling(runif(nf, min=0, max=max_dist_mue))
	no_male <- numeric(nf)
	date_receptivite <- floor(max_dist_mue/2)
	age <- 0
	nb_mue <- 0

	taille_male <- numeric(nf)
	dist_mue_male <- numeric(nf)
	femelle <- data.frame(no, taille, dist_mue, date_receptivite, age, nb_mue, no_male, taille_male, dist_mue_male)
	# windows()
	# hist(femelle$dist_mue[femelle$dist_mue <= femelle$date_receptivite])

	# Males :

	no <- c(1:nm)
	taille <- rnorm(nm,2.75,0.2)
	max_dist_mue <- (6.75+14.83*taille)  
	dist_mue <- ceiling(runif(nm, min=0, max=max_dist_mue))  
	age <- 0
	nb_mue <- 0
	no_femelle <- numeric(nm)
	taille_femelle <- numeric(nm)
	dist_mue_femelle <- numeric(nm)
	male <- data.frame(no, taille, dist_mue, age, nb_mue, no_femelle, taille_femelle, dist_mue_femelle)
	# windows()
	# hist(male$dist_mue - femelle$dist_mue)
	# x <- male$dist_mue - femelle$dist_mue
	# length(x[x >= 0])
	
	
	# no_male_selec <- sample(male$no,nf)
	# femelle$no_male <- no_male_selec
	# male$no_femelle[no_male_selec] <- femelle$no

	# femelle$taille_male <- male$taille[no_male_selec]
	# femelle$dist_mue_male <- male$dist_mue[no_male_selec]

	# male$taille_femelle[no_male_selec] <- femelle$taille
	# male$dist_mue_femelle[no_male_selec] <- femelle$dist_mue
	
	
	# windows()
	
	# hist(femelle$dist_mue)

	###########################
	# VIEILLISSEMENT POPULATION
	###########################

	for (tps in 1:tps_max) {

		######################
		# MORTALITE/NAISSANCES
		######################

		### Mortalité :
		
	  # combien de males et de femelles vont mourir aujourd'hui ?
		nb_male_mort <- floor(nm*txmort_male)
		nb_femelle_mort <- floor(nf*txmort_femelle)
		
	  # la mort s'abat aléatoirement sur les individus :
		no_male_mort <- sample(male$no, nb_male_mort)
		no_femelle_mort <- sample(femelle$no, nb_femelle_mort)
		
	  # les individus appariés aux morts se retrouvent seuls :
		veuf <- femelle$no_male[no_femelle_mort]
		veuve <- male$no_femelle[no_male_mort]
		male[veuf, 6:8] <- 0
		femelle[veuve, 7:9] <- 0
		
		taille_mort <- male$taille[no_male_mort]
		taille_morte <- femelle$taille[no_femelle_mort]
		dist_mue_mort <- male$dist_mue[no_male_mort]
		dist_mue_morte <- femelle$dist_mue[no_femelle_mort]
		
	  # suppression des individus malchanceux :
		male[no_male_mort, 2:8] <- 0 
		femelle[no_femelle_mort, 2:9] <- 0

		

	  ### Naissances :
	  # pour chaque mort, un individu nait :
  
		# femelle$taille[no_femelle_mort] <- taille_morte
		femelle$taille[no_femelle_mort] <- rnorm(no_femelle_mort,2,0.2)
		
		max_dist_mue <- ceiling(6.75 + 14.83 * femelle$taille[no_femelle_mort])
		femelle$dist_mue[no_femelle_mort] <- max_dist_mue
		# femelle$dist_mue[no_femelle_mort] <- ceiling(runif(no_femelle_mort, min=0, max=max_dist_mue))
		# femelle$dist_mue[no_femelle_mort] <- dist_mue_morte
		femelle$date_receptivite[no_femelle_mort] <- floor(femelle$dist_mue[no_femelle_mort]/2)
		
		# male$taille[no_male_mort] <- taille_mort
		male$taille[no_male_mort] <- rnorm(no_male_mort,2.75,0.2)
		max_dist_mue <- ceiling(6.75 + 14.83 * male$taille[no_male_mort])
		# male$dist_mue[no_male_mort] <- ceiling(runif(no_male_mort, min=0, max=max_dist_mue))
		male$dist_mue[no_male_mort] <- max_dist_mue
		# male$dist_mue[no_male_mort] <- dist_mue_mort

		# tous les individus vieillissent d'un jour :
		femelle$age <- femelle$age + 1
		male$age <- male$age + 1	
		femelle$dist_mue <- femelle$dist_mue -1
		femelle$dist_mue_male[femelle$dist_mue_male != 0] <- femelle$dist_mue_male[femelle$dist_mue_male != 0] -1
		male$dist_mue <- male$dist_mue -1
		male$dist_mue_femelle[male$dist_mue_femelle != 0] <- male$dist_mue_femelle[male$dist_mue_femelle != 0] -1
		
		# y en a-t-il qui mue aujourd'hui (femelle et male) ?
		nbmuetoday <- length(femelle$no[femelle$dist_mue == 0]) + length(male$no[male$dist_mue == 0])

	
		# si un des membres de la paire mue, le couple se sépare : 
		if (nbmuetoday > 0) {
			# quelles femelles muent aujourd'hui ?
			femelle_en_mue <- femelle$no[femelle$dist_mue == 0]
			# males appariés avec une femelle qui mue aujourd'hui :
			male_apparie <- femelle$no_male[femelle_en_mue]  
			
			# idem du point du vue du mâle :
			male_en_mue <- male$no[male$dist_mue == 0]
			femelle_apparie <- male$no_femelle[male_en_mue]
			
			# comptage du nombre de reproductions aujourd'hui :
			nbreprotoday <- length(male_apparie)
			
			# femelle devient libre :
			femelle$no_male[femelle_en_mue] <- 0
			femelle$taille_male[femelle_en_mue] <- 0
			femelle$dist_mue_male[femelle_en_mue] <- 0
			male$no_femelle[male_apparie] <- 0
			male$taille_femelle[male_apparie] <- 0
			male$dist_mue_femelle[male_apparie] <- 0
			
			# male devient libre :
			male$no_femelle[male_en_mue] <- 0
			male$taille_femelle[male_en_mue] <- 0
			male$dist_mue_femelle[male_en_mue] <- 0
			femelle$no_male[femelle_apparie] <- 0
			femelle$taille_male[femelle_apparie] <- 0
			femelle$dist_mue_male[femelle_apparie] <- 0
			
			# les individus grandissent après mue (+10%) :
			femelle$taille[femelle_en_mue] <- femelle$taille[femelle_en_mue]*1.10
			femelle$dist_mue[femelle_en_mue] <- ceiling(6.75+14.83*femelle$taille[femelle_en_mue])
			femelle$date_receptivite[femelle_en_mue] <- floor(femelle$dist_mue[femelle_en_mue]/2)
		  
			male$taille[male_en_mue] <- male$taille[male_en_mue]*1.10
			male$dist_mue[male_en_mue] <- ceiling(6.75+14.83*male$taille[male_en_mue])
			
			# on compte 1 mue de plus : 
			femelle$nb_mue[femelle_en_mue] <- femelle$nb_mue[femelle_en_mue] + 1 
			male$nb_mue[male_en_mue] <- male$nb_mue[male_en_mue] + 1
		}
	
		############
		#APPARIEMENT
		############
		

		# combien de femelles sont libres & receptives aujourd'hui ?
		
		nblibretoday <- length(femelle$no[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite])
		# nblibretoday <- length(femelle$no[femelle$no_male == 0])
		
		nbreceptive <- length(femelle$no[femelle$dist_mue <= femelle$date_receptivite])
		
		# combien de males sont célibataires ?
		nbcelibataire <- length(male$no[male$no_femelle == 0])
		
		# s'il y a des individus libres, on lance l'appariement
		if (nblibretoday > 0 & nbcelibataire > 0){
		  # pour chaque femelle (ligne = compteur de ligne) :
		  
			# for (ligne in femelle$no[femelle$no_male == 0) {
			for (ligne in femelle$no[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite]) {
		
				# quels mâles sont disponibles (i.e. célibataires et qui muent APRES la femelle) ?
	
				disponible <- male$no[male$no_femelle == 0 & (male$dist_mue > femelle$dist_mue[ligne])]
				# disponible <- male$no[male$no_femelle == 0]
				
				if (length(disponible) != 0){
					# /!\ pour eviter le "bug du sample" :
					if (length(disponible) == 1) {
						no_male_selec <- disponible
					} else if (length(disponible) > 1) {
					  # choix aléatoire du male :
						no_male_selec <- sample(disponible,1)
					}
					femelle$no_male[ligne] <- no_male_selec
					male$no_femelle[no_male_selec] <- femelle$no[ligne]
					femelle$taille_male[ligne] <- male$taille[no_male_selec]
					femelle$dist_mue_male[ligne] <- male$dist_mue[no_male_selec]
					male$taille_femelle[no_male_selec] <- femelle$taille[ligne]
					male$dist_mue_femelle[no_male_selec] <- femelle$dist_mue[ligne]
				}
			}
		}
		
		nbreceptive <- length(femelle$no[femelle$dist_mue <= femelle$date_receptivite])
		nblibretoday <- length(femelle$no[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite])
		# nblibretoday <- length(femelle$no[femelle$no_male == 0])
		disponible <- male$no[male$no_femelle == 0 & (male$dist_mue > femelle$dist_mue[ligne])]
		# disponible <- male$no[male$no_femelle == 0]
		
		# table(femelle$no_male)
		# table(male$no_femelle)

		coeff <- cor.test(femelle$taille_male[femelle$no_male != 0], femelle$taille[femelle$no_male != 0], method = "pearson")$estimate
		nb_ind_en_mue[tps] <- nbmuetoday
		nb_fem_libre[tps] <- nblibretoday
		nb_fem_receptive[tps] <- nbreceptive
		nb_mal_dispo[tps] <- length(disponible)
		nb_couples[tps] <- length(femelle$no[femelle$no_male != 0])
		nb_repro[tps] <- nbreprotoday
		coeff_pearson[tps] <- coeff
		temps[tps] <- tps
		
		
	}
	femelle
	male
	nb_ind_en_mue
	nb_fem_libre
	nb_mal_dispo 
	nb_fem_receptive
	nb_couples
	nb_repro
	coeff_pearson
	temps
	
	if (iteration == instant) {
			screen(screeninstant)
			plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : réplicat", instant), asp=1)
			abline(0,1, lty=3)
			reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
			abline(reg)
			instant <- instant + 1
			screeninstant <- screeninstant + 1
	}
	
	nb_receptive_moy <- nb_receptive_moy + nb_fem_receptive/repetition
	nb_fem_libre_moy <- nb_fem_libre_moy + nb_fem_libre/repetition
	nb_couples_moy <- nb_couples_moy + nb_couples/repetition
	nb_repro_moy <- nb_repro_moy + nb_repro/repetition
	coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/repetition 
	taille_moy_fem <- taille_moy_fem + femelle$taille/repetition
	taille_moy_mal <- taille_moy_mal + male$taille/repetition

}

nb_receptive_moy
nb_fem_libre_moy
nb_couples_moy
nb_repro_moy
coeff_pearson_moy
taille_moy_fem
taille_moy_mal

windows()
	split.screen(c(1,2))
	screen(1)
	hist(taille_moy_fem, xlab= 'Taille', main = 'Distribution taille femelles', breaks = 40)
	screen(2)
	hist(taille_moy_mal, xlab= 'Taille', main = 'Distribution taille males', breaks = 40)

	windows()
	plot(nb_couples_moy, type="l", ylim=c(-1,500), xlab='Jours', col="red", main='Suivi des femelles')
	points(nb_receptive_moy, type="l", col="pink")
	points(nb_fem_libre_moy, type="l", col="orange")
	
	windows()
	plot(nb_repro_moy, type='l', xlab='Jours', main='Nombre de reproductions par jour')
	
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	

fin <- Sys.time()
warnings()
	fin - debut
