rm(list=ls())

debut <- Sys.time()

tps_max <- 1200
repetition <- 10

fem_5j_avt_mue_moy <- numeric(tps_max)
fem_10j_avt_mue_moy <- numeric(tps_max)
fem_15j_avt_mue_moy <- numeric(tps_max)
fem_5j_avt_rec_moy <- numeric(tps_max)
pret_fem_5j_avt_mue_moy <- numeric(tps_max)
pret_fem_10j_avt_mue_moy <- numeric(tps_max)
pret_fem_15j_avt_mue_moy <- numeric(tps_max)
nb_receptive_moy <- numeric(tps_max)
nb_fem_dispo_moy <- numeric(tps_max)
nb_male_dispo_moy <- numeric(tps_max)
nb_male_seul_moy <- numeric(tps_max)
nb_couples_moy <- numeric(tps_max)
nb_repro_moy <- numeric(tps_max)
coeff_pearson_moy <- numeric(tps_max)

instant <- 200
screeninstant <- 1
split.screen(c(2,3))

for (iteration in 1:repetition){

	nb_ind_en_mue <- numeric(tps_max)
	fem_5j_avt_mue <- numeric(tps_max)
	fem_10j_avt_mue <- numeric(tps_max)
	fem_15j_avt_mue <- numeric(tps_max)
	fem_5j_avt_rec <- numeric(tps_max)
	pret_fem_5j_avt_mue <- numeric(tps_max)
	pret_fem_10j_avt_mue <- numeric(tps_max)
	pret_fem_15j_avt_mue <- numeric(tps_max)
	nb_fem_dispo <- numeric(tps_max)
	nb_male_dispo <- numeric(tps_max)
	nb_male_seul <- numeric(tps_max)
	nb_couples <- numeric(tps_max)
	nb_repro <- numeric(tps_max)
	nb_receptive <- numeric(tps_max)
	coeff_pearson <- numeric(tps_max)
	nm <- 1500
	nf <- 1500
	taille_moy_fem <- numeric(nf)
	taille_moy_mal <- numeric(nm)
	txmort_male <- 0.005
	txmort_femelle <- 0.005
	

	###################
	#TRAME DISTRIBUTION
	###################

	# Femelles :
	
	f_no <- c(1:nf)
	f_taille <- rnorm(nf,2,0.2)
	# f_max_dist_mue <- ceiling(6.75+14.83*f_taille)
	f_max_dist_mue <- rep(40,nf)
	# f_dist_mue <- rep(40,nf)
	f_dist_mue <- ceiling(runif(nf, min=0, max=f_max_dist_mue))
	f_date_receptivite <- floor(f_max_dist_mue/2)
	f_age <- numeric(nf)
	f_nb_mue <- numeric(nf)
	f_no_male <- numeric(nf)
	f_taille_male <- numeric(nf)
	f_dist_mue_male <- numeric(nf)

	# windows()
	# hist(f_dist_mue)

	# Males :

	m_no <- c(1:nm)
	m_taille <- rnorm(nm,2.75,0.2)
	# m_max_dist_mue <- ceiling(6.75+14.83*m_taille)  
	m_max_dist_mue <- rep(40,nm)
	# m_dist_mue <- rep(40, nm)
	m_dist_mue <- ceiling(runif(nm, min=0, max=m_max_dist_mue)) 	
	m_age <- numeric(nm)
	m_nb_mue <- numeric(nm)
	m_no_femelle <- numeric(nm)
	m_taille_femelle <- numeric(nm)
	m_dist_mue_femelle <- numeric(nm)

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
		
		taille_mort <- m_taille[no_male_mort]
		taille_morte <- f_taille[no_femelle_mort]
		dist_mue_mort <- m_dist_mue[no_male_mort]
		dist_mue_morte <- f_dist_mue[no_femelle_mort]
		
	  # suppression des individus malchanceux :
		f_taille[no_femelle_mort] <- 0
		f_max_dist_mue[no_femelle_mort] <- 0
		f_dist_mue[no_femelle_mort] <- 0
		f_date_receptivite[no_femelle_mort] <- 0
		f_age[no_femelle_mort] <- 0
		f_nb_mue[no_femelle_mort] <- 0
		f_no_male[no_femelle_mort] <- 0
		f_taille_male[no_femelle_mort] <- 0
		f_dist_mue_male[no_femelle_mort] <- 0
		
		m_taille[no_male_mort] <- 0
		m_max_dist_mue[no_male_mort] <- 0
		m_dist_mue[no_male_mort] <- 0  
		m_age[no_male_mort]<- 0
		m_nb_mue[no_male_mort] <- 0
		m_no_femelle[no_male_mort] <- 0
		m_taille_femelle[no_male_mort] <- 0
		m_dist_mue_femelle[no_male_mort] <- 0

		
	  ### Naissances :
	  # pour chaque mort, un individu nait :
  
		# f_taille[no_femelle_mort] <- taille_morte
		f_taille[no_femelle_mort] <- rnorm(nb_femelle_mort,2,0.2)
		# f_max_dist_mue[no_femelle_mort] <- ceiling(6.75 + 14.83 * f_taille[no_femelle_mort])
		f_max_dist_mue[no_femelle_mort] <- 40
		f_dist_mue[no_femelle_mort] <- f_max_dist_mue[no_femelle_mort]
		# f_dist_mue[no_femelle_mort] <- dist_mue_morte
		f_date_receptivite[no_femelle_mort] <- floor(f_max_dist_mue[no_femelle_mort]/2)
		
		# m_taille[no_male_mort] <- taille_mort
		m_taille[no_male_mort] <- rnorm(nb_male_mort,2.75,0.2)
		# m_max_dist_mue[no_male_mort] <- ceiling(6.75 + 14.83 * m_taille[no_male_mort])
		m_max_dist_mue[no_male_mort] <- 40
		m_dist_mue[no_male_mort] <- m_max_dist_mue[no_male_mort]
		# m_dist_mue[no_male_mort] <- dist_mue_mort

		
		# tous les individus vieillissent d'un jour :
		f_age <- f_age + 1
		m_age <- m_age + 1	
		# tous les individus se rapprochent de leur date de mue : 
		f_dist_mue <- f_dist_mue - 1
		m_dist_mue <- m_dist_mue - 1		
		# les compagnons se rapprochent aussi de leur mue :
		f_dist_mue_male[f_dist_mue_male != 0] <- f_dist_mue_male[f_dist_mue_male != 0] -1
		m_dist_mue_femelle[m_dist_mue_femelle != 0] <- m_dist_mue_femelle[m_dist_mue_femelle != 0] -1
		
		# y en a-t-il qui mue aujourd'hui (femelle et male) ?
		nbmuetoday <- length(f_no[f_dist_mue == 0]) + length(m_no[m_dist_mue == 0])
	
		# si un des membres de la paire mue, le couple se sépare : 
		if (nbmuetoday > 0) {
			# quelles femelles muent aujourd'hui ?
			femelle_en_mue <- f_no[f_dist_mue <= 0]
			# qui parmi elles sont appariées ?
			femelle_en_mue_apparie <-f_no[(f_dist_mue <= 0) & (f_no_male != 0)]
			# males appariés avec une femelle qui mue aujourd'hui :
			male_apparie <- f_no_male[femelle_en_mue]  
			
			# idem pour les mâles :
			male_en_mue <- m_no[m_dist_mue <= 0]
			male_en_mue_apparie <- m_no[(m_dist_mue <= 0) & (m_no_femelle != 0)]
			femelle_apparie <- m_no_femelle[male_en_mue]
			
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
			f_taille[femelle_en_mue] <- f_taille[femelle_en_mue]*1.10
			m_taille[male_en_mue] <- m_taille[male_en_mue]*1.10
			# ils sont ensuite au maximum de la distance à leur prochaine mue :
			# f_dist_mue[femelle_en_mue] <- ceiling(6.75+14.83*f_taille[femelle_en_mue])  
			f_dist_mue[femelle_en_mue] <- 40
			# m_dist_mue[male_en_mue] <- ceiling(6.75+14.83*m_taille[male_en_mue])
			m_dist_mue[male_en_mue] <- 40
			f_date_receptivite[femelle_en_mue] <- floor(f_dist_mue[femelle_en_mue]/2) #ici, dist_mue = max_dist_mue
			
			# on compte 1 mue de plus : 
			f_nb_mue[femelle_en_mue] <- f_nb_mue[femelle_en_mue] + 1
			m_nb_mue[male_en_mue] <- m_nb_mue[male_en_mue] + 1
		}
	
		############
		#APPARIEMENT
		############

		# combien de femelles sont libres & receptives aujourd'hui ?
		
		nb_femelle_dispo <- length(f_no[(f_no_male == 0) & (f_dist_mue <= f_date_receptivite)])
		# nb_femelle_dispo <- length(f_no[f_no_male == 0])
		
		# lesquelles ?
		no_femelle_dispo <- f_no[(f_no_male == 0) & (f_dist_mue <= f_date_receptivite)]
		
		# combien sont receptives ? (comptage)
		nbreceptive <- length(f_no[f_dist_mue <= f_date_receptivite])
		
		# combien de males sont célibataires ?
		nb_male_celibataire <- length(m_no[m_no_femelle == 0])
		
		# s'il y a des individus libres, on lance l'appariement
		if ((nb_femelle_dispo > 0) & (nb_male_celibataire > 0)){
		
			# shaker des femelles disponibles (les 1eres ne sont pas privilégiées) :
			no_femelle_dispo <- sample(no_femelle_dispo, nb_femelle_dispo)
		
			# pour chaque femelle disponible (ligne = compteur de ligne) :
			# for (ligne in f_no[f_no_male == 0]) {
			for (ligne in no_femelle_dispo){
		
				# quels mâles sont disponibles (i.e. célibataires et qui muent APRES la femelle) ?
	
				no_male_dispo <- m_no[(m_no_femelle == 0) & (m_dist_mue > f_dist_mue[ligne])]
				# no_male_dispo <- m_no[m_no_femelle == 0]
				
				if (length(no_male_dispo) != 0){
					# /!\ pour eviter le "bug du sample" :
					if (length(no_male_dispo) == 1) {
						no_male_selec <- no_male_dispo
					} else if (length(no_male_dispo) > 1) {
					  # choix aléatoire du male :
						no_male_selec <- sample(no_male_dispo,1)
					}
					f_no_male[ligne] <- no_male_selec
					m_no_femelle[no_male_selec] <- f_no[ligne]
					f_taille_male[ligne] <- m_taille[no_male_selec]
					f_dist_mue_male[ligne] <- m_dist_mue[no_male_selec]
					m_taille_femelle[no_male_selec] <- f_taille[ligne]
					m_dist_mue_femelle[no_male_selec] <- f_dist_mue[ligne]
				}
			}
		}
		
		# table(f_no_male)
		# table(m_no_femelle)

		# nb_femelle_dispo <- length(f_no[f_no_male == 0 & f_dist_mue <= f_date_receptivite])
		# nb_femelle_dispo <- length(f_no[f_no_male == 0])
		nbreceptive <- length(f_no[f_dist_mue <= f_date_receptivite])
		nb_male_celibataire <- length(m_no[m_no_femelle == 0])
		# no_male_dispo <- m_no[m_no_femelle == 0 & (m_dist_mue > f_dist_mue[ligne])]
		# no_male_dispo <- m_no[m_no_femelle == 0]
		
		
		# suivi des femelles x jours avant mue :
		
		# femelle_5j_avt_mue <- f_no[(f_dist_mue <= 5) & (f_dist_mue >= 1)]
		# nb_fem_5j_avt_mue <- length(femelle_5j_avt_mue)
		# femelle_10j_avt_mue <- f_no[(f_dist_mue <= 10) & (f_dist_mue >= 1)]
		# nb_fem_10j_avt_mue <- length(femelle_10j_avt_mue)
		# femelle_15j_avt_mue <- f_no[(f_dist_mue <= 15) & (f_dist_mue >= 1)]
		# nb_fem_15j_avt_mue <- length(femelle_15j_avt_mue)
		
		femelle_15j_avt_mue <- f_no[(f_dist_mue <= 20) & (f_dist_mue >= 1)]
		nb_fem_15j_avt_mue <- length(femelle_15j_avt_mue)
		
		
		
		# suivi des mâles pouvant prendre les femelles ci-dessus :
		
		# pretendant_fem_5j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 5)]
		# nb_pretendant_fem_5j_avt_mue <- length(m_no[(m_no_femelle == 0) & (m_dist_mue > 5)])
		# pretendant_fem_10j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 10)]
		# nb_pretendant_fem_10j_avt_mue <- length(m_no[(m_no_femelle == 0) & (m_dist_mue > 10)])
		# pretendant_fem_15j_avt_mue <- m_no[(m_no_femelle == 0) & (m_dist_mue > 15)]
		# nb_pretendant_fem_15j_avt_mue <- length(m_no[(m_no_femelle == 0) & (m_dist_mue > 15)])
		
		# pretendant_fem_5j_avt_mue <- m_no[(m_dist_mue > 5)]
		# nb_pretendant_fem_5j_avt_mue <- length(m_no[(m_dist_mue > 5)])
		# pretendant_fem_10j_avt_mue <- m_no[(m_dist_mue > 10)]
		# nb_pretendant_fem_10j_avt_mue <- length(m_no[(m_dist_mue > 10)])
		# pretendant_fem_15j_avt_mue <- m_no[(m_dist_mue > 15)]
		# nb_pretendant_fem_15j_avt_mue <- length(m_no[(m_dist_mue > 15)])
		
		pretendant_fem_15j_avt_mue <- m_no[(m_dist_mue > 20)]
		nb_pretendant_fem_15j_avt_mue <- length(m_no[(m_dist_mue > 20)])
		
		nb_fem_5j_avt_rec <- length(f_no[f_dist_mue <= f_date_receptivite + 5 & f_dist_mue >= f_date_receptivite + 1])
		
		coeff <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")$estimate
		nb_ind_en_mue[tps] <- nbmuetoday
		nb_fem_dispo[tps] <- nb_femelle_dispo
		# fem_5j_avt_mue[tps] <- nb_fem_5j_avt_mue
		# fem_10j_avt_mue[tps] <- nb_fem_10j_avt_mue
		fem_15j_avt_mue[tps] <- nb_fem_15j_avt_mue
		fem_5j_avt_rec[tps] <- nb_fem_5j_avt_rec
		# pret_fem_5j_avt_mue[tps] <- nb_pretendant_fem_5j_avt_mue
		# pret_fem_10j_avt_mue[tps] <- nb_pretendant_fem_10j_avt_mue
		pret_fem_15j_avt_mue[tps] <- nb_pretendant_fem_15j_avt_mue
		nb_receptive[tps] <- nbreceptive
		nb_male_dispo[tps] <- length(no_male_dispo)
		nb_male_seul[tps] <- nb_male_celibataire
		nb_couples[tps] <- length(f_no[f_no_male != 0])
		nb_repro[tps] <- nbreprotoday
		coeff_pearson[tps] <- coeff
	
		if(tps == instant ){
			screen(screeninstant)
			plot(f_dist_mue~f_taille, xlab="Taille femelle", ylab="Distance à la mue", main=paste("Jour",instant))
			instant <- instant + 200
			screeninstant <- screeninstant + 1
		}
	}

	# fem_5j_avt_mue_moy <- fem_5j_avt_mue_moy + fem_5j_avt_mue/repetition
	# fem_10j_avt_mue_moy <- fem_10j_avt_mue_moy + fem_10j_avt_mue/repetition
	fem_15j_avt_mue_moy <- fem_15j_avt_mue_moy + fem_15j_avt_mue/repetition
	fem_5j_avt_rec_moy <- fem_5j_avt_rec_moy + fem_5j_avt_rec/repetition
	# pret_fem_5j_avt_mue_moy <- pret_fem_5j_avt_mue_moy + pret_fem_5j_avt_mue/repetition
	# pret_fem_10j_avt_mue_moy <- pret_fem_10j_avt_mue_moy + pret_fem_10j_avt_mue/repetition
	pret_fem_15j_avt_mue_moy <- pret_fem_15j_avt_mue_moy + pret_fem_15j_avt_mue/repetition
	nb_receptive_moy <- nb_receptive_moy + nb_receptive/repetition
	nb_fem_dispo_moy <- nb_fem_dispo_moy + nb_fem_dispo/repetition
	nb_male_seul_moy <- nb_male_seul_moy + nb_male_seul/repetition
	nb_male_dispo_moy <- nb_male_dispo_moy + nb_male_dispo/repetition
	nb_couples_moy <- nb_couples_moy + nb_couples/repetition
	nb_repro_moy <- nb_repro_moy + nb_repro/repetition
	coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/repetition 
	taille_moy_fem <- taille_moy_fem + f_taille/repetition
	taille_moy_mal <- taille_moy_mal + m_taille/repetition
	
}
	femelle <- data.frame(f_no, f_taille, f_dist_mue, f_date_receptivite, f_age, f_nb_mue, f_no_male, f_taille_male, f_dist_mue_male)
	male <- data.frame(m_no, m_taille, m_dist_mue, m_age, m_nb_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)
	
# nb_receptive_moy
# nb_fem_dispo_moy
# nb_couples_moy
# nb_repro_moy
# coeff_pearson_moy
# taille_moy_fem
# taille_moy_mal

windows()
	split.screen(c(1,2))
	screen(1)
	hist(taille_moy_fem, xlab= 'Taille', main = 'Distribution taille femelles', breaks = 40)
	screen(2)
	hist(taille_moy_mal, xlab= 'Taille', main = 'Distribution taille males', breaks = 40)

	windows()
	plot(nb_couples_moy, type="l", lwd=3, xlim=c(0,1200), ylim=c(-1,500), xlab='Jours', col="red", main='Suivi des femelles')
	points(nb_receptive_moy, type="l", col="pink")
	points(nb_fem_dispo_moy, type="l", col="orange")
	points(nb_couples_moy + nb_male_seul_moy, type="l", col="black")
	points(nb_male_dispo_moy, type="l", col="blue")
	points(fem_5j_avt_rec_moy, type="l", col="brown")
	points(fem_5j_avt_mue_moy, type="l", col="green")
	
	windows()
	plot(nb_repro_moy, type='l', xlab='Jours', main='Nombre de reproductions par jour')
	
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : réplicat", iteration), asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	
	windows()
	plot(nb_fem_dispo_moy, type="l", col="orange")
	
	windows()
	plot(fem_5j_avt_mue_moy, type="l", lwd=3,xlim=c(0,300), ylim=c(500,1000), xlab="jours", ylab="effectif", main="Suivi des individus 20 jours avant mue des femelles", col="green")
	# points(fem_10j_avt_mue_moy, type="l", lwd=3, col="blue")
	points(fem_15j_avt_mue_moy, type="l", lwd=3, col="brown")
	# points(pret_fem_5j_avt_mue_moy, type="l", col="green")
	# points(pret_fem_10j_avt_mue_moy, type="l", col="blue")
	points(pret_fem_15j_avt_mue_moy, type="l", col="brown")
	points(nb_couples_moy, type="l", lty=3, lwd=3, col="red")
	mtext("traits épais : femelles ; traits fins : mâles prétendants ; pointillés : couples", 3, cex  = 0.8)
	
	windows()
	plot(fem_5j_avt_rec_moy, type="l", col="brown")
	
fin <- Sys.time()
warnings()
	fin - debut