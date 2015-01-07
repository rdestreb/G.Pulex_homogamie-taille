rm(list=ls())
graphics.off()

#Prendre en compte la date de receptivité des femelles ? Non = 0 ; Oui = 1
recept <- 0
# Choix des males ? Aléatoire = 0 ; Si mue APRES la femelle = 1 
choix_male <- 0

tx_croissance <- 1.05

txmort_male <- 0
txmort_femelle <- 0

nm <- 500
nf <- 500

debut <- Sys.time()

tps_max <- 10000
repetition <- 5

coeff_pearson_moy <- numeric(tps_max)


instant <- 1
screeninstant <- 1
split.screen(c(2,3))

for (iteration in 1:repetition){


	taille_fem_seule <- numeric(tps_max)
	taille_male_seul <- numeric(tps_max)
	taille_male <- numeric(tps_max)
	taille_femelle <- numeric(tps_max)
	coeff_pearson <- numeric(tps_max)
	
	

###########################
##### CREATION POPULATION #
###########################

	# Femelles :
	
	f_no <- c(1:nf)
	f_taille <- round(rnorm(nf,2,0.2),3)  
	f_max_dist_mue <- round((6.75+14.83*f_taille),2)
	f_dist_mue <- round((runif(nf, min=0, max=f_max_dist_mue)),2)
	f_date_receptivite <-round((f_max_dist_mue/2),2)
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
	
	
###############################
##### DYNAMIQUE POPULATION ####
###############################
	
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
		f_date_receptivite[no_femelle_mort] <- round(f_max_dist_mue[no_femelle_mort]/2)
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
			f_date_receptivite[femelle_en_mue] <- round(f_max_dist_mue[femelle_en_mue]/2)
			m_max_dist_mue[male_en_mue] <- round((6.75+14.83*m_taille[male_en_mue]),2)
			m_dist_mue[male_en_mue] <- m_max_dist_mue[male_en_mue]
			
			# on compte 1 mue de plus : 
			f_nb_mue[femelle_en_mue] <- f_nb_mue[femelle_en_mue] + 1
			m_nb_mue[male_en_mue] <- m_nb_mue[male_en_mue] + 1
		}

		
		################################
		######### APPARIEMENT ##########
		################################

		
		
		############
		#receptivité
		############
		
		if(recept == 1){
			# combien de femelles sont libres & receptives aujourd'hui ?
			nb_femelle_dispo <- length(f_no[(f_no_male == 0) & (f_dist_mue <= f_date_receptivite)])
			# lesquelles ?
			no_femelle_dispo <- f_no[(f_no_male == 0) & (f_dist_mue <= f_date_receptivite)]
		}else if (recept == 0){
			nb_femelle_dispo <- length(f_no[f_no_male == 0])
			no_femelle_dispo <- f_no[f_no_male == 0]
		}
		
		# combien sont receptives ? (comptage)
		nbreceptive <- length(f_no[f_dist_mue <= f_date_receptivite])
		
		# combien de males sont célibataires ?
		nb_male_celibataire <- length(m_no[m_no_femelle == 0])
		
		# s'il y a des individus libres, on lance l'appariement
		if ((nb_femelle_dispo > 0) & (nb_male_celibataire > 0)){
		
			if (nb_femelle_dispo > 1){
			# shaker des femelles disponibles (les têtes de liste ne sont plus privilégiées) :
				no_femelle_dispo <- sample(no_femelle_dispo, nb_femelle_dispo)
			}
			
			# pour chaque femelle disponible (ligne = compteur de ligne) :
			
			for (ligne in no_femelle_dispo){
			
				##############
				# CHOIX MALE #
				##############
				
				# quels mâles sont disponibles (i.e. célibataires et qui muent APRES la femelle) ?
				if (choix_male == 1){
					no_male_dispo <- m_no[(m_no_femelle == 0) & (m_dist_mue > f_dist_mue[ligne])]
				}else if(choix_male == 0){
					no_male_dispo <- m_no[m_no_femelle == 0]
				}
				nb_male_dispo <- length(no_male_dispo)
				if (nb_male_dispo != 0){
					# /!\ pour eviter le "bug du sample" :
					if (nb_male_dispo == 1) {
						no_male_selec <- no_male_dispo
					} else if (nb_male_dispo > 1) {
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
		
		
		# compilation des valeurs (en vue d'un plot) :
		coeff <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")$estimate
		coeff_pearson[tps] <- coeff
		
		coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/tps_max
	
	}

	# moyennes des valeurs sur tous les réplicats :
	
	
}
	femelle <- data.frame(f_no, f_taille, f_dist_mue, f_date_receptivite, f_age, f_nb_mue, f_no_male, f_taille_male, f_dist_mue_male)
	male <- data.frame(m_no, m_taille, m_dist_mue, m_age, m_nb_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)

	# graphs :

	

	# homogamie :
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : réplicat", iteration), asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	
fin <- Sys.time()
warnings()
fin - debut