rm(list=ls())
graphics.off()
debut <- Sys.time()
# Choix des males ? Al�atoire = 0 ; Si mue APRES la femelle = 1 
choix_male <- 1

tps_max <- 1000
repetition <- 1

tx_croissance <- 1.1

tx_mort <- 0.012

taille_pop_totale <- seq(500,5000,500)

sex_ratio <- 0.5 #sex-ratio = nb males/taille pop totale

# instant <- 1
# cpt_sr <- 1
# for ( sr_boucle in sex_ratio){

	pearson_tp <- numeric(length(taille_pop_totale))
	IC95_max_tp <- numeric(length(taille_pop_totale))
	IC95_min_tp <- numeric(length(taille_pop_totale))
	tp_boucle <- 0
	cpt <- 0  #compteur (nombre de tour de code)

	for (tp_boucle in taille_pop_totale){

		cpt <- cpt + 1

		txmort_male <- tx_mort
		txmort_femelle <- tx_mort
		
		nm <- round(sex_ratio * tp_boucle)
		nf <- round(tp_boucle - nm)

		# nb_couples_moy <- numeric(tps_max)
		coeff_pearson_moy <- numeric(tps_max)
		IC95_max_moy <- numeric(tps_max)
		IC95_min_moy <- numeric(tps_max)
		pearson_1000 <- 0
		IC95_max_1000 <- 0
		IC95_min_1000 <- 0
		f_no <- numeric(nf)
		f_taille <-numeric(nf)
		f_max_dist_mue <- numeric(nf)
		f_dist_mue <- numeric(nf)

		m_no <- numeric(nm)
		m_taille <-numeric(nm)
		m_max_dist_mue <- numeric(nm)
		m_dist_mue <- numeric(nm)
		

		for (iteration in 1:repetition){

			nb_couples <- numeric(tps_max)
			coeff_pearson <- numeric(tps_max)
			IC95_max <- numeric(tps_max)
			IC95_min <- numeric(tps_max)

		################################
		##### CREATION POPULATION ######
		################################

			# Femelles :
			
			f_no <- c(1:nf)
			f_taille <- round(rnorm(nf,2,0.2),3)  
			f_max_dist_mue <- round((6.75+14.83*f_taille),2)
			f_dist_mue <- round((runif(nf, min=0, max=f_max_dist_mue)),2)
			f_no_male <- numeric(nf)
			f_taille_male <- numeric(nf)
			f_dist_mue_male <- numeric(nf)

			# Males :

			m_no <- c(1:nm)
			m_taille <- round(rnorm(nm,2.75,0.2),3)
			m_max_dist_mue <- round((6.75+14.83*m_taille),2)	
			m_dist_mue <- round((runif(nm, min=0, max=m_max_dist_mue)),2)
			m_no_femelle <- numeric(nm)
			m_taille_femelle <- numeric(nm)
			m_dist_mue_femelle <- numeric(nm)
			
		################################
		##### DYNAMIQUE POPULATION #####
		################################
			
			for (tps in 1:tps_max) {

				########################
				# MORTALITE/NAISSANCES #
				########################

				### Mortalit� :
				
			  # combien de males et de femelles vont mourir aujourd'hui ?
				nb_male_mort <- floor(nm*txmort_male)
				nb_femelle_mort <- floor(nf*txmort_femelle)
				

			  # la mort s'abat al�atoirement sur les individus :
				no_male_mort <- sample(m_no, nb_male_mort)
				no_femelle_mort <- sample(f_no, nb_femelle_mort)
				
			  # les individus appari�s aux morts se retrouvent seuls :
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
				f_no_male[no_femelle_mort] <- 0
				f_taille_male[no_femelle_mort] <- 0
				f_dist_mue_male[no_femelle_mort] <- 0
				
				m_taille[no_male_mort] <- round(rnorm(nb_male_mort,2.75,0.2),3)
				m_max_dist_mue[no_male_mort] <- round((6.75 + 14.83 * m_taille[no_male_mort]),2)
				m_dist_mue[no_male_mort] <- m_max_dist_mue[no_male_mort]
				m_no_femelle[no_male_mort] <- 0
				m_taille_femelle[no_male_mort] <- 0
				m_dist_mue_femelle[no_male_mort] <- 0
			
				################################
				######## VIEILLISSEMENT ########
				################################
				
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
			
				# si un des membres de la paire mue, le couple se s�pare : 
				if (nbmuetoday > 0) {
					# quelles femelles muent aujourd'hui ?
					femelle_en_mue <- f_no[f_dist_mue <= 0]
					# qui parmi elles sont appari�es ?
					femelle_en_mue_apparie <-f_no[(f_dist_mue <= 0) & (f_no_male != 0)]
					# males appari�s avec une femelle qui mue aujourd'hui :
					male_apparie <- f_no_male[femelle_en_mue_apparie]
					
					# idem pour les m�les :
					male_en_mue <- m_no[m_dist_mue <= 0]
					male_en_mue_apparie <- m_no[(m_dist_mue <= 0) & (m_no_femelle != 0)]
					femelle_apparie <- m_no_femelle[male_en_mue_apparie]			
					
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
					
					# les individus grandissent apr�s mue (+10%) :
					f_taille[femelle_en_mue] <- round(f_taille[femelle_en_mue]*tx_croissance ,3)
					m_taille[male_en_mue] <- round(m_taille[male_en_mue]*tx_croissance ,3)
					# ils sont ensuite au maximum de la distance � leur prochaine mue :
					f_max_dist_mue[femelle_en_mue] <- round((6.75+14.83*f_taille[femelle_en_mue]),2)
					f_dist_mue[femelle_en_mue] <- f_max_dist_mue[femelle_en_mue]
					m_max_dist_mue[male_en_mue] <- round((6.75+14.83*m_taille[male_en_mue]),2)
					m_dist_mue[male_en_mue] <- m_max_dist_mue[male_en_mue]
				}
				
				################################
				######### APPARIEMENT ##########
				################################		

				nb_femelle_dispo <- length(f_no[f_no_male == 0])
				no_femelle_dispo <- f_no[f_no_male == 0]
				
				# combien de males sont c�libataires ?
				nb_male_celibataire <- length(m_no[m_no_femelle == 0])
				
				# s'il y a des individus libres, on lance l'appariement
				if ((nb_femelle_dispo > 0) & (nb_male_celibataire > 0)){
				
					# if (nb_femelle_dispo > 1){
					# shaker des femelles disponibles (les t�tes de liste ne sont plus privil�gi�es) :
						# no_femelle_dispo <- sample(no_femelle_dispo, nb_femelle_dispo)
					# }
					
					# pour chaque femelle disponible (ligne = compteur de ligne) :
					
					for (ligne in no_femelle_dispo){
					
						##############
						# CHOIX MALE #
						##############
						
						# quels m�les sont disponibles (i.e. c�libataires et qui muent APRES la femelle) ?
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
							  # choix al�atoire du male :
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
				test_pearson <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")
				coeff <- test_pearson$estimate
				IC95_min[tps] <- test_pearson$conf.int[1]
				IC95_max[tps] <- test_pearson$conf.int[2]
				coeff_pearson[tps] <- coeff
				# nb_couples[tps] <- length(f_no[f_no_male != 0])
			}

			# moyennes des valeurs sur tous les r�plicats :
			coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/repetition 
			IC95_min_moy <- IC95_min_moy + IC95_min/repetition
			IC95_max_moy <- IC95_max_moy + IC95_max/repetition
			# nb_couples_moy <- nb_couples_moy + nb_couples/repetition
			pearson_1000 <- pearson_1000 + coeff_pearson[1000]/repetition
			IC95_min_1000 <- IC95_min_1000 + IC95_min[1000]/repetition
			IC95_max_1000 <- IC95_max_1000 + IC95_max[1000]/repetition
		}
		pearson_tp[cpt] <- pearson_1000
		IC95_min_tp[cpt] <- IC95_min_1000
		IC95_max_tp[cpt] <- IC95_max_1000
	}
	
	# if(instant == cpt_sr){
		# pearson_tp_sr <- data.frame(taille_pop_totale, pearson_tp, IC95_min_tm, IC95_max_tm)
		# write.table(pearson_tp_sr, paste("pearson_tp_", instant,".csv"))
		# instant <- instant + 1
		# cpt_sr <- cpt_sr + 1
	# }
# }

	# femelle <- data.frame(f_no, f_taille, f_dist_mue, f_no_male, f_taille_male, f_dist_mue_male)
	# male <- data.frame(m_no, m_taille, m_dist_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)

	# graphs :
	
	# suivi des individus :
	# windows()
	# plot(nb_couples_moy, type="l", lwd=3, xlim=c(0,tps_max), ylim=c(0,max(nf,nm)), xlab='Jours', ylab="Effectifs", col="red", main='Suivi des femelles')
	
	# homogamie :
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : r�plicat", iteration), asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appari�s")
	abline(h=0)
	
	windows()
	hist(f_taille)
	
	windows()
	hist(m_taille)
	
	windows()
	plot(pearson_tp~taille_pop_totale, type ="l", ylim=c(0,1), xlim=c(min(taille_pop_totale),max(taille_pop_totale)), ylab="Coefficient de Pearson", xlab="Taille de la population", 
			main="Valeurs d'homogamie � t = 1000 en fonction de la taille de la population",
			sub=paste("tx_mortalit� = ",tx_mort," ; tx_croissance = ",tx_croissance," ; sex-ratio = ",sex_ratio," ; r�plicats = ",repetition ))
			
			
# data_sr1 <- read.table("pearson_tm_ 1 .csv")
# data_sr2 <- read.table("pearson_tm_ 2 .csv")
# data_sr3 <- read.table("pearson_tm_ 3 .csv")
# windows()
# plot(pearson_tm ~ tx_mort, data = data_sr1,
		# type = "l", col = "red", lwd = 3, 
		# ylim = c(-0.1, 1),
		# xlab = "Taux de mortalit�",
		# ylab = "coeff homogamie",
		# main = "Valeurs d'homogamie � t = 1000 en fonction du taux de mortalit�",
		# sub=paste("effectif pop = ",taille_pop_totale," ; tx_croissance = ",tx_croissance," ; sex-ratio = ",0.4," ; r�plicats = ",repetition))
# lines(IC95_min_tm ~ tx_mort, data = data_sr1, col = "black", lwd = 1, lty = 3)
# lines(IC95_max_tm ~ tx_mort, data = data_sr1, col = "black", lwd = 1, lty = 3)
		
# windows()
# plot(pearson_tm ~ tx_mort, data = data_sr2,
		# type = "l", col = "red", lwd = 3, 
		# ylim = c(-0.1, 1),
		# xlab = "Taux de mortalit�",
		# ylab = "coeff homogamie",
		# main = "Valeurs d'homogamie � t = 1000 en fonction du taux de mortalit�",
		# sub=paste("effectif pop = ",taille_pop_totale," ; tx_croissance = ",tx_croissance," ; sex-ratio = ",0.5," ; r�plicats = ",repetition ))
# lines(IC95_min_tm ~ tx_mort, data = data_sr2, col = "black", lwd = 1, lty = 3)
# lines(IC95_max_tm ~ tx_mort, data = data_sr2, col = "black", lwd = 1, lty = 3)
		
# windows()
# plot(pearson_tm ~ tx_mort, data = data_sr3,
		# type = "l", col = "red", lwd = 3, 
		# ylim = c(-0.1, 1),
		# xlab = "Taux de mortalit�",
		# ylab = "coeff homogamie",
		# main = "Valeurs d'homogamie � t = 1000 en fonction du taux de mortalit�",
		# sub=paste("effectif pop = ",taille_pop_totale," ; tx_croissance = ",tx_croissance," ; sex-ratio = ",0.6," ; r�plicats = ",repetition ))
# lines(IC95_min_tm ~ tx_mort, data = data_sr3, col = "black", lwd = 1, lty = 3)
# lines(IC95_max_tm ~ tx_mort, data = data_sr3, col = "black", lwd = 1, lty = 3)


	
fin <- Sys.time()
warnings()
fin - debut