rm(list=ls())
# graphics.off()

debut <- Sys.time()

tps_max <- 1000
repetition <- 1
tx_croissance <- 1.1
coeff_pearson_moy <- numeric(tps_max)

for (iteration in 1:repetition){

	nm <-500
	nf <-500	
	coeff_pearson <- numeric(tps_max)


###############################
##### CREATION POPULATION #####
###############################

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
		
		###################################
		######### VIEILLISSEMENT ##########
		###################################
	
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
			f_taille[femelle_en_mue] <- round(f_taille[femelle_en_mue]*tx_croissance,3)
			m_taille[male_en_mue] <- round(m_taille[male_en_mue]*tx_croissance,3)
			# ils sont ensuite au maximum de la distance à leur prochaine mue :
			f_max_dist_mue[femelle_en_mue] <- round((6.75+14.83*f_taille[femelle_en_mue]),2)
			f_dist_mue[femelle_en_mue] <- f_max_dist_mue[femelle_en_mue]
			m_max_dist_mue[male_en_mue] <- round((6.75+14.83*m_taille[male_en_mue]),2)
			m_dist_mue[male_en_mue] <- m_max_dist_mue[male_en_mue]
		}
		
		###################################
		########### APPARIEMENT ###########
		###################################
		
		
		# combien de femelles sont libres aujourd'hui ?
		nb_femelle_celibataire <- length(f_no[f_no_male == 0])
		no_femelle_celibataire <- f_no[f_no_male == 0]
		
		# combien de males sont célibataires ?
		nb_male_celibataire <- length(m_no[m_no_femelle == 0])
		
		# s'il y a des individus libres, on lance l'appariement
		if ((nb_femelle_celibataire > 0) & (nb_male_celibataire > 0)){
		
			# shaker des femelles disponibles (les têtes de liste ne sont plus privilégiées) :
			# /!\ "bug du sample" /!\
		
			if (nb_femelle_celibataire > 1){
				no_femelle_celibataire <- sample(no_femelle_celibataire, nb_femelle_celibataire)
			}
			
			# pour chaque femelle disponible (ligne = compteur de ligne) :
			
			for (ligne in no_femelle_celibataire){
			
				##################
				### CHOIX MALE ###
				##################
				
				# quels mâles sont encore célibataires ?
				no_male_celibataire <- m_no[m_no_femelle == 0]
				
				if (length(no_male_celibataire) != 0){
					# /!\  "bug du sample"  /!\
					if (length(no_male_celibataire) == 1) {
						no_male_selec <- no_male_celibataire
					} else if (length(no_male_celibataire) > 1) {
						# choix aléatoire du male :
						no_male_selec <- sample(no_male_celibataire,1)
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
	}

	# moyennes des valeurs sur tous les réplicats :
	coeff_pearson_moy <- coeff_pearson_moy + coeff_pearson/repetition 
}
	femelle <- data.frame(f_no, f_taille, f_dist_mue, f_no_male, f_taille_male, f_dist_mue_male)
	male <- data.frame(m_no, m_taille, m_dist_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)
	
	############
	## GRAPHS ##
	############
	
	# homogamie (taille des mâles en couple en fonction de la taille des femelles en couple) :
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : réplicat", iteration), asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	abline((mean(m_taille)-mean(f_taille)),1)
	
	# valeur du coeff de Pearson à chaque pas de temps :
	windows()
	plot(coeff_pearson_moy, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	
	# Distribution :
	windows()
	hist(f_taille)
	
	windows()
	hist(m_taille)
	
	mean(f_taille)
	mean(m_taille)	
	sd(f_taille)
	sd(m_taille)
	
fin <- Sys.time()
warnings()
fin - debut