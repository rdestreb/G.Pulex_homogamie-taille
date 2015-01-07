rm(list=ls())
graphics.off()

# Choix des males ? Aléatoire = 0 ; Si mue APRES la femelle = 1 
choix_male <- 1

tx_croissance <- 1.1

txmort_male <- 0.012
txmort_femelle <- 0.012

nm <- 500
nf <- 500

debut <- Sys.time()

tps_max <- 1000
iteration <- 2
cpt <- 0
taille_male <- numeric(tps_max)
males_dispo <- numeric(tps_max)
nb_male_seul <- numeric(tps_max)
nb_couples <- numeric(tps_max)
coeff_pearson <- numeric(tps_max)
temps <- numeric(tps_max)
taille_male_couple_1000 <- numeric(iteration)
taille_male_seul_1000 <- numeric(iteration)
taille_femelle_couple_1000 <- numeric(iteration)
taille_femelle_seule_1000 <- numeric(iteration)

replicat <- numeric(nm*iteration)

for (tour in 1:iteration){

	cpt <- cpt + 1
	
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
			f_taille[femelle_en_mue] <- round(f_taille[femelle_en_mue]*tx_croissance ,3)
			m_taille[male_en_mue] <- round(m_taille[male_en_mue]*tx_croissance ,3)
			# ils sont ensuite au maximum de la distance à leur prochaine mue :
			f_max_dist_mue[femelle_en_mue] <- round((6.75+14.83*f_taille[femelle_en_mue]),2)
			f_dist_mue[femelle_en_mue] <- f_max_dist_mue[femelle_en_mue]
			m_max_dist_mue[male_en_mue] <- round((6.75+14.83*m_taille[male_en_mue]),2)
			m_dist_mue[male_en_mue] <- m_max_dist_mue[male_en_mue]
		}
		
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

		# nb_male_dispo <- length(m_no[m_no_femelle == 0])

		
		# compilation des valeurs (en vue d'un plot) :
		coeff <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")$estimate
		coeff_pearson[tps] <- coeff
		nb_male_seul[tps] <- nb_male_dispo
		nb_couples[tps] <- length(f_no[f_no_male != 0])
		temps[tps] <- tps 
		
		
		taille_male_couple_1000 <- m_taille[m_no_femelle != 0]
		taille_male_seul_1000 <- m_taille[m_no_femelle == 0]

		
		taille_femelle_couple_1000 <- f_taille[f_no_male != 0]
		taille_femelle_seule_1000 <- f_taille[f_no_male == 0]
	}
		
	taille_males <- c(taille_male_seul_1000, taille_male_couple_1000)
	etat_males <- c(rep("Seul",length(taille_male_seul_1000)), rep("En couple",length(taille_male_couple_1000)))
	tableau <- data.frame(taille_males, etat_males)
	write.table(tableau,paste("boxplot mâles",tour,".csv"))
	
	taille_femelles <- c(taille_femelle_seule_1000, taille_femelle_couple_1000)
	etat_femelles <- c(rep("Seule",length(taille_femelle_seule_1000)), rep("En couple",length(taille_femelle_couple_1000)))
	tableau2 <- data.frame(taille_femelles, etat_femelles)
	write.table(tableau2,paste("boxplot femelles",tour,".csv"))
	
}
	# femelle <- data.frame(f_no, f_taille, f_dist_mue, f_no_male, f_taille_male, f_dist_mue_male)
	# male <- data.frame(m_no, m_taille, m_dist_mue, m_no_femelle, m_taille_femelle, m_dist_mue_femelle)
	

	
	
	

	
	# graphs :
	
	# suivi des individus :
	windows()
	plot(nb_couples, type="l", xlim=c(0,tps_max), ylim=c(0,max(nf,nm)), xlab='Jours', ylab="Effectifs", col="red", main='Suivi des femelles')
	
	# homogamie :
	windows()
	plot(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille", asp=1)
	abline(0, 1, lty = 3)
	reg <- lm(f_taille_male[f_no_male != 0]~f_taille[f_no_male != 0])
	abline(reg)
	
	windows()
	plot(coeff_pearson, type="l", ylim=c(-0.2,1), xlab="jours", main="Coefficient de correlation de Pearson entre la taille des individus appariés")
	abline(h=0)
	
	windows()
	hist(f_taille)
	
	windows()
	hist(m_taille)
	
	windows()
	boxplot(tableau$taille_males~tableau$etat_males, ylim=c(min(taille_males)-0.05,max(taille_males)+0.05), ylab="Taille", 
			main=paste("Tailles moyennes des mâles seuls ou en couple"), 
			sub=paste("n = ",nm,"    ;    tx_croissance = ",tx_croissance,"    ;    tx_mortalité = ",txmort_male,"    ;    S-R = ",nm/(nm+nf)))
			
	windows()
	boxplot(tableau2$taille_femelles~tableau2$etat_femelles, ylim=c(min(taille_femelles)-0.05,max(taille_femelles)+0.05), ylab="Taille", 
			main=paste("Tailles moyennes des femelles seules ou en couple"), 
			sub=paste("n = ",nf,"    ;    tx_croissance = ",tx_croissance,"    ;    tx_mortalité = ",txmort_femelle,"    ;    S-R = ",nm/(nm+nf)))
	
	
fin <- Sys.time()
warnings()
fin - debut