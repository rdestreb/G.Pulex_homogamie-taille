rm(list=ls())

debut <- Sys.time()

tps_max <- 1200
temps <- numeric(tps_max)
nb_ind_en_mue <- numeric(tps_max)
nb_fem_libre <- numeric(tps_max)
nb_mal_dispo <- numeric(tps_max)
nb_couples <- numeric(tps_max)
nb_repro <- numeric(tps_max)
coeff_pearson <- numeric(tps_max)
nm <- 550
nf <- 550
txmort_male <- 0.005
txmort_femelle <- 0.005
instant <- 200
screeninstant <- 1

# pour le suivi du male n°1, alias Georges :
nb_mort_georges <- 0
nb_femelle_georges <- 0
no_femelle_georges <- 0    # femelle avec qui Georges s'est apparié, pas forcément accouplé
nb_repro_georges <- 0
mate_georges <- 0    # mate = femelle avec qui georges s'est accouplé
nrj_georges <- numeric(tps_max)

#pour le suivi de la femelle n°1, alias Georgette :
nb_mort_georgette <- 0
nb_male_georgette <- 0
no_male_georgette <- 0   # males avec qui Georgette s'est appariée, pas forcément accouplée
nb_repro_georgette <- 0
mate_georgette <- 0    # mate = mâle avec qui Georgette s'est accouplée
nrj_georgette <- numeric(tps_max)

split.screen(c(2,3))
	
###################
#TRAME DISTRIBUTION
###################

# Femelles :

no <- c(1:nf)
taille <- rnorm(nf,2,0.2)
nrj <- 30
max_dist_mue <- ceiling((6.75+14.83*taille))
dist_mue <- ceiling(runif(nf, min=0, max=max_dist_mue))
no_male <- numeric(nf)
date_receptivite <- floor(max_dist_mue/2)
age <- 0
nb_mue <- 0

taille_male <- numeric(nf)
dist_mue_male <- numeric(nf)
femelle <- data.frame(no, taille, nrj, dist_mue, date_receptivite, age, nb_mue, no_male, taille_male, dist_mue_male)

	# windows()
	# hist(femelle$dist_mue[femelle$dist_mue <= femelle$date_receptivite])

# Males :

no <- c(1:nm)
taille <- rnorm(nm,2.75,0.2)
nrj <- ceiling(30 + 10 * taille)
max_dist_mue <- (6.75+14.83*taille)  
dist_mue <- ceiling(runif(nm, min=0, max=max_dist_mue))  
age <- 0
nb_mue <- 0
no_femelle <- numeric(nm)
taille_femelle <- numeric(nm)
dist_mue_femelle <- numeric(nm)
male <- data.frame(no, taille, nrj, dist_mue, age, nb_mue, no_femelle, taille_femelle, dist_mue_femelle)


plot(f_dist_mue~f_taille)

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
	
  # Georges est-il mort aujourd'hui ?
	if (nb_mort_georges == 0 & min(no_male_mort) == 1){
	tps_vie_georges <- male$age[1]
	nb_mort_georges <- nb_mort_georges + 1
	taille_georges <- male$taille[1]
	nb_mue_georges <- male$nb_mue[1]
  # si Georges ne meurt pas d'ici la fin :
	}else if (nb_mort_georges == 0){
	tps_vie_georges <- male$age[1] 
	taille_georges <- male$taille[1]
	nb_mue_georges <- male$nb_mue[1]
	}
  
  # Georgette est-elle morte aujourd'hui ?
	if (nb_mort_georgette == 0 & min(no_femelle_mort) == 1){
		tps_vie_georgette <- femelle$age[1]
		nb_mort_georgette <- nb_mort_georgette + 1
		taille_georgette <- femelle$taille[1]
		nb_mue_georgette <- femelle$nb_mue[1]
  # si Georgette ne meurt pas d'ici la fin :
	}else if (nb_mort_georgette == 0){
		tps_vie_georgette <- femelle$age[1]
		taille_georgette <- femelle$taille[1]
		nb_mue_georgette <- femelle$nb_mue[1]
	}
  
  # les individus appariés aux morts se retrouvent seuls :
	veuf <- femelle$no_male[no_femelle_mort]
	veuve <- male$no_femelle[no_male_mort]
	male[veuf, 6:8] <- 0
	femelle[veuve, 7:9] <- 0
	
  # suppression des individus malchanceux :
    male[no_male_mort, 2:8] <- 0 
    femelle[no_femelle_mort, 2:9] <- 0
	
	

  ### Naissances :
  # pour chaque mort, un individu nait :
  
    femelle$taille[no_femelle_mort] <- rnorm(nb_femelle_mort,2,0.2)
	femelle$nrj[no_femelle_mort] <- 30
    max_dist_mue <- ceiling(6.75 + 14.83 * femelle$taille[no_femelle_mort])
    femelle$dist_mue[no_femelle_mort] <- max_dist_mue
    femelle$date_receptivite[no_femelle_mort] <- floor(femelle$dist_mue[no_femelle_mort]/2)
    
    male$taille[no_male_mort] <- rnorm(nb_male_mort,2.75,0.2)
	male$nrj[no_male_mort] <- 30 + 10 * male$taille[no_male_mort]
    max_dist_mue <- ceiling(6.75 + 14.83 * male$taille[no_male_mort])
    male$dist_mue[no_male_mort] <- max_dist_mue
  
    # tous les individus vieillissent d'un jour :
	femelle$age <- femelle$age + 1
	male$age <- male$age + 1	
    femelle$dist_mue <- femelle$dist_mue -1
    femelle$dist_mue_male[femelle$dist_mue_male != 0] <- femelle$dist_mue_male[femelle$dist_mue_male != 0] -1
    male$dist_mue <- male$dist_mue - 1
    male$dist_mue_femelle[male$dist_mue_femelle != 0] <- male$dist_mue_femelle[male$dist_mue_femelle != 0] -1
	
	# gain d'une unité d'energie par jour :
	male$nrj <- male$nrj + 1
    
    # y en a-t-il qui mue aujourd'hui (femelle et male) ?
    nbmuetoday <- length(femelle$dist_mue[femelle$dist_mue == 0]) + length(male$dist_mue[male$dist_mue == 0])

   
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
		nbreprotoday <- length (male_apparie[male_apparie != 0])
		
		
		# si Georgette mue et était appariée, on lui compte 1 reproduction de plus :
		if (min(femelle_en_mue) == 1 & femelle$no_male[1] != 0 & nb_mort_georgette == 0){
			nb_repro_georgette <- nb_repro_georgette + 1
			mate_georgette <- c(mate_georgette, femelle$no_male[1])
		}
		#idem pour Georges :
		if (min(male_apparie) == 1 & male$no_femelle[1] != 0 & nb_mort_georges == 0) {
			nb_repro_georges <- nb_repro_georges + 1
			mate_georges <- c(mate_georges, male$no_femelle[1])
		}
		
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
    
		# males en gardiennage perdent de l'energie (-1/jour):
		male$nrj[male$no_femelle != 0] <- male$nrj[male$no_femelle != 0] - 2
		
	}

		############
		#APPARIEMENT
		############
    

		# combien de femelles sont libres & receptives aujourd'hui ?
		nblibretoday <- length(femelle$no_male[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite])
		# combien de males sont célibataires ?
		nbcelibataire <- length(male$no[male$no_femelle == 0])
		
		# s'il y a des individus libres, on lance l'appariement
		if (nblibretoday > 0 & nbcelibataire > 0){
			# pour chaque femelle (ligne = compteur de ligne) :
			for (ligne in femelle$no[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite]) {
		
				# quels mâles sont disponibles (i.e. célibataires, qui muent APRES la femelle et ont assez d'energie) ?
				disponible <- male$no[male$no_femelle == 0 & (male$dist_mue > femelle$dist_mue[ligne] & male$nrj > femelle$dist_mue[ligne])]

				# si la femelle est disponible (i.e. libre + receptive + mature) :
				# if (femelle$no_male[ligne] == 0 & femelle$dist_mue[ligne] <= femelle$date_receptivite[ligne]) {
				# /!\ pour eviter le "bug du sample" :
                if (length(disponible) == 1) {
                    femelle$no_male[ligne] <- disponible
                    male$no_femelle[disponible] <- femelle$no[ligne]
                } else if (length(disponible) > 1) {
                  # choix aléatoire du male :
                    no_male_selec <- sample(disponible,1)
					
                    femelle$no_male[ligne] <- no_male_selec
                    male$no_femelle[no_male_selec] <- femelle$no[ligne]
                }
                femelle$taille_male[ligne] <- male$taille[no_male_selec]
                femelle$dist_mue_male[ligne] <- male$dist_mue[no_male_selec]
                
                male$taille_femelle[no_male_selec] <- femelle$taille[ligne]
                male$dist_mue_femelle[no_male_selec] <- femelle$dist_mue[ligne]
				
				# si Georgette est disponible et a choisi un mâle, on enregistre ce dernier :
				if (ligne == 1 & nb_mort_georgette == 0 & no_male_selec != 0){
					nb_male_georgette <- nb_male_georgette + 1
					no_male_georgette <- c(no_male_georgette, no_male_selec)
				}
				# si Georges est l'heureux élu, on enregistre la femelle qui l'a choisi :
				if (no_male_selec == 1 & nb_mort_georges == 0) {
					nb_femelle_georges <- nb_femelle_georges + 1
					no_femelle_georges <- c(no_femelle_georges, male$no_femelle[1])
				}		
           }
        }
    

# table(femelle$no_male)
# table(male$no_femelle)




	
	coeff <- cor.test(femelle$taille_male[femelle$no_male != 0], femelle$taille[femelle$no_male != 0], method = "pearson")$estimate
    nb_ind_en_mue[tps] <- nbmuetoday
    nb_fem_libre[tps] <- nblibretoday
	nb_mal_dispo[tps] <- length(disponible)
	nb_couples[tps] <- length(femelle$no[femelle$no_male != 0])
	nb_repro[tps] <- nbreprotoday
	coeff_pearson[tps] <- coeff
	temps[tps] <- tps
	
	if (nb_mort_georges == 0) {
	nrj_georges[tps] <- male$nrj[1]
	}
	if (nb_mort_georgette == 0){
	nrj_georgette[tps] <- femelle$nrj[1]
	}
	
	
	if (tps == instant) {
		screen(screeninstant)
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = paste("Homogamie taille : jour", instant), asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		instant <- instant + 200
		screeninstant <- screeninstant + 1	
	}
}

femelle
male
nb_ind_en_mue
nb_fem_libre
nb_mal_dispo 
nb_couples
nb_repro
coeff_pearson
temps


# la vie de Georges :
tps_vie_georges
taille_georges 
nb_mue_georges 
nb_femelle_georges
no_femelle_georges
nb_repro_georges
mate_georges
nrj_georges

# la vie de Georgette :
tps_vie_georgette
taille_georgette
nb_mue_georgette
nb_male_georgette
no_male_georgette
nb_repro_georgette
mate_georgette
nrj_georgette


windows()
split.screen(c(1,2))
screen(1)
hist(femelle$taille, xlab= 'Taille', main = 'Distribution taille femelles', breaks = 40)
screen(2)
hist(male$taille, xlab= 'Taille', main = 'Distribution taille males', breaks = 40)

windows()
plot(nb_couples, type="l", xlab='Jours', main='Nombre de couples par jour')

windows()
barplot(nb_repro, xlab='Jours', main='Nombre de reproductions par jour')

windows()
plot(coeff_pearson, type="l", xlab="jours", main="Coefficient de correlation de Pearson entre taille des individus appariés")

windows()
plot(nrj_georges, type="l", xlab="jours", main="budget énergétique de Georges")

windows()
plot(nrj_georgette, type="l", xlab="jours", main="budget énergétique de Georgette")

fin <- Sys.time()
warnings()
fin - debut
