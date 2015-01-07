rm(list=ls())

debut <- Sys.time()

tps_max <- 300
nb_mue <- numeric(tps_max)
nb_libre <- numeric(tps_max)
nm <- 150
nf <- 100
txmort_male <- 0.01
txmort_femelle <- 0.01
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
femelle <- data.frame(no, taille, dist_mue, date_receptivite, no_male, taille_male, dist_mue_male)


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
male <- data.frame(no, taille, dist_mue, no_femelle, taille_femelle, dist_mue_femelle)


###########################
# VIEILLISSEMENT POPULATION
###########################
  
for (tps in 1:tps_max) {
  
    # tous les individus vieillissent d'un jour
    femelle$dist_mue <- femelle$dist_mue -1
    femelle$dist_mue_male[femelle$dist_mue_male != 0] <- femelle$dist_mue_male[femelle$dist_mue_male != 0] -1
    male$dist_mue <- male$dist_mue -1
    male$dist_mue_femelle[male$dist_mue_femelle != 0] <- male$dist_mue_femelle[male$dist_mue_femelle != 0] -1
    
    # y en a-t-il qui mue aujourd'hui (femelle et male) ?
    nbmuetoday <- length(femelle$dist_mue[femelle$dist_mue == 0]) + length(male$dist_mue[male$dist_mue == 0])

        
# si un des membres de la paire mue, le couple se sépare : 
    if (nbmuetoday > 0) {
        femelle_en_mue <- femelle$no[femelle$dist_mue == 0]
        #males appariés avec une femelle qui mue aujourd'hui :
        male_apparie <- femelle$no_male[femelle_en_mue]  
        
        male_en_mue <- male$no[male$dist_mue == 0]
        femelle_apparie <- male$no_femelle[male_en_mue]
        
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
        for (ligne in 1 : nf) {
		
          # quels mâles sont célibataires ? (i.e. prêts à être appariés) ?
            celibataire <- male$no[male$no_femelle == 0]
            
           # si la femelle est disponible (i.e. libre + receptive + mature) :
			if (femelle$no_male[ligne] == 0 & femelle$dist_mue[ligne] <= femelle$date_receptivite[ligne]) {
           # /!\ pour eviter le "bug du sample" :
                if (length(celibataire) == 1) {
                    femelle$no_male[ligne] <- male$no[celibataire]
                    male$no_femelle[celibataire] <- femelle$no[ligne]
                } else if (length(celibataire) > 1) {
                  # choix aléatoire du male :
                    no_male_selec <- sample(celibataire,1)
                    femelle$no_male[ligne] <- no_male_selec
                    male$no_femelle[no_male_selec] <- femelle$no[ligne]
                }
                femelle$taille_male[ligne] <- male$taille[no_male_selec]
                femelle$dist_mue_male[ligne] <- male$dist_mue[no_male_selec]
                
                male$taille_femelle[no_male_selec] <- femelle$taille[ligne]
                male$dist_mue_femelle[no_male_selec] <- femelle$dist_mue[ligne]
            }
        }
    }

# table(femelle$no_male)
# table(male$no_femelle)


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
	male[veuf, 4:6] <- 0
	femelle[veuve, 5:7] <- 0
	
  # suppression des individus malchanceux :
    male[no_male_mort, 2:6] <- 0 
    femelle[no_femelle_mort, 2:7] <- 0

	

### Naissances :
  # pour chaque mort, un individu nait :
  
    femelle$taille[no_femelle_mort] <- rnorm(nb_femelle_mort,1.6,0.1)
    max_dist_mue <- ceiling(6.75 + 14.83 * femelle$taille[no_femelle_mort])
    femelle$dist_mue[no_femelle_mort] <- max_dist_mue
    femelle$date_receptivite[no_femelle_mort] <- floor(femelle$dist_mue[no_femelle_mort]/2)
    
    male$taille[no_male_mort] <- rnorm(nb_male_mort,1.8,0.1)
    max_dist_mue <- ceiling(6.75 + 14.83 * male$taille[no_male_mort])
    male$dist_mue[no_male_mort] <- max_dist_mue

    
    femelle
    male
    
    nb_mue[tps] <- nbmuetoday
    nb_libre[tps] <- nblibretoday
	

	
	if (tps == 50) {
	
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 50", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		screen(2)
	}else if (tps == 100) {
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 100", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		screen(3)
	}else if (tps == 150) {
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 150", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)	
		screen(4)
	}else if (tps == 200) {
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 200", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		screen(5)
	}else if (tps == 250) {
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 250", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		screen(6)
	}else if (tps == 300) {
		plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : jour 300", asp=1)
		abline(0,1, lty=3)
		reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
		abline(reg)
		screen(7)
	}
}
femelle
male
nb_mue
nb_libre
fin <- Sys.time()


# plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : 500M/600F", asp=1)
# abline(0,1, lty=3)
# reg <- lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0]))
# abline(reg)
# summary(reg)



warnings()
fin - debut
