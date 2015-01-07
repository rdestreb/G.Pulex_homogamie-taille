rm(list=ls())

tps_max <- 300
nb_mue <- numeric(tps_max)
nb_libre <- numeric(tps_max)
nm <- 300
nf <- 100
txmort_male <- 0.2
txmort_femelle <- 0.2

###################
#TRAME DISTRIBUTION
###################

# Femelles :

no <- c(1:nf)
taille <- rnorm(nf,2,0.2)
max_dist_mue <- ceiling((6.75+14.83*taille))
dist_mue <- ceiling(runif(nf, min=0, max=max_dist_mue))
no_male <- numeric(nf)
date_receptivite <- ceiling(max_dist_mue/2)

taille_male <- numeric(nf)
dist_mue_male <- numeric(nf)
femelle <- data.frame(no, taille, dist_mue, date_receptivite, no_male, taille_male, dist_mue_male)


# Males :

no <- c(1:nm)
taille <- rnorm(nm,2.75,0.2)
max_dist_mue <- (6.75+14.83*taille)  
dist_mue <- ceiling(runif(nm, min=0, max=max_dist_mue))  
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
    femelle$dist_mue_male[femelle$no_male != 0] <- femelle$dist_mue_male[femelle$no_male != 0] -1
    male$dist_mue <- male$dist_mue -1
    male$dist_mue_femelle[male$no_femelle != 0] <- male$dist_mue_femelle[male$no_femelle != 0] -1
    
    # y en a-t-il qui mue aujourd'hui (femelle et male) ?
    nbmuetoday <- length(femelle$dist_mue[femelle$dist_mue == 0]) + length(male$dist_mue[male$dist_mue == 0])
#     nbmuetoday_male <- length(male$dist_mue[male$dist_mue == 0])
        
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
        femelle$date_receptivite[femelle_en_mue] <- ceiling(femelle$dist_mue[femelle_en_mue]/2)
      
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
      
      # quelles femelles sont libres & receptives ?
        no_femelle_libre <- femelle$no[femelle$no_male == 0 & femelle$dist_mue <= femelle$date_receptivite]
      # quels mâles sont célibataires ? (i.e. prêts à être appariés) ?
        celibataire <- male$no[male$no_femelle == 0]
        
		  # quelles femelles vont s'apparier ?
  			if (nblibretoday == 1) {
  				chanceuse <- no_femelle_libre
  			}else{
          # les femelles qui auront la chance de s'apparier sont déterminées aléatoirement
          # on sélectionne autant de femelles qu'il y a de males ou femelles disponibles, en fonction de qui est limitant
  				chanceuse <- sample(no_femelle_libre,min(nbcelibataire,nblibretoday))
  			}
  				
  		  # quels mâles seront sélectionnés ?
  			if (nbcelibataire == 1) {
  				no_male_selec <- celibataire 
  			}else{
          #idem que pour les femelles : sélection aléatoire d'un nombre de males fonction du sexe disponible limitant
  				no_male_selec <- sample(celibataire, min(nbcelibataire,nblibretoday))
  			}

            femelle$no_male[chanceuse] <- no_male_selec
            femelle$taille_male[chanceuse] <- male$taille[no_male_selec]
            femelle$dist_mue_male[chanceuse] <- male$dist_mue[no_male_selec]
            
            male$no_femelle[no_male_selec] <- chanceuse
            male$taille_femelle[no_male_selec] <- femelle$taille[chanceuse]
            male$dist_mue_femelle[no_male_selec] <- femelle$dist_mue[chanceuse]          
    }
    
# table(femelle$no_male)
# table(male$no_femelle)


    ######################
    # MORTALITE/NAISSANCES
    ######################

    # Mortalité :
    
    # combien de males et de femelle vont mourir aujourd'hui ?
#     nb_male_mort <- floor(nm*txmort_male)
#     nb_femelle_mort <- floor(nf*txmort_femelle)
#     
#     # la mort s'abat aléatoirement sur les individus :
#     no_male_mort <- sample(male$no, nb_male_mort)
#     no_femelle_mort <- sample(femelle$no, nb_femelle_mort)
#     
#     # suppression des individus malchanceux :
#     male[no_male_mort,] <- 0 
#     femelle[no_femelle_mort,] <- 0
#     
#     # Naissances :
#     
#     # combien d'individus vont naitre aujourd'hui ?
#     nb_naissance <- nb_male_mort + nb_femelle_mort  # se compense ici à mortalité
#     
#     # combien de males et de femelles vont naitre aujourd'hui ?
#     sex_ratio <- nf / (nf + nm)
#     nb_male_ne <- floor(nb_naissance * (1 - sex_ratio))
#     nb_femelle_ne <- floor(nb_naissance * sex_ratio)
#     
#     
#     # génération des nouveaux individus :
#     no <- c(femelle$no, (nf + 1) : (nf + nb_femelle_ne))
#     taille <- c(femelle$taille, rnorm(nb_femelle_ne, 2, 0.2))
#     max_dist_mue <- ceiling(6.75 + 14.83 * taille[(nf + 1) : (nf + nb_femelle_ne)])
#     dist_mue <- c(femelle$dist_mue, max_dist_mue)
#     date_receptivite <- c(femelle$date_receptivite, ceiling(max_dist_mue/2))
#     no_male <- c(femelle$no_male, numeric(nb_femelle_ne))
#     taille_male <- c(femelle$taille_male, numeric(nb_femelle_ne))
#     dist_mue_male <- c(femelle$dist_mue_male, numeric(nb_femelle_ne))
#     femelle <- data.frame(no, taille, dist_mue, date_receptivite, no_male, taille_male, dist_mue_male)
#     
#     no <- c(male$no, (nm + 1) : (nm + nb_male_ne))
#     taille <- c(male$taille, rnorm(nb_male_ne, 2.75, 0.2))
#     max_dist_mue <- ceiling(6.75 + 14.83 * taille[(nm + 1) : (nm + nb_male_ne)])
#     dist_mue <- c(male$dist_mue, max_dist_mue)
#     no_femelle <- c(male$no_femelle, numeric(nb_male_ne))
#     taille_femelle <- c(male$taille_femelle, numeric(nb_male_ne))
#     dist_mue_femelle <- c(male$dist_mue_femelle, numeric(nb_male_ne))
#     male <- data.frame(no, taille, dist_mue, no_femelle, taille_femelle, dist_mue_femelle)
    
    
    
    
    
    femelle
    male
    
    nb_mue[tps] <- nbmuetoday
    nb_libre[tps] <- nblibretoday
}

nb_mue
nb_libre
plot(femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0], xlab="taille femelle", ylab="taille male", main = "Homogamie taille : 500M/600F", asp=1)
abline(0,1, lty=3)
abline(lm((femelle$taille_male[femelle$no_male != 0]~femelle$taille[femelle$no_male != 0])))

warnings()