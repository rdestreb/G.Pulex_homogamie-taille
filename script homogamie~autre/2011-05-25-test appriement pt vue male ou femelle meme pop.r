rm(list=ls())
graphics.off()

simulation <- 10000
pearson_femelle <- numeric(simulation)
pearson_male <- numeric(simulation)
################################
##### CREATION POPULATION ######
################################

nf <- 500
nm <- 500

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
m_dist_mue <- round((runif(nm, min=0, max=f_max_dist_mue)),2)
m_no_femelle <- numeric(nm)
m_taille_femelle <- numeric(nm)
m_dist_mue_femelle <- numeric(nm)

for (no_simu in 1:simulation) {

	#######################################
	###	APPARIEMENT	POINT VUE FEMELLE	###
	#######################################

	f_no <- sample(f_no,nf)

	for (ligne in f_no){
		
		no_male_dispo <- m_no[(m_no_femelle == 0) & (m_dist_mue >= f_dist_mue[ligne])]
		nb_male_dispo <-length(no_male_dispo)
		
		if( nb_male_dispo != 0) {
		
			if (nb_male_dispo > 1) {
				no_male_selec <- sample(no_male_dispo,1)
			} else if (nb_male_dispo == 1) {
				no_male_selec <- no_male_dispo
			}
			
			f_no_male[ligne] <- no_male_selec
			m_no_femelle[no_male_selec] <- f_no[ligne]
			
			f_taille_male[ligne] <- m_taille[no_male_selec]
			m_taille_femelle[no_male_selec] <- f_taille[ligne]
			
			f_dist_mue_male[ligne] <- m_dist_mue[no_male_selec]
			m_dist_mue_femelle[no_male_selec] <- f_dist_mue[ligne]
		}	
	}
	
	
	femelle <- data.frame(f_no,f_taille,f_dist_mue,f_no_male,f_taille_male,f_dist_mue_male)
	male <- data.frame(m_no,m_taille,m_dist_mue,m_no_femelle,m_taille_femelle,m_dist_mue_femelle)

	test_pearson <- cor.test(f_taille_male[f_no_male != 0], f_taille[f_no_male != 0], method = "pearson")
	pearson_femelle[no_simu] <- test_pearson$estimate
	
	
	f_no_male <- numeric(nf)
	f_taille_male <- numeric(nf)
	f_dist_mue_male <- numeric(nf)
	
	m_no_femelle <- numeric(nm)
	m_taille_femelle <- numeric(nm)
	m_dist_mue_femelle <- numeric(nm)
	
	
	
	
	#######################################
	###	APPARIEMENT POINT DE VUE MALE	###
	#######################################
	
	m_no <- sample(m_no,nm)

	for (ligne in m_no){
		
		no_femelle_dispo <- f_no[(f_no_male == 0) & ( m_dist_mue[ligne] >= f_dist_mue )]
		nb_femelle_dispo <-length(no_femelle_dispo)
		
		if( nb_femelle_dispo != 0) {
		
			if (nb_femelle_dispo > 1) {
				no_femelle_selec <- sample(no_femelle_dispo,1)
			} else if (nb_femelle_dispo == 1) {
				no_femelle_selec <- no_femelle_dispo
			}
			
			m_no_femelle[ligne] <- no_femelle_selec
			f_no_male[no_femelle_selec] <- m_no[ligne]
			
			m_taille_femelle[ligne] <- f_taille[no_femelle_selec]
			f_taille_male[no_femelle_selec] <- m_taille[ligne]
			
			m_dist_mue_femelle[ligne] <- f_dist_mue[no_femelle_selec]
			f_dist_mue_male[no_femelle_selec] <- m_dist_mue[ligne]
		}	
	}
	
	
	femelle <- data.frame(f_no,f_taille,f_dist_mue,f_no_male,f_taille_male,f_dist_mue_male)
	male <- data.frame(m_no,m_taille,m_dist_mue,m_no_femelle,m_taille_femelle,m_dist_mue_femelle)

	test_pearson <- cor.test(m_taille_femelle[m_no_femelle != 0], m_taille[m_no_femelle != 0], method = "pearson")
	pearson_male[no_simu] <- test_pearson$estimate
	
	
	f_no_male <- numeric(nf)
	f_taille_male <- numeric(nf)
	f_dist_mue_male <- numeric(nf)
	
	m_no_femelle <- numeric(nm)
	m_taille_femelle <- numeric(nm)
	m_dist_mue_femelle <- numeric(nm)
	
	
}
windows()
plot(pearson_femelle, type="l", ylim = c(-0.2,1))
windows()
plot(pearson_male, type="l", ylim = c(-0.2,1))

tableau <- data.frame(pearson_femelle,pearson_male)
windows()
boxplot(tableau)