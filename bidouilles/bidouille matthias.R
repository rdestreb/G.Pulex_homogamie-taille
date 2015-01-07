rm(list=ls())
graphics.off()


nf <- 500
nm <- 500

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
	
	
	
	hist(m_dist_mue - f_dist_mue)
	x <- m_dist_mue - f_dist_mue
	
	y <- length(x[x < 0])/500
	
	y
	