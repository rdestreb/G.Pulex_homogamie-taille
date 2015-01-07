rm(list=ls())

###################
#DISTRIBUTION MALES
###################

nm<-100

no_male<-c(1:nm)
taille_male<-rnorm(nm,2.75,0.2)
max_dist_mue_male<-(6.75+14.83*taille_male)  
dist_mue_male<-ceiling(runif(nm,min=0,max=max_dist_mue_male))  
no_femelle_appariee<-numeric(nm)
male<-data.frame(no_male,taille_male,dist_mue_male,no_femelle_appariee)

# plot(max_dist_mue_male~taille_male,data=male)
# plot(dist_mue_male~taille_male)
# points(max_dist_mue_male~taille_male,data=male,col="red",pch=19)

######################
#DISTRIBUTION FEMELLES
######################

nf<-50

no_femelle<-c(1:nf)
taille_femelle<-rnorm(nf,2,0.2)
max_dist_mue_femelle<-(6.75+14.83*taille_femelle)
dist_mue_femelle<-ceiling(runif(nf,min=0,max=max_dist_mue_femelle))
no_male_apparie<-numeric(nf)
femelle<-data.frame(no_femelle,taille_femelle,dist_mue_femelle,no_male_apparie)

#  plot(dist_mue_femelle~taille_femelle)
#  points(max_dist_mue_femelle~taille_femelle,data=femelle,col="red",pch=19)

############
#APPARIEMENT
############

i<-1

if (nm >= nf){
    for (i in 1:nf) {
        no_male_selec<-sample(male$indiv[male$fap==0],1)
        femelle$map[i]<-no_male_selec
        male$fap[male$indiv==no_male_selec]<-femelle$indiv[i]
    }
}else{
    for (i in 1:nm){
        no_femelle_selec<-sample(femelle$indiv[femelle$map==0],1)
        male$fap[i]<-no_femelle_selec
        femelle$map[femelle$indiv==no_femelle_selec]<-male$indiv[i]
    }
}


femelle
male
#table(femelle$map)
#table(male$fap)


##########
#HOMOGAMIE
##########


paire<-c(1:min(nf,nm))
nfem<-femelle$indiv[femelle$map!=0]
nmal<-femelle$map[femelle$map!=0]
taille_fem<-femelle$taille[femelle$map!=0]
taille_mal<-male$taille[nmal]

homogamie<-data.frame(paire,nfem,nmal,taille_fem,taille_mal)
homogamie
plot(taille_mal~taille_fem)

# x<-femelle[1,]
# y<-femelle[8,]
# femelle[8,]<-x
# femelle[1,]<-y

#######################
#EVOLUTION DISTRIBUTION
#######################

#Cf fonctions dynamue, dynapop
# 
dynamuef(3)
# dynamuem(6)
# 
# dynapopf(3,0.8,0.2)
# dynapopm(1,0.8,0.2)


