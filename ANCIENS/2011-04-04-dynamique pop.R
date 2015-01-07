#dynamique de la pop : apparition/disparition d'individus
#/!\ en réalité : "naissance" = nouveaux individus apparaissant dans la pop, d'où dist à mue != max

#taux de natalité /tps ; 0 < txnat < 1  (fixé ou déterminé aléatoirement)
txnat=0
#taux de mortalité /tps ; 0 < txmor < 1  (fixé ou déterminé aléatoirement)
txmor=0
#temps en jour
tps=0 
nais=0
mort=0
  
#Pour femelles :

dynapopf<-function (tps,txnat,txmor) {
  for (x in seq(1,tps,1)) {
    #nombre de lignes à supprimer dans tableau distribution
      mort<-floor(nf*txmor)
    #sélection aléatoire des lignes à supprimer
      l<-sample(nf,mort)
      femelle[l,]<-0
    #nombre d'individus "nés" (nb de lignes nouvelles à générer)
      nais<-floor(nf*txnat)  

      indiv<-c(femelle$indiv,(nf+1):(nf+nais))
      taille<-c(femelle$taille,rnorm(nais,2,0.2))      
      maxdist_mue<-c(maxdist_mue,6.75+14.83*taille[(nf+1):(nf+nais)])
      dist_mue<-c(femelle$dist_mue,ceiling(runif(nais,min=0,max=maxdist_mue)))
      map<-c(femelle$map,numeric(nais))
    #tableau incluant les nouveaux individus :
      femelle<-data.frame(indiv,taille,dist_mue,map) 
  }
  return(femelle)
}

#Pour males :

dynapopm<-function (tps,txnat,txmor) {
  for (x in seq(1,tps,1)) {
      nais<-floor(nm*txnat)   
      mort<-floor(nm*txmor)
      
      l<-sample(nm,mort)
      male[l,]<-0

      indiv<-c(male$indiv,(nm+1):(nm+nais))
      taille<-c(male$taille,rnorm(nais,2.75,0.2))
      maxdist_mue<-c(maxdist_mue,6.75+14.83*taille[(nm+1):(nm+nais)])
      dist_mue<-c(male$dist_mue,ceiling(runif(nais,min=0,max=maxdist_mue)))
      fap<-c(male$fap,numeric(nais))
      male<-data.frame(indiv,taille,dist_mue,fap)  
  }
    return(male)
}



