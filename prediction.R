# Prediction en "nahead" heures en avant (parex: nahead=1 prévision heure par heure, nahead=24 prévision jour par jour, nahead=24*7 prévision sem par sem)
# Input: 
#   Données d'apprentissage (donapp)
#   Données de test (dontest)
#   Model (model)
#   nahead
# Output: Le vecteur de prédiction
# Forme du modèle: Variable réponse ~ Les covariables + termes autorégressifs (lag1,lag2,lag3,lag4,lag5,lag24,lag48,lag72,lag96,lag120,lag144,lag168)
prediction = function(donapp,dontest,model,nahead){
  napp = length(donapp$Zone10)
  ntest = length(dontest$Zone10)
  ntot = napp+ntest
  j=0
  pred = c()
  lags = c(1,2,3,4,5,24,48,72,96,120,144,168)
  while ((napp+nahead)<=ntot){
    j=j+1
    p = numeric(nahead)
    for (i in 1:nahead){
      indice = nahead*(j-1)+i
      # Par exemple: on prédit à 24 heures, alors lag1 à lag24 est pris dans l'ensemble prédit, lag48... lag168 est pris dans l'ensemble des vraies données.
      for (k in lags) assign(paste("lag", k, sep = ""), if (i<=k) donapp$Zone10[napp+i-k] else p[i-k]);
      
      # Créer un data.frame de 1 élément composant des covariables et des termes autorégressifs pour prédire
      dt=
        data.frame(dontest[indice,],lag1,lag2,lag3,lag4,lag5,lag24,lag48,lag72,lag96,lag120,lag144,lag168);
      p[i] = predict(model,dt)
    }
    donapp = rbind(donapp,dontest[(nahead*(j-1)+1):(nahead*j),])
    napp = napp + nahead
    pred = c(pred,p)
  }
  # Prédire des éléments restants. Par exemple: on a 100 éléments et prédire de jour par jour. 96 premiers elements sont prédits par la boucle précédente, 4 autres sont prédits par cette boucle
  if (napp+nahead>ntot){
    j = j+1
    p = numeric(ntot-napp)
    for (i in 1:(ntot-napp)){
      indice = nahead*(j-1)+i
      for (k in lags) assign(paste("lag", k, sep = ""), if (i<=k) donapp$Zone10[napp+i-k] else p[i-k]);
      dt=data.frame(dontest[indice,],lag1,lag2,lag3,lag4,lag5,lag24,lag48,lag72,lag96,lag120,lag144,lag168);
      p[i] = predict(model,dt)
    }
    pred = c(pred,p)
  }
  return(pred)
}
