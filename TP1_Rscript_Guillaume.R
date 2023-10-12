wine <- read.csv("wine.csv")
print(xtable(wine[1:5,1:6] , type = "latex", file = "TP1.tex", digits=3,
             caption="Extrait du tableau d'origine"))

X <- wine[4:32]
W <- diag(1/21 , nr=21)
M <- diag(1/29, 29)
Unit <- matrix(data=c(1), nrow=21, ncol=1)

Xc<- sqrt(21/20)*scale(X)
X <- Xc
print(xtable(X[1:4,1:5] , type = "latex", file = "TP1.tex", digits=3,
             caption='Extrait des valeurs centrées réduites'))
#Matrice des indicatrices des appellations
Y <- matrix(0, nrow=21, ncol=3)
for (i in 1:21) {
  if (wine[i,2]=="Bourgueuil") {Y[i,1]<-1};
  if (wine[i, 2]=="Chinon") {Y[i,2]<-1};
  if (wine[i, 2]=="Saumur") {Y[i,3]<-1}
}
#Matrice des indicatrices des sols
Z <- matrix(0, nrow=21, ncol=4)
for (i in 1:21) {
  if (wine[i,3]=="Env1") {Z[i,1]<-1};
  if (wine[i, 3]=="Env2") {Z[i,2]<-1};
  if (wine[i, 3]=="Reference") {Z[i,3]<-1};
  if (wine[i, 3]=="Env4") {Z[i, 4]<-1}
}

#Barycentres des variables sensorielles
Bar <- t(X) %*% W %*% Unit
#print(xtable(t(Bar)[,1:4] , type = "latex", file = "TP1.tex", digits=3,
#             caption ='Barycentre du nuage'))

#Inertie totale du nuage
In = sum(apply(1/21*X^2 ,1, sum))

#Poids et barycentre par appellation 
Poids_app <- t(Y)%*%W%*%Unit
print(xtable(Poids_app , type = "latex", file = "TP1.tex", digits=3,
             caption = 'Poids par appellation'))

Moy_cat= solve(t(Y)%*%W%*%Y)%*%t(Y)%*%W%*%X
#print(xtable(Moy_cat[,1:5] , type = "latex", file = "TP1.tex", digits=3,
#             caption = 'Barycentre des appellations'))

#Calcul normes euclidiennes
Normes<- apply(Moy_cat^2, 1, sum)
print(xtable(data.frame(Normes) , type = "latex", file = "TP1.tex", digits =3,
             caption = 'Normes des barycentres par appellation'))

#Calcul de l'inertie inter-appellations
In_inter <- Normes%*%Poids_app

#Calcul du R^2
R2 <- In_inter/In

#Variance inter-appellation
var_inter <- t(Poids_app)%*%Moy_cat^2
R2var<- var_inter
print(xtable(data.frame(R2var)[,1:5], type ="latex", file="TP1.tex", digits=3,
             caption="Extrait du résultat du $R**2$ par appellation"))


#Variables les moins liées à l'appellation
subset(R2var, select = R2var<0.01)
print(xtable(subset(R2var, select = R2var<0.01), type ="latex", file="TP1.tex", 
             digits=3, caption="Variables les moins liées à l'appellation"))

#Variables les plus liées à l'appellation
subset(R2var, select = R2var>0.33)
print(xtable(subset(R2var, select = R2var>0.33), type ="latex", file="TP1.tex", 
             digits=3, caption="Variables les plus liées à l'appellation"))

#Calcul de la moy arithmétique du R2 des variables
moy_R2var<-mean(R2var)

#Vérifification de l'égalité entre le R2 et la moy arithmétique du R2 des variables
delta<- moy_R2var - R2

print(xtable(data.frame(moy_R2var, R2), type ="latex", file="TP1.tex", digits=3,
             caption="Vérification de l'égalité entre le $R^2$ et la moyenne 
                arithmétique du $R^2$ des variables"))


#PARTIE 2

#Projection sur Y
projy <- Y %*% solve(t(Y) %*%W%*% Y) %*% t(Y)%*%W

#fonction de la projection sur les différrentes variables
projxj<-function(j) {
  proj <- X[,j] %*% solve(t(X[,j]) %*%W%*% X[,j]) %*% t(X[,j])%*%W

  return(proj)
}
#Matrice de toutes les traces (pour tous les j) du produit Proj_xj*Proj_Y
V2 <- matrix( 0, nrow=1, ncol=29)
for (j in 1:29) {
  V2[,j]<- sum(diag(projxj(j)%*%projy))
}
print(xtable(V2, type="latex", file="TP1.tex", digits=3,
             caption="Extrait du $R^2$ des différentes variables sensorielles"))

#Trace du produit R*proj_Y
R = X%*%M%*%t(X)%*%W
R_Y = R%*%projy
traceRY <-sum(diag(R_Y))

#Trace de proj_xj*proj_Z
projz <- Z %*% solve(t(Z) %*%W%*% Z) %*% t(Z)%*%W
V3 <- matrix(0, nrow=1, ncol=29)
for (j in 1:29) {
  V3[,j]<- sum(diag(projxj(j)%*%projz))
}
print(xtable(V3, type="latex", file="TP1.tex", digits=3,
             caption="Extrait des traces du produit de la projection sur les
             variables sensorielles et de la projection sur Z"))

#Trace de R*projz
traceRprojz <- sum(diag(R%*%projz))
#print(xtable(traceRprojz, type="latex", file = "TP1.tex", digits=3,
#            caption="Extrait des traces du produit de R et de la projection sur Z"))

