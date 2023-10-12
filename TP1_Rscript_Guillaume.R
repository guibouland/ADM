wine <- read.csv("wine.csv")
print(xtable(wine[1:4,1:6] , type = "latex", file = "TP1.tex", caption="Extrait du tableau d'origine"))
X <- wine[4:32]
Xc<- sqrt(21/20)*scale(X)
W <-diag(1/21 , nr=21)
print(xtable(Xc[1:4,1:5] , type = "latex", file = "TP1.tex", caption='Extrait des valeurs centrées réduites'))

Unit <- matrix(data=c(1), nrow=21, ncol=1)

#Calcul des barycentres des variables quantitatives
Bar <- t(Xc) %*% W %*% Unit

print(xtable(Bar , type = "latex", file = "TP1.tex", caption ='Barycentre du nuage'))


#Calcul de l'inertie totale du nuage
In = sum(apply(1/21*Xc^2 ,1, sum))



C <- matrix(0, nrow=21, ncol=3)
for (i in 1:21) {
  if (wine[i,2]=="Bourgueuil") {C[i,1]<-1};
  if (wine[i, 2]=="Chinon") {C[i,2]<-1};
  if (wine[i, 2]=="Saumur") {C[i,3]<-1}
}

#Poids et barycentre par appellation 
Poids_app <- t(C)%*%W%*%Unit
print(xtable(Poids_app , type = "latex", file = "TP1.tex", caption = 'Poids par appellation'))


Moy_cat= solve(t(C)%*%W%*%C)%*%t(C)%*%W%*%Xc
print(xtable(Moy_cat[,1:5] , type = "latex", file = "TP1.tex", caption = 'Barycentre des appellations'))


#Calcul normes euclidiennes
Normes<- apply(Moy_cat^2, 1, sum)
print(xtable(data.frame(Normes) , type = "latex", file = "TP1.tex", caption = 'Normes des barycentres par appellation'))



#Calcul de l'inertie inter-appellations
In_inter <- Normes%*%Poids_app

#Calcul du R^2
R2 <- In_inter/In

#explication 11%

#Calcul de la variance inter appellation qui est égal au 
#R2 puisque que la variance totale de la colonne vaut 1
var_inter <- t(Poids_app)%*%Moy_cat^2
R2var<- var_inter
print(xtable(data.frame(R2var)[,1:5], type ="latex", file="TP1.tex", caption="Extrait du résultat du $R**2$ par appellation"))


#Variables les moins liées à l'appellation
subset(R2var, select = R2var<0.01)
print(xtable(subset(R2var, select = R2var<0.01), type ="latex", file="TP1.tex", caption="Variables les moins liées à l'appellation"))

#Variables les plus liées à l'appellation
subset(R2var, select = R2var>0.33)
print(xtable(subset(R2var, select = R2var>0.33), type ="latex", file="TP1.tex", caption="Variables les plus liées à l'appellation"))

#Calcul de la moy arithmétique du R2 des variables
moy_R2var<-mean(R2var)

#Vérifification de l'égalité entre le R2 et la moy arithmétique du R2 des variables
delta<- moy_R2var - R2

print(xtable(data.frame(moy_R2var, R2), type ="latex", file="TP1.tex", caption="Vérification de l'égalité entre le $R^2$ et la moyenne arithmétique du $R^2$ des variables"))





#PARTIE 2
X<-Xc
Y<-C
#Construction de la matrice Z
Z <- matrix(0, nrow=21, ncol=4)
for (i in 1:21) {
  if (wine[i,3]=="Env1") {Z[i,1]<-1};
  if (wine[i, 3]=="Env2") {Z[i,2]<-1};
  if (wine[i, 3]=="Reference") {Z[i,3]<-1};
  if (wine[i, 3]=="Env4") {Z[i, 4]<-1}
}
#Définition de M
M=diag(1/29, 29)


#1a a repondre papier
#cours

