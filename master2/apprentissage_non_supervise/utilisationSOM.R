library(class)
library(MASS)
library(kohonen)

# Chargement des donnees (puissance consommee en watts pour 140 manoeuvres d'aiguillage ; une mesure effectuee toutes les 0.01s)
# Chargement des labels (1:etat normal, 2:defaut mineur, 3:defaut majeur, 4:arret d'exploitation)
x= as.matrix(read.table("aiguillage.txt",header=F))
labels =as.vector(as.matrix(read.table("labels.txt",header=F)) )

# Lancer de l'algorithme 
SOM.AIG <- som(x, rlen = 200, alpha = c(0.05,0.01), grid = somgrid(10,10, "rectangular"))

# Erreur entre les donnees et les centres au cours des iterations (critere d'inertie intra-classe moyen)
# l'erreur decroit au cours des iterations
# Augmenter le parametre rlen et observer comment se comporte l'erreur
plot(SOM.AIG,type="changes")

# Creation d'un code de couleurs
coolBlueHotRed <- function(n, alpha = 1)
{
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# Qualite de representation des centres
# pour chaque noeud, c'est la distance moyenne entre les observations et leur centre 
# plus cette distance est petite, plus la qualite de representation est bonne
# (la couleur grise signifie qu'aucune observation n'est associee au noeud)
plot(SOM.AIG,type="quality")

# Centres associes a chaque noeud (pour chaque noeud, les variables sont affichees les unes apres les autres)
plot(SOM.AIG,type="codes")

# Nombre d'observations associees a chaque noeud
# (la couleur grise signifie qu'aucune observation n'est associee au noeud)
plot(SOM.AIG,type="counts")

# Carte avec affichage de points colores suivant les vrais labels
# (les points representent les donnees)
plot(SOM.AIG,type="mapping",labels=labels,col=labels)

# U-matrix (matrice de voisinage)
plot(SOM.AIG, type="dist.neighbours",palette.name = coolBlueHotRed)

# Courbes associees aux noeuds 
matplot(t(SOM.AIG$codes),type="l",lty=1,col=rgb(colour1))

# Lancer des kmeans sur les noeuds
KM = kmeans(SOM.AIG$codes,4,50,100)

# Carte avec noeuds colores suivant les classes fournies par les kmeans 
couleur = c("gray","pink","lightgreen","yellow")
plot(SOM.AIG,type="mapping",bgcol=couleur[KM$cluster])

# U-matrix (matrice de voisinage) avec separation des noeuds (classes fournies par les kmeans)
plot(SOM.AIG, type="dist.neighbours",palette.name = coolBlueHotRed)
add.cluster.boundaries(SOM.AIG, KM$cluster)

# Courbes associees aux classes
par(mfrow=c(1,2))
matplot(t(KM$centers),type="l",lty=1)
plot(SOM.AIG, type="mapping", bgcol =KM$cluster)

