# Lecture des donnees

# gordon
data = read.table('/media/ngoctu/769829E69829A599/workspace/master2/master2/exploration_visuelle_des_donnees/Projet_Visualisation/gordon-2002_database.txt', 
                  sep = '\t')
X = t(data[-(1:2), ])[-1, ]
y = as.vector(t(data[2, ]))

