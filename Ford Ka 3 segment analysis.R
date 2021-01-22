# Load the dataset
da_fka = read.csv(file = "Segmentation/ford_ka.csv", header = TRUE)

# create a vector with the columns I want.
# sel_variables is just the names of the columns A01 to A24.
Att_Questions = da_fka[, 2:25]

set.seed(113) # I set a seed so the clusters are always labeled in the same way

kmeans_fka_3seg = kmeans(Att_Questions,   # this is the data I want to clsuter
                    centers = 3,        # I am asking for 3 clusters
                    nstart = 100,       # I want the algo to try different starting points
                    iter.max = 300)     # I set the max number of iterations to a large 
                                        #     number

# The object kmeans_fka_3seg has now information on cluster membership 
# the centroids of each cluster,  and other statistical diagnostic 
# information.

# try typing this and you'll get all the elements
kmeans_fka_3seg

# these are the cluster membership
kmeans_fka_3seg$cluster

# these are the centroids
kmeans_fka_3seg$centers

kmeans_fka_3seg$size
kmeans_fka_3seg$betweenss

rbind(kmeans_fka_3seg$centers, apply(Att_Questions, 2, mean))
t(rbind(kmeans_fka_3seg$centers, apply(Att_Questions, 2, mean)))
View(t(rbind(kmeans_fka_3seg$centers, apply(Att_Questions, 2, mean))))

da_fka$cluster = kmeans_fka_3seg$cluster
da_fka[da_fka$cluster == "2", ]

table(da_fka$INCOME, da_fka$cluster)
table(da_fka$KIDSCAT, da_fka$cluster)
table(da_fka$MARITST, da_fka$cluster)
table(da_fka$GENDER, da_fka$cluster)
table(da_fka$FIRSTCAR, da_fka$cluster)

da_fka  %>% group_by(cluster) %>% summarise(mean(AGE))

table(da_fka$TOPBOT3, da_fka$cluster)

