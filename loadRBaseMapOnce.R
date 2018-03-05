mapNL <- get_googlemap('Netherlands', zoom = 7)
save(mapNL, file="output/mapNL.rda")

#Restart R or RStudio before running the code below
#load("output/mapNL.rda")
#ggmap(mapNL)
