library(ompr)
library(ROI.plugin.glpk)
library(ompr.roi)
#install.packages("ROI")
library(ROI)
library(geosphere)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggrepel)
library(ggmap)

# mapNLbw <- get_googlemap('Netherlands', zoom = 7, color = "bw")
mapNL <- get_googlemap('Netherlands', zoom = 7)
# map2 <- get_googlemap('Netherlands', zoom = 8)
# ggmap(mapNLbw, extent = 'device')
# ggmap(mapNL, extent = 'device')
# ggmap(map2, extent = 'device')


m <- as.matrix(ChessClubs20162017[,c("Long", "Lat")])
md <- distm(m)
#md_df <- as.data.frame(md)

distanceCost <- function(i, j) {
  md[i, j]
}
#distanceCost(1,8)


#Our optimization problem is:
#Each club is in 1 group
#Groupweighth is between 72 and 77
#Grouplength of the 4 groups is 23 or 24
#Optimization is minimize distance
#The first approach failed as with 1 variable club and group it is not possible to calculate the distance
model1 <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n, j = 1:c, type = "binary") %>%

  add_constraint(sum_expr(x[i, j], j = 1:c) == 1, i = 1:n) %>%

  add_constraint(sum_expr(x[i, j], i = 1:n) == g, j = 1:c) %>%

  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) >= minTotalWeight, j = 1:c) %>%
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) <= maxTotalWeight, j = 1:c)

#Fail
  set_objective(sum_expr(distanceCost(x[i, k]), i = 1:n, k = 1:n), j = 1:c, sense="min")

result1 <- solve_model(model1, with_ROI(solver = "glpk", verbose = TRUE))
get_solution(result1,x[i, j])




#approach 2 try to fix this problem by adding a similar variable and use the distance
#by combining these values, this fails as well as the solution is non-linear, which is not supported
model2 <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n, j = 1:c, type = "binary") %>%
  add_variable(y[k, l], k = 1:n, l = 1:c, type = "binary") %>%
  
  #Add a copy of variable x to calculate distance
  add_constraint(x[i, j] == y[k, l], i = 1:n, j = 1:c, k = 1:n, l = 1:c, k==i, l==j) %>%

    
  add_constraint(sum_expr(x[i, j], j = 1:c) == 1, i = 1:n) %>%
  add_constraint(sum_expr(x[i, j], i = 1:n) >= gMin, j = 1:c) %>%
  add_constraint(sum_expr(x[i, j], i = 1:n) <= gMax, j = 1:c) %>%
  
  
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) >= minTotalWeight, j = 1:c) %>%
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) <= maxTotalWeight, j = 1:c) %>%

#Error: The objective is probably non-linear. Currently, only linear functions are supported.
set_objective(sum_expr(distanceCost(i, k) * x[i, j] * y[k, l], i = 1:n, k = 1:n, j = 1:c, l = 1:c), sense="min")

result2 <- solve_model(model2, with_ROI(solver = "glpk", verbose = TRUE))

get_solution(result2,x[i, j])





#That is a it of a dissappointment
#We could abandon this approach, and go for non-linear. 
#But lets try a few other approaches


#Traveling Salesman approach, performance becomes quickly a disaster
model3 <- MIPModel() %>%
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:c, type = "binary") %>%
  
  #help variable for ruling out subtours
  add_variable(u[i, k], i = 1:n, k = 1:c, type = "integer", lb = 1, ub = g) %>% 
  
  #distance cost
  set_objective(sum_expr(distanceCost(i, j) * x[i, j, k], i = 1:n, j = 1:n, k = 1:c), "min") %>%

  #you cannot go to the same club
  add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:c) %>%
  
  #each club has to be in just one group
  add_constraint(sum_expr(x[i, j, k], k = 1:c, j = 1:n) == 1, i = 1:n) %>%
  add_constraint(sum_expr(x[i, j, k], k = 1:c, i = 1:n) == 1, j = 1:n) %>%
  
  #each group k has to be of equal size
  add_constraint(sum_expr(x[i, j, k], i = 1:n, j = 1:n) == g, k = 1:c) %>%

  #each group k need to be equal in strength
  add_constraint(sum_expr(x[i, j, k] * weightLevel[i], j = 1:n, i = 1:n) >= minTotalWeight, k = 1:c) %>%
  add_constraint(sum_expr(x[i, j, k] * weightLevel[i], j = 1:n, i = 1:n) <= maxTotalWeight, k = 1:c) %>%
  
  #if group k is in city i it needs to leave it as well
  add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 1:n, k = 1:c) %>%
  
  # ensure no subtours, the first 4 teams need to be the group head of each group
  add_constraint(u[i, k] - u[j, k] + 1 <= n * (1 - x[i, j, k]), i = 1:n, j = 2:n, k = 1) %>%
  add_constraint(u[i, k] - u[j, k] + 1 <= n * (1 - x[i, j, k]), i = 2:n, j = 3:n, k = 2) %>% 
  add_constraint(u[i, k] - u[j, k] + 1 <= n * (1 - x[i, j, k]), i = 3:n, j = 4:n, k = 3) %>%
  add_constraint(u[i, k] - u[j, k] + 1 <= n * (1 - x[i, j, k]), i = 4:n, j = 5:n, k = 4)  
  # this works as 1-6 x=1 u=1 | 6-12 x=1 u=2 | 12-18 x=1 u=3 | 18-7 x=1 u=4 | 7-1 x=1 u=5 the last is not a constraint due to j=2:n   
model3
  
result3 <- solve_model(model3, with_ROI(solver = "glpk", verbose = TRUE))
# get_solution(result3,x[i, j, k]) %>% filter(value > 0)
# get_solution(result3,u[i, k]) %>% filter(value > 0)

solution <- get_solution(result3, x[i, j, k]) %>% 
  filter(value > 0) 
kable(head(solution))

paths <- select(solution, i, j, k) %>% 
  rename(from = i, to = j, group = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(ChessClubs20162017, by = c("idx_val" = "Id"))
kable(head(arrange(paths, trip_id), 4))

ggplot(ChessClubs20162017, aes(Long, Lat)) + 
  geom_point() + 
  geom_line(data = paths, aes(group = trip_id, color = factor(group))) + 
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result3), 2))) #+
















# It is difficult to optimize the 'Salesman' route. A relative simple problem with 20 clubs and 4 groups is already running for ages.
# It is time for a different route
# We take x random points in our search space and choose the 4 best points to create clusters with minimal distance


minLat <- min(ChessClubs20162017$Lat)
maxLat <- max(ChessClubs20162017$Lat)
minLong <- min(ChessClubs20162017$Long)
maxLong <- max(ChessClubs20162017$Long)

deltaLat <- maxLat - minLat
deltaLong <- maxLong - minLong

centroidLoc <- 96

set.seed(12345)
centroid_locations <- data.frame(
  IdCentroid = 1:centroidLoc,
  LongCentroid = runif(centroidLoc) * deltaLong + minLong, #x
  LatCentroid = runif(centroidLoc) * deltaLat + minLat #y
)


#Top centroid previous run
df1 <- data.frame(IdCentroid = centroidLoc + 1, LongCentroid = 4.933878, LatCentroid = 52.38954)
df2 <- data.frame(IdCentroid = centroidLoc + 2, LongCentroid = 6.404083, LatCentroid = 52.64013)
df3 <- data.frame(IdCentroid = centroidLoc + 3, LongCentroid = 5.487537, LatCentroid = 51.88348)
df4 <- data.frame(IdCentroid = centroidLoc + 4, LongCentroid = 4.409887, LatCentroid = 51.88037)

centroid_locations <- rbind(centroid_locations, df1)
centroid_locations <- rbind(centroid_locations, df2)
centroid_locations <- rbind(centroid_locations, df3)
centroid_locations <- rbind(centroid_locations, df4)

#p <- ggplot(ChessClubs20162017, aes(Long, Lat)) + 

p <- ggmap(mapNL, extent = 'device') +
  geom_point(data = ChessClubs20162017, aes(Long, Lat), color = "blue") + 
  geom_point(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid), color = "red", alpha = 0.5, shape = 17) 
  
  #scale_x_continuous(limits = c(minLong, maxLong)) +
  #scale_y_continuous(limits = c(minLat, maxLat)) 
  # theme(axis.title = element_blank(), 
  #       axis.ticks = element_blank(), 
  #       axis.text = element_blank(), panel.grid = element_blank())

p + 
  geom_label_repel(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid, label=as.character(IdCentroid)), box.padding = 0.25, point.padding = 0.3, segment.color = 'grey50') +
  ggtitle("Warehouse location problem for chess cup", "Blue dots are clubs. Light red triangles show potential centers.")

centroid_locations <- centroid_locations %>%
  filter(LongCentroid > 4.3) %>%
  filter(LongCentroid < 6.8) %>%
  filter(LatCentroid > 51.3) %>%
  filter(LatCentroid < 52.9)

p <- ggmap(mapNL, extent = 'device') +
  geom_point(data = ChessClubs20162017, aes(Long, Lat), color = "blue") + 
  geom_point(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid), color = "red", alpha = 0.5, shape = 17) 

p + 
  geom_label_repel(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid, label=as.character(IdCentroid)), box.padding = 0.25, point.padding = 0.3, segment.color = 'grey50') +
  ggtitle("Warehouse location problem for Chess League Cup", "Blue dots are clubs. Light red triangles show potential centers.")



centroidLoc <- nrow(centroid_locations)
m1 <- as.matrix(ChessClubs20162017[,c("Long", "Lat")])
m2 <- as.matrix(centroid_locations[,c("LongCentroid", "LatCentroid")])
md2 <- distm(m1, m2)

distanceCost2 <- function(i, j) {
  md2[i,j]^2
}

centroid_locations$Id <- row_number(centroid_locations$IdCentroid)



cl <- centroidLoc

model4 <- MIPModel() %>%
  # 1 iff i gets assigned to centroid j
  add_variable(x[i, j], i = 1:n, j = 1:cl, type = "binary") %>%
  
  #dummy
  add_variable(u[j], j = 1:cl, type = "binary") %>%
  
  add_constraint(sum_expr(u[j], j = 1:cl) == 4) %>% 
  
  #distance cost
  set_objective(sum_expr(distanceCost2(i, j) * x[i, j], i = 1:n, j = 1:cl), "min") %>%

  # every club needs to be assigned to a group
  add_constraint(sum_expr(x[i, j], j = 1:cl) == 1, i = 1:n) %>% 

  # each group k has to be of equal size
  add_constraint(sum_expr(x[i, j], i = 1:n) >= gMin * u[j], j = 1:cl) %>%
  add_constraint(sum_expr(x[i, j], i = 1:n) <= gMax * u[j], j = 1:cl) %>%
  
  #each group k need to be equal in strength
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) >= minTotalWeight * u[j], j = 1:cl) %>%
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) <= maxTotalWeight * u[j], j = 1:cl)
#model4

result4 <- solve_model(model4, with_ROI(solver = "glpk", verbose = TRUE))

result4 %>% 
  get_solution(u[j])

matching <- result4 %>% 
  get_solution(x[i,j]) %>%
  filter(value > 0) %>%  
  select(i, j)




plot_assignment <- matching %>% 
  inner_join(ChessClubs20162017, by = c("i" = "Id")) %>% 
  inner_join(centroid_locations, by = c("j" = "Id"))

# plot_centroid <- centroid_locations %>% 
#   inner_join(ChessClubs20162017, by = c("IdCentroid" = "Id")) %>% 
#   filter(Id %in% unique(matching$j))

p + 
  geom_segment(aes(x = LongCentroid, y = LatCentroid, xend = Long, yend = Lat), data = plot_assignment) + 
  geom_point(aes(x = LongCentroid, y = LatCentroid), data = centroid_locations, color = "red", size = 3, shape = 17) +
  # ggrepel::geom_label_repel(data  = plot_warehouses, 
  #                           aes(label = paste0("fixed costs:", "WEIGHT", "; clubs: ", n)), 
  #                           size = 2, nudge_y = 20) + 
  ggtitle(paste0("Cost - optimal groups and clubs assignment"),
          "Big red triangles show groups centroid, light red are unused centroid locations. 
          Dots represent clubs served by the respective group.")







#Some reordering and export
ChessClubs20162017New <- matching %>% 
  inner_join(ChessClubs20162017, by = c("i" = "Id"))

ChessClubs20162017New <- rename(ChessClubs20162017New, Id = i)

jGroups <- sort(unique(ChessClubs20162017New$j))
ChessClubs20162017New$BekerNew <- mapvalues(ChessClubs20162017New$j, from = jGroups, to = c("A", "B", "C", "D"))
ChessClubs20162017New$j <- NULL

write.csv(ChessClubs20162017New, file = "output\\leagueChessClub2.csv")


#The comparison is done in Power BI

