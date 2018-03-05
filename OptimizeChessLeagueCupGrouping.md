In this article we investigate the pairings for the Chess League Cup in the Netherlands. I have some doubts about the classification of the groupings. We use different techniques to solve this optimization problem:

-   Power BI Visualization & Comparison

-   R (Optimization using OMPR & Visualization)

-   SQL Server (Geocoding address Chessclubs)

Chess League Cup Explanation
----------------------------

As with football there is a League Cup and the regular national competition in the Netherlands. Though the regular competition is a single round robin event of 10 teams. Just 9 games leaves plenty of time to play individual tournaments or multiple foreign league competitions. In the regular competition you play with 10 players in the highest 2 classes and with 8 players per team in the lower classes.

In the League Cup you play with 4 players often during the week on the regular chess club evening of the home playing team. The League cup is based on a knockout system where around 100 teams participate. The teams are divided in 4 groups and the winners of each group go to the finals where the cup winner will be decided.

The 4 groups are divided in equal sized groups based on strength (based on level in national competition) and secondly on distance. So, one group in the North, East, West and South, right? Well not exactly, the Dutch chess teams are more or less divided according to population density in the country. The league cup games are played in the evening and can take over 4 hours. Short travelling distances are preferable. The teams in the North, South and East have to travel a lot as these areas are too small to form a single group. And yes, I happen to live and play chess in one of these regions...

The Problem
-----------

But the distance penalty seemed to be a bit unfair with long driving's (ok perspective of the Netherlands...). Of course an idea is to make 8 groups instead of 4 groups. Then play 3 games in the final weekend or 2 games in the final weekend and one battle with a neighboring group winner to qualify for the finals. The latter solution makes sense, but there are traditions for a reason.

Enough talk, let’s see whether the solution was optimized or whether my gut feeling was correct.

What do we need:

-   the clubs and their playing venue location

-   the national competition league of the club (for strength assessment)

-   the average rating of the teams (for strength assessment)

-   the group in the League Cup

As usually I work with the Microsoft stack, that’s where we start.

The first thing is to get the data of the chess clubs and optionally put it in a database, sql server. Unfortunately there is no webservice or something to get this data easily. I might could have asked for this data but I extracted the data from the website (at that point still [schaakbond.nl](www.schaakbond.nl) instead of the new website [schaken.nl](www.schaken.nl)).

I did some data wrangling/munging, sounds cooler than the just plain good old ETL/ELT. In this case it was mainly fixing small errors from the parsing. In the end I extracted the addresses of the 150 clubs in the national league and I looked up the addresses of the 10 champions from the regions. These are all the potential clubs which can participate in the league cup, though second teams are excluded.

![Address Chessclub](images/AddressChessclub.png)

In the next figure the team information is presented.

<table style="width:93%;">
<caption>Team information</caption>
<colgroup>
<col width="22%" />
<col width="70%" />
</colgroup>
<thead>
<tr class="header">
<th>Dutch name</th>
<th>Translation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><p>Klasse</p></td>
<td><p>National league</p></td>
</tr>
<tr class="even">
<td><p>Beker</p></td>
<td><p>Cup group</p></td>
</tr>
<tr class="odd">
<td><p>Team</p></td>
<td><p>TeamName (2 indicates second team of the club)</p></td>
</tr>
<tr class="even">
<td><p>KNSB and FIDE</p></td>
<td><p>Average strength of the team</p></td>
</tr>
<tr class="odd">
<td><p>KNSB</p></td>
<td><p>National rating</p></td>
</tr>
<tr class="even">
<td><p>FIDE</p></td>
<td><p>International rating</p></td>
</tr>
<tr class="odd">
<td><p>Gem. leeftijd</p></td>
<td><p>Average age of the team. As a BI-er I am very well aware of our habit to add data in the pipeline nobody needs...</p></td>
</tr>
</tbody>
</table>

![Chessclub Information](images/InformationChessclub.png)

Not every team participates in the league cup, therefor the blanks. There can be different reasons for not participating.

The next step is to calculate the location (latitude, longitude) of the club based on the playing venue address of the club. I use the method illustrated by Alastair Aitchison in his book *Pro Spatial with SQL Server 2012*, <https://alastaira.wordpress.com/2012/05/04/geocoding-in-sql-server-with-the-bing-maps-locations-api/>. So a typical call to get the sql server geography object looks like this.

![GeoCode SQL Server using Bingmaps](images/GeoCodeSQLServer.png)

There are different ways to get the latitude longitude coordinate, but it is not our intention to discuss this in detail. Now we import the data in Power BI Desktop and create some initial visualization to get a rough idea about the data. ![Power BI initial EDA](images/PowerBI_InitialEDA.png)

The brief EDA gave some clues that improvement does not seem impossible. On the left visualization we see the actual 4 league cup groups as played in the season 2016-2017. In the middle we see two versions of the k-means clustering with 4 and 8 clusters (groups), using R within Power BI. In this example strength and equal sized cluster are not considered, but it gives an idea about the data. On the right visualizations we see the division of strength over the club locations. The bottom right visualization is based on FIDE rating, while the upper visualization is based on the national league class.

Once again, my personal annoyance was that each year we play in group south, including some pretty far western clubs, while living in the east. More concretely, is the league cup already the optimal solution or is there a solution which is better in all relevant aspects. So what should we take into consideration:

-   average strength (based on national league class)

-   group size (equal length over the groups)

-   avg distance and avg squared distance in the group

Unfortunately, there is no magic button in Power BI to get the answer for this problem. Clustering algorithms have a different purpose, similarity, and cannot be used for this problem.

The solution can be found in linear optimization type of problems. Math is certainly not my expertise area, but let’s see whether we can come up with some results. As a Microsoft guy nowadays you need to be savvy with R, so lets fire up R studio!

I found a nice looking package from Dirk Schumacher, *OMPR*, Optimization Modelling Package. More information can be found [here](https://dirkschumacher.github.io/ompr/index.html). In general in works a bit like the package *Caret*, for Regression and Classification type of problems. It provides a universal front-end for many different optimization algorithms.

R
-

After importing the data in R, we do a little preprocessing before building the model. In the specific year we have 93 clubs participating in the league cup. We set the number of groups to 4, and the size of the group to a minimum of 23 clubs and a maximum of 24 clubs. We use the same weight setting as in Power BI. But we add the maximum and minimum weight for a group to ensure equal strength groups.

At development time I used a subset, for example 20 clubs and 4 groups, to quickly test the solution. I played a bit with the *OMPR* package and tried different approaches to solve the problem.

Some less successful attempts have been tried with different models. They are within R script *Optimize ChessLeagueCupGroupings.R* in GitHub. One of the attempts was the famous TSP, Traveling Salesman Problem. It wasn’t particularly successful as the problem space grew massively. But the idea was to have 4 chains of clubs equal in size and strength. Unfortunately, testing it with just 20 cities already took too long, while with 10 cities and 2 groups it was ok.

Here we look into the final model. But first the preprocessing.

``` r
rm(list=setdiff(ls(), "mapNL"))

library(ompr)
library(ROI.plugin.glpk)
library(ompr.roi)
library(ROI)
library(geosphere)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggrepel)
library(ggmap)
```

Preprocessing
-------------

In the preprocessing we load the data from the already geocoded chessclubs, csv file, and set a few variables for the groupings.

``` r
source("Preprocessing ChessLeagueCup.R")
```

Create a map of the Netherlands (once).

``` r
load("output/mapNL.rda")
```

Optimization Model
------------------

As a reminder we need to take into account similar strength and equal group sizes and reduce the overall distance between the clubs assigned to the 4 groups.

For our final solution a number of random points are choosen within the studyarea (added with the best points from previous runs). Then we choose the 4 most optimized points to create the 4 groups with a minimal (squared) distance from the clubs to the grouppoint (centroid approximation). Taking into account the constraints for equal strength and equal group sizes. The proposed solution is related to the following type of problem: finding the most optimal warehouse location to serve customers.

``` r
#Study area extent based on locations of the chessclub
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

#Add top centroid from previous run
df1 <- data.frame(IdCentroid = centroidLoc + 1, LongCentroid = 4.933878, LatCentroid = 52.38954)
df2 <- data.frame(IdCentroid = centroidLoc + 2, LongCentroid = 6.404083, LatCentroid = 52.64013)
df3 <- data.frame(IdCentroid = centroidLoc + 3, LongCentroid = 5.487537, LatCentroid = 51.88348)
df4 <- data.frame(IdCentroid = centroidLoc + 4, LongCentroid = 4.409887, LatCentroid = 51.88037)

centroid_locations <- rbind(centroid_locations, df1)
centroid_locations <- rbind(centroid_locations, df2)
centroid_locations <- rbind(centroid_locations, df3)
centroid_locations <- rbind(centroid_locations, df4)

#it was a bit too ambitious to have the study area crossing the North Sea, Belgium and Germany.
#Lets trim it a bit more
centroid_locations <- centroid_locations %>%
  filter(LongCentroid > 4.1) %>%
  filter(LongCentroid < 6.6) %>%
  filter(LatCentroid > 51.2) %>%
  filter(LatCentroid < 53.1)
```

We have now our random centroid locations. Let's visualize our data.

``` r
p <- ggmap(mapNL, extent = 'device') +
  geom_point(data = ChessClubs20162017, aes(Long, Lat), color = "blue") + 
  geom_point(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid), color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(minLong, maxLong)) +
  scale_y_continuous(limits = c(minLat, maxLat)) 

p + 
#  geom_label_repel(data = centroid_locations, aes(x = LongCentroid, y = LatCentroid, label=as.character(IdCentroid)), box.padding #= 0.25, point.padding = 0.3, segment.color = 'grey50') +
  ggtitle("'Warehouse' location problem for Chess League Cup", "Blue dots are clubs, Light red triangles show potential centers.")
```

<img src="OptimizeChessLeagueCupGrouping_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto auto auto 0;" />

The suggested centroid locations look reasonable.

DistanceMatrix
--------------

We are almost ready, to start crafting the optimization model. But it is a handy to make an initial Distance Matrix and not calculate again and again in the model optimization function. A squared distance is used as a distance measure. So it tries too limit larger distances. The distance function is not the driving distance though (this could be sort of *geocoded* as well).

``` r
centroidLoc <- nrow(centroid_locations)
m1 <- as.matrix(ChessClubs20162017[,c("Long", "Lat")])
m2 <- as.matrix(centroid_locations[,c("LongCentroid", "LatCentroid")])
md2 <- distm(m1, m2)

distanceCost2 <- function(i, j) {
  md2[i,j]^2
}

centroid_locations$Id <- row_number(centroid_locations$IdCentroid) #As some point locations are removed, create a new Id field
cl <- centroidLoc #a small variable names helps too read the model

rm(m1, m2, centroidLoc, df1, df2, df3, df4, deltaLong, deltaLat) #cleanup
```

Model
-----

Finally! The model is easy to read and understand. It was one of the reasons for choosing this package.

``` r
model4 <- MIPModel() %>%
  # 1 if i gets assigned to centroid_location j otherwise 0
  add_variable(x[i, j], i = 1:n, j = 1:cl, type = "binary") %>%
  
  #the centroid locations (dummy variable)
  add_variable(u[j], j = 1:cl, type = "binary") %>%
  
  #exactly 4 groups are allowed
  add_constraint(sum_expr(u[j], j = 1:cl) == 4) %>% 
  
  #minimize squared distance cost
  set_objective(sum_expr(distanceCost2(i, j) * x[i, j], i = 1:n, j = 1:cl), "min") %>%

  # every chessclub needs to be assigned to exactly one group
  add_constraint(sum_expr(x[i, j], j = 1:cl) == 1, i = 1:n) %>% 

  # each of the 4 groups has to be of equal size
  add_constraint(sum_expr(x[i, j], i = 1:n) >= gMin * u[j], j = 1:cl) %>%
  add_constraint(sum_expr(x[i, j], i = 1:n) <= gMax * u[j], j = 1:cl) %>%
  
  #each of the 4 groups has to be of equal strength
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) >= minTotalWeight * u[j], j = 1:cl) %>%
  add_constraint(sum_expr(x[i, j] * weightLevel[i], i = 1:n) <= maxTotalWeight * u[j], j = 1:cl)

#Lets inspect the model created
model4
```

    ## Mixed linear integer optimization problem
    ## Variables:
    ##   Continuous: 0 
    ##   Integer: 0 
    ##   Binary: 6298 
    ## Model sense: minimize 
    ## Constraints: 362

Now we run the model using the glpk solver

``` r
result4 <- solve_model(model4, with_ROI(solver = "glpk", verbose = TRUE))
```

    ## <SOLVER MSG>  ----
    ## GLPK Simplex Optimizer, v4.47
    ## 362 rows, 6298 columns, 31490 non-zeros
    ##       0: obj =  0.000000000e+000  infeas = 9.700e+001 (94)
    ## *   253: obj =  1.236354690e+012  infeas = 2.274e-015 (1)
    ## *   500: obj =  8.712538744e+011  infeas = 0.000e+000 (0)
    ## *  1000: obj =  4.987494456e+010  infeas = 6.297e-016 (0)
    ## *  1170: obj =  3.919634122e+010  infeas = 4.441e-017 (0)
    ## OPTIMAL SOLUTION FOUND
    ## GLPK Integer Optimizer, v4.47
    ## 362 rows, 6298 columns, 31490 non-zeros
    ## 6298 integer variables, all of which are binary
    ## Integer optimization begins...
    ## +  1170: mip =     not found yet >=              -inf        (1; 0)
    ## +  2031: >>>>>  2.471864739e+011 >=  4.263027948e+010  82.8% (62; 0)
    ## +  3992: >>>>>  1.727665160e+011 >=  4.554063146e+010  73.6% (215; 9)
    ## +  5333: mip =  1.727665160e+011 >=  4.966066692e+010  71.3% (303; 96)
    ## +  6340: >>>>>  1.696382545e+011 >=  5.055920563e+010  70.2% (391; 97)
    ## +  8803: mip =  1.696382545e+011 >=  6.917296209e+010  59.2% (478; 143)
    ## + 12260: mip =  1.696382545e+011 >=  7.660774154e+010  54.8% (588; 160)
    ## + 15873: mip =  1.696382545e+011 >=  8.811110055e+010  48.1% (705; 183)
    ## + 18763: mip =  1.696382545e+011 >=  9.094521280e+010  46.4% (849; 195)
    ## + 21344: mip =  1.696382545e+011 >=  9.381951476e+010  44.7% (995; 206)
    ## + 23636: mip =  1.696382545e+011 >=  9.510877569e+010  43.9% (1134; 216)
    ## + 23916: >>>>>  1.684059921e+011 >=  9.510877569e+010  43.5% (1152; 216)
    ## + 26363: mip =  1.684059921e+011 >=  9.623257085e+010  42.9% (1265; 243)
    ## + 28975: >>>>>  1.639851247e+011 >=  9.675488984e+010  41.0% (1394; 252)
    ## + 32095: mip =  1.639851247e+011 >=  9.752051108e+010  40.5% (1510; 340)
    ## Time used: 60.0 secs.  Memory used: 14.3 Mb.
    ## + 34657: mip =  1.639851247e+011 >=  9.818190382e+010  40.1% (1646; 353)
    ## + 35779: mip =  1.639851247e+011 >=  9.847701723e+010  39.9% (1697; 358)
    ## + 36013: >>>>>  1.614383733e+011 >=  9.847701723e+010  39.0% (1713; 358)
    ## + 37267: mip =  1.614383733e+011 >=  9.870858723e+010  38.9% (1745; 478)
    ## + 38596: mip =  1.614383733e+011 >=  9.944889530e+010  38.4% (1829; 488)
    ## + 39716: mip =  1.614383733e+011 >=  9.985416170e+010  38.1% (1893; 495)
    ## + 42057: mip =  1.614383733e+011 >=  1.005681215e+011  37.7% (2035; 508)
    ## + 45415: mip =  1.614383733e+011 >=  1.015555201e+011  37.1% (2184; 525)
    ## + 45836: >>>>>  1.606941897e+011 >=  1.017297278e+011  36.7% (2205; 528)
    ## + 48449: mip =  1.606941897e+011 >=  1.021944294e+011  36.4% (2301; 591)
    ## + 49187: >>>>>  1.554849223e+011 >=  1.022479093e+011  34.2% (2364; 594)
    ## + 51686: mip =  1.554849223e+011 >=  1.030560110e+011  33.7% (2289; 1010)
    ## + 55417: mip =  1.554849223e+011 >=  1.041166757e+011  33.0% (2410; 1036)
    ## + 59271: mip =  1.554849223e+011 >=  1.049086059e+011  32.5% (2558; 1063)
    ## + 63210: mip =  1.554849223e+011 >=  1.057714949e+011  32.0% (2706; 1089)
    ## Time used: 120.0 secs.  Memory used: 25.4 Mb.
    ## + 66966: mip =  1.554849223e+011 >=  1.067085429e+011  31.4% (2873; 1117)
    ## + 70008: mip =  1.554849223e+011 >=  1.070962505e+011  31.1% (3012; 1136)
    ## + 70481: >>>>>  1.553848283e+011 >=  1.072214176e+011  31.0% (3032; 1138)
    ## + 72259: mip =  1.553848283e+011 >=  1.077155626e+011  30.7% (3084; 1169)
    ## + 75246: mip =  1.553848283e+011 >=  1.083367423e+011  30.3% (3170; 1193)
    ## + 76722: mip =  1.553848283e+011 >=  1.091054517e+011  29.8% (3220; 1209)
    ## + 77651: mip =  1.553848283e+011 >=  1.092263095e+011  29.7% (3288; 1214)
    ## + 80171: mip =  1.553848283e+011 >=  1.102003240e+011  29.1% (3335; 1248)
    ## + 82213: mip =  1.553848283e+011 >=  1.105496541e+011  28.9% (3398; 1261)
    ## + 85087: mip =  1.553848283e+011 >=  1.111471771e+011  28.5% (3511; 1280)
    ## + 88474: mip =  1.553848283e+011 >=  1.121252728e+011  27.8% (3618; 1313)
    ## + 92748: mip =  1.553848283e+011 >=  1.136898166e+011  26.8% (3712; 1353)
    ## + 94937: mip =  1.553848283e+011 >=  1.139443486e+011  26.7% (3819; 1366)
    ## Time used: 180.0 secs.  Memory used: 40.7 Mb.
    ## + 98933: mip =  1.553848283e+011 >=  1.147728034e+011  26.1% (3898; 1414)
    ## +103324: mip =  1.553848283e+011 >=  1.157636978e+011  25.5% (3972; 1461)
    ## +107876: mip =  1.553848283e+011 >=  1.167820482e+011  24.8% (4069; 1512)
    ## +110320: mip =  1.553848283e+011 >=  1.173438975e+011  24.5% (4114; 1534)
    ## +114014: mip =  1.553848283e+011 >=  1.180818866e+011  24.0% (4205; 1587)
    ## +116665: mip =  1.553848283e+011 >=  1.184871655e+011  23.7% (4252; 1618)
    ## +119885: mip =  1.553848283e+011 >=  1.191591824e+011  23.3% (4302; 1657)
    ## +122367: mip =  1.553848283e+011 >=  1.193954750e+011  23.2% (4402; 1683)
    ## +126424: mip =  1.553848283e+011 >=  1.203478543e+011  22.5% (4435; 1742)
    ## +129240: mip =  1.553848283e+011 >=  1.207225134e+011  22.3% (4465; 1777)
    ## +132946: mip =  1.553848283e+011 >=  1.213275367e+011  21.9% (4534; 1826)
    ## +134212: mip =  1.553848283e+011 >=  1.214901120e+011  21.8% (4597; 1836)
    ## Time used: 240.0 secs.  Memory used: 57.0 Mb.
    ## +136932: mip =  1.553848283e+011 >=  1.219403416e+011  21.5% (4680; 1867)
    ## +139833: mip =  1.553848283e+011 >=  1.221683034e+011  21.4% (4760; 1895)
    ## +144089: mip =  1.553848283e+011 >=  1.227463861e+011  21.0% (4813; 1943)
    ## +147293: mip =  1.553848283e+011 >=  1.230496882e+011  20.8% (4913; 1976)
    ## +153051: mip =  1.553848283e+011 >=  1.236831919e+011  20.4% (4952; 2048)
    ## +157766: mip =  1.553848283e+011 >=  1.242688583e+011  20.0% (5042; 2106)
    ## +162377: mip =  1.553848283e+011 >=  1.248457462e+011  19.7% (5128; 2165)
    ## +166509: mip =  1.553848283e+011 >=  1.252464388e+011  19.4% (5211; 2216)
    ## +170905: mip =  1.553848283e+011 >=  1.256637583e+011  19.1% (5314; 2270)
    ## +175846: mip =  1.553848283e+011 >=  1.262857413e+011  18.7% (5322; 2354)
    ## +178334: >>>>>  1.544210002e+011 >=  1.264647315e+011  18.1% (5393; 2393)
    ## +182599: mip =  1.544210002e+011 >=  1.270584094e+011  17.7% (5328; 2647)
    ## Time used: 300.0 secs.  Memory used: 72.4 Mb.
    ## +188930: mip =  1.544210002e+011 >=  1.276289650e+011  17.3% (5362; 2749)
    ## +194464: mip =  1.544210002e+011 >=  1.279881434e+011  17.1% (5405; 2830)
    ## +199993: mip =  1.544210002e+011 >=  1.284269614e+011  16.8% (5474; 2902)
    ## +205564: mip =  1.544210002e+011 >=  1.287454790e+011  16.6% (5536; 2980)
    ## +210140: mip =  1.544210002e+011 >=  1.291428422e+011  16.4% (5613; 3047)
    ## +215406: mip =  1.544210002e+011 >=  1.295664783e+011  16.1% (5650; 3131)
    ## +220511: mip =  1.544210002e+011 >=  1.299841889e+011  15.8% (5723; 3205)
    ## +223866: mip =  1.544210002e+011 >=  1.303863388e+011  15.6% (5753; 3267)
    ## +228475: mip =  1.544210002e+011 >=  1.306290455e+011  15.4% (5729; 3352)
    ## +232700: mip =  1.544210002e+011 >=  1.309189175e+011  15.2% (5788; 3424)
    ## +237459: mip =  1.544210002e+011 >=  1.313387537e+011  14.9% (5802; 3497)
    ## +243159: mip =  1.544210002e+011 >=  1.315930367e+011  14.8% (5855; 3588)
    ## Time used: 360.0 secs.  Memory used: 90.1 Mb.
    ## +247447: mip =  1.544210002e+011 >=  1.318377170e+011  14.6% (5926; 3654)
    ## +251661: mip =  1.544210002e+011 >=  1.321203528e+011  14.4% (6017; 3721)
    ## +256275: mip =  1.544210002e+011 >=  1.323069013e+011  14.3% (6097; 3784)
    ## +260608: mip =  1.544210002e+011 >=  1.325513773e+011  14.2% (6188; 3847)
    ## +263386: mip =  1.544210002e+011 >=  1.325661054e+011  14.2% (6239; 3882)
    ## +265643: mip =  1.544210002e+011 >=  1.326615411e+011  14.1% (6277; 3911)
    ## +267950: mip =  1.544210002e+011 >=  1.328143609e+011  14.0% (6354; 3949)
    ## +269987: mip =  1.544210002e+011 >=  1.328206106e+011  14.0% (6419; 3973)
    ## +270624: >>>>>  1.540105216e+011 >=  1.329591942e+011  13.7% (6430; 3986)
    ## +275843: mip =  1.540105216e+011 >=  1.331469071e+011  13.5% (6375; 4204)
    ## +279758: mip =  1.540105216e+011 >=  1.334140309e+011  13.4% (6426; 4274)
    ## +283475: mip =  1.540105216e+011 >=  1.336950182e+011  13.2% (6489; 4325)
    ## +287937: mip =  1.540105216e+011 >=  1.339852265e+011  13.0% (6526; 4418)
    ## Time used: 420.0 secs.  Memory used: 102.5 Mb.
    ## +293917: mip =  1.540105216e+011 >=  1.342495449e+011  12.8% (6535; 4535)
    ## +296889: >>>>>  1.480105129e+011 >=  1.344432148e+011   9.2% (6543; 4590)
    ## +300990: mip =  1.480105129e+011 >=  1.346645185e+011   9.0% (4788; 7903)
    ## +303886: mip =  1.480105129e+011 >=  1.350385087e+011   8.8% (4798; 8008)
    ## +308412: mip =  1.480105129e+011 >=  1.352574243e+011   8.6% (4800; 8127)
    ## +313212: mip =  1.480105129e+011 >=  1.356668224e+011   8.3% (4745; 8272)
    ## +318559: mip =  1.480105129e+011 >=  1.359916040e+011   8.1% (4700; 8434)
    ## +321516: mip =  1.480105129e+011 >=  1.361668226e+011   8.0% (4670; 8516)
    ## +323943: mip =  1.480105129e+011 >=  1.363698088e+011   7.9% (4616; 8633)
    ## +326050: mip =  1.480105129e+011 >=  1.365733770e+011   7.7% (4590; 8702)
    ## +329459: mip =  1.480105129e+011 >=  1.368132781e+011   7.6% (4540; 8827)
    ## +333145: mip =  1.480105129e+011 >=  1.371072136e+011   7.4% (4512; 8964)
    ## +337076: mip =  1.480105129e+011 >=  1.375079751e+011   7.1% (4451; 9103)
    ## Time used: 480.0 secs.  Memory used: 103.6 Mb.
    ## +340620: mip =  1.480105129e+011 >=  1.376912992e+011   7.0% (4426; 9221)
    ## +345084: mip =  1.480105129e+011 >=  1.380853802e+011   6.7% (4362; 9422)
    ## +350692: mip =  1.480105129e+011 >=  1.385139435e+011   6.4% (4240; 9667)
    ## +354157: mip =  1.480105129e+011 >=  1.388578145e+011   6.2% (4163; 9839)
    ## +357136: mip =  1.480105129e+011 >=  1.390618012e+011   6.0% (4083; 9980)
    ## +362076: mip =  1.480105129e+011 >=  1.395202846e+011   5.7% (3969; 10209)
    ## +368353: mip =  1.480105129e+011 >=  1.399852376e+011   5.4% (3737; 10538)
    ## +373598: mip =  1.480105129e+011 >=  1.404449639e+011   5.1% (3556; 10844)
    ## +378783: mip =  1.480105129e+011 >=  1.408396190e+011   4.8% (3396; 11139)
    ## +383972: mip =  1.480105129e+011 >=  1.412618554e+011   4.6% (3224; 11430)
    ## +388982: mip =  1.480105129e+011 >=  1.417664934e+011   4.2% (3029; 11757)
    ## +392783: mip =  1.480105129e+011 >=  1.422127514e+011   3.9% (2821; 12134)
    ## Time used: 540.0 secs.  Memory used: 103.6 Mb.
    ## +396720: mip =  1.480105129e+011 >=  1.428210460e+011   3.5% (2585; 12568)
    ## +398422: >>>>>  1.475556012e+011 >=  1.430664428e+011   3.0% (2476; 12759)
    ## +402073: mip =  1.475556012e+011 >=  1.435817195e+011   2.7% (2006; 13845)
    ## +406340: mip =  1.475556012e+011 >=  1.442424416e+011   2.2% (1631; 14793)
    ## +410314: mip =  1.475556012e+011 >=  1.449247822e+011   1.8% (1280; 15752)
    ## +413198: mip =  1.475556012e+011 >=  1.455624185e+011   1.4% (964; 16909)
    ## +414735: mip =  1.475556012e+011 >=  1.460105507e+011   1.0% (737; 17903)
    ## +416434: mip =  1.475556012e+011 >=  1.464200564e+011   0.8% (501; 18998)
    ## +417697: mip =  1.475556012e+011 >=  1.468899638e+011   0.5% (263; 20706)
    ## +418064: >>>>>  1.472613739e+011 >=  1.471252795e+011 < 0.1% (188; 21463)
    ## +418316: mip =  1.472613739e+011 >=     tree is empty   0.0% (0; 24309)
    ## INTEGER OPTIMAL SOLUTION FOUND
    ## <!SOLVER MSG> ----

``` r
#choosen centroid locations
selected_centroid <- result4 %>% 
  get_solution(u[j]) %>%
  filter(value > 0) %>%
  select(j)


#Take group assignments
matching <- result4 %>% 
  get_solution(x[i,j]) %>%
  filter(value > 0) %>%  
  select(i, j)
```

Now we integrate the results and visualize the results.

``` r
plot_assignment <- matching %>% 
  inner_join(ChessClubs20162017, by = c("i" = "Id")) %>% 
  inner_join(centroid_locations, by = c("j" = "Id"))

p + 
  geom_segment(aes(x = LongCentroid, y = LatCentroid, xend = Long, yend = Lat), data = plot_assignment) + 
  geom_point(aes(x = LongCentroid, y = LatCentroid), data = centroid_locations[centroid_locations$Id %in% selected_centroid$j, ], color = "red", size = 3, shape = 17) +

  ggtitle("Optimal groups for the Chess League Cup",
          "Based on the possible centroid locations. 
Big red triangles show the 4 selected centroid locations. 
Light red are unused centroid locations, blue dots represent chessclubs.")
```

<img src="OptimizeChessLeagueCupGrouping_files/figure-markdown_github/unnamed-chunk-10-1.png" style="display: block; margin: auto auto auto 0;" />

*And no, we still don't have mountains here...*

The results look like an improvement. Let's compare the results in Power BI. Therefor we first need to export the results. Export the data to csv.

``` r
#Some reordering and export
ChessClubs20162017New <- matching %>% 
  inner_join(ChessClubs20162017, by = c("i" = "Id"))

ChessClubs20162017New <- rename(ChessClubs20162017New, Id = i)

jGroups <- sort(unique(ChessClubs20162017New$j))
ChessClubs20162017New$BekerNew <- mapvalues(ChessClubs20162017New$j, from = jGroups, to = c("A", "B", "C", "D"))
ChessClubs20162017New$j <- NULL

write.csv(ChessClubs20162017New, file = "output\\leagueChessClub.csv")
```

Power BI
--------

The results in Power BI ![Power BI Optimization Results](images/PowerBI_OptimizationResults.png)

Well it seems my gutfeeling was about right. The average distance dropped with almost 10 km (20 km for roundtrip). Ok, the club in province North Holland need to use some bridges, but that seems acceptable. Everything appears better, apart from the SD Weight. It is a little higher for 1 group. This indicates there are more strong and weak teams compared tp the other groups. I think this is acceptable as well.

These statistics are created using DAX (measures), the most interesting is the DistanceAvg and Distance2Avg (squared) measures. They provide the average distance between the selected clubs (within the group in this case). In DAX we can do this in the following way:

    DistanceAvg = AVERAGEX( 
        FILTER(
            CROSSJOIN(
                SELECTCOLUMNS ( ChessLeagueCup; 
                  "Lat_1"; ChessLeagueCup[Lat]; 
                  "Long_1"; ChessLeagueCup[Long]; 
                  "ClubNaam1"; ChessLeagueCup[ClubNaam]);
                SELECTCOLUMNS ( ChessLeagueCup; 
                  "Lat_2"; ChessLeagueCup[Lat]; 
                  "Long_2"; ChessLeagueCup[Long]; 
                  "ClubNaam2"; ChessLeagueCup[ClubNaam])
           );
           [ClubNaam1] <> [ClubNaam2]
        ); 
        6371 *((2*ASIN(SQRT((SIN((RADIANS([Lat_1])-RADIANS([Lat_2]))/2)^2)+COS(RADIANS([Lat_1]))*COS(RADIANS([Lat_2]))*(SIN((RADIANS([Long_1])-RADIANS([Long_2]))/2)^2)))))
    )

It would have been nice if this can be added in a kind of UDF as now the formula is copied a couple of times.

After all these analysis, reporting and writing, it is time to do some real calculations again and bring home that Cup ones more!

You can view the Power BI report [here](https://app.powerbi.com/view?r=eyJrIjoiZWIzOTBiYmEtMzc5OC00NmE0LWJhMWItMmQyMDAwNzlkMTgwIiwidCI6Ijg3NGM1MzA1LWI0MDktNGU5Ni04ODhiLTQ4ODViNWQ0ZDYwNiIsImMiOjl9).

All the code can be found on GitHub [ChessLeagueCup](https://github.com/Ruud-Janssen/ChessLeagueCup).
