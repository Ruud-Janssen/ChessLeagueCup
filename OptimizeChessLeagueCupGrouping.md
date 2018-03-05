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

*Address information: Clubname, Clubcode, Street, Postal code, City and Province*

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
    ## +  3373: mip =  2.471864739e+011 >=  4.554063146e+010  81.6% (153; 9)
    ## +  3992: >>>>>  1.727665160e+011 >=  4.554063146e+010  73.6% (215; 9)
    ## +  4909: mip =  1.727665160e+011 >=  4.865716627e+010  71.8% (257; 95)
    ## +  5812: mip =  1.727665160e+011 >=  5.055920563e+010  70.7% (335; 97)
    ## +  6340: >>>>>  1.696382545e+011 >=  5.055920563e+010  70.2% (391; 97)
    ## +  7741: mip =  1.696382545e+011 >=  6.883820897e+010  59.4% (433; 139)
    ## +  9972: mip =  1.696382545e+011 >=  7.245957448e+010  57.3% (533; 147)
    ## + 12907: mip =  1.696382545e+011 >=  7.994939205e+010  52.9% (606; 167)
    ## + 16548: mip =  1.696382545e+011 >=  8.834692615e+010  47.9% (741; 185)
    ## + 19445: mip =  1.696382545e+011 >=  9.103993846e+010  46.3% (901; 197)
    ## + 21935: mip =  1.696382545e+011 >=  9.421738460e+010  44.5% (1029; 209)
    ## + 23916: >>>>>  1.684059921e+011 >=  9.510877569e+010  43.5% (1152; 216)
    ## Time used: 60.0 secs.  Memory used: 12.2 Mb.
    ## + 26361: mip =  1.684059921e+011 >=  9.623257085e+010  42.9% (1263; 243)
    ## + 28514: mip =  1.684059921e+011 >=  9.675488984e+010  42.5% (1361; 252)
    ## + 28975: >>>>>  1.639851247e+011 >=  9.675488984e+010  41.0% (1394; 252)
    ## + 30405: mip =  1.639851247e+011 >=  9.696464215e+010  40.9% (1417; 335)
    ## + 32959: mip =  1.639851247e+011 >=  9.763099726e+010  40.5% (1572; 341)
    ## + 35668: mip =  1.639851247e+011 >=  9.847701723e+010  39.9% (1686; 358)
    ## + 36013: >>>>>  1.614383733e+011 >=  9.847701723e+010  39.0% (1713; 358)
    ## + 37107: mip =  1.614383733e+011 >=  9.870858723e+010  38.9% (1729; 478)
    ## + 37827: mip =  1.614383733e+011 >=  9.913139972e+010  38.6% (1772; 483)
    ## + 39063: mip =  1.614383733e+011 >=  9.951596440e+010  38.4% (1850; 491)
    ## + 40331: mip =  1.614383733e+011 >=  1.000562929e+011  38.0% (1929; 498)
    ## + 41458: mip =  1.614383733e+011 >=  1.004503749e+011  37.8% (1991; 504)
    ## + 41946: mip =  1.614383733e+011 >=  1.005681215e+011  37.7% (2023; 508)
    ## + 42307: mip =  1.614383733e+011 >=  1.005681215e+011  37.7% (2058; 508)
    ## Time used: 120.0 secs.  Memory used: 18.3 Mb.
    ## + 42744: mip =  1.614383733e+011 >=  1.007367082e+011  37.6% (2082; 511)
    ## + 43108: mip =  1.614383733e+011 >=  1.007386899e+011  37.6% (2106; 512)
    ## + 44343: mip =  1.614383733e+011 >=  1.011915216e+011  37.3% (2145; 520)
    ## + 45700: mip =  1.614383733e+011 >=  1.017297278e+011  37.0% (2195; 528)
    ## + 45836: >>>>>  1.606941897e+011 >=  1.017297278e+011  36.7% (2205; 528)
    ## + 46866: mip =  1.606941897e+011 >=  1.018262440e+011  36.6% (2221; 586)
    ## + 48285: mip =  1.606941897e+011 >=  1.021944294e+011  36.4% (2292; 591)
    ## + 49187: >>>>>  1.554849223e+011 >=  1.022479093e+011  34.2% (2364; 594)
    ## + 51304: mip =  1.554849223e+011 >=  1.030384324e+011  33.7% (2251; 1009)
    ## + 53279: mip =  1.554849223e+011 >=  1.034269426e+011  33.5% (2328; 1021)
    ## + 55290: mip =  1.554849223e+011 >=  1.041166757e+011  33.0% (2398; 1035)
    ## + 57261: mip =  1.554849223e+011 >=  1.044658181e+011  32.8% (2463; 1051)
    ## + 59154: mip =  1.554849223e+011 >=  1.048003420e+011  32.6% (2558; 1061)
    ## Time used: 180.0 secs.  Memory used: 24.4 Mb.
    ## + 62462: mip =  1.554849223e+011 >=  1.054522680e+011  32.2% (2697; 1083)
    ## + 65376: mip =  1.554849223e+011 >=  1.063379951e+011  31.6% (2800; 1104)
    ## + 68418: mip =  1.554849223e+011 >=  1.069482586e+011  31.2% (2939; 1126)
    ## + 70481: >>>>>  1.553848283e+011 >=  1.072214176e+011  31.0% (3032; 1138)
    ## + 74304: mip =  1.553848283e+011 >=  1.081473004e+011  30.4% (3136; 1186)
    ## + 76783: mip =  1.553848283e+011 >=  1.091054517e+011  29.8% (3227; 1209)
    ## + 78519: mip =  1.553848283e+011 >=  1.096990007e+011  29.4% (3305; 1230)
    ## + 81526: mip =  1.553848283e+011 >=  1.103763412e+011  29.0% (3389; 1254)
    ## + 84445: mip =  1.553848283e+011 >=  1.111295211e+011  28.5% (3468; 1277)
    ## + 86616: mip =  1.553848283e+011 >=  1.114997549e+011  28.2% (3586; 1294)
    ## + 91428: mip =  1.553848283e+011 >=  1.131246097e+011  27.2% (3682; 1340)
    ## + 94937: mip =  1.553848283e+011 >=  1.139443486e+011  26.7% (3819; 1366)
    ## + 98739: mip =  1.553848283e+011 >=  1.147728034e+011  26.1% (3895; 1413)
    ## Time used: 240.0 secs.  Memory used: 42.1 Mb.
    ## +103324: mip =  1.553848283e+011 >=  1.157636978e+011  25.5% (3972; 1461)
    ## +106573: mip =  1.553848283e+011 >=  1.164629617e+011  25.0% (4042; 1493)
    ## +109767: mip =  1.553848283e+011 >=  1.172559761e+011  24.5% (4107; 1529)
    ## +112737: mip =  1.553848283e+011 >=  1.177897284e+011  24.2% (4193; 1562)
    ## +116946: mip =  1.553848283e+011 >=  1.185077799e+011  23.7% (4254; 1623)
    ## +120086: mip =  1.553848283e+011 >=  1.191591824e+011  23.3% (4306; 1659)
    ## +122308: mip =  1.553848283e+011 >=  1.193954750e+011  23.2% (4394; 1683)
    ## +127299: mip =  1.553848283e+011 >=  1.203687891e+011  22.5% (4457; 1750)
    ## +132591: mip =  1.553848283e+011 >=  1.213044660e+011  21.9% (4509; 1821)
    ## +134280: mip =  1.553848283e+011 >=  1.214901120e+011  21.8% (4608; 1836)
    ## +137274: mip =  1.553848283e+011 >=  1.219649831e+011  21.5% (4691; 1871)
    ## +140006: mip =  1.553848283e+011 >=  1.222429135e+011  21.3% (4769; 1900)
    ## Time used: 300.0 secs.  Memory used: 59.1 Mb.
    ## +144421: mip =  1.553848283e+011 >=  1.227463861e+011  21.0% (4827; 1945)
    ## +147540: mip =  1.553848283e+011 >=  1.230969121e+011  20.8% (4911; 1979)
    ## +151383: mip =  1.553848283e+011 >=  1.236112644e+011  20.4% (4931; 2031)
    ## +156493: mip =  1.553848283e+011 >=  1.242688583e+011  20.0% (4987; 2095)
    ## +159244: mip =  1.553848283e+011 >=  1.244700106e+011  19.9% (5057; 2123)
    ## +161888: mip =  1.553848283e+011 >=  1.248054385e+011  19.7% (5138; 2152)
    ## +165792: mip =  1.553848283e+011 >=  1.252464388e+011  19.4% (5200; 2206)
    ## +169684: mip =  1.553848283e+011 >=  1.255088965e+011  19.2% (5291; 2250)
    ## +173295: mip =  1.553848283e+011 >=  1.260425989e+011  18.9% (5338; 2314)
    ## +178039: mip =  1.553848283e+011 >=  1.264647315e+011  18.6% (5354; 2393)
    ## +178334: >>>>>  1.544210002e+011 >=  1.264647315e+011  18.1% (5393; 2393)
    ## +180804: mip =  1.544210002e+011 >=  1.268062325e+011  17.9% (5325; 2615)
    ## +184333: mip =  1.544210002e+011 >=  1.271953921e+011  17.6% (5324; 2674)
    ## Time used: 360.0 secs.  Memory used: 72.4 Mb.
    ## +188805: mip =  1.544210002e+011 >=  1.276289650e+011  17.3% (5366; 2745)
    ## +192984: mip =  1.544210002e+011 >=  1.279414089e+011  17.1% (5349; 2817)
    ## +196007: mip =  1.544210002e+011 >=  1.281346020e+011  17.0% (5423; 2853)
    ## +198825: mip =  1.544210002e+011 >=  1.282897155e+011  16.9% (5477; 2885)
    ## +203874: mip =  1.544210002e+011 >=  1.287042796e+011  16.7% (5518; 2956)
    ## +206588: mip =  1.544210002e+011 >=  1.289153796e+011  16.5% (5574; 2997)
    ## +209256: mip =  1.544210002e+011 >=  1.291392939e+011  16.4% (5594; 3035)
    ## +212112: mip =  1.544210002e+011 >=  1.292681578e+011  16.3% (5627; 3078)
    ## +213625: mip =  1.544210002e+011 >=  1.294015019e+011  16.2% (5630; 3105)
    ## +215001: mip =  1.544210002e+011 >=  1.295322576e+011  16.1% (5637; 3125)
    ## +215327: mip =  1.544210002e+011 >=  1.295664783e+011  16.1% (5649; 3130)
    ## +216691: mip =  1.544210002e+011 >=  1.296563689e+011  16.0% (5659; 3149)
    ## Time used: 420.0 secs.  Memory used: 81.7 Mb.
    ## +217770: mip =  1.544210002e+011 >=  1.297691381e+011  16.0% (5652; 3169)
    ## +218756: mip =  1.544210002e+011 >=  1.299591222e+011  15.8% (5673; 3186)
    ## +220396: mip =  1.544210002e+011 >=  1.299841889e+011  15.8% (5708; 3205)
    ## +221870: mip =  1.544210002e+011 >=  1.301411195e+011  15.7% (5717; 3240)
    ## +224148: mip =  1.544210002e+011 >=  1.303863388e+011  15.6% (5750; 3275)
    ## +227778: mip =  1.544210002e+011 >=  1.306290455e+011  15.4% (5729; 3342)
    ## +231139: mip =  1.544210002e+011 >=  1.308249008e+011  15.3% (5759; 3395)
    ## +233691: mip =  1.544210002e+011 >=  1.310606535e+011  15.1% (5795; 3437)
    ## +237765: mip =  1.544210002e+011 >=  1.313387537e+011  14.9% (5805; 3500)
    ## +238720: mip =  1.544210002e+011 >=  1.313387537e+011  14.9% (5810; 3512)
    ## +239544: mip =  1.544210002e+011 >=  1.314731715e+011  14.9% (5811; 3535)
    ## +241520: mip =  1.544210002e+011 >=  1.315262374e+011  14.8% (5828; 3566)
    ## Time used: 480.0 secs.  Memory used: 88.5 Mb.
    ## +243826: mip =  1.544210002e+011 >=  1.315930367e+011  14.8% (5889; 3593)
    ## +245369: mip =  1.544210002e+011 >=  1.316703369e+011  14.7% (5927; 3614)
    ## +248565: mip =  1.544210002e+011 >=  1.320118165e+011  14.5% (5946; 3670)
    ## +251118: mip =  1.544210002e+011 >=  1.320133353e+011  14.5% (6014; 3707)
    ## +252601: mip =  1.544210002e+011 >=  1.321203528e+011  14.4% (6053; 3728)
    ## +253462: mip =  1.544210002e+011 >=  1.322603167e+011  14.4% (6087; 3741)
    ## +255379: mip =  1.544210002e+011 >=  1.322937001e+011  14.3% (6101; 3769)
    ## +257368: mip =  1.544210002e+011 >=  1.323668991e+011  14.3% (6124; 3798)
    ## +258872: mip =  1.544210002e+011 >=  1.323919239e+011  14.3% (6167; 3817)
    ## +262161: mip =  1.544210002e+011 >=  1.325655944e+011  14.2% (6178; 3869)
    ## +263687: mip =  1.544210002e+011 >=  1.325744575e+011  14.1% (6243; 3887)
    ## +265663: mip =  1.544210002e+011 >=  1.326615411e+011  14.1% (6278; 3911)
    ## Time used: 540.0 secs.  Memory used: 96.8 Mb.
    ## +266848: mip =  1.544210002e+011 >=  1.327986933e+011  14.0% (6342; 3927)
    ## +269146: mip =  1.544210002e+011 >=  1.328143609e+011  14.0% (6401; 3961)
    ## +270624: >>>>>  1.540105216e+011 >=  1.329591942e+011  13.7% (6430; 3986)
    ## +273304: mip =  1.540105216e+011 >=  1.330629414e+011  13.6% (6364; 4166)
    ## +276471: mip =  1.540105216e+011 >=  1.332830944e+011  13.5% (6358; 4222)
    ## +278386: mip =  1.540105216e+011 >=  1.333451551e+011  13.4% (6433; 4245)
    ## +282092: mip =  1.540105216e+011 >=  1.335532219e+011  13.3% (6467; 4306)
    ## +285052: mip =  1.540105216e+011 >=  1.337236918e+011  13.2% (6516; 4358)
    ## +288642: mip =  1.540105216e+011 >=  1.339852265e+011  13.0% (6524; 4432)
    ## +292959: mip =  1.540105216e+011 >=  1.342027238e+011  12.9% (6527; 4520)
    ## +296889: >>>>>  1.480105129e+011 >=  1.344432148e+011   9.2% (6543; 4590)
    ## +299941: mip =  1.480105129e+011 >=  1.346597125e+011   9.0% (4807; 7874)
    ## Time used: 600.0 secs.  Memory used: 103.6 Mb.
    ## +303093: mip =  1.480105129e+011 >=  1.348895240e+011   8.9% (4785; 7982)
    ## +305499: mip =  1.480105129e+011 >=  1.350483478e+011   8.8% (4790; 8056)
    ## +308920: mip =  1.480105129e+011 >=  1.352815131e+011   8.6% (4795; 8142)
    ## +313083: mip =  1.480105129e+011 >=  1.356665887e+011   8.3% (4741; 8271)
    ## +315277: mip =  1.480105129e+011 >=  1.357380099e+011   8.3% (4735; 8331)
    ## +316923: mip =  1.480105129e+011 >=  1.358524156e+011   8.2% (4714; 8385)
    ## +318281: mip =  1.480105129e+011 >=  1.359916040e+011   8.1% (4702; 8428)
    ## +321371: mip =  1.480105129e+011 >=  1.361668226e+011   8.0% (4670; 8515)
    ## +323747: mip =  1.480105129e+011 >=  1.363512712e+011   7.9% (4618; 8621)
    ## +325588: mip =  1.480105129e+011 >=  1.365163292e+011   7.8% (4607; 8680)
    ## +327490: mip =  1.480105129e+011 >=  1.366743286e+011   7.7% (4561; 8759)
    ## +330790: mip =  1.480105129e+011 >=  1.369557757e+011   7.5% (4518; 8878)
    ## Time used: 660.0 secs.  Memory used: 103.6 Mb.
    ## +333145: mip =  1.480105129e+011 >=  1.371072136e+011   7.4% (4512; 8964)
    ## +334720: mip =  1.480105129e+011 >=  1.373500690e+011   7.2% (4502; 9019)
    ## +337811: mip =  1.480105129e+011 >=  1.375079751e+011   7.1% (4453; 9123)
    ## +340647: mip =  1.480105129e+011 >=  1.376912992e+011   7.0% (4421; 9226)
    ## +343980: mip =  1.480105129e+011 >=  1.380238416e+011   6.7% (4368; 9381)
    ## +347234: mip =  1.480105129e+011 >=  1.382135732e+011   6.6% (4328; 9507)
    ## +350701: mip =  1.480105129e+011 >=  1.385139435e+011   6.4% (4239; 9668)
    ## +353355: mip =  1.480105129e+011 >=  1.388578145e+011   6.2% (4185; 9803)
    ## +357138: mip =  1.480105129e+011 >=  1.390618012e+011   6.0% (4082; 9981)
    ## +360454: mip =  1.480105129e+011 >=  1.395125216e+011   5.7% (3997; 10144)
    ## +364625: mip =  1.480105129e+011 >=  1.397159772e+011   5.6% (3869; 10357)
    ## +368835: mip =  1.480105129e+011 >=  1.401314700e+011   5.3% (3723; 10555)
    ## Time used: 720.0 secs.  Memory used: 103.6 Mb.
    ## +372053: mip =  1.480105129e+011 >=  1.403665839e+011   5.2% (3604; 10747)
    ## +374438: mip =  1.480105129e+011 >=  1.405357962e+011   5.1% (3531; 10903)
    ## +377066: mip =  1.480105129e+011 >=  1.406817298e+011   5.0% (3463; 11033)
    ## +379339: mip =  1.480105129e+011 >=  1.408740006e+011   4.8% (3377; 11163)
    ## +380307: mip =  1.480105129e+011 >=  1.410290178e+011   4.7% (3336; 11240)
    ## +381268: mip =  1.480105129e+011 >=  1.410676064e+011   4.7% (3297; 11309)
    ## +382600: mip =  1.480105129e+011 >=  1.411524345e+011   4.6% (3263; 11368)
    ## +384745: mip =  1.480105129e+011 >=  1.414044949e+011   4.5% (3200; 11479)
    ## +387747: mip =  1.480105129e+011 >=  1.416937197e+011   4.3% (3075; 11688)
    ## +390543: mip =  1.480105129e+011 >=  1.419793596e+011   4.1% (2941; 11929)
    ## +392696: mip =  1.480105129e+011 >=  1.422127514e+011   3.9% (2825; 12128)
    ## +394928: mip =  1.480105129e+011 >=  1.425814561e+011   3.7% (2709; 12318)
    ## Time used: 780.0 secs.  Memory used: 103.6 Mb.
    ## +396443: mip =  1.480105129e+011 >=  1.426753033e+011   3.6% (2610; 12493)
    ## +398422: >>>>>  1.475556012e+011 >=  1.430664428e+011   3.0% (2476; 12759)
    ## +400652: mip =  1.475556012e+011 >=  1.434209561e+011   2.8% (2095; 13603)
    ## +403078: mip =  1.475556012e+011 >=  1.436419746e+011   2.7% (1925; 13998)
    ## +405570: mip =  1.475556012e+011 >=  1.440706652e+011   2.4% (1709; 14584)
    ## +408298: mip =  1.475556012e+011 >=  1.445501905e+011   2.0% (1468; 15236)
    ## +411210: mip =  1.475556012e+011 >=  1.451410782e+011   1.6% (1180; 16157)
    ## +413508: mip =  1.475556012e+011 >=  1.457081853e+011   1.3% (916; 17091)
    ## +416288: mip =  1.475556012e+011 >=  1.464200564e+011   0.8% (530; 18878)
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
                SELECTCOLUMNS ( ChessLeagueCup; "Lat_1"; ChessLeagueCup[Lat]; "Long_1"; ChessLeagueCup[Long]; "ClubNaam1"; ChessLeagueCup[ClubNaam]);
                SELECTCOLUMNS ( ChessLeagueCup; "Lat_2"; ChessLeagueCup[Lat]; "Long_2"; ChessLeagueCup[Long]; "ClubNaam2"; ChessLeagueCup[ClubNaam])
           );
           [ClubNaam1] <> [ClubNaam2]
        ); 
        6371 *((2*ASIN(SQRT((SIN((RADIANS([Lat_1])-RADIANS([Lat_2]))/2)^2)+COS(RADIANS([Lat_1]))*COS(RADIANS([Lat_2]))*(SIN((RADIANS([Long_1])-RADIANS([Long_2]))/2)^2)))))
    )

It would have been nice if this can be added in a kind of UDF as now the formula is copied a couple of times.

After all these analysis, reporting and writing, it is time to do some real calculations again and bring home that Cup ones more!

You can view the Power BI report [here](https://app.powerbi.com/view?r=eyJrIjoiZWIzOTBiYmEtMzc5OC00NmE0LWJhMWItMmQyMDAwNzlkMTgwIiwidCI6Ijg3NGM1MzA1LWI0MDktNGU5Ni04ODhiLTQ4ODViNWQ0ZDYwNiIsImMiOjl9).

All the code can be found on GitHub [ChessLeagueCup](https://github.com/Ruud-Janssen/ChessLeagueCup).
