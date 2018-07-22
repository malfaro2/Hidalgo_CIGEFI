Clusters
================

Construcción de clusters con las observaciones de 41 estaciones en CA
=====================================================================

Existen muchas opciones para agrupar los caudales de las estaciones, así que se elige trabajar con 3:

-   Distancia univariada de las variancias de cada estación.
-   k-means usando los primeros componentes de PCA.
-   Cluster para las series de tiempo utilizando <https://www.jstatsoft.org/article/view/v062i01/v62i01.pdf>

Los datos
---------

Los datos consisten en observaciones mensuales de cuadal en 74 estaciones: las fechas de observación van desde enero de 1969 hasta diciembre de 1979. Cada estación representa una cuenca hidrológica en Centro América, y en total se tienen observaciones de 12 meses en cada uno de los 11 años para 74 locaciones. También, se tienen datos de climatología mensual para cada una de las estaciones.

    ## [1] 132  74

    ## [1] 12 74

Las locaciones de las estaciones se pueden apreciar en el siguiente mapa:

    ## [1] 74  4

    ##      left    bottom     right       top 
    ## -112.1223    5.6804  -75.2067   31.9196

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=18.8,-93.6645&zoom=5&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Como las locaciones incluyen varias estaciones en el Norte de México, se realiza un corte en la latitud 20 y en la longitud -100, para obtener 41 estaciones localizadas en Centroamérica:

    ## [1] 41  4

    ##      left    bottom     right       top 
    ## -101.7405    6.7175  -76.1505   20.5115

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=13.6145,-88.9455&zoom=5&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Clustering
----------

-   OPCIÓN 1: Utilizar las variancias y luego agrupar por magnitud.

``` r
caudal2 <- caudal[,-c(1:3)][,estaciones]
aa<-round(c(apply(caudal2,2,var)),4)
loca$rank_var <- cut_number(aa,5)
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
```

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=13.6145,-88.9455&zoom=5&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

``` r
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=rank_var))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

Descripción de los clusters:

``` r
medianas <- apply(caudal2,2,median)
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","Mediana_de_cluster")
## Area promedio de cada cluster:
as.integer(tapply(data$area, data$rank_var,mean))
```

    ## [1]      5452      6800      7414      2336 150501132

``` r
## Mediana del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$rank_var,median),4)
```

    ##    [0,0.868] (0.868,3.16]     (3.16,7]     (7,23.6]   (23.6,113] 
    ##       0.2100       0.6966       1.2184       2.9613       5.8753

``` r
matplot(caudal2,type="l", col=data$rank_var)
legend("topright", levels(data$rank_var),col=1:5,cex=0.8,fill=1:5)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

-   OPCIÓN 2: k-means usando los componentes de PCA.

Primero, se deben calcular los PCA utilizando las anomalías en lugar de las observaciones. Como se tiene la climatología mensual para cada locación, el cálculo consiste en restar la climatología mensual a cada observación, según el mes correspondiente. Luego, se procede a hacer el PCA y por último agrupar las estaciones utilizando k-means de los primeros 10 componentes.

``` r
clima2 <- (as.matrix(clima[,-1]) %x% rep(1, 11))[,estaciones]
anomalies <- caudal2-clima2
pc <- princomp(anomalies)
plot(pc)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

``` r
mydata <- (pc$loadings[,1:10])
fit <- kmeans(mydata, 6) 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
```

    ##   Group.1      Comp.1      Comp.2      Comp.3        Comp.4      Comp.5
    ## 1       1 -0.13463590 -0.07126630 -0.15472253  0.0394500874  0.01534448
    ## 2       2 -0.11845462 -0.04857904  0.18338524  0.2758140488  0.18511996
    ## 3       3 -0.04140286 -0.03168384 -0.03829811 -0.0224804920  0.09577900
    ## 4       4 -0.17920240  0.01375610  0.02655788 -0.1144877691  0.07590926
    ## 5       5 -0.24592266  0.91456306 -0.16664605  0.0744912421  0.03767719
    ## 6       6 -0.22262990 -0.01853355  0.13014545 -0.0004914704 -0.21409357
    ##        Comp.6       Comp.7       Comp.8       Comp.9      Comp.10
    ## 1  0.02896805  0.070019476 -0.085889424 -0.001251744  0.004495888
    ## 2 -0.15186986  0.036780594 -0.036635744  0.253885261  0.145636885
    ## 3 -0.08529693 -0.130412535  0.004526803 -0.038252626  0.037207140
    ## 4  0.04641769  0.152081548  0.254671091 -0.033237309  0.072798053
    ## 5 -0.11972971  0.009819237 -0.093040079 -0.003632703 -0.002704492
    ## 6 -0.13728246 -0.081248504  0.038758394 -0.119961190 -0.060593126

``` r
loca <- data.frame(loca, cluster=fit$cluster)
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=as.character(cluster)))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-2.png)

Descripción de los clusters:

``` r
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","cluster2","Mediana_de_cluster")
## Area promedio de cada cluster:
round(as.integer(tapply(data$area,data$cluster2,mean)),0)
```

    ## [1] 100339702       218      6626      1851        73       947

``` r
## Medianan del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 1.4733 0.3344 0.5439 4.6981 9.4685 4.1007

Series de datos según cluster
-----------------------------

``` r
matplot(caudal2,type="l", col=data$cluster2)
legend("topright", levels(as.factor(data$cluster2)),col=1:6,cex=0.8,fill=1:6)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

Series de anomalías según cluster
---------------------------------

``` r
matplot(anomalies,type="l", col=data$cluster2)
legend("topright", levels(as.factor(data$cluster2)),col=1:6,cex=0.8,fill=1:6)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

-   OPCIÓN 3: Cluster para las series de tiempo.

En este caso también se deben calcular las anomalías. Luego, se procede a aplicar el algoritmo de TSclust, que se describe aquí: <https://www.jstatsoft.org/article/view/v062i01/v62i01.pdf>

``` r
dpred <- diss(anomalies, "ACF", p=0.05)
hc.pred <- hclust(dpred)
plot(hc.pred)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

``` r
aa<-cutree(hc.pred, k = 6)
loca <- data.frame(loca, clusterTS=aa )
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=as.character(clusterTS)))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png)

Descripción de los clusters:

``` r
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","cluster2","clusterTS", "Mediana_de_cluster")
## Area promedio de cada cluster:
as.integer(tapply(data$area, data$clusterTS,mean))
```

    ## [1]     8372 80271495     3563     2686      836     1487

``` r
## Mediana del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$clusterTS,median),4)
```

    ##      1      2      3      4      5      6 
    ## 1.0043 4.1007 0.2885 0.2879 1.4182 5.0170

Series de datos según cluster
-----------------------------

``` r
matplot(caudal2,type="l", col=data$clusterTS)
legend("topright", levels(as.factor(data$clusterTS)),col=1:6,cex=0.8,fill=1:6)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)

Series de anomalías según cluster
---------------------------------

``` r
matplot(anomalies,type="l", col=data$clusterTS)
legend("topright", levels(as.factor(data$clusterTS)),col=1:6,cex=0.8,fill=1:6)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)
