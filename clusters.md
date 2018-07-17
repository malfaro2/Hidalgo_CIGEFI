Clusters
================

Construcción de clusters con las observaciones de 74 estaciones en CA
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

Clustering
----------

-   OPCIÓN 1: Utilizar las variancias y luego agrupar por magnitud.

``` r
aa<-round(c(apply(caudal[,-c(1:3)],2,var)),4)
loca$rank_var <- cut_number(aa,5)
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
```

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=18.8,-93.6645&zoom=5&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

``` r
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=rank_var))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Descripción de los clusters:

``` r
medianas <- apply(caudal[,-c(1:3)],2,median)
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","Mediana_de_cluster")
## Area promedio de cada cluster:
round(tapply(data$area, data$rank_var,mean),4)
```

    ##     [0,0.19]  (0.19,0.71]  (0.71,3.14]  (3.14,9.64]   (9.64,113] 
    ##    54370.667     9323.790     4055.396     6172.333 80268425.533

``` r
## Mediana del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$rank_var,median),4)
```

    ##    [0,0.19] (0.19,0.71] (0.71,3.14] (3.14,9.64]  (9.64,113] 
    ##      0.0215      0.1631      0.4938      1.2625      4.7279

-   OPCIÓN 2: k-means usando los componentes de PCA.

Primero, se deben calcular los PCA utilizando las anomalías en lugar de las observaciones. Como se tiene la climatología mensual para cada locación, el cálculo consiste en restar la climatología mensual a cada observación, según el mes correspondiente. Luego, se procede a hacer el PCA y por último agrupar las estaciones utilizando k-means de los primeros 10 componentes.

``` r
clima2 <- as.matrix(clima[,-1]) %x% rep(1, 11)
anomalies <- caudal[,-c(1:3)]-clima2
pc <- princomp(anomalies)
plot(pc)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
mydata <- (pc$loadings[,1:10])
fit <- kmeans(mydata, 6) 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
```

    ##   Group.1      Comp.1      Comp.2      Comp.3      Comp.4      Comp.5
    ## 1       1 -0.23102533 -0.89974452 -0.25458406 -0.03555553  0.04093526
    ## 2       2 -0.17505225  0.02100372 -0.01848551  0.08860101  0.03406538
    ## 3       3 -0.04493411  0.02000804 -0.03369640 -0.01198930 -0.04525669
    ## 4       4 -0.07236389  0.03996056 -0.02568390 -0.05189148  0.11409761
    ## 5       5 -0.09226363  0.10238656 -0.24924551 -0.01029817  0.31521900
    ## 6       6 -0.21909198  0.04687819  0.10998503  0.06257886  0.04684744
    ##         Comp.6       Comp.7       Comp.8      Comp.9      Comp.10
    ## 1  0.027942613 -0.117214555  0.047311531  0.07357872  0.004026195
    ## 2  0.046912464  0.010931621 -0.251844773  0.01360782  0.065233715
    ## 3 -0.001876355 -0.027558776 -0.004999674 -0.03049197  0.004020674
    ## 4  0.159820495  0.003223046  0.107345441  0.02723663  0.128023216
    ## 5 -0.238211378 -0.012421407  0.054284488 -0.16210708 -0.025254710
    ## 6  0.015694867 -0.033484327  0.084285252  0.17957066 -0.310820339

``` r
loca <- data.frame(loca, cluster=fit$cluster)
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=as.character(cluster)))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-2.png)

Descripción de los clusters:

``` r
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","cluster2","Mediana_de_cluster")
## Area promedio de cada cluster:
round(tapply(data$area,data$cluster2,mean),4)
```

    ##            1            2            3            4            5 
    ## 7.300000e+01 1.844143e+03 2.104611e+04 8.076500e+03 1.454000e+03 
    ##            6 
    ## 3.010004e+08

``` r
## Medianan del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 9.4685 4.6981 0.1775 1.2963 0.9366 4.6555

-   OPCIÓN 3: Cluster para las series de tiempo.

En este caso también se deben calcular las anomalías. Luego, se procede a aplicar el algoritmo de TSclust, que se describe aquí: <https://www.jstatsoft.org/article/view/v062i01/v62i01.pdf>

``` r
dpred <- diss(anomalies, "ACF", p=0.05)
hc.pred <- hclust(dpred)
plot(hc.pred)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
aa<-cutree(hc.pred, k = 6)
loca <- data.frame(loca, clusterTS=aa )
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=as.character(clusterTS)))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-2.png)

Descripción de los clusters:

``` r
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","cluster2","clusterTS", "Mediana_de_cluster")
## Area promedio de cada cluster:
round(tapply(data$area, data$clusterTS,mean),4)
```

    ##            1            2            3            4            5 
    ##    12408.600   111564.200     4427.158    13718.250     8339.222 
    ##            6 
    ## 70828364.091

``` r
## Mediana del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 9.4685 4.6981 0.1775 1.2963 0.9366 4.6555
