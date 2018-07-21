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

    ##   Group.1      Comp.1      Comp.2       Comp.3      Comp.4       Comp.5
    ## 1       1 -0.07138688 -0.03575786  0.053521355  0.06858059  0.239623080
    ## 2       2 -0.05526741 -0.01910186 -0.002173103  0.01800686  0.008018061
    ## 3       3 -0.37626234 -0.06992115  0.062077039 -0.19469820  0.010506763
    ## 4       4 -0.24592266  0.91456306 -0.166646054  0.07449124  0.037677192
    ## 5       5 -0.21298497 -0.02347980  0.026609701 -0.07421125 -0.034856633
    ## 6       6 -0.12027666 -0.10233604 -0.286085611  0.10603650  0.026838837
    ##        Comp.6       Comp.7       Comp.8       Comp.9       Comp.10
    ## 1 -0.05560944 -0.133807882 -0.066066737 -0.160753964 -0.0208977881
    ## 2 -0.09832156 -0.004165624  0.006686226  0.010004756  0.1042556856
    ## 3  0.27822100 -0.054305194 -0.336906070  0.211989055  0.0261137531
    ## 4 -0.11972971  0.009819237 -0.093040079 -0.003632703 -0.0027044916
    ## 5 -0.02891006 -0.039503197  0.202251556  0.007406858 -0.0869365436
    ## 6 -0.01058128  0.108502863 -0.045613799 -0.048656972  0.0003403794

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

    ## [1]      1324      6971 602000668        73      1511      7266

``` r
## Medianan del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 0.6966 0.3910 9.0093 9.4685 4.5827 1.8953

-   OPCIÓN 3: Cluster para las series de tiempo.

En este caso también se deben calcular las anomalías. Luego, se procede a aplicar el algoritmo de TSclust, que se describe aquí: <https://www.jstatsoft.org/article/view/v062i01/v62i01.pdf>

``` r
dpred <- diss(anomalies, "ACF", p=0.05)
hc.pred <- hclust(dpred)
plot(hc.pred)
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
aa<-cutree(hc.pred, k = 6)
loca <- data.frame(loca, clusterTS=aa )
ggmap(sq_map) + geom_point(data = loca, mapping = aes(x = Longitud, y = Latitud, colour=as.character(clusterTS)))
```

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)

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
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 0.6966 0.3910 9.0093 9.4685 4.5827 1.8953
