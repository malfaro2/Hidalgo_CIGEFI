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

<!-- -->

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=18.8,-93.6645&zoom=5&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

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

Primero, se deben calcular los PCA utilizando las anomalías en lugar de las observaciones. Como se tiene la climatología mensual para cada locación, el cálculo consiste en restar la climatología mensual a cada observación, según el mes correspondiente. Luego, se procede a hacer el PCA y por último agrupar las estaciones utilizando k-means.

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

    ##   Group.1      Comp.1      Comp.2      Comp.3        Comp.4       Comp.5
    ## 1       1 -0.05125520  0.02909881 -0.05445704  0.0008186019  0.026668324
    ## 2       2 -0.20806905  0.01347129  0.17347811 -0.8315766147  0.217386109
    ## 3       3 -0.06629321  0.02822561  0.01429151  0.0131680644  0.007003244
    ## 4       4 -0.12317920  0.02414893  0.06747755  0.0828433577  0.079118705
    ## 5       5 -0.07997562  0.02238608 -0.11116728  0.0253192447 -0.033255513
    ## 6       6 -0.12008978 -0.05523674 -0.05684092 -0.0314264300 -0.106779423
    ##        Comp.6       Comp.7       Comp.8       Comp.9      Comp.10
    ## 1 -0.01780312  0.006284231  0.009981032 -0.007468676  0.004360678
    ## 2  0.26814490  0.267250381 -0.091022408 -0.068145605 -0.018697883
    ## 3  0.10964016 -0.157906345  0.045651317 -0.067226617 -0.050433257
    ## 4  0.07604138  0.004967214 -0.120290036 -0.004541390  0.168701354
    ## 5  0.06639439 -0.001874692 -0.136263009  0.011557863 -0.003318273
    ## 6 -0.06934770  0.001143187  0.038789309  0.030578343  0.013384317
    ##         Comp.11     Comp.12      Comp.13      Comp.14     Comp.15
    ## 1  0.0070690943 -0.01101407  0.024859704 -0.028505107  0.01400534
    ## 2 -0.0388565369 -0.03372980  0.007952296  0.053920668  0.04383237
    ## 3  0.0466489518 -0.01989926  0.016041620  0.043139092  0.01470809
    ## 4  0.0003574004  0.10137164 -0.033743081  0.099748296 -0.14868432
    ## 5 -0.0115762082 -0.10129288  0.019149540 -0.009139446  0.07220448
    ## 6 -0.0544634196  0.07928775  0.014895904  0.061572854 -0.01607643
    ##        Comp.16      Comp.17      Comp.18     Comp.19      Comp.20
    ## 1 -0.021864079 -0.005406755  0.004599997 -0.03278429 -0.008874313
    ## 2  0.026368912  0.048220831 -0.002071238  0.07376977 -0.031880265
    ## 3  0.019750806  0.025634377 -0.036808281  0.01909085 -0.073803755
    ## 4  0.028094769 -0.012262011 -0.081192736 -0.08843847  0.070592273
    ## 5  0.006401919 -0.182405315  0.023480598  0.12989158  0.086875537
    ## 6 -0.065994938  0.002731419 -0.035894864  0.05991687 -0.053046797

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-2.png)

Descripción de los clusters:

``` r
data <- as_tibble(cbind(loca,medianas))
names(data) <- c("cod","lat","lon","area","rank_var","cluster2","Mediana_de_cluster")
## Area promedio de cada cluster:
round(tapply(data$area, data$cluster2,mean),4)
```

    ##            1            2            3            4            5 
    ## 2.867119e+04 2.000000e+02 2.012262e+03 1.888167e+03 2.579857e+03 
    ##            6 
    ## 1.094585e+08

``` r
## Medianan del Caudal de cada cluster:                                         
round(tapply(data$Mediana_de_cluster, data$cluster2,median),4)
```

    ##      1      2      3      4      5      6 
    ## 0.2077 4.7279 0.5400 1.6557 0.2013 0.2057

-   OPCIÓN 3: Cluster para las series de tiempo.

En este caso también se deben calcular las anomalías. Luego, se procede a aplicar el algoritmo de TSclust, que se describe aquí: <https://www.jstatsoft.org/article/view/v062i01/v62i01.pdf>

![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)![](clusters_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-2.png)

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
    ## 0.2077 4.7279 0.5400 1.6557 0.2013 0.2057
