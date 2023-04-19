pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer", "tidyverse","rgdal","spdep","knitr","kableExtra","tmap","gtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando um shapefile -------------------------------------------------
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

# Características básicas do objeto shp_sp
summary(shp_sp)

# Classe e tipo do objeto carregado
class(shp_sp)
typeof(shp_sp)


# Acessando a base de dados e outros componentes do objeto shp_sp ---------


# Para acessar a base de dados de um shapefile, devemos utilizar o operador @:
shp_sp@data

shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 16)

# Para acessar as variáveis da base de dados atrelada ao shapefile, utilizaremos
# o operador $:
shp_sp@data$NM_MUNICIP
shp_sp@data$CD_GEOCMU

# Carregando uma base de dados real a respeito dos casos de covid
# nos municípios de SP:

dados_covid <- read.csv('dados_covid19.csv', sep = ';', dec = ',', stringsAsFactors = T)

# Para combinar os dados do objeto dados_sp com a base de dados de nosso 
# shapefile, podemos utilizar a função merge():
shp_dados_sp <- merge(x = shp_sp,
                      y = dados_covid,
                      by.x = "CD_GEOCMU",
                      by.y = "Cod_IBGE")

shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Salvando nosso shapefile:
writeOGR(obj = shp_dados_sp, 
         layer = "nosso_novo_shapefile", 
         driver = "ESRI Shapefile", 
         dsn = "shp_covid")
_____________________________________________________________________

#Vizinhanças por Contiguidade ----------------------------------------

# Carregando um shapefile:
shp_sp <- readOGR(dsn = "shp_covid", layer = "nosso_novo_shapefile")
  
# Visualizando o shapefile
plot(shp_sp)

  
# Estabelecendo vizinhanças por contiguidade, critério queen:
vizinhos_queen <- poly2nb(pl = shp_sp,
                            queen = TRUE,
                            row.names = shp_sp@data$NM_MUNI)
  
# Visualizando a vizinhança estabelecida:
plot(shp_sp, border = "lightgray")
plot(vizinhos_queen, 
       coordinates(shp_sp), 
       add = TRUE, 
       col = "#33638DFF")
  
# Informações relevantes sobre a vizinhança queen estabelecida:
summary(vizinhos_queen)
  
# Ok! Cadê a matriz W?
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                          style = "B",
                          zero.policy = TRUE)
  
# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_queen) <- shp_sp@data$NM_MUNI
rownames(matrizW_queen) <- shp_sp@data$NM_MUNI
View(matrizW_queen)


# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável Total de mortes por municipio (obts_20), vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = "obts_20", style = "quantile", n = 10, palette = "-magma",
          title = "Óbitos 2020") +
  tm_legend(position=c("left","bottom")) +
  tm_borders()

# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável Total de casos por municipio (cass_20), vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = "cass_20", style = "quantile", n = 10, palette = "-magma",
          title = "Casos 2020") +
  tm_legend(position=c("left","bottom")) +
  tm_borders()

# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável Total de casos por municipio (cass_21), vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = "cass_21", style = "quantile", n = 10, palette = "-magma",
          title = "Casos 21") +
  tm_legend(position=c("left","bottom")) +
  tm_borders()

# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável Total de mortes por municipio (obts_21), vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = "obts_21", style = "quantile", n = 10, palette = "-magma",
          title = "Óbitos 2021") +
  tm_legend(position=c("left","bottom")) +
  tm_borders()

# 01) Autocorrelação Global – a Estatística I de Moran --------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen)
class(listw_queen)

# Após isso, poderemos utilizar a função
options(scipen = 999)

moran.test(x = shp_sp@data$cass_20, 
           listw = listw_queen, 
           zero.policy = TRUE)

moran.test(x = shp_sp@data$obts_20, 
           listw = listw_queen, 
           zero.policy = TRUE)

moran.test(x = shp_sp@data$cass_21, 
           listw = listw_queen, 
           zero.policy = TRUE)

moran.test(x = shp_sp@data$obts_21, 
           listw = listw_queen, 
           zero.policy = TRUE)

# 02) O Diagrama da Estatística I de Moran --------------------------------
moran.plot(x = shp_sp@data$cass_20, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Casos confirmados por 100 mil habitantes/2020", 
           ylab = "Casos confirmados por 100 mil habitantes/2020 - Espacialmente Defasado",
           pch = 19)

moran.plot(x = shp_sp@data$obts_20, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Óbitos confirmados por 100 mil habitantes/2020", 
           ylab = "Óbitos confirmados por 100 mil habitantes/2020 - Espacialmente Defasado",
           pch = 19)

moran.plot(x = shp_sp@data$cass_21, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Casos confirmados por 100 mil habitantes/2021", 
           ylab = "Casos confirmados por 100 mil habitantes/2021 - Espacialmente Defasado",
           pch = 19)

moran.plot(x = shp_sp@data$obts_21, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Óbitos confirmados por 100 mil habitantes/2021", 
           ylab = "Óbitos confirmados por 100 mil habitantes/2021 - Espacialmente Defasado",
           pch = 19)

# 03) Autocorrelação Local – a Estatística Moran Local --------------------

# Seguindo o proposto por Anselin (1995), devemos padronizar em linha nossa 
# matriz de pesos espaciais W:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)

listw_queen <- mat2listw(matrizW_queen_linha)

## casos 2020##

# Considerando a variável casos_20 do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local_casos_20 <- localmoran(x = shp_sp@data$cass_20, 
                          listw = listw_queen, 
                          zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa_casos_20 <- cbind(shp_sp, moran_local_casos_20)

# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local_casos_20))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_c20 <- shp_sp@data$cass_20 - mean(shp_sp@data$cass_20)

centro_c20

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_c20 <- moran_local_casos_20[,1] - mean(moran_local_casos_20[,1])

centro_moran_local_c20

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_c20 > 0 & centro_moran_local_c20 > 0] <- "HH"
quadrantes[centro_c20 > 0 & centro_moran_local_c20 < 0] <- "HL"
quadrantes[centro_c20 < 0 & centro_moran_local_c20 > 0] <- "LH"
quadrantes[centro_c20 < 0 & centro_moran_local_c20 < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local_casos_20[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_casos_20@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa_casos_20) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_layout(title = "Casos 2020")
tm_borders()

## obitos 2020##

# Considerando a variável casos_20 do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local_obitos_20 <- localmoran(x = shp_sp@data$obts_20, 
                                   listw = listw_queen, 
                                   zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa_obitos_20 <- cbind(shp_sp, moran_local_obitos_20)

# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local_obitos_20))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_ob20 <- shp_sp@data$obts_20 - mean(shp_sp@data$obts_20)

centro_ob20

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_ob20 <- moran_local_obitos_20[,1] - mean(moran_local_obitos_20[,1])

centro_moran_local_ob20

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_ob20 > 0 & centro_moran_local_ob20 > 0] <- "HH"
quadrantes[centro_ob20 > 0 & centro_moran_local_ob20 < 0] <- "HL"
quadrantes[centro_ob20 < 0 & centro_moran_local_ob20 > 0] <- "LH"
quadrantes[centro_ob20 < 0 & centro_moran_local_ob20 < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local_obitos_20[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_obitos_20@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa_obitos_20) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_layout(title = "Óbitos 2020") +
tm_borders()

## casos 2021##

# Considerando a variável casos_21 do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local_casos_21 <- localmoran(x = shp_sp@data$cass_21, 
                                   listw = listw_queen, 
                                   zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa_casos_21 <- cbind(shp_sp, moran_local_casos_21)

# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local_casos_21))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_c21 <- shp_sp@data$cass_21 - mean(shp_sp@data$cass_21)

centro_c21

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_c21 <- moran_local_casos_21[,1] - mean(moran_local_casos_21[,1])

centro_moran_local_c21

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_c21 > 0 & centro_moran_local_c21 > 0] <- "HH"
quadrantes[centro_c21 > 0 & centro_moran_local_c21 < 0] <- "HL"
quadrantes[centro_c21 < 0 & centro_moran_local_c21 > 0] <- "LH"
quadrantes[centro_c21 < 0 & centro_moran_local_c21 < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local_casos_21[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_casos_21@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa_casos_21) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_layout(title = "Casos 2021") +
tm_borders()

## obitos 2021##

# Considerando a variável casos_20 do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local_obitos_21 <- localmoran(x = shp_sp@data$obts_21, 
                                    listw = listw_queen, 
                                    zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa_obitos_21 <- cbind(shp_sp, moran_local_obitos_21)

# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local_obitos_21))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_ob21 <- shp_sp@data$obts_21 - mean(shp_sp@data$obts_21)

centro_ob21

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_ob21 <- moran_local_obitos_21[,1] - mean(moran_local_obitos_21[,1])

centro_moran_local_ob21

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_ob21 > 0 & centro_moran_local_ob21 > 0] <- "HH"
quadrantes[centro_ob21 > 0 & centro_moran_local_ob21 < 0] <- "HL"
quadrantes[centro_ob21 < 0 & centro_moran_local_ob21 > 0] <- "LH"
quadrantes[centro_ob21 < 0 & centro_moran_local_ob21 < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local_obitos_21[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_obitos_21@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa_obitos_21) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_layout(title = "Óbitos 2021") +
tm_borders()
