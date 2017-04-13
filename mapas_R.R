#
setwd("Y:/Marina/Mapas Vereadores SP 2016")

#pegando os pacotes que vamos usar
install.packages("dplyr")
install.packages("ggmap")
install.packages("maptools")
install.packages("spdep")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("readr")
install.packages("data.table")
install.packages("stringi")
install.packages("tidyr")
install.packages("stringr")

library(dplyr)
library(ggmap)
library(maptools)
library(spdep)
library(leaflet)
library(RColorBrewer)
library(readr)
library(data.table)
library(stringi)
library(tidyr)
library(stringr)

#baixando o arquivo com os endereços das zonas eleitorais de 2016
#http://www.tre-sp.jus.br/eleitor/titulo-e-local-de-votacao/consulta-por-zona-eleitoral-e-bairro

#abrindo o arquivo
enderecos <- read.table("arquivo.csv", header = F, sep=",", quote = "\"", encoding = "latin1")

# tirar os acentos dos endereços e deixando tudo em minusculas
enderecos$enderecos <- tolower(stri_trans_general(enderecos$V4, "Latin-ASCII"))

#convertendo os endereços S/N para o número 1
enderecos$enderecos <- gsub("S/N", "1", enderecos$enderecos)

#criando uma coluna de ID, que vai ser usada pro merge depois
enderecos$id <- seq.int(nrow(enderecos))

#criando o complemento, necessário pro endereçamento
enderecos$complemento <- "Sao Paulo, Brasil"

#criando a variável com o endereço completo
enderecos$final <- paste(enderecos$enderecos, enderecos$complemento, sep=". ")

#vendo como ficou
glimpse(enderecos)


## Georreferenciamento
#o georreferenciamento leva um tempo. aqui estamos pegando o arquivo pronto 
geocode_sp <- geocode(enderecos$final)
geocode_sp$numero <- seq.int(nrow(enderecos))

latlong <- read.table("geocode_sp_final.csv", header = T, sep=",", quote = "\"", encoding = "latin1")

glimpse(latlong)

#renomeando a variável que é o id para facilitar o join
latlong <- latlong %>% 
rename(id = X)

  
## Join
enderecos_sp <- inner_join(enderecos, latlong, by="id") %>%
  rename(bairro = V1,
         nomezona = V2,
         nomelocal = V3,
         end_original = V4,
         z1 = V5,
         z2 = V6,
         end = enderecos,
         comp = complemento,
         endereco = final)

glimpse(enderecos_sp)

## Arrumando a planilha da zona/secao

#fazendo um subset dessas colunas para trabalhar 

zona <- enderecos_sp %>%
  select(id, z1, z2) %>%

glimpse(zona)

zona$z1 <- as.character(zona$z1)

#contando quantas zonas tem em cada variável - z1 e z2. Vamos usar o  "ª;" como indicador de uma zona
zona$z1_n <- str_count(zona$z1, "ª;")
zona$z2_n <- str_count(zona$z2, "ª;")

#vendo como ficou
glimpse(zona)

##vendo o máximo de zonas contidas numa mesma linha
zona <- zona %>% 
  arrange(desc(z1_n))

head(zona)

zona <- zona %>% 
  arrange(desc(z2_n))

head(zona)

#a variável z1 tem no máximo 46 zonas contidas, z2 tem no máximo 24.

str_split(
    z1, 
    sep = "ª;",
    names = c(1:46))

zona <- str_split(zona, z1, c("[1:46"))

c(1:46)

#########################
#####CONTINUAR DAQUI#####
#########################






## Abrir 

library(readxl)
secao_final <- read_excel("~/secao_final.xls")
View(secao_final)

## Merge do secao_final com os resultados

###### candidato vira coluna, por seção

## Group by endereco.. Total validos, candidato.. para tirar a proporcao

## Banco final: lat, lon, endereco, variavel de votacao (nome:"votacao_45") (no STATA precisa de id)

## Plotar os mapas

write.csv(geocode_sp, file = "geocode_sp_final.csv")











