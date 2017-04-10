#configurando a pasta em que os arquivos serão salvos
setwd("C:/Users/Marina/Desktop/Desafio1")
#abrindo os pacotes que vou usar usar. Eles já estavam instalados. 

#install.packages("readr")
#install.packages("dplyr")

library(readr)
library(dplyr)

########################################
###### Parte 1 - abrindo os dados ######
########################################

#baixando o arquivo com os resultados eleitorais de 2016
url_result <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2016.zip"
download.file(url_result, "temp.zip", quiet = F)
#descompactando o arquivo e removendo o .zip da pasta
unzip("temp.zip")
file.remove("temp.zip")

#baixando o arquivo com os dados de candidaturas
url_cand <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2016.zip"
download.file(url_cand, "temp.zip", quiet = F)
#descompactando o arquivo e removendo o .zip da pasta
unzip("temp.zip")
file.remove("temp.zip")


##selecionando os arquivos da região Sudeste
#(troquei o Sul pelo Sudeste porque tava pra montar o banco do município de São Paulo pra minha dissertação)

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.resultados <- grep(pattern="votacao_candidato_munzona_2016_", lista.arquivos, value=TRUE)
print(lista.resultados)

#pegando somente os arquivos dos estados do Sudeste
lista.resultados <- lista.resultados[c(7,10,18,25)]
print(lista.resultados)

#criando o dataframe vazio que receberá os dados
resultados <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio

for(arquivo in lista.resultados){
  print (arquivo)
  d <- read_delim(file.path(getwd(),arquivo), 
                  col_names = c("DATA_GERACAO",
                                "HORA_GERACAO",
                                "ANO_ELEICAO", 
                                "NUM_TURNO",
                                "DESCRICAO_ELEICAO",
                                "SIGLA_UF",
                                "SIGLA_UE",
                                "CODIGO_MUNICIPIO",
                                "NOME_MUNICIPIO",
                                "NUMERO_ZONA",
                                "CODIGO_CARGO",
                                "NUMERO_CAND", 
                                "SEQUENCIAL_CANDIDATO",
                                "NOME_CANDIDATO",
                                "NOME_URNA_CANDIDATO",
                                "DESCRICAO_CARGO",
                                "COD_SIT_CAND_SUPERIOR",
                                "DESC_SIT_CAND_SUPERIOR",
                                "CODIGO_SIT_CANDIDATO",
                                "DESC_SIT_CANDIDATO",
                                "CODIGO_SIT_CAND_TOT",
                                "DESC_SIT_CAND_TOT",
                                "NUMERO_PARTIDO",
                                "SIGLA_PARTIDO",
                                "NOME_PARTIDO",
                                "SEQUENCIAL_LEGENDA",
                                "NOME_COLIGACAO",
                                "COMPOSICAO_LEGENDA",
                                "TOTAL_VOTOS",
                                "TRANSITO"),
                  delim = ";")
  resultados <-bind_rows(resultados, d)
} 
#removendo o banco de dados parcial ao final do empilhando
rm(d)

#checando se os quatro estados foram empilhados
table(resultados$SIGLA_UF)



##repetindo os passos para criar o arquivo das candidaturas##

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.candidatos <- grep(pattern="consulta_cand_2016_", lista.arquivos, value=TRUE)
print(lista.candidatos)

#pegando somente os arquivos dos estados do Sudeste
lista.candidatos <- lista.candidatos[c(9,12,20,27)]
print(lista.candidatos)

#criando o dataframe vazio que receberá os dados
candidatos <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio


for(arquivo in lista.candidatos){
  print (arquivo)
  d <- read_delim(file.path(getwd(),arquivo), 
                  col_names = c("DATA_GERACAO",
                                "HORA_GERACAO",
                                "ANO_ELEICAO",
                                "NUM_TURNO",
                                "DESCRICAO_ELEICAO",
                                "SIGLA_UF",
                                "SIGLA_UE",
                                "DESCRICAO_UE",
                                "CODIGO_CARGO",
                                "DESC_CARGO",
                                "NOME_CANDIDATO",
                                "SEQUENCIAL_CANDIDATO",
                                "NUMERO_CANDIDATO",
                                "CPF_CAND",
                                "NOME_URNA_CANDIDATO",
                                "COD_SITUACAO_CANDIDATURA",
                                "DES_SITUACAO_CANDIDATURA",
                                "NUMERO_PARTIDO",
                                "SIGLA_PARTIDO",
                                "NOME_PARTIDO",
                                "CODIGO_LEGENDA",
                                "SIGLA_LEGENDA",
                                "COMPOSICAO_LEGENDA",
                                "NOME_LEGENDA",
                                "CODIGO_OCUPACAO",
                                "DESCRICAO_OCUPACAO",
                                "DATA_NASCIMENTO",
                                "NUM_TITULO_ELEITORAL_CANDIDATO",
                                "IDADE_DATA_ELEICAO",
                                "CODIGO_SEXO",
                                "DESCRICAO_SEXO",
                                "COD_GRAU_INSTRUCAO",
                                "DESCRICAO_GRAU_INSTRUCAO",
                                "CODIGO_ESTADO_CIVIL",
                                "DESCRICAO_ESTADO_CIVIL",
                                "COD_COR_RACA",
                                "DESC_COR_RACA",
                                "CODIGO_NACIONALIDADE",
                                "DESCRICAO_NACIONALIDADE",
                                "SIGLA_UF_NASCIMENTO",
                                "CODIGO_MUNICIPIO_NASCIMENTO",
                                "NOME_MUNICIPIO_NASCIMENTO",
                                "DESPESA_MAX_CAMPANHA",
                                "COD_SIT_TOT_TURNO",
                                "DESC_SIT_TOT_TURNO"),
                  delim = ";")
  candidatos <-bind_rows(candidatos, d)
} 
#removendo o banco de dados parcial ao final do empilhando
rm(d)

##DUVIDA: AQUI DEU UM ERRO:
#See spec(...) for full column specifications.
#Warning: 86614 parsing failures.
#row col   expected     actual                                                         file
#1  -- 45 columns 46 columns 'C:/Users/Marina/Desktop/Desafio1/consulta_cand_2016_SP.txt'
#2  -- 45 columns 46 columns 'C:/Users/Marina/Desktop/Desafio1/consulta_cand_2016_SP.txt'
#3  -- 45 columns 46 columns 'C:/Users/Marina/Desktop/Desafio1/consulta_cand_2016_SP.txt'
#4  -- 45 columns 46 columns 'C:/Users/Marina/Desktop/Desafio1/consulta_cand_2016_SP.txt'
#5  -- 45 columns 46 columns 'C:/Users/Marina/Desktop/Desafio1/consulta_cand_2016_SP.txt'
#... ... .......... .......... ............................................................
#See problems(...) for more details.

#problems()
#Error in withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")) : 
#  argument "x" is missing, with no default

#checando se os quatro estados foram empilhados
table(candidatos$SIGLA_UF)


############################################
###### Parte 2 - data frame resultados #####
############################################

#selecionando as linhas que contem resultados para vereador, deixando o banco com as variáveis e renomeando-as
resultados <- resultados %>% 
  filter(DESCRICAO_CARGO == "PREFEITO") %>%
  select(SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, NOME_PARTIDO, NUMERO_CAND, TOTAL_VOTOS) %>%
  rename(ue = SIGLA_UE,
         uf = SIGLA_UF,
         cod_mun = CODIGO_MUNICIPIO, 
         partido = NOME_PARTIDO, 
         num_cand = NUMERO_CAND, 
         votos = TOTAL_VOTOS)

############################################
###### Parte 3 - data frame candidatos #####
############################################

#selecionando as linhas que contem candiadtos para vereador, deixando o banco com as variáveis e renomeando-as
candidatos <- candidatos %>% 
  filter(DESC_CARGO == "PREFEITO") %>%
  select(SIGLA_UE, SIGLA_UF,NOME_PARTIDO, NUMERO_CANDIDATO, DESCRICAO_OCUPACAO, DESCRICAO_SEXO ,DESCRICAO_GRAU_INSTRUCAO) %>%
  rename(
    uf= SIGLA_UF,
    ue = SIGLA_UE,
    partido = NOME_PARTIDO, 
    num_cand = NUMERO_CANDIDATO, 
    ocup = DESCRICAO_OCUPACAO, 
    sexo = DESCRICAO_SEXO, 
    educ = DESCRICAO_GRAU_INSTRUCAO)

##########################################################
###### Parte 4 - agregando e combinando por município#####
##########################################################

#criando um banco que tem o agregado de votos por unidade eleitoral
resultado_mun <- resultados %>% 
  group_by(ue) %>% 
  summarise(votos = sum(votos))

#adicionando o voto total por municipio no banco de resultados
resultados_join <- inner_join(resultados, resultado_mun, by = "ue")

#renomeando as variáveis voto.x (votação individual) e voto.y (votação total do municipio)
resultados <- resultados_join %>% rename(voto_cand = votos.x, voto_total_mun = votos.y)

#criando a nova variável que tem a proporção de votos recebidos
resultados <- resultados %>% 
  mutate(prop_mun = voto_cand / voto_total_mun)

##########################################################
###### Parte 5 - agregando e combinando por candidato#####
##########################################################

#criando um banco que tem o agregado de votos por candidato em cada unidade eleitoral
resultado_cand <- resultados %>% 
  group_by(uf, ue, num_cand) %>% 
  summarise(votos_cand = sum(voto_cand))

#adicionando o voto total por candidato no banco de resultados
resultados_cand_join <- inner_join(resultados, resultado_cand, by = c("uf","ue","num_cand"))

#vendo como ficaram as variáveis depois do join
glimpse(resultados_cand_join)

#criando a nova variável que tem a proporção de votos recebidos
resultados <- resultados_cand_join %>% 
  mutate(prop_mun_cand = voto_cand / votos_cand)

##########################################################
###### Parte 6 - agregando e combinando por candidato#####
##########################################################

#juntando o banco de candidatos e o de resultados pelo left join, para que as correspondência seja feita
#pelo banco das candidaturas
#também deixa o banco apenas com as variáveis únicas
resultados <- resultados %>%
  left_join(candidatos, resultados, by = c("ue", "num_cand"))

#olhando quais são as variáveis que ficaram no banco
glimpse(resultados)

#renomeando as que ficaram repetidas no join
resultados <- resultados %>%
  rename(uf = uf.x, partido = partido.x) %>%
  select(uf, ue, cod_mun, partido, num_cand, voto_cand, voto_total_mun, prop_mun, ocup, sexo, educ)
       

#removendo duplicados
resultados <- resultados[!duplicated(resultados),]


##########################################################
###### Parte 7 - treinando tabelas de resultados #########
##########################################################

#15- Produza uma tabela que indique o total de votos recebido por cada partido.
resultados_partido <- resultados %>% 
  group_by(partido) %>% 
  summarise(votos_partido = sum(voto_cand)) %>%
  arrange(desc(votos_partido))

#16- Produza uma tabela com o total de votos por ocupação d@s candidat@s.
resultados_ocupacao <- resultados %>% 
  group_by(ocup) %>% 
  summarise(votos_ocup = sum(voto_cand)) %>%
  arrange(desc(votos_ocup))

#17- Produza uma tabela com o total de votos por sexo d@s candidat@s.
resultados_sexo <- resultados %>% 
  group_by(sexo) %>% 
  summarise(votos_sexo = sum(voto_cand)) %>%
  arrange(desc(votos_sexo))

#18- Produza uma tabela com o total de votos por grau de escolaridade d@s candidat@s.
resultados_educ <- resultados %>% 
  group_by(educ) %>% 
  summarise(votos_educ = sum(voto_cand)) %>%
  arrange(desc(votos_educ))
