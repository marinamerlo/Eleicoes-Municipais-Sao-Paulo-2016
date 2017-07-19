setwd("C:\\Users\\d841255\\Desktop\\filiados")

library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(stringi)
#install.packages("gender")
library(gender)
library(tidyverse)

lista.arquivos <-list.files(file.path(getwd()))
lista.banco <- grep(pattern="filiados_", lista.arquivos, value=TRUE)

filiados <- data.frame()

for (arquivo in lista.banco) {
  unzip(arquivo)
  files <- list.files(path = "C:\\Users\\d841255\\Desktop\\filiados\\aplic\\sead\\lista_filiados\\uf\\", pattern="filiados_", full.names = TRUE)
}

for (file in files) {
  dado <- fread(file) 
  dado <- dado %>%
    filter(`NOME DO MUNICIPIO` == "SÃO PAULO")
    filiados<-bind_rows(filiados, dado)
}

#por alguma razão ele ta criando com linhas repetidas! dando o distinct, o dado fica certo.
#outros ajustes no banco:
filiados <- filiados %>%
#tirando duplicados
  distinct() %>%
#renomeando as variáveis
  rename(ext_data = `DATA DA EXTRACAO`,
         ext_hora_ext = `HORA DA EXTRACAO`,
         insc = `NUMERO DA INSCRICAO`,
         nome = `NOME DO FILIADO`,
         sigla = `SIGLA DO PARTIDO`,
         nome_partido = `NOME DO PARTIDO`,
         uf = UF,
         cod_mun = `CODIGO DO MUNICIPIO`,
         nome_mun = `NOME DO MUNICIPIO`,
         zona = `ZONA ELEITORAL`,
         secao = `SECAO ELEITORAL`,
         data_fili = `DATA DA FILIACAO`,
         situ_reg = `SITUACAO DO REGISTRO`,
         tipo_reg = `TIPO DO REGISTRO`,
         data_proces = `DATA DO PROCESSAMENTO`,
         data_desfili = `DATA DA DESFILIACAO`,
         data_cancel = `DATA DO CANCELAMENTO`,
         data_regula = `DATA DA REGULARIZACAO`,
         motivo_cancel = `MOTIVO DO CANCELAMENTO`) %>%
  #extrai apenas o primeiro nome do nome completo, retira acentos e deixa em minúsculo       
  mutate(primeironome = word(nome, 1)) %>%
  mutate(primeironome = tolower(stri_trans_general(primeironome, "Latin-ASCII")))

#salvando o banco gerado
write.table(filiados, "filiados_SP.csv", sep = ";", fileEncoding ="latin1", row.names = F)


###pegando os nomes dos candidatos do brasil todo (socorro!)

setwd("C:\\Users\\d841255\\Desktop\\candidatos")

#baixando o arquivo com os dados de candidaturas
url_cand <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2016.zip"
download.file(url_cand, "temp.zip", quiet = F)
#descompactando o arquivo e removendo o .zip da pasta
unzip("temp.zip")
file.remove("temp.zip")

##repetindo os passos para criar o arquivo das candidaturas##
#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.candidatos <- grep(pattern="consulta_cand_2016_", lista.arquivos, value=TRUE)
print(lista.candidatos)
#criando o dataframe vazio que receberá os dados
candidatos <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio
for(arquivo in lista.candidatos){
  print (arquivo)
  d <- read.delim(arquivo, sep = ";", header = F)
  candidatos <-bind_rows(candidatos, d)
} 
#removendo o banco de dados parcial ao final do empilhando
rm(d)

colnames(candidatos)<- c("DATA_GERACAO",
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
                         "DESC_SIT_TOT_TURNO")



nomes <- candidatos %>%
  select(NOME_CANDIDATO, DESCRICAO_SEXO) %>%
  rename(sexo = DESCRICAO_SEXO,
         nome_comp = NOME_CANDIDATO)


#deixando uma lista única de primeiros nomes por gênero
nomes <- nomes %>%
  mutate(primeironome = word(nome_comp, 1)) %>%
  mutate(primeironome = tolower(stri_trans_general(primeironome, "Latin-ASCII"))) %>%
  select(-nome_comp)%>%
  distinct()

nomes$duplicado <- duplicated(nomes$primeironome)
unico <- unique(nomes$primeironome)

duplicados <- nomes %>%
  filter(duplicado == "TRUE") %>%
  select(-sexo)

nomes <- nomes %>%
  filter(duplicado == FALSE)


#juntando no banco de filiados
filiados <- filiados %>%
  left_join(nomes, by = "primeironome")

#vendo o que falta
falta <- filiados %>%
  filter(is.na(sexo))

#criando uma lista de nomes que falta verificar o sexo
falta_gen <- falta %>%
  select(insc,primeironome)

#aplicando o algoritmo do pacote gender
gender <- gender(falta_gen$primeironome, method = "ssa")

#vendo se as proporções estão similares
table(filiados$sexo)
table(gender$gender)

#deixando o banco de filiados apenas com os casos completos - isto é, com os nomes que foram classificados
#com a lista de candidaturas
filia_cand <- filiados %>%
  filter(complete.cases(.))

#deixando uma lista de primeiros nomes e gênero a partir do pacote gender
genero_gender <- gender %>%
  select(name, gender) %>%
  rename(primeironome = name,
         sexo = gender)

#aplicando a classificação do pacote gender nos nomes que faltaram classificação
filia_gender <- left_join(falta, genero_gender, by ="primeironome")

#limpando o banco para receber os dados da coluna gerada pelo gender
filia_gender <- filia_gender %>%
  select(-sexo.x) %>%
  rename(sexo = sexo.y)

#juntando o banco classificado pelas candidaturas e pelo gender, e limpando repetições
filia_completo <- bind_rows(filia_gender, filia_cand) %>%
  distinct()


#padronizando a codificação da variável
filia_completo <- filia_completo %>%
  mutate(sexo = ifelse(sexo == "female", "FEMININO", sexo)) %>%
  mutate(sexo = ifelse(sexo == "male", "MASCULINO", sexo)) %>%
  mutate(sexo = ifelse(is.na(sexo), "INDETERMINADO", sexo))

#vendo como ficou
table(filia_completo$sexo)

#salvando a tabela final
write.table(filia_completo, "filiados_SP_completo.csv", sep = ";", fileEncoding ="UTF-8", row.names = F)

#limpando a área
rm(falta_gen)
rm(filia_cand)
rm(filia_gender)
rm(gender)
rm(genero_gender)
rm(filiados)
rm(falta)
rm(candidatos)
rm(dado)
rm(nomes)

##fazendo as variáveis!

###refazer as contagens considerando apenas os filiados regulares
filiados_reg <- filia_completo %>%
  filter(situ_reg == "REGULAR")

filia_mulheres <- filiados_reg %>%
  filter(sexo == "FEMININO") %>%
  group_by(sigla) %>%
  summarise(n_mulheres_partido = n())

filia_homens <- filiados_reg %>%
  filter(sexo == "MASCULINO") %>%
  group_by(sigla) %>%
  summarise(n_homens_partido = n())

filia_indet <- filiados_reg %>%
  filter(sexo == "INDETERMINADO") %>%
  group_by(sigla) %>%
  summarise(n_indet_partido = n())

filia_total <- filiados_reg %>%
  group_by(sigla) %>%
  summarise(n_partido = n())

filiados_reg <- filiados_reg %>%
  mutate(n_total = n()) %>%
  left_join(filia_mulheres, by = "sigla") %>%
  left_join(filia_homens, by = "sigla") %>%
  left_join(filia_indet, by = "sigla") %>%
  left_join(filia_total, by = "sigla") %>%
  mutate(pct = n_mulheres_partido / n_partido)
