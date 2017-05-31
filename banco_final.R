#configurando a pasta em que os arquivos serão salvos
setwd("C:\\Users\\d841255\\Desktop\\dados")

#abrindo os pacotes que vou usar usar. Eles já estavam instalados. 

#install.packages("readr")
#install.packages("dplyr")

library(readr)
library(dplyr)
library(data.table)

########################################
###### Parte 1 - abrindo os dados ######
########################################

#baixando o arquivo com todos os resultados eleitorais de 2016
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


##selecionando os arquivos do Estado de São Paulo

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.resultados <- grep(pattern="votacao_candidato_munzona_2016_", lista.arquivos, value=TRUE)
print(lista.resultados)

#pegando somente o arquivo de São Paulo
lista.resultados <- lista.resultados[c(25)]
print(lista.resultados)

#criando o dataframe vazio que receberá os dados
resultados <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio
for(arquivo in lista.resultados){
  resultados <- fread(file.path(getwd(), arquivo), stringsAsFactors = F, encoding = "Latin-1", header = F)
  names(resultados) <- c("DATA_GERACAO",
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
                         "TRANSITO")
}

#checando se os quatro estados foram empilhados
table(resultados$SIGLA_UF)



##repetindo os passos para criar o arquivo das candidaturas##

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.candidatos <- grep(pattern="consulta_cand_2016_", lista.arquivos, value=TRUE)
print(lista.candidatos)

#pegando somente o arquivo de São Paulo
lista.candidatos <- lista.candidatos[c(27)]
print(lista.candidatos)

#criando o dataframe vazio que receberá os dados
candidatos <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio

for(arquivo in lista.candidatos){
  candidatos <- fread(file.path(getwd(), arquivo), stringsAsFactors = F, encoding = "Latin-1", header = F)
  names(candidatos) <-  c("DATA_GERACAO",
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
                          "DESC_SIT_TOT_TURNO",
                          "EMAIL")
}

#checando se os quatro estados foram empilhados
table(candidatos$SIGLA_UF)


############################################
###### Parte 2 - data frame resultados #####
############################################
names(resultados)
#selecionando as linhas que contem resultados para vereador, deixando o banco com as variáveis e renomeando-as
resultados <- resultados %>% 
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  select(SIGLA_UF, 
         SIGLA_UE, 
         CODIGO_MUNICIPIO,
         NUMERO_ZONA,
         SIGLA_PARTIDO, 
         NUMERO_CAND, 
         SEQUENCIAL_CANDIDATO,
         NOME_CANDIDATO,
         NOME_URNA_CANDIDATO,
         COMPOSICAO_LEGENDA,
         TOTAL_VOTOS,
         NOME_MUNICIPIO) %>%
  rename(uf = SIGLA_UF, 
         ue = SIGLA_UE, 
         cod_mun = CODIGO_MUNICIPIO,
         num_zona = NUMERO_ZONA,
         sigla = SIGLA_PARTIDO, 
         num_cand =NUMERO_CAND, 
         seq = SEQUENCIAL_CANDIDATO,
         nome = NOME_CANDIDATO,
         nome_urna = NOME_URNA_CANDIDATO,
         colig = COMPOSICAO_LEGENDA,
         votos_total = TOTAL_VOTOS,
         nome_mun = NOME_MUNICIPIO)

############################################
###### Parte 3 - data frame candidatos #####
############################################
names(candidatos)
#selecionando as linhas que contem candiadtos para vereador, deixando o banco com as variáveis e renomeando-as
candidatos <- candidatos %>% 
  filter(DESC_CARGO == "VEREADOR") %>%
  select(SEQUENCIAL_CANDIDATO,
         CPF_CAND,
         DES_SITUACAO_CANDIDATURA,
         NOME_LEGENDA,
         NUM_TITULO_ELEITORAL_CANDIDATO,
         IDADE_DATA_ELEICAO,
         DESCRICAO_SEXO,
         DESCRICAO_OCUPACAO, 
         DESCRICAO_GRAU_INSTRUCAO,
         DESCRICAO_ESTADO_CIVIL,
         DESC_COR_RACA,
         DESPESA_MAX_CAMPANHA,
         DESC_SIT_TOT_TURNO) %>%
  rename(
    seq = SEQUENCIAL_CANDIDATO,
    cpf = CPF_CAND,
    situ = DES_SITUACAO_CANDIDATURA,
    colig_nome = NOME_LEGENDA,
    titulo = NUM_TITULO_ELEITORAL_CANDIDATO,
    idade = IDADE_DATA_ELEICAO,
    genero = DESCRICAO_SEXO,
    ocup = DESCRICAO_OCUPACAO, 
    instru = DESCRICAO_GRAU_INSTRUCAO,
    estcivil = DESCRICAO_ESTADO_CIVIL,
    cor = DESC_COR_RACA,
    despmax = DESPESA_MAX_CAMPANHA,
    result = DESC_SIT_TOT_TURNO)

##########################################################
###### Parte 4 - agregando e combinando por município#####
##########################################################

#deixando as variáveis de voto como numérica e a seq, nossa chave, como string
resultados$votos_total <- as.numeric(resultados$votos_total)
resultados$seq <- as.character(resultados$seq)
candidatos$seq <- as.character(candidatos$seq)

#criando um banco que tem o agregado de votos por município
resultado_mun <- resultados %>% 
  group_by(ue) %>% 
  summarise(votos_total_mun = sum(votos_total))

#adicionando o voto total por municipio no banco de resultados
resultados<- right_join(resultados, resultado_mun, by = "ue")

#criando um banco que tem o agregado de votos por candidato
resultado_cand <- resultados %>% 
  group_by(seq) %>% 
  summarise(votos_total_cand = sum(votos_total))

#adicionando o voto total por municipio no banco de resultados e criando uma variável da porcentagem do candidato
resultados <- resultados %>%
  right_join(resultado_cand, by = "seq") %>%
  mutate(prop_mun_cand = votos_total_cand / votos_total_mun)

##########################################################
###### Parte 6 - agregando e combinando por candidato#####
##########################################################

#juntando o banco de candidatos e o de resultados 
#também deixa o banco apenas com as variáveis únicas
resultados_2 <- inner_join(candidatos, resultados, by = c("seq"))

#olhando quais são as variáveis que ficaram no banco pra checar que todas vieram
glimpse(resultados_2)

###############################################################
###### Parte 6 - selecionando apenas a cidade de São Paulo#####
###############################################################

#vendo qual o código da unidade eleitoral de São Paulo para fazer o filtro.
#como é a maior cidade, vamos ordenar o banco pelo maior número de votos totais no municipio

resultados_2 <- resultados_2 %>% arrange(desc(votos_total_mun))
head(resultados_2)

#o número da cidade de SP é 71072

dados_SP <- resultados_2 %>% 
  filter(ue == "71072")

#banco com resultados por candidatos únicos como linha. Seleciona todas as linhas exceto as de voto por zona.
dados_cand <- dados_SP %>%
  select(-votos_total, -num_zona)

#deixa as linhas únicas por candidato.
dados_cand <- distinct(dados_cand) 
dados <- dados_cand

#vendo se tem os números corretos de eleitos
table(dados_cand$result, dados_cand$genero)

#salvando os dois bancos
write.table(dados_cand, "result_cand_SP.csv", sep = ";", fileEncoding ="latin1", row.names = F)
write.table(dados_SP, "result_candzona_SP.csv", sep = ";", fileEncoding ="latin1", row.names = F)

######################################
##########DADOS DE RECEITAS##########
######################################

#descompactando o arquivo e removendo o .zip
url_financiamento <-"http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_contas_final_2016.zip"
download.file(url_financiamento,"prestacao_contas_final_2016.zip")
unzip("prestacao_contas_final_2016.zip")
file.remove("prestacao_contas_final_2016.zip")

#listando os documentos com os dados
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#pegando somente os documentos somente das receitas
lista.arquivos <- grep(pattern="receitas_candidatos_", lista.arquivos, value=TRUE)
lista.arquivos
#excluindo o arquivo BR
lista.arquivos <- lista.arquivos[c(26)]
lista.arquivos
#abrindo o dataframe
for(arquivo in lista.arquivos){
  dados <- fread(file.path(getwd(), arquivo), stringsAsFactors = F, encoding = "Latin-1", header = T)
  }
#checando como os dados ficaram por variavel
head(dados)

receitas <- dados %>%
  filter(`Sigla da UE` =="71072" & Cargo == "Vereador") %>%
   rename(
   codeleicao =  `Cód. Eleição`,
   desceleicao = `Desc. Eleição`,
   datahora = `Data e hora`,
   cnpjprest = `CNPJ Prestador Conta`,
   seq = `Sequencial Candidato`,
   uf = UF,
   ue = `Sigla da UE`,
   nomeue = `Nome da UE`,
   sigla = `Sigla  Partido`,
   num_cand = `Numero candidato`,
   cargo = Cargo,
   nome = `Nome candidato`,
   cpf = `CPF do candidato`,
   cpf_vice = `CPF do vice/suplente`,
   num_recibo = `Numero Recibo Eleitoral`,
   num_doc = `Numero do documento`,
   cpf_doador = `CPF/CNPJ do doador`,
   nome_doador = `Nome do doador`,
   nome_doador_receita = `Nome do doador (Receita Federal)`,
   ue_doador = `Sigla UE doador`,
   num_part_doador = `Número partido doador`,
   num_cand_doador = `Número candidato doador`,
   setor_doador = `Setor econômico do doador`,
   cod_setor_doador = `Cod setor econômico do doador`,
   data_receita = `Data da receita`,
   valor = `Valor receita`,
   tipo = `Tipo receita`,
   fonte = `Fonte recurso`,
   especie = `Especie recurso`,
   desc_receita = `Descricao da receita`,
   cpf_doador_origem = `CPF/CNPJ do doador originário`,
   nome_doador_origem = `Nome do doador originário`,
   nome_doador_origem_receita = `Nome do doador originário (Receita Federal)`,
   tipo_origem = `Tipo doador originário`,
   setor_origem = `Setor econômico do doador originário`)

receitas <- receitas %>%
  mutate(valor = as.numeric(sub(",", ".", valor)))%>%
  select(-cpf,-uf,-ue,-sigla,-num_cand,-nome) #variáveis que contém no banco de resultados/candidaturas
#salvando o banco com as observações por receita recebida
write.table(receitas, file="receitas_2016.csv", sep=";", row.names=FALSE)

#agregando as receitas pelo CPF dos candidatos e pelo tipo de receita recebida
receitas1 <- aggregate(receitas$valor, by = list(receitas$seq, receitas$tipo), FUN="sum")
#renomeando as variaveis
names(receitas1) <- c("seq", "tipo", "valor")
#deixando observacoes unicas pra cada candidato por CPF
receitas1 <- reshape(receitas1, timevar = "tipo", idvar = "seq", direction = "wide")

#agregando as receitas pelo CPF dos candidatos e pelo tipo de receita recebida
receitas2 <- aggregate(receitas$valor, by = list(receitas$seq, receitas$fonte), FUN="sum")
#renomeando as variaveis
names(receitas2) <- c("seq", "fonte", "valor")
#deixando observacoes unicas pra cada candidato por CPF
receitas2 <- reshape(receitas2, timevar = "fonte", idvar = "seq", direction = "wide")

receitas <- receitas1 %>%
  left_join(receitas2, by ="seq") %>%
  rename(valor_tipo_internet_cand = `valor.Doações pela Internet`,
         valor_tipo_eventos_cand =`valor.Comercialização de bens ou realização de eventos`,
         valor_tipo_candidatos_cand = `valor.Recursos de outros candidatos`,
         valor_tipo_partidos_cand = `valor.Recursos de partido político`,
         valor_tipo_ni_cand = `valor.Recursos de origens não identificadas`,
         valor_tipo_pfisica_cand = `valor.Recursos de pessoas físicas`,
         valor_tipo_proprio_cand = `valor.Recursos próprios`,
         valor_tipo_aplic_cand = `valor.Rendimentos de aplicações financeiras`,
         valor_origem_fundo_cand = `valor.Fundo Partidario`,
         valor_origem_outros_cand =`valor.Outros Recursos`)

#salvando os arquivos com as observações por candidato
write.table(receitas, file="receitas_2016_unico.csv", sep=";", row.names=FALSE)

############################################################################
#####JUNTANDO O BANCO DE CANDIDATURAS/RESULTADOS COM O DE RECEITAS##########
############################################################################

#juntando com o banco de candidaturas e resultados
#receitas <- fread("receitas_2016_unico.csv")
#dados_cand <- fread("result_cand_SP.csv")
#juntando com o banco de candidaturas e resultados
glimpse(dados_cand) #ver arquivo banco_candidaturas_resultados.R
glimpse(receitas) #ver o script banco_receitas.R

#deixando a variável chave dos dois bancos, o número sequencial do candidato, como numérico
receitas$seq <- as.numeric(receitas$seq)
dados_cand$seq <- as.numeric(dados_cand$seq)

#leftjoin, para que os dados do receitas que têm equivalência ao dados_SP sejam adicionados ao dados_SP
dados <- left_join(dados_cand, receitas, by="seq")

##Checando o banco final com o que consta no TSE

##vendo o número único de candidatos e comparando com o que tem no TSE hoje (28/04)
seq <- as.data.frame(unique(dados$seq))
colnames(seq) <- c("seq")

#Temos 1275 candidatos - no site do TSE, são 1315.

#checando se essa diferença de 40 candidatos apresenta viés de gênero
unidados <- left_join(seq, dados, by="seq")
unidados <- unidados %>%
  select(seq, genero, result) %>%
  distinct(seq, genero, result) 

#checando se os dados contém os 55 eleitos
table(unidados$genero, unidados$result)

#teste qui quadrado pra diferença de proporções pra saber se os candidatos faltantes
#são independentes do gênero
#o primeiro número é o que contém nos dados, o segundo é o que consta no TSE
prop.dif.f <- 393/418
print(prop.dif.f)
prop.dif.m <- 882/919
print(prop.dif.m)
prop.dif.t <- 1275/1315

prop.dif <-as.data.frame(cbind(prop.dif.f, prop.dif.m))
chisq.test(prop.dif)

#	Chi-squared test for given probabilities
#data:  prop.dif
#X-squared = 0.00020111, df = 1, p-value = 0.9887

#limpando pra deixar as variáveis que interessam:
dados <- dados %>%
  select(-codeleicao, -desceleicao, -datahora)

write.table(dados, file="dados_final_unico.csv", sep=";", row.names=FALSE)
