setwd("C:/Users/Marina/Desktop/Financiamento")

#descompactando o arquivo e removendo o .zip
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
dados <- read.table(file.path(getwd(), arquivo), sep=";", header=TRUE, stringsAsFactors = F)

#checando como os dados ficaram por variavel
glimpse(dados)


#como os valores vieram em chr, vamos substituir as virgulas por pontos e deixar o resto em numerico:
dados$Valor.receita <- sub(",", ".", dados$Valor.receita)
dados$Valor.receita <- as.numeric(dados$Valor.receita)
#checando se todos os valores estao como numericos:
plot(density(dados$Valor.receita))  
#checando se existe missing:
length(dados$Valor.receita[is.na(dados$Valor.receita)])

#agregando as receitas pelo CPF dos candidatos e pelo tipo de receita recebida
receitas <- aggregate(dados$Valor.receita, by = list(dados$Sequencial.Candidato, dados$Tipo.receita), FUN="sum")
#renomeando as variaveis
names(receitas) <- c("seq", "tipo_receita", "valor")
#deixando observacoes unicas pra cada candidato por CPF
receitas <- reshape(receitas, timevar = "tipo_receita", idvar = "seq", direction = "wide")
#salvando os arquivos
#write.table(dados, file="receita_candidatos_2014.csv", sep=";", row.names=FALSE)
write.table(receitas, file="receitas_2016.csv", sep=";", row.names=FALSE)
