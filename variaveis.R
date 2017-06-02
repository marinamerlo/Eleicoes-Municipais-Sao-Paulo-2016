setwd(D/Dropbox/Mestrado/Seminário Discente/2017/Dados)
### criando as variáveis necessárias ##

#começando a partir do script banco_final.R

library(ggplot2)
library(dplyr)


################################################################################################
#####################################VARIÁVEIS DE VOTOS E CANDIDATURA###########################
###############################################################################################

dados <- dados %>% 
  mutate(genero = recode(genero, 
                         "MASCULINO" = "Masculino", 
                         "FEMININO" = "Feminino"))

dados <- dados %>% 
  mutate(situ = recode(situ, 
                       "RENÚNCIA" = "Renúncia", 
                       "DEFERIDO" = "Deferido", 
                       "INDEFERIDO" = "Indeferido", 
                       "INDEFERIDO COM RECURSO" ="Indeferido com Recurso"))

dados <- dados %>% 
mutate(result = recode(result,
                     "ELEITO POR MÉDIA" = "Eleito por média", 
                     "ELEITO POR QP" = "Eleito por QP", 
                     "NÃO ELEITO" = "Não Eleito", 
                      "SUPLENTE" ="Suplente"))

 
#variável pra indicar se foi eleito ou não em 2016
dados <- dados %>% 
  mutate(eleito = recode(result, 
                         "Eleito por média" = "Eleito", 
                         "Eleito por QP" = "Eleito", 
                         "Suplente" = "Não Eleito",
                         "Não Eleito" = "Não Eleito"))
dados$eleito <- as.factor(dados$eleito)
summary(dados$eleito)


#deixando gênero como factor
dados$genero.f <- as.factor(dados$genero)
summary(dados$genero.f)

#total de candidaturas 
dados$cand_total <- 1275

#contagem de candidaturas femininas por partido
genero_partido <- dados %>%
  filter(genero == "Feminino") %>%
  group_by(sigla) %>%
  summarise(cand_part_fem = n())

#contagem de candidaturas femininas por coligação
genero_colig <- dados %>%
  filter(genero == "Feminino") %>%
  group_by(colig) %>%
  summarise(cand_colig_fem = n())

#total de candidaturas femininas por situação
genero_situ <- dados %>%
  filter(genero == "Feminino") %>%
  group_by(situ) %>%
  summarise(cand_situ_fem = n())

#total de candidaturas por partido
candidaturas_partido <- dados %>%
  group_by(sigla) %>%
  summarise(cand_part = n())

#total de candidaturas por coligação
candidaturas_colig <- dados %>%
  group_by(colig) %>%
  summarise(cand_colig = n())

#total de candidaturas por situação
candidaturas_situ <- dados %>%
  group_by(situ) %>%
  summarise(cand_situ = n())

##juntando no banco de dados
dados <- dados %>%
  left_join(genero_partido, by = "sigla") %>%
  left_join(genero_colig, by = "colig") %>%
  left_join(genero_situ, by = "situ") %>%
  left_join(candidaturas_partido, by = "sigla") %>%
  left_join(candidaturas_colig, by = "colig") %>%
  left_join(candidaturas_situ, by = "situ")

#fazendo as variáveis de proporção de candidaturas femininas por partido e coligação
dados <- dados %>%
  mutate(cand_part_fem_pct = (cand_part_fem / cand_part)) %>%
  mutate(cand_colig_fem_pct = (cand_colig_fem / cand_colig)) %>%
  mutate(cand_situ_fem_pct = (cand_situ_fem / cand_situ))

dados_votos_eleitas <- dados %>%
  filter(genero == "Feminino" & eleito == "Eleito") %>%
  summarise(soma_votos = sum(votos_total_cand, na.rm = TRUE),
            media_votos = mean(votos_total_cand, na.rm = TRUE),
            mediana_votos = median(votos_total_cand, na.rm = TRUE),
            desvio_votos = sd(votos_total_cand, na.rm = TRUE),
            minimo_votos = min(votos_total_cand, na.rm = TRUE),
            maximo_votos = max(votos_total_cand, na.rm = TRUE))

dados_votos_eleitos <- dados %>%
  filter(genero == "Masculino" & eleito == "Eleito") %>%
  summarise(soma_votos = sum(votos_total_cand, na.rm = TRUE),
            media_votos = mean(votos_total_cand, na.rm = TRUE),
            mediana_votos = median(votos_total_cand, na.rm = TRUE),
            desvio_votos = sd(votos_total_cand, na.rm = TRUE),
            minimo_votos = min(votos_total_cand, na.rm = TRUE),
            maximo_votos = max(votos_total_cand, na.rm = TRUE))

dados_votos_mulheres <- dados %>%
  filter(genero == "Feminino") %>%
  summarise(soma_votos = sum(votos_total_cand, na.rm = TRUE),
            media_votos = mean(votos_total_cand, na.rm = TRUE),
            mediana_votos = median(votos_total_cand, na.rm = TRUE),
            desvio_votos = sd(votos_total_cand, na.rm = TRUE),
            minimo_votos = min(votos_total_cand, na.rm = TRUE),
            maximo_votos = max(votos_total_cand, na.rm = TRUE))

dados_votos_homens <- dados %>%
  filter(genero == "Masculino") %>%
  summarise(soma_votos = sum(votos_total_cand, na.rm = TRUE),
            media_votos = mean(votos_total_cand, na.rm = TRUE),
            mediana_votos = median(votos_total_cand, na.rm = TRUE),
            desvio_votos = sd(votos_total_cand, na.rm = TRUE),
            minimo_votos = min(votos_total_cand, na.rm = TRUE),
            maximo_votos = max(votos_total_cand, na.rm = TRUE))


################################################################################################
#####################################VARIÁVEIS DE RECURSOS#####################################
###############################################################################################

##########################
### TOTAL DE RECURSOS ###

#variável que tem o total de recursos recebidos por candidato
valor_total_cand <- dados %>% 
  group_by(seq) %>% 
  summarise(valor_total_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_total_cand, by = "seq")
#variável que tem o total de recursos recebidos por partido (soma dos candidatos)
valor_total_partido <- dados %>% 
  group_by(sigla) %>% 
  summarise(valor_total_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_total_partido, by = "sigla")
#variável que tem o total de recursos recebidos por coligação (soma dos candidatos)
valor_total_colig <- dados %>% 
  group_by(colig) %>% 
  summarise(valor_total_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_total_colig, by = "colig")


###############################
### ORIGEM FUNDO PARTIDÁRIO ###

#total de origem fundo partidário por candidato
valor_origem_fundo_cand <- dados %>% 
  filter(fonte == "Fundo Partidario") %>%
  group_by(seq) %>% 
  summarise(valor_origem_fundo_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_origem_fundo_cand, by = "seq")
#total de origem fundo partidário por partido (soma dos candidatos)
valor_origem_fundo_partido <- dados %>% 
  filter(fonte == "Fundo Partidario") %>%
  group_by(sigla) %>% 
  summarise(valor_origem_fundo_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_origem_fundo_partido, by = "sigla")
#total de origem fundo partidário por coligação (soma dos candidatos)
valor_origem_fundo_colig <- dados %>% 
  filter(fonte == "Fundo Partidario") %>%
  group_by(colig) %>% 
  summarise(valor_origem_fundo_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_origem_fundo_colig, by = "colig")


##############################
### TIPO RECURSOS PRÓPRIOS ###

#total do tipo recursos próprios por candidato
valor_tipo_proprio_cand <- dados %>% 
  filter(tipo == "Recursos próprios") %>%
  group_by(seq) %>% 
  summarise(valor_tipo_proprio_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_proprio_cand, by = "seq") 
#total do tipo recursos próprios por partido (soma dos candidatos)
valor_tipo_proprio_partido <- dados %>% 
  filter(tipo == "Recursos próprios") %>%
  group_by(sigla) %>% 
  summarise(valor_tipo_proprio_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_proprio_partido, by = "sigla") 
#total do tipo recursos próprios por coligação (soma dos candidatos)
valor_tipo_proprio_colig <- dados %>% 
  filter(tipo == "Recursos próprios") %>%
  group_by(colig) %>% 
  summarise(valor_tipo_proprio_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_proprio_colig, by = "colig") 


############################
### TIPO RECURSOS OUTROS ###

#especificando quais são os tipos de recursos operacionalizados como "Outros"
outros <- c("Doações pela Internet", 
            "Comercialização de bens ou realização de eventos", 
            "Recursos de origens não identificadas", 
            "Rendimentos de aplicações financeiras")

#total do tipo outros por candidato
valor_tipo_outros_cand <- dados %>% 
  filter(tipo %in% outros) %>%
  group_by(seq) %>% 
  summarise(valor_tipo_outros_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_outros_cand, by = "seq") 
#total do tipo outros por partido (soma dos candidatos)
valor_tipo_outros_partido <- dados %>% 
  filter(tipo %in% outros) %>%
  group_by(sigla) %>% 
  summarise(valor_tipo_outros_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_outros_partido, by = "sigla") 
#total do tipo outros por coligação (soma dos candidatos)
valor_tipo_outros_colig <- dados %>% 
  filter(tipo %in% outros) %>%
  group_by(colig) %>% 
  summarise(valor_tipo_outros_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_outros_colig, by = "colig") 


#######################################
### TIPO RECURSOS OUTROS CANDIDATOS ###

#total do tipo recursos próprios por candidato
valor_tipo_candidatos_cand <- dados %>% 
  filter(tipo == "Recursos de outros candidatos") %>%
  group_by(seq) %>% 
  summarise(valor_tipo_candidatos_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_candidatos_cand, by = "seq") 
#total do tipo recursos próprios por partido (soma dos candidatos)
valor_tipo_candidatos_partido <- dados %>% 
  filter(tipo == "Recursos de outros candidatos") %>%
  group_by(sigla) %>% 
  summarise(valor_tipo_candidatos_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_candidatos_partido, by = "sigla") 
#total do tipo recursos próprios por coligação (soma dos candidatos)
valor_tipo_candidatos_colig <- dados %>% 
  filter(tipo == "Recursos de outros candidatos") %>%
  group_by(colig) %>% 
  summarise(valor_tipo_candidatos_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_candidatos_colig, by = "colig") 


########################################
### TIPO RECURSOS PARTIDOS POLÍTICOS ###

#total do tipo recursos do partido por candidato
valor_tipo_partidos_cand <- dados %>% 
  filter(tipo == "Recursos de partido político") %>%
  group_by(seq) %>% 
  summarise(valor_tipo_partidos_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_partidos_cand, by = "seq") 
#total do tipo recursos do partido por partido (soma dos candidatos)
valor_tipo_partidos_partido <- dados %>% 
  filter(tipo == "Recursos de partido político") %>%
  group_by(sigla) %>% 
  summarise(valor_tipo_partidos_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_partidos_partido, by = "sigla") 
#total do tipo recursos do partido por coligação (soma dos candidatos)
valor_tipo_partidos_colig <- dados %>% 
  filter(tipo == "Recursos de partido político") %>%
  group_by(colig) %>% 
  summarise(valor_tipo_partidos_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_partidos_colig, by = "colig") 


###################################
### TIPO RECURSOS PESSOA FÍSICA ###

#total do tipo recursos do partido por candidato
valor_tipo_pfisica_cand <- dados %>% 
  filter(tipo == "Recursos de pessoas físicas") %>%
  group_by(seq) %>% 
  summarise(valor_tipo_pfisica_cand = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_pfisica_cand, by = "seq") 
#total do tipo recursos do partido por partido (soma dos candidatos)
valor_tipo_pfisica_partido <- dados %>% 
  filter(tipo == "Recursos de pessoas físicas") %>%
  group_by(sigla) %>% 
  summarise(valor_tipo_pfisica_partido = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_pfisica_partido, by = "sigla") 
#total do tipo recursos do partido por coligação (soma dos candidatos)
valor_tipo_pfisica_colig <- dados %>% 
  filter(tipo == "Recursos de pessoas físicas") %>%
  group_by(colig) %>% 
  summarise(valor_tipo_pfisica_colig = sum(as.numeric(valor), na.rm=TRUE))
dados <- dados %>%
  left_join(valor_tipo_pfisica_colig, by = "colig") 


## porcentagens de recursos por tipo e origem por candidato ##
dados <- dados %>%
  mutate(valor_origem_fundo_cand_pct = valor_origem_fundo_cand / valor_total) %>%
  mutate(valor_tipo_proprio_cand_pct = valor_tipo_proprio_cand / valor_total) %>%
  mutate(valor_tipo_outros_cand_pct = valor_tipo_outros_cand / valor_total) %>%
  mutate(valor_tipo_candidatos_cand_pct = valor_tipo_candidatos_cand / valor_total) %>%
  mutate(valor_tipo_partidos_cand_pct = valor_tipo_partidos_cand / valor_total) %>%
  mutate(valor_tipo_pfisica_cand_pct = valor_tipo_pfisica_cand / valor_total) 



dados <- dados %>%
  rowwise() %>%
   mutate(valor_total = sum(valor_origem_fundo_cand, valor_origem_outros_cand, na.rm=T))

#teste do boxplot
ggplot()+ 
  geom_boxplot(aes(x = eleito, y = valor_total, fill=genero), data = dados) +
  scale_y_log10() +
  theme_minimal() +
  scale_fill_brewer(name = "Porte do Município", palette = "Set1") + 
  labs(title = "Receita  por Habitante",
       y ="Receita por habitante")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=45, hjust=1))



##############################################################################################################
##daqui pra baixo, ainda falta fazer/checar o código#########################################################
##############################################################################################################

#tirando o log dos recursos totais
dados$rectotal.log <- as.numeric(log(dados$rectotal))
#transformando o -Inf em zero, pois é o resultado do log de 0
dados$rectotal.log[is.infinite(dados$rectotal.log) ] <- 0
dados$rectotal.log[dados$rectotal.log < 0] <- 0.0000000000000000000001
summary(dados$rectotal.log)


#tirando o log dos recursos partidários
dados$recpart.log <- as.numeric(log(dados$recpart))
#transformando o -Inf em zero, pois é o resultado do log de 0
dados$recpart.log[is.infinite(dados$recpart.log) ] <- 0
dados$recpart.log[dados$recpart.log < 0] <- 0.0000000000000000000001
summary(dados$recpart.log)

#tirando o log dos recursos de outros candidatos
dados$reccand.log <- as.numeric(log(dados$reccand))
#transformando o -Inf em zero, pois é o resultado do log de 0
dados$reccand.log[is.infinite(dados$reccand.log) ] <- 0
dados$reccand.log[dados$reccand.log < 0] <- 0.0000000000000000000001
summary(dados$reccand.log)


#variável pra indicar se recebeu recursos ou não
dados$recurso.f <- ifelse(dados$rectotal == 0, c("Nao Recebeu Recursos"), c("Recebeu Recursos"))
dados$recurso.f <- as.factor(dados$recurso.f)
summary(dados$recurso.f)

#variável para identificar candidaturas sem votos e sem recursos
dados$laranja.f <- ifelse(dados$recurso.f == "Nao Recebeu Recursos" & (dados$votos_total_cand == 0), c("Laranja"), c("0"))
table(dados$laranja.f)
dados$laranja.f <- as.factor(dados$laranja.f)
summary(dados$laranja.f)


#variável pra indicar se recebeu recursos do partido ou não
dados$recurso.part.f <- ifelse(dados$recpart > 0, c("1"), c("0"))
dados$recurso.part.f <- as.factor(dados$recurso.part.f)
summary(dados$recurso.part.f)

#variável pra indicar se recebeu votos ou não
dados$voto.f <- ifelse(dados$votos_total_cand == 0, c("0"), c("1"))
dados$voto.f <- as.factor(dados$voto.f)
#Deixando os NA como zero porque representam não ter recebido votos em 2014:
dados$voto.f[is.na(dados$voto.f)] <- 0
summary(dados$voto.f)


#soma de recursos partidários por partido
recursos_partido <- dados %>%
  filter(recpart > 0 | recpart != NA) %>%
  group_by(sigla) %>%
  summarise(recpart_total = sum(recpart))

#soma de recursos partidários por partido para mulheres
recursos_partido_fem <- dados %>%
  filter(genero == "Feminino") %>%
  filter(recpart > 0 | recpart != NA) %>%
  group_by(sigla) %>%
  summarise(recpart_total_fem = sum(recpart))


##juntando no banco de dados

dados <- dados %>%
  left_join(recursos_partido, by = "sigla") %>%
  left_join(recursos_partido_fem, by = "sigla")


#fazendo as variáveis de proporção de candidaturas femininas por partido e coligação
dados <- dados %>%
  mutate(recpart_fem_pct = (recpart_total_fem / recpart_total))


####################################################
#####VARIÁVEIS DESCRITIVAS DOS VOTOS E RECURSOS#####
####################################################

dados_recursos_eleitas <- dados %>%
  filter(genero == "Feminino" & eleito == "Eleito") %>%
  summarise(soma_rectotal = sum(rectotal, na.rm = TRUE),
            media_rectotal = mean(rectotal, na.rm = TRUE),
            mediana_rectotal = median(rectotal, na.rm = TRUE),
            desvio_rectotal = sd(rectotal, na.rm = TRUE),
            minimo_rectotal = min(rectotal, na.rm = TRUE),
            maximo_rectotal = max(rectotal, na.rm = TRUE),
            soma_recpart = sum(recpart, na.rm = TRUE),
            media_recpart = mean(recpart, na.rm = TRUE),
            mediana_recpart = median(recpart, na.rm = TRUE),
            desvio_recpart = sd(recpart, na.rm = TRUE),
            minimo_recpart = min(recpart, na.rm = TRUE),
            maximo_recpart = max(recpart, na.rm = TRUE))

dados_recursos_eleitos <- dados %>%
  filter(genero == "Masculino" & eleito == "Eleito") %>%
  summarise(soma_rectotal = sum(rectotal, na.rm = TRUE),
            media_rectotal = mean(rectotal, na.rm = TRUE),
            mediana_rectotal = median(rectotal, na.rm = TRUE),
            desvio_rectotal = sd(rectotal, na.rm = TRUE),
            minimo_rectotal = min(rectotal, na.rm = TRUE),
            maximo_rectotal = max(rectotal, na.rm = TRUE),
            soma_recpart = sum(recpart, na.rm = TRUE),
            media_recpart = mean(recpart, na.rm = TRUE),
            mediana_recpart = median(recpart, na.rm = TRUE),
            desvio_recpart = sd(recpart, na.rm = TRUE),
            minimo_recpart = min(recpart, na.rm = TRUE),
            maximo_recpart = max(recpart, na.rm = TRUE))

dados_recursos_mulheres <- dados %>%
  filter(genero == "Feminino") %>%
  summarise(soma_rectotal = sum(rectotal, na.rm = TRUE),
            media_rectotal = mean(rectotal, na.rm = TRUE),
            mediana_rectotal = median(rectotal, na.rm = TRUE),
            desvio_rectotal = sd(rectotal, na.rm = TRUE),
            minimo_rectotal = min(rectotal, na.rm = TRUE),
            maximo_rectotal = max(rectotal, na.rm = TRUE),
            soma_recpart = sum(recpart, na.rm = TRUE),
            media_recpart = mean(recpart, na.rm = TRUE),
            mediana_recpart = median(recpart, na.rm = TRUE),
            desvio_recpart = sd(recpart, na.rm = TRUE),
            minimo_recpart = min(recpart, na.rm = TRUE),
            maximo_recpart = max(recpart, na.rm = TRUE))

dados_recursos_homens <- dados %>%
  filter(genero == "Masculino") %>%
  summarise(soma_rectotal = sum(rectotal, na.rm = TRUE),
            media_rectotal = mean(rectotal, na.rm = TRUE),
            mediana_rectotal = median(rectotal, na.rm = TRUE),
            desvio_rectotal = sd(rectotal, na.rm = TRUE),
            minimo_rectotal = min(rectotal, na.rm = TRUE),
            maximo_rectotal = max(rectotal, na.rm = TRUE),
            soma_recpart = sum(recpart, na.rm = TRUE),
            media_recpart = mean(recpart, na.rm = TRUE),
            mediana_recpart = median(recpart, na.rm = TRUE),
            desvio_recpart = sd(recpart, na.rm = TRUE),
            minimo_recpart = min(recpart, na.rm = TRUE),
            maximo_recpart = max(recpart, na.rm = TRUE))






