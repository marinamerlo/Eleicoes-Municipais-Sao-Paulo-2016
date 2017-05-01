setwd(D/Dropbox/Mestrado/Seminário Discente/2017/Dados)
### criando as variáveis necessárias ##

library(ggplot2)
library(dplyr)

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


#renomeando as variáveis de recursos para nomes mais curtos
make.names(names(dados))

dados <- dados %>%
  rename(reccom = `valor.Comercialização de bens ou realização de eventos`,
         recint = `valor.Doações pela Internet`,
         recni = `valor.Recursos de origens não identificadas`,
         reccand = `valor.Recursos de outros candidatos`,
         recpart = `valor.Recursos de partido político`,
         recfis = `valor.Recursos de pessoas físicas`,
         recprop = `valor.Recursos próprios`,
         recfin = `valor.Rendimentos de aplicações financeiras`)
    
#variável que tem o total de recursos recebidos
dados <- dados %>% 
  rowwise() %>% 
  mutate(rectotal = sum(as.numeric(reccom), 
                        as.numeric(recint), 
                        as.numeric(recni), 
                        as.numeric(reccand), 
                        as.numeric(recpart),
                        as.numeric(recfis),
                        as.numeric(recprop),
                        as.numeric(recfin), na.rm=TRUE))

#vendo se ficou ok
summary(dados$rectotal)  
  
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


#tirando o log dos votos
dados$voto_total_cand.log <- as.numeric(log(dados$votos_total_cand))
#transformando o -Inf em zero, pois é o resultado do log de 0
dados$voto_total_cand.log[is.infinite(dados$voto_total_cand.log) ] <- 0
summary(dados$voto_total_cand.log)


#tirando o log dos recursos totais
dados$rectotal.log <- as.numeric(log(dados$rectotal))
#transformando o -Inf em zero, pois é o resultado do log de 0
dados$rectotal.log[is.infinite(dados$rectotal.log) ] <- 0
dados$rectotal.log[dados$rectotal.log < 0] <- 0.0000000000000000000001
summary(dados$rectotal.log)

plot(dados$rectotal.log,dados$voto_total_cand.log)

#variável pra indicar se recebeu recursos ou não
dados$recurso.f <- ifelse(dados$rectotal == 0, c("Nao Recebeu Recursos"), c("Recebeu Recursos"))
dados$recurso.f <- as.factor(dados$recurso.f)
summary(dados$recurso.f)


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



################################
#####VARIÁVEIS DE RECURSOS#####
###############################

#renomeando as variáveis de recursos para nomes mais curtos
make.names(names(dados))

dados <- dados %>%
  rename(reccom = `valor.Comercialização de bens ou realização de eventos`,
         recint = `valor.Doações pela Internet`,
         recni = `valor.Recursos de origens não identificadas`,
         reccand = `valor.Recursos de outros candidatos`,
         recpart = `valor.Recursos de partido político`,
         recfis = `valor.Recursos de pessoas físicas`,
         recprop = `valor.Recursos próprios`,
         recfin = `valor.Rendimentos de aplicações financeiras`)

#variável que tem o total de recursos recebidos
dados <- dados %>% 
  rowwise() %>% 
  mutate(rectotal = sum(as.numeric(reccom), 
                        as.numeric(recint), 
                        as.numeric(recni), 
                        as.numeric(reccand), 
                        as.numeric(recpart),
                        as.numeric(recfis),
                        as.numeric(recprop),
                        as.numeric(recfin), na.rm=TRUE))

#vendo se ficou ok
summary(dados$rectotal)


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



