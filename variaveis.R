### criando as variáveis necessárias a partir do banco final ###

dados <- dados %>% 
  mutate(genero = recode(genero, 
                         "MASCULINO" = "Masculino", 
                         "FEMININO" = "Feminino"))

dados <- dados %>% 
  mutate(situ = recode(situ, 
                       "RENUNCIA" = "Renúncia", #por alguma razão, ele não dá o replace nesse caso. Tive que recorrer ao Excel.
                       "DEFERIDO" = "Deferido", 
                       "INDEFERIDO" = "Indeferido", 
                       "INDEFERIDO COM RECURSO" ="Indeferido com Recurso"))


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
  mutate(rectotal = sum(reccom, recint, recni, reccand, recpart, recfis, recprop, recfin, na.rm=TRUE))

#vendo se ficou ok
summary(dados$rectotal)  
  

#variável pra indicar se foi eleito ou não em 2016
dados$eleito <- ifelse(dados$result == "ELEITO POR M\xc9DIA" | dados$result == "ELEITO POR QP", c("Eleito"), c("Nao Eleito"))
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
summary(dados$rectotal.log)

plot(dados$rectotal.log,dados$voto_total_cand.log)

#variável pra indicar se recebeu recursos ou não
dados$recurso.f <- ifelse(dados$rectotal == 0, c("Nao Recebeu Recursos"), c("Recebeu Recursos"))
dados$recurso.f <- as.factor(dados$recurso.f)
summary(dados$recurso.f)


#################
####contagens####
#################

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

#contagem total de candidaturas por partido
candidaturas_partido <- dados %>%
  group_by(sigla) %>%
  summarise(cand_part = n())

#contagem total de candidaturas por coligação
candidaturas_colig <- dados %>%
  group_by(colig) %>%
  summarise(cand_colig = n())

#contagem total de candidaturas por situação
candidaturas_situ <- dados %>%
  group_by(situ) %>%
  summarise(cand_situ = n())

##juntando no banco de dados as contagens
dados <- dados %>%
  left_join(genero_partido, by = "sigla") %>%
  left_join(genero_colig, by = "colig") %>%
  left_join(genero_situ, by = "situ") %>%
  left_join(candidaturas_partido, by = "sigla") %>%
  left_join(candidaturas_colig, by = "colig") %>%
  left_join(candidaturas_situ, by = "situ")

#fazendo as variáveis de proporção de gênero
dados <- dados %>%
  mutate(cand_part_fem_pct = (cand_part_fem / cand_part)) %>%
  mutate(cand_colig_fem_pct = (cand_colig_fem / cand_colig)) %>%
  mutate(cand_situ_fem_pct = (cand_situ_fem / cand_situ))

dados_cand <- dados %>%
  select(-votos_total, -num_zona)

dados_cand <- distinct(dados_cand) 
