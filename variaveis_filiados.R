
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


#resumo dos partidos
filia_partidos <- filia_total %>%
  left_join(filia_mulheres, by = "sigla") %>%
  left_join(filia_homens, by = "sigla") %>%
  left_join(filia_indet, by = "sigla") %>%
  mutate(fem_masc = (n_mulheres_partido / n_homens_partido)*100) %>%
  mutate(pct_fem = (n_mulheres_partido / n_partido)*100) %>%
  mutate(pct_masc = (n_homens_partido / n_partido)*100) %>%
  mutate(pct_indet = (n_indet_partido / n_partido)*100)

#salvando a tabela final dos resumos dos partidos
write.table(filia_partidos, "filiados_partidos.csv", sep = ";", dec=",", fileEncoding ="UTF-8", row.names = F)
