install.packages("ggplot2")
library(ggplot2)
setwd("D:/Dropbox/Mestrado/Seminário Discente/2017/graficos")

#boxplot dos votos
bplot <- ggplot(dados, aes(genero, log.votos_total_cand)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Votos por Gênero", x = "Gênero", y = "Votos (log)")
ggsave("boxplot_votos.png", width = 10, height = 5)


##gráfico de barras de candidaturas por partido e por gênero 
g <-ggplot(data = dados, aes(x = reorder(sigla, cand_part_fem_pct),  y = cand_part, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red")  +
#colorido:    scale_fill_manual(values = c("maroon4", "seagreen")) +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Partido", x = "Partido", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1)) + 
  coord_flip()
ggsave("barplot_candidaturas_genero_partido_polar.png", width = 10, height = 5)


##gráfico de barras de candidaturas por coligação e por gênero
g <-ggplot(data = dados, aes(x = reorder(colig, cand_colig_fem_pct),  y = cand_colig, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red")  +
#colorido:    scale_fill_manual(values = c("maroon4", "seagreen")) +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Coligação", x = "Coligação", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1)) + 
  coord_flip()
ggsave("barplot_candidaturas_genero_colig.png", width = 10, height = 5)


##gráfico de barras de candidaturas por situação e por gênero
g <-ggplot(data = dados, aes(x = reorder(situ, -cand_situ_fem_pct),  y = cand_situ, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red")  +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Situação de Registro", x = "Situação do Registro", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1))
ggsave("barplot_candidaturas_genero_situ.png", width = 10, height = 5)



#kernel density dos votos
kdensity <- ggplot(data=dados, aes(x=voto_total_cand.log, fill=factor(genero))) +
  geom_density(alpha=0.5) +
  theme_bw()+
  scale_fill_grey(name = "Gênero", start = 0.6, end = 0, na.value = "red") +
  labs(title = "Gráfico de Densidade dos Votos por Gênero",
       x = "Log dos Votos Totais",
       y = "Densidade")
ggsave("kernel_votos.png", width = 10, height = 5)


##Votos por eleito e gênero
#tirando o Suplicy porque ele distorce o gráfico
dados <- dados %>%
  arrange(desc(votos_total_cand))
dados_semsuplicy <- dados[-c(1), ]


p <- ggplot(dados_semsuplicy, aes(x = eleito, y = votos_total_cand)) +
  theme_bw() +
  scale_colour_grey(name = "Gênero", start = 0, end = 0.6, na.value = "red") +
  scale_shape_manual(values=c(19)) +
  geom_jitter(alpha = 0.7, aes(colour = factor(genero), size = votos_total_cand)) +
  labs(title ="Votação entre eleitos e não eleitos", 
       x = "Resultado", 
       y = "Votos") + 
  scale_size_continuous(name = "Número de votos",
                      breaks = c(0, 1000, 10000, 50000, 100000),
                      labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos")) 
ggsave("jitter_resultado_genero_votos.png", width = 10, height = 5)


##votação coligação e gênero
#usando o banco de dados sem o suplicy, mas filtrando só por coligações que tiveram candidatos eleitos.
#fazendo uma variável que indica se houve candidato eleito - dummy, pra poder somar depois

dados <- dados %>% 
  mutate(eleito_dummy = recode(result, 
                               "Eleito por média" = 1, 
                               "Eleito por QP" = 1, 
                               "Suplente" = 0,
                               "Não Eleito" = 0))
#fazendo a variável vereadores, que indica quanto cada coligação elegeu no total
summary(dados$eleito_dummy)
dados_colig <- dados %>%
  group_by(colig, eleito_dummy) %>%
  summarise(vereadores = sum(eleito_dummy)) %>%
  filter(eleito_dummy == 1)

#criando um banco que só tenha os candidatos que estão em coligações que elegeram ao menos um vereador
dados_coligacoes <- dados_semsuplicy %>%
  right_join(dados_colig, by = "colig")

#fazendo o gráfico
p <- ggplot(dados_coligacoes, aes(x = colig, y = votos_total_cand, label = nome_urna)) +
  theme_bw() +
  scale_colour_manual(name = "Gênero", values = c("black","grey69")) +
  scale_shape_manual(name="Resultado", values=c(18,4)) +
  geom_jitter(alpha = 0.7, aes(colour = factor(genero), size = votos_total_cand, shape = eleito)) +
  labs(title ="Votação Por Coligação (somente que elegeram candidatos)", 
       x = "Coligação", 
       y = "Votos") + 
  scale_size_continuous(name = "Número de votos",
                        breaks = c(0, 1000, 10000, 50000, 100000),
                        labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos")) +
  geom_text_repel(data= dados_eleitas, size=2) +
  theme(axis.text.x=element_text(angle=40, hjust=1))
ggsave("jitter_resultado_genero_votos_coligacao.png", width = 10, height = 5)

#versão colorida com fundo preto:
p <- ggplot(dados_coligacoes, aes(x = colig, y = votos_total_cand, alpha=genero,label = nome_urna)) +
  theme_bw() +
  scale_colour_manual(name = "Gênero", values = c("coral", "seagreen1")) +
  scale_shape_manual(name="Resultado", values=c(18,4)) +
  geom_jitter(aes(colour = factor(genero), size = votos_total_cand, shape = eleito), guide=FALSE) +
  scale_alpha_manual(values = c(1, 0.4), guide=FALSE) + 
  labs(title ="Votação Por Coligação (somente que elegeram candidatos)", 
       x = "Coligação", 
       y = "Votos") + 
  scale_size_continuous(name = "Número de votos",
                        breaks = c(0, 1000, 10000, 50000, 100000),
                        labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos")) +
  geom_text_repel(data= dados_eleitas, size=2, colour="coral4") +
  theme(axis.text.x=element_text(angle=25, hjust=1)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), #tira as linhas
          panel.background=element_blank()) + #tira as linhas
  theme(panel.background=element_rect(fill='black')) #pinta o fundo de preto
ggsave("jitter_resultado_genero_votos_coligacao_cor.png", width = 10, height = 5)

###################################################
#########GRÁFICOS PARA FINANCIAMENTO################
###################################################

#boxplot dos recursos totais
bptotal <- ggplot(dados, aes(genero, rectotal.log)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Recursos Totais por Gênero", x = "Gênero", y = "Recursos Totais (log)")
bplot
ggsave("boxplot_recursos_totais.png", width = 10, height = 5)


#boxplot dos recursos do partido
bppart <- ggplot(dados, aes(genero, recpart.log)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Recursos Partidários por Gênero", x = "Gênero", y = "Recursos Partidários (log)")
bplot
ggsave("boxplot_recursos_partidarios.png", width = 10, height = 5)


#boxplot dos recursos do partido
bplot3 <- ggplot(dados, aes(genero, reccand.log)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Recursos de outros candidatos por Gênero", x = "Gênero", y = "Recursos de outros candidatos (log)")
bplot
ggsave("boxplot_recursos_candidatos.png", width = 10, height = 5)

require(gridExtra)
grid.arrange(bplot2, bplot3, ncol=1)
ggsave("boxplot_recursos_candidatos_partidos.png", width = 10, height = 5, arrangeGrob(bplot2, bplot3, ncol=1))



##gráfico de barras de candidaturas por partido e por gênero 
g <-ggplot(data = dados, aes(x = reorder(sigla, -recpart_fem_pct),  y = recpart, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red") +
  labs(title ="Distribuição dos recursos partidários por gênero", x = "Partido", y = "% de recursos partidários") + 
  theme(axis.text.x=element_text(angle=50, hjust=1))
g
ggsave("barplot_recursospartido_candidatos.png", width = 10, height = 5)


#kernel density dos recursos partidários e total
ktotal <- ggplot(data=dados, aes(x=rectotal.log, fill=factor(genero))) +
  geom_density(alpha=0.5) +
  theme_bw()+
  scale_fill_grey(name = "Gênero", start = 0.6, end = 0, na.value = "red") +
  labs(title = "Densidade dos Recursos Totais por Gênero",
       x = "Log dos Recursos Totais",
       y = "Densidade")

kpart<- ggplot(data=dados, aes(x=recpart.log, fill=factor(genero))) +
  geom_density(alpha=0.5) +
  theme_bw()+
  scale_fill_grey(name = "Gênero", start = 0.6, end = 0, na.value = "red") +
  labs(title = "Densidade dos Recursos Partidários por Gênero",
       x = "Log dos Recursos Partidários",
       y = "Densidade")

##juntando os kdensity com os boxplot numa mesma figura
ggsave("boxplot_kdensity_recursos_candidatos_partidos.png", width = 10, height = 5, arrangeGrob(bptotal, ktotal, bppart, kpart, ncol=2))


##Recursos des eleites
dados_eleites <- dados %>%
  filter(eleito == "Eleito") %>%
  arrange(desc(rectotal))
glimpse(dados_eleites)
dados_eleites <- dados_eleites[-c(1), ]
#deixando os NA dos recursos como 0 pra indicar que não recebeu nada
dados_eleites$recpart.log[is.na(dados_eleites$recpart.log)] <- 0
dados_eleites$rectotal.log[is.na(dados_eleites$rectotal.log)] <- 0
library(ggrepel)

p <- ggplot(dados_eleites, aes(x = recpart.log, 
                       y = rectotal.log, 
                       label = nome_urna, 
                       size = votos_total_cand, 
                       colour = factor(genero))) +
     theme_bw() +
     scale_colour_grey(name = "Gênero", start = 0, end = 0.6, na.value = "red") +
     scale_shape_manual(values=c(19)) +
     geom_jitter(alpha = 0.7) +
          labs(title ="Distribuição de Recursos Partidários e Totais entre os eleitos", 
                   x = "Recursos Partidários", 
                   y = "Recursos totais") + 
     scale_size_continuous(name = "Número de votos",
                    breaks = c(0, 5000, 10000, 50000, 100000),
                    labels=c("Zero votos", "5 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos")) + 
     geom_text_repel(data = dados_eleitas, size=2) +
     theme(axis.text.x=element_text(angle=40, hjust=1))
p
ggsave("jitter_recursos_genero_votos.png", width = 10, height = 5)


#boxplot dos votos dos eleitos
bplot <- ggplot(dados_eleites, aes(genero, voto_total_cand.log)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Votos dos Eleitos por Gênero", x = "Gênero", y = "Votos (log)")
bplot
ggsave("boxplot_votos_eleites.png", width = 10, height = 5)


#kernel density dos votos eleitos
kdensity <- ggplot(data=dados_eleites, aes(x=voto_total_cand.log, fill=factor(genero))) +
  geom_density(alpha=0.5) +
  theme_bw()+
  scale_fill_grey(name = "Gênero", start = 0.6, end = 0, na.value = "red") +
  labs(title = "Gráfico de Densidade dos Votos dos Eleitos por Gênero",
       x = "Log dos Votos Totais",
       y = "Densidade")
kdensity
ggsave("kernel_votos_eleites.png", width = 10, height = 5)

ggsave("boxplot_kernel_votos_eleites.png", width = 10, height = 5, arrangeGrob(bplot, kdensity, ncol=1))






