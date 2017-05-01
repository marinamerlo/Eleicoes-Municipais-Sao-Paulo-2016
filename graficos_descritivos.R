install.packages("ggplot2")
library(ggplot2)
setwd("D:/Dropbox/Mestrado/Seminário Discente/2017/graficos")

#boxplot dos votos
bplot <- ggplot(dados, aes(genero, log.votos_total_cand)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Votos por Gênero", x = "Gênero", y = "Votos (log)")
ggsave("boxplot_votos.png", width = 10, height = 5)


##gráfico de barras de candidaturas por partido e por gênero (polar)
g <-ggplot(data = dados, aes(x = reorder(sigla, -cand_part_fem_pct),  y = cand_part, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red")  +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Partido", x = "Partido", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1)) + 
  coord_polar(theta = "x", direction=1 )
ggsave("barplot_candidaturas_genero_partido_polar.png", width = 10, height = 5)


##gráfico de barras de candidaturas por coligação e por gênero
g <-ggplot(data = dados, aes(x = reorder(colig, -cand_colig_fem_pct),  y = cand_colig, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_grey(start = 0.5, end = 0.7, na.value = "red")  +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Coligação", x = "Coligação", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1))
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
kdensity <- ggplot(data=dados, aes(x=log.votos_total_cand, fill=genero)) +
  geom_density(alpha=0.4) +
  theme_bw()+
  scale_x_continuous(name="Log dos Votos Totais") +
  scale_y_continuous(name="Densidade") +
  guides(fill=guide_legend(title="Gênero"))
ggsave("kernel_votos.png", width = 15, height = 5)



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



