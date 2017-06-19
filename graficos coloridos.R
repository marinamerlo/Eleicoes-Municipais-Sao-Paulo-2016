library(ggplot2)
library(ggrepel)
setwd("D:/Dropbox/Mestrado/Seminário Discente/2017/graficos")


##densidade e boxplot por gênero e resultado
kdensity <- ggplot(data=dados, aes(x=voto_total_cand.log, fill=factor(genero))) +
  geom_density(alpha=0.4, size = 0.05, colour = "gray30") +
  theme_minimal()+
  scale_fill_manual(name = "Gênero", values = c("maroon4", "seagreen")) +
  labs(title = "Densidade dos Votos",
       x = "Votos Totais (log)",
       y = NULL) + 
  facet_grid(. ~eleito) + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  scale_y_reverse() + 
  coord_flip()
kdensity

boxplot <- ggplot(dados, aes(genero, voto_total_cand.log, fill=factor(genero))) +
  theme_minimal()+
  geom_boxplot(alpha=0.4, lwd=0.1, colour = "gray30")+
  scale_fill_manual(name = "Gênero", values = c("maroon4", "seagreen")) +
  facet_grid(. ~eleito) +
labs(title = "Distribuição dos votos",
                            y = "Votos Totais (log)",
                            x = "Gênero") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

boxplot

library(gridExtra)
grid.arrange(kdensity, boxplot, ncol=1)
ggsave("boxplot_kdensity_votos_resultado_genero.png", width = 5, height = 5, arrangeGrob(bplot2, bplot3, ncol=1))


##gráfico de barras de candidaturas por coligação e por gênero
g <-ggplot(data = dados, aes(x = reorder(colig, cand_colig_fem_pct),  y = cand_colig, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal()+
  scale_fill_manual(name = "Gênero", values = c("maroon4", "seagreen")) +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Coligação", x = "Coligação", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1), 
        legend.position = "bottom") +
  coord_flip() +
  scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75, 1),
                labels = c("100%","75%","50%", "25%", "0%"))
g 
ggsave("barplot_candidaturas_genero_colig_cor.png", width = 8, height = 5)

##gráfico de barras de candidaturas por partido e por gênero 
g <-ggplot(data = dados, aes(x = reorder(sigla, cand_part_fem_pct),  y = cand_part, fill = genero)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw()+
  scale_fill_manual(name = "Gênero", values = c("maroon4", "seagreen")) +
  geom_hline(yintercept = 0.7) +
  labs(title ="Gênero das candidaturas por Partido", x = "Partido", y = "% de candidaturas") + 
  theme(axis.text.x=element_text(angle=50, hjust=1), 
        legend.position = "bottom") +
  coord_flip() +
  scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75, 1),
                labels = c("100%","75%","50%", "25%", "0%"))
g
ggsave("barplot_candidaturas_genero_partido_cor.png", width = 8, height = 5)

library(ggrepel)
p <- ggplot(dados_semsuplicy, aes(x = eleito, y = votos_total_cand, alpha=as.factor(genero))) +
  theme_bw() +
  scale_colour_manual(name = "Gênero", values = c("coral", "seagreen1")) +
  scale_shape_manual(values=c(19)) +
  geom_jitter(aes(colour = factor(genero), size = votos_total_cand)) +
  scale_alpha_manual(values = c(0.9, 0.2)) + 
  labs(title ="Votação entre eleitos e não eleitos", 
       x = "Resultado", 
       y = "Votos") + 
  scale_size_continuous(name = "Número de votos",
                        breaks = c(0, 1000, 10000, 50000, 100000),
                        labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos"),
                        range = c(0.5,15)) + 
  theme(panel.grid.major=element_blank(),
        panel.background=element_blank()) + 
  theme(panel.background=element_rect(fill='black'))+
  theme(axis.text.x=element_text(size=15))
p
ggsave("jitter_resultado_genero_votos_cor.png", width = 10, height = 5)





p <- ggplot(dados_coligacoes, aes(x = colig, y = votos_total_cand, alpha=genero,label = nome_urna)) +
  theme_bw() +
  scale_colour_manual(name = "Gênero", values = c("coral", "seagreen1")) +
  scale_shape_manual(name="Resultado", values=c(18,4)) +
  geom_jitter(aes(colour = factor(genero), size = votos_total_cand, shape = eleito), guide=FALSE) +
  scale_alpha_manual(values = c(1, 0.4), guide=FALSE) + 
  theme(axis.text.x=element_text(angle=25, hjust=1))  + 
  theme(panel.grid.major=element_blank(),
        panel.background=element_blank()) + 
  theme(panel.background=element_rect(fill='black')) +
  geom_vline(xintercept=c(1.5,2.5,3.5, 4.5, 5.5, 6.5, 7.5, 8.5,9.5),color="gray15") +
  labs(title ="Votação Por Coligação (somente que elegeram candidatos)", 
       x = "Coligação", 
       y = "Votos") + 
  scale_size_continuous(name = "Número de votos",
                        breaks = c(0, 1000, 10000, 50000, 100000),
                        labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos"),
                        range = c(0.5,10)) +
  geom_text_repel(data= dados_eleitas, size=2, colour="coral2")

ggsave("jitter_resultado_genero_votos_coligacao_cor.png", width = 10, height = 5)

p <- ggplot(dados_semsuplicy, aes(x = eleito, y = voto_total_cand.log, alpha=as.factor(genero))) +
  theme_bw() +
  scale_colour_manual(name = "Gênero", values = c("coral", "seagreen1")) +
  scale_shape_manual(values=c(19)) +
  geom_jitter(aes(colour = factor(genero), size = votos_total_cand)) +
  scale_alpha_manual(values = c(0.9, 0.2)) + 
  labs(title ="Votação entre eleitos e não eleitos", 
       x = "Resultado", 
       y = "Votos (log)") + 
  scale_size_continuous(name = "Número de votos",
                        breaks = c(0, 1000, 10000, 50000, 100000),
                        labels=c("Zero votos", "1 mil votos",  "10 mil votos", "50 mil votos", "100 mil votos"),
                        range = c(0.5,15)) + 
  theme(panel.grid.major=element_blank(),
        panel.background=element_blank()) + 
  theme(panel.background=element_rect(fill='black'))+
  theme(axis.text.x=element_text(size=15))
p
ggsave("jitter_resultado_genero_votos_log_cor.png", width = 10, height = 5)
