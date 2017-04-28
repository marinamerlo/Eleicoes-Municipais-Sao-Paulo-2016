install.packages("ggplot2")
library(ggplot2)

#boxplot dos votos
bplot <- ggplot(dados_SP, aes(genero, log.votos_total_cand)) +
  theme_bw()+
  geom_boxplot()+
  labs(title ="Distribuição de Votos por Gênero", x = "Gênero", y = "Votos (log)")
ggsave("boxplot_votos.png", width = 5, height = 5)




dados_SP$log.votos_total_cand <- log(dados_SP$votos_total_cand)
dados_SP$log.votos_total_cand[which(dados_SP$log.votos_total_cand == -Inf)] <- 0
summary(dados_SP$log.votos_total_cand)


kdensity <- ggplot(data=dados_SP, aes(x=log.votos_total_cand, fill=genero)) +
  geom_density(alpha=0.4) +
  theme_bw()+
  scale_x_continuous(name="Log dos Votos Totais") +
  scale_y_continuous(name="Densidade") +
  guides(fill=guide_legend(title="Gênero"))
ggsave("kernel_votos.png", width = 15, height = 5)
