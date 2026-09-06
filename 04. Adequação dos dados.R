####################################################################
# Autor: Marcelo Mangini Dias
# Data: 16/09/2025
# Última Atualização: 21/10/2025
# Objetivo: Análise de adequação dos dados para a realização da AFE
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(readxl)
library(psych)
library(ggplot2)
library(reshape2)
library(flextable)
library(dplyr)
library(MVN)

####################################################################
# Banco de Dados e variáveis de interesse
####################################################################
dados <- read_excel("AFE.xlsx")


variaveis_analise <- c("MD_PT1", "MD_PT2", 
                       "MD_PF", 
                       "MD_MC1", "MD_MC2", 
                       "MD_C", 
                       "MD_P", 
                       "MT_S", 
                       "MT_C", 
#                       "MR_Q",      remoção a posteriori de variáveis 
#                       "MR_C",     com KMO baixo
#                       "CL_1", "CL_2", 
                       "MT_F", 
                       "CB_A", 
                       "CB_R", 
                       "CP_A1", "CP_A2", 
                       "CP_P", 
#                       "CE_P1", 
                       "CE_P2", 
                       "CE_T")

variaveis_existentes <- variaveis_analise[variaveis_analise %in% names(dados)]
dados_limpos <- dados %>% 
  dplyr::select(all_of(variaveis_existentes)) %>% 
  na.omit()

####################################################################
# KMO
####################################################################

matriz_correlacao <- cor(dados_limpos, use = "complete.obs")
kmo_resultado <- psych::KMO(matriz_correlacao)
kmo_geral <- kmo_resultado$MSA
kmo_individual <- kmo_resultado$MSAi

classificar_kmo <- function(valor) {
  case_when(
    valor >= 0.90 ~ "Excelente",
    valor >= 0.80 ~ "Bom", 
    valor >= 0.70 ~ "Médio",
    valor >= 0.60 ~ "Medíocre",
    valor >= 0.50 ~ "Ruim",
    TRUE ~ "Inaceitável"
  )
}

####################################################################
# Bartlett e testes de normalidade
####################################################################

bartlett <- cortest.bartlett(matriz_correlacao, n = nrow(dados_limpos))

teste_mardia <- mvn(data = dados_limpos, mvnTest = "mardia", multivariatePlot = "qq")
print(teste_mardia$multivariateNormality)

teste_hz <- mvn(data = dados_limpos, mvnTest = "hz", multivariatePlot = "qq")
print(teste_hz$multivariateNormality)


####################################################################
# correlações
####################################################################
correlacoes_valores <- matriz_correlacao[upper.tri(matriz_correlacao)]
correlacoes_abs <- abs(correlacoes_valores)

estatisticas_corr <- list(
  media = mean(correlacoes_abs),
  moderadas = sum(correlacoes_abs >= 0.30 & correlacoes_abs < 0.90),
  altas = sum(correlacoes_abs >= 0.90),
  baixas = sum(correlacoes_abs < 0.30),
  total = length(correlacoes_abs)
)

####################################################################
# Resultados
####################################################################
tabela_kmo <- data.frame(
  Variavel = c("KMO GERAL", names(kmo_individual)),
  MSA = c(kmo_geral, kmo_individual),
  Classificacao = c(classificar_kmo(kmo_geral), sapply(kmo_individual, classificar_kmo)),
  stringsAsFactors = FALSE
) %>%
  mutate(MSA = round(MSA, 3))

tabela_testes <- data.frame(
  Teste = c("Bartlett Chi-quadrado", "Graus de liberdade", "p-valor", 
            "Adequação da matriz", "Correlações moderadas (0.30-0.89)", 
            "Correlações altas (≥0.90)", "Correlações baixas (<0.30)", 
            "Correlação média absoluta"),
  Valor = c(round(bartlett$chisq, 2), bartlett$df, 
            format.pval(bartlett$p.value, digits = 3, eps = 0.001),
            ifelse(bartlett$p.value < 0.05, "Adequada", "Inadequada"),
            estatisticas_corr$moderadas, estatisticas_corr$altas, 
            estatisticas_corr$baixas, round(estatisticas_corr$media, 3)),
  Status = c("", "", ifelse(bartlett$p.value < 0.05, "✓ Significativo", "✗ Não significativo"),
             ifelse(bartlett$p.value < 0.05, "✓ OK", "✗ Problema"),
             ifelse(estatisticas_corr$moderadas > 0, "✓ Adequado", "⚠ Verificar"),
             ifelse(estatisticas_corr$altas > 0, "⚠ Multicolinearidade", "✓ OK"),
             ifelse(estatisticas_corr$baixas == estatisticas_corr$total, "✗ Inadequado", "✓ OK"),
             ifelse(estatisticas_corr$media >= 0.25, "✓ Adequada", "⚠ Baixa")),
  stringsAsFactors = FALSE
)

ft_kmo <- flextable(tabela_kmo) %>%
  set_header_labels(Variavel = "Variável/Teste", MSA = "MSA", Classificacao = "Classificação") %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  width(width = c(2, 1, 1.5)) %>%
  bg(bg = "#2E8B57", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  bg(i = 1, bg = "#E8F5E8") %>%
  bold(i = 1) %>%
  color(~ Classificacao %in% c("Ruim", "Inaceitável", "Medíocre"), ~ Classificacao, color = "#E53E3E") %>%
  bold(~ Classificacao %in% c("Ruim", "Inaceitável", "Medíocre"), ~ Classificacao)

print(ft_kmo)

ft_testes <- flextable(tabela_testes) %>%
  set_header_labels(Teste = "Teste/Estatística", Valor = "Valor", Status = "Status") %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  width(width = c(2.5, 1.2, 1.3)) %>%
  bg(bg = "#4169E1", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  color(~ grepl("✗|⚠", Status), ~ Status, color = "#E53E3E") %>%
  color(~ grepl("✓", Status), ~ Status, color = "#38A169") %>%
  bold(~ !grepl("^$", Status), ~ Status)

print(ft_testes)

matriz_melt <- melt(matriz_correlacao)
names(matriz_melt) <- c("Var1", "Var2", "Correlacao")
mapa_calor <- ggplot(matriz_melt, aes(x = Var1, y = Var2, fill = Correlacao)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#B22222", mid = "white", high = "#4169E1",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlação"
  ) +
  geom_text(aes(label = round(Correlacao, 2)), 
            color = ifelse(abs(matriz_melt$Correlacao) > 0.6, "white", "black"),
            size = 2.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 9)
  ) +
  labs(
    title = "Matriz de Correlação",
    subtitle = paste("KMO =", round(kmo_geral, 3), "| Bartlett p <", 
                     ifelse(bartlett$p.value < 0.001, "0.001", round(bartlett$p.value, 3)))
  ) +
  coord_fixed()

print(mapa_calor)

cat("\n=== RESUMO FINAL ===\n")
cat("KMO Geral:", round(kmo_geral, 3), "(", classificar_kmo(kmo_geral), ")\n")
cat("Teste de Bartlett: p-valor", format.pval(bartlett$p.value, digits = 3), 
    "(", ifelse(bartlett$p.value < 0.05, "matriz adequada", "matriz inadequada"), ")\n")
cat("Correlação média:", round(estatisticas_corr$media, 3), "\n")
cat("Variáveis com MSA < 0.60:", sum(kmo_individual < 0.60), "\n")
cat("Correlações moderadas/fortes:", estatisticas_corr$moderadas + estatisticas_corr$altas, 
    "de", estatisticas_corr$total, "\n")
