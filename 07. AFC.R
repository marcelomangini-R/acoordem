####################################################################
# Autor: Marcelo Mangini Dias
# Data: 17/11/2025
# Última Atualização: 12/01/2026
# Objetivo: Análise Fatorial Confirmatória 
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(lavaan)
library(semPlot)
library(boot)
library(readxl)
library(semTools)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(tibble)
library(openxlsx)
library(coin)
library(rstatix)
library(jsonlite)
library(moments)

####################################################################
# Banco de dados
####################################################################
dados <- read_excel("AFC.xlsx")
cat("Dados carregados:", nrow(dados), "observações\n\n")

####################################################################
# METADADOS DOS ITENS (ESSENCIAL PARA O WEB APP)
####################################################################

# ATENÇÃO: Preencha esta seção com as informações corretas dos seus itens!
metadados_itens <- data.frame(
  Item = c("MD_PT1", "MD_PT2", "MD_PF", "MD_P",
           "MT_S", "MT_C", "MT_F", "MR_Q", "MR_C",
           "CB_A", "CB_R", "CP_P", "CE_P1", "CE_P2", "CE_T"),
  
  Descricao = c(
    "pinos na tábua - mão preferida",
    "pinos na tábua - mão não preferida",
    "Mudar pinos de fileira",
    "Pesponto",
    "Traçado simples",
    "Traçado complexo",
    "Cópia de figuras geométricas",
    "Manejo de tesoura - Recorte reto",
    "Manejo de tesoura - Recorte curvo",
    "Agarrar a bola repicada",
    "Repicar bola no chão",
    "Polichinelo",
    "Equilíbrio – perna preferida",
    "Equilíbrio – perna não preferida",
    "Tandem"
  ),
  
    Metrica = c(
    "tempo_segundos", "tempo_segundos", "tempo_segundos", "tempos_segundos",
    "erros", "erros", "acertos", "erros", "erros",
    "acertos", "acertos", "pontos", "tempo_segundos", "tempo_segundos", "pontos"
  ),
  
  Foi_Invertido = c(
    TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, FALSE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  
  # PREENCHA: se invertido, qual foi a fórmula? 
  # Exemplos: "max - x + min", "1/x", "100 - x"
  Formula_Inversao = c(
    "max - x + min", "max - x + min", "max - x + min", "max - x + min",
    "max - x + min", "max - x + min", NA, "max - x + min", "max - x + min",
    NA, NA, NA, NA, NA, NA
  ),
  
  Valor_Min = c(
    10, 11, 2, 2,
    0, 0, 2, 0, 0,
    0, 0, 1, 1, 1, 0
  ),
  
  Valor_Max = c(
    40, 44, 52, 121,
    38, 56, 10, 12, 34,
    5, 5, 3, 30, 28, 18
  ),
  
  stringsAsFactors = FALSE
)

cat("ATENÇÃO: Verifique se os metadados dos itens estão corretos!\n")
cat("Itens marcados como invertidos:", 
    sum(metadados_itens$Foi_Invertido), "de", nrow(metadados_itens), "\n\n")

# Exibir tabela de metadados
ft_metadados <- flextable(metadados_itens) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left") %>%
  bg(i = ~ Foi_Invertido == TRUE, j = "Foi_Invertido", bg = "#FFF9C4") %>%
  set_caption("Metadados dos Itens") %>%
  add_footer_lines("Verifique se  informações estão corretas!")

print(ft_metadados)
cat("\n")

####################################################################
# Especificação dos modelos
####################################################################

modelo_hierarquico <- '
  Fator1 =~ MD_PT1 + MD_PT2 + MD_PF + MD_P
  Fator2 =~ MT_S + MT_C + MT_F + MR_Q + MR_C
  Fator3 =~ CB_A + CB_R + CP_P + CE_P1 + CE_P2 + CE_T
  
  Geral =~ Fator1 + Fator2 + Fator3
  
  MR_Q ~~ MR_C
'

modelo_bifactor <- '
  Geral =~ MD_PT1 + MD_PT2 + MD_PF + MD_P + 
           MT_S + MT_C + MT_F + MR_Q + MR_C + 
           CB_A + CB_R + CP_P + CE_P1 + CE_P2 + CE_T
  
  Fator1 =~ MD_PT1 + MD_PT2 + MD_PF + MD_P
  Fator2 =~ MT_S + MT_C + MT_F + MR_Q + MR_C
  Fator3 =~ CB_A + CB_R + CP_P + CE_P1 + CE_P2 + CE_T
  
  Fator1 ~~ 0*Fator2
  Fator1 ~~ 0*Fator3
  Fator2 ~~ 0*Fator3
  
  Geral ~~ 0*Fator1
  Geral ~~ 0*Fator2
  Geral ~~ 0*Fator3
  
  MR_Q ~~ MR_C
'

modelo_unifatorial <- '
  Geral =~ MD_PT1 + MD_PT2 + MD_PF + MD_P + 
           MT_S + MT_C + MT_F + MR_Q + MR_C + 
           CB_A + CB_R + CP_P + CE_P1 + CE_P2 + CE_T
  
  MR_Q ~~ MR_C
'

####################################################################
# Ajuste dos modelos
####################################################################

fit_hierarquico <- cfa(modelo_hierarquico, data = dados, 
                       estimator = "MLR", missing = "fiml")

fit_bifactor <- cfa(modelo_bifactor, data = dados, 
                    estimator = "MLR", missing = "fiml",
                    control = list(iter.max = 10000, eval.max = 20000, rel.tol = 1e-6))

fit_unifatorial <- cfa(modelo_unifatorial, data = dados, 
                       estimator = "MLR", missing = "fiml")

####################################################################
# Tabela 1: Comparação de índices de ajuste
####################################################################

extrair_indices <- function(fit, nome) {
  idx <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                            "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                            "srmr", "aic", "bic"))
  data.frame(
    Modelo = nome,
    `χ²` = round(idx["chisq"], 2),
    gl = idx["df"],
    `p-valor` = ifelse(idx["pvalue"] < 0.001, "< 0.001", round(idx["pvalue"], 3)),
    CFI = round(idx["cfi"], 3),
    TLI = round(idx["tli"], 3),
    RMSEA = round(idx["rmsea"], 3),
    `IC 90%` = paste0("[", round(idx["rmsea.ci.lower"], 3), "; ", 
                      round(idx["rmsea.ci.upper"], 3), "]"),
    SRMR = round(idx["srmr"], 3),
    AIC = round(idx["aic"], 1),
    BIC = round(idx["bic"], 1),
    check.names = FALSE
  )
}

tab_comparacao <- rbind(
  extrair_indices(fit_hierarquico, "Hierárquico"),
  extrair_indices(fit_bifactor, "Bifactor"),
  extrair_indices(fit_unifatorial, "Unifatorial")
)

ft_comparacao <- flextable(tab_comparacao) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  bg(i = ~ CFI >= 0.95, j = "CFI", bg = "#C8E6C9") %>%
  bg(i = ~ CFI >= 0.90 & CFI < 0.95, j = "CFI", bg = "#FFF9C4") %>%
  bg(i = ~ TLI >= 0.95, j = "TLI", bg = "#C8E6C9") %>%
  bg(i = ~ TLI >= 0.90 & TLI < 0.95, j = "TLI", bg = "#FFF9C4") %>%
  bg(i = ~ RMSEA <= 0.06, j = "RMSEA", bg = "#C8E6C9") %>%
  bg(i = ~ RMSEA > 0.06 & RMSEA <= 0.08, j = "RMSEA", bg = "#FFF9C4") %>%
  bg(i = ~ SRMR <= 0.08, j = "SRMR", bg = "#C8E6C9") %>%
  set_caption("Tabela 1. Comparação de Índices de Ajuste dos Modelos") %>%
  add_footer_lines("Nota: Verde = ajuste adequado; Amarelo = ajuste aceitável. Critérios: CFI/TLI ≥ 0.90; RMSEA ≤ 0.08; SRMR ≤ 0.08")

print(ft_comparacao)

####################################################################
# Funções auxiliares para análises
####################################################################

calc_ave_cr <- function(fit) {
  std <- standardizedSolution(fit)
  loadings <- subset(std, op == "=~", select = c(lhs, rhs, est.std))
  fatores <- unique(loadings$lhs)
  resultados <- data.frame(Fator = character(), AVE = numeric(), CR = numeric(), stringsAsFactors = FALSE)
  for (f in fatores) {
    lam <- loadings$est.std[loadings$lhs == f]
    var_e <- 1 - lam^2
    AVE <- sum(lam^2) / (sum(lam^2) + sum(var_e))
    CR  <- (sum(lam))^2 / ((sum(lam))^2 + sum(var_e))
    resultados <- rbind(resultados, data.frame(Fator = f, AVE = AVE, CR = CR))
  }
  resultados
}

####################################################################
# Tabela 2: AVE e CR
####################################################################

tab_ave_cr_h <- calc_ave_cr(fit_hierarquico) %>% mutate(Modelo = "Hierárquico")
tab_ave_cr_b <- calc_ave_cr(fit_bifactor) %>% mutate(Modelo = "Bifactor")
tab_ave_cr_u <- calc_ave_cr(fit_unifatorial) %>% mutate(Modelo = "Unifatorial")

tab_ave_cr <- bind_rows(tab_ave_cr_h, tab_ave_cr_b, tab_ave_cr_u) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

ft_ave_cr <- flextable(tab_ave_cr) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left") %>%
  bg(i = ~ AVE >= 0.50, j = "AVE", bg = "#C8E6C9") %>%
  bg(i = ~ CR >= 0.70, j = "CR", bg = "#C8E6C9") %>%
  set_caption("Tabela 2. Variância Média Extraída (AVE) e Confiabilidade Composta (CR)") %>%
  add_footer_lines("Nota: Verde = valores adequados (AVE ≥ 0.50; CR ≥ 0.70)")

print(ft_ave_cr)
cat("\n")

####################################################################
# Tabela 3: Validade Discriminante
####################################################################

validade_discriminante <- function(fit, nome){
  cor_lv <- lavInspect(fit, "cor.lv")
  ave <- calc_ave_cr(fit)$AVE
  fatores <- unique(calc_ave_cr(fit)$Fator)
  combs <- t(combn(fatores, 2))
  resultados <- data.frame()
  for(i in 1:nrow(combs)){
    f1 <- combs[i,1]; f2 <- combs[i,2]
    cor <- abs(cor_lv[f1,f2])
    sqrt_ave1 <- sqrt(ave[which(fatores == f1)])
    sqrt_ave2 <- sqrt(ave[which(fatores == f2)])
    status <- ifelse(sqrt_ave1 > cor & sqrt_ave2 > cor, "✓", "✗")
    resultados <- rbind(resultados, 
                        data.frame(Fator_A = f1, Fator_B = f2,
                                   `|r|` = round(cor,3),
                                   `√AVE_A` = round(sqrt_ave1,3),
                                   `√AVE_B` = round(sqrt_ave2,3),
                                   Validade = status,
                                   check.names = FALSE))
  }
  resultados
}

tab_disc <- validade_discriminante(fit_hierarquico, "Hierárquico")

ft_disc <- flextable(tab_disc) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Tabela 3. Validade Discriminante (Critério de Fornell & Larcker)") %>%
  add_footer_lines("Nota: ✓ = validade discriminante adequada (√AVE > |correlação|)")

print(ft_disc)
cat("\n")

####################################################################
# Tabela 4: Confiabilidade
####################################################################

calc_conf <- function(fit, nome_modelo) {
  std <- standardizedSolution(fit)
  cargas <- std %>% filter(op == "=~") %>% select(lhs, rhs, est.std)
  fatores <- unique(cargas$lhs)
  resultados <- data.frame()
  
  for (fator in fatores) {
    lam <- cargas$est.std[cargas$lhs == fator]
    var_e <- 1 - lam^2
    k <- length(lam)
    alpha <- (k / (k - 1)) * (1 - sum(var_e) / sum((lam^2) + var_e))
    omega <- (sum(lam))^2 / ((sum(lam))^2 + sum(var_e))
    CR <- (sum(lam)^2) / ((sum(lam)^2) + sum(var_e))
    resultados <- rbind(resultados, data.frame(Modelo = nome_modelo, Fator = fator,
                                               `α` = round(alpha, 3), `ω` = round(omega, 3),
                                               CR = round(CR, 3), check.names = FALSE))
  }
  return(resultados)
}

tab_conf <- bind_rows(
  calc_conf(fit_hierarquico, "Hierárquico"),
  calc_conf(fit_bifactor, "Bifactor"),
  calc_conf(fit_unifatorial, "Unifatorial")
)

ft_conf <- flextable(tab_conf) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left") %>%
  bg(i = ~ `α` >= 0.70, j = "α", bg = "#C8E6C9") %>%
  bg(i = ~ `ω` >= 0.70, j = "ω", bg = "#C8E6C9") %>%
  bg(i = ~ CR >= 0.70, j = "CR", bg = "#C8E6C9") %>%
  set_caption("Tabela 4. Consistência Interna e Confiabilidade Composta") %>%
  add_footer_lines("Nota: Verde = confiabilidade adequada (≥ 0.70). α = Alfa de Cronbach; ω = Ômega de McDonald")

print(ft_conf)
cat("\n")

####################################################################
# Tabela 5: Cargas Fatoriais
####################################################################

tab_cargas <- standardizedSolution(fit_hierarquico) %>% 
  filter(op == "=~") %>% 
  select(Fator = lhs, Item = rhs, λ = est.std, SE = se, p = pvalue) %>%
  mutate(λ = round(λ, 3), SE = round(SE, 3),
         p = ifelse(p < 0.001, "< 0.001", round(p, 3)),
         `**` = ifelse(p == "< 0.001", "***", ifelse(as.numeric(p) < 0.01, "**",
                                                     ifelse(as.numeric(p) < 0.05, "*", "")))) %>%
  select(-p)

ft_cargas <- flextable(tab_cargas) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left") %>%
  bg(i = ~ λ >= 0.70, j = "λ", bg = "#C8E6C9") %>%
  bg(i = ~ λ >= 0.50 & λ < 0.70, j = "λ", bg = "#FFF9C4") %>%
  set_caption("Tabela 5. Cargas Fatoriais Padronizadas (Modelo Hierárquico)") %>%
  add_footer_lines("Nota: *** p < 0.001; ** p < 0.01; * p < 0.05. Verde = forte (λ ≥ 0.70); Amarelo = moderada (0.50-0.69)")

print(ft_cargas)
cat("\n")

