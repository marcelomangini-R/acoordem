####################################################################
# Autor: Marcelo Mangini Dias
# Data: 03/10/2025
# Última Atualização: 24/10/2025
# Objetivo: Análise Fatorial Exploratória
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(readxl)
library(psych)
library(lavaan)
library(semTools)
library(EGAnet)
library(mirt)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)

####################################################################
# Banco de Dados e variáveis de interesse
####################################################################
dados <- read_excel("AFE.xlsx")

variaveis         <- c("MD_PT1", "MD_PT2", 
                       "MD_PF", 
                       #                       "MD_MC1", "MD_MC2", 
                       "MD_C", 
                       "MD_P", 
                       "MT_S", 
                       "MT_C", 
                       #                       "MR_Q", 
                       "MR_C",
                       #                       "CL_1", "CL_2", 
                       "MT_F", 
                       "CB_A", 
                       "CB_R", 
                       "CP_A1", "CP_A2", 
                       "CP_P", 
                       "CE_P1", "CE_P2", 
                       "CE_T")
dados_afe <- dados[, variaveis]

####################################################################
# Análise Fatorial Exploratória 
####################################################################

efa_resultado <- fa(dados_afe, nfactors = 3, fm = "minres", rotate = "oblimin")
efa_resultado

####################################################################
# Índices de adequação do modelo: RMSEA, CFI, TLI
####################################################################

modelo_lavaan <- '
F1 =~ MD_PT1 + MD_PT2 + MD_PF + MD_P + MT_F + CE_P1 + CE_P2 + CE_T
F2 =~ MD_C + MT_S + MT_C + MR_C
F3 =~ CB_A + CB_R + CP_A1 + CP_A2
'
fit_lavaan <- cfa(modelo_lavaan, data = dados_afe, estimator = "MLR", std.lv = TRUE)
summary(fit_lavaan, fit.measures = TRUE, standardized = TRUE)

####################################################################
# Índices de confiabilidade (Ω), replicabilidade (H) e Variância Média Extraída
####################################################################

confiabilidade <- compRelSEM(fit_lavaan)
AVE_result <- AVE(fit_lavaan)

tabela_final <- tibble(
  Fator = names(confiabilidade),
  Omega = round(as.numeric(confiabilidade), 3),
  AVE = round(as.numeric(AVE_result), 3)
)


pe <- parameterEstimates(fit_lavaan, standardized = TRUE)
loadings_stdall <- pe %>%
  dplyr::filter(op == "=~") %>%
  dplyr::select(lhs, rhs, std.all, std.lv) %>%
  dplyr::rename(
    Fator = lhs,
    Item = rhs,
    lambda_std_all = std.all,
    lambda_std_lv = std.lv
  )

H_stdall <- loadings_stdall %>%
  dplyr::mutate(uni_stdall = 1 - lambda_std_all^2) %>%
  dplyr::group_by(Fator) %>%
  dplyr::summarise(
    n_itens = dplyr::n(),
    soma_lambda = sum(lambda_std_all, na.rm = TRUE),
    soma_uni = sum(uni_stdall, na.rm = TRUE),
    H_stdall = (soma_lambda^2) / (soma_lambda^2 + soma_uni)
  ) %>%
  dplyr::ungroup()

H_stdlv <- loadings_stdall %>%
  dplyr::mutate(uni_stdlv = 1 - lambda_std_lv^2) %>%
  dplyr::group_by(Fator) %>%
  dplyr::summarise(
    H_stdlv = (sum(lambda_std_lv, na.rm = TRUE)^2) /
      (sum(lambda_std_lv, na.rm = TRUE)^2 + sum(1 - lambda_std_lv^2, na.rm = TRUE))
  ) %>%
  dplyr::ungroup()

tabela_completa <- tabela_final %>%
  dplyr::left_join(H_stdall %>% dplyr::select(Fator, H_stdall), by = "Fator") %>%
  dplyr::left_join(H_stdlv %>% dplyr::select(Fator, H_stdlv), by = "Fator") %>%
  dplyr::mutate(
    H_stdall = round(H_stdall, 3),
    H_stdlv = round(H_stdlv, 3)
  )

tabela_rep <- flextable(tabela_completa) %>%
  set_header_labels(
    Fator = "Fator",
    Omega = "Confiabilidade (Ω)",
    AVE = "Variância Média Extraída",
    H_stdall = "Replicabilidade (H - std.all)",
    H_stdlv = "Replicabilidade (H - std.lv)"
  ) %>%
  theme_booktabs() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 11, part = "all")

tabela_rep

####################################################################
# Unidimensionalidade
####################################################################

L <- as.matrix(efa_resultado$loadings)
R <- cor(dados_afe, use = "pairwise.complete.obs")

common <- L %*% t(L)
total_var <- diag(R)
ECV <- sum(diag(common)) / sum(total_var)
residuals <- R - common
MIREAL <- mean(abs(residuals[lower.tri(residuals)]))
UniCo <- cor(c(R[lower.tri(R)]), c(common[lower.tri(common)]))
unidim <- data.frame(UniCo, ECV, MIREAL)
flextable(round(unidim, 3))


####################################################################
# Pratt - Importância dos Itens
####################################################################
loadings <- as.data.frame(efa_resultado$loadings[1:ncol(dados_afe), ])
communalities <- efa_resultado$communality
pratt <- sweep(loadings^2, 1, communalities, FUN = "*")
pratt_rel <- sweep(pratt, 2, colSums(pratt), FUN = "/")

tabela_pratt <- pratt_rel %>%
  mutate(Item = rownames(loadings)) %>%
  relocate(Item)

tabela_pratt_ft <- tabela_pratt
tabela_pratt_ft[-1] <- round(tabela_pratt_ft[-1], 3)  # arredonda todas as colunas, exceto Item

flextable(tabela_pratt_ft)
