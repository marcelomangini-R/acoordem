####################################################################
# Autor: Marcelo Mangini Dias
# Data: 31/01/2026
# Última Atualização: 02/03/2026
# Objetivo: Cálculo de escore 
####################################################################

####################################################################
# Bibliotecas
####################################################################

library(readxl)
library(dplyr)
library(lavaan)
library(openxlsx)
library(tidyr)
library(jsonlite)
library(tibble)

####################################################################
# Banco de dados
####################################################################

dados <- read_xlsx("AFC.xlsx")

itens <- c(
  "MD_PT1", "MD_PT2", "MD_PF", "MD_P",
  "MT_S", "MT_C", "MT_F", "MR_Q", "MR_C",
  "CB_A", "CB_R", "CP_P", "CE_P1", "CE_P2", "CE_T"
)



####################################################################
# Base normativa
####################################################################
estat_itens <- dados %>%
  select(all_of(itens)) %>%
  summarise(across(
    everything(),
    list(
      media = mean,
      dp = sd,
      minimo = min,
      maximo = max
    ),
    na.rm = TRUE
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(.+)_(media|dp|minimo|maximo)"
  )

####################################################################
# Ajuste do modelo da AFC
####################################################################

modelo <- '
Fator1 =~ MD_PT1 + MD_PT2 + MD_PF + MD_P
Fator2 =~ MT_S + MT_C + MT_F + MR_Q + MR_C
Fator3 =~ CB_A + CB_R + CP_P + CE_P1 + CE_P2 + CE_T

Geral =~ Fator1 + Fator2 + Fator3

MR_Q ~~ MR_C
'

fit <- cfa(
  modelo,
  data = dados,
  std.lv = TRUE,
  missing = "listwise"
)

####################################################################
# Cargas fatoriais (lambda)
# Matriz de covariância (sigma)
# pesos fatoriais (B)
####################################################################
Lambda <- inspect(fit, "est")$lambda[, c("Fator1", "Fator2", "Fator3")]

dados_complete <- lavInspect(fit, "data")
Sigma <- cov(dados_complete[, itens])

stopifnot(all(rownames(Lambda) == colnames(Sigma)))

Sigma_inv <- solve(Sigma)

B <- Sigma_inv %*% Lambda %*%
  solve(t(Lambda) %*% Sigma_inv %*% Lambda)

colnames(B) <- colnames(Lambda)


###########################################################################
# Validação estrutural: mínimos normativos, imputação, médias e centramento
###########################################################################

min_itens <- estat_itens %>%
  select(Item, minimo) %>%
  deframe()

dados_imp <- as.data.frame(dados_complete[, itens])
for (j in itens) {
  dados_imp[[j]][is.na(dados_imp[[j]])] <- min_itens[j]
}

media_itens <- colMeans(dados_imp)

Xc <- sweep(dados_imp, 2, media_itens, "-")

eta_manual <- as.matrix(Xc) %*% B
colnames(eta_manual) <- c("Fator1", "Fator2", "Fator3")

eta_lavaan <- lavPredict(fit, type = "lv")[, c("Fator1", "Fator2", "Fator3")]

stopifnot(all(rownames(B) == colnames(Xc)))

###########################################################################
# Validação dimensional e de comportamento monotômico
###########################################################################

stopifnot(
  nrow(Xc) == nrow(eta_manual),
  ncol(B) == ncol(eta_manual)
)


itens_F1 <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_P")
itens_F2 <- c("MT_S", "MT_C", "MT_F", "MR_Q", "MR_C")
itens_F3 <- c("CB_A", "CB_R", "CP_P", "CE_P1", "CE_P2","CE_T")

cor_F1 <- cor(
  eta_manual[, "Fator1"],
  rowSums(Xc[, itens_F1]),
  use = "pairwise.complete.obs"
)
cor_F2 <- cor(
  eta_manual[, "Fator2"],
  rowSums(Xc[, itens_F2]),
  use = "pairwise.complete.obs"
)
cor_F3 <- cor(
  eta_manual[, "Fator3"],
  rowSums(Xc[, itens_F3]),
  use = "pairwise.complete.obs"
)

stopifnot(cor_F1 > 0.7)
stopifnot(cor_F2 > 0.7)
stopifnot(cor_F3 > 0.7)

###########################################################################
# Validação de escala
###########################################################################
range_eta_f1 <- range(eta_manual[, "Fator1"])
range_soma_f1 <- range(rowSums(Xc[, itens_F1]))
range_eta_f2 <- range(eta_manual[, "Fator2"])
range_soma_f2 <- range(rowSums(Xc[, itens_F2]))
range_eta_f3 <- range(eta_manual[, "Fator3"])
range_soma_f3 <- range(rowSums(Xc[, itens_F3]))

stopifnot(
  max(abs(range_eta_f1)) < max(abs(range_soma_f1))
)
stopifnot(
  max(abs(range_eta_f2)) < max(abs(range_soma_f2))
)
stopifnot(
  max(abs(range_eta_f3)) < max(abs(range_soma_f3))
)

###########################################################################
# Validação de reprodutibilidade
###########################################################################

x_raw <- as.data.frame(dados_imp[, itens])

itens_ordem <- rownames(B)
x_raw <- x_raw[, itens_ordem]

Xc <- sweep(x_raw, 2, media_itens, "-")
eta_manual_js_base <- as.matrix(Xc) %*% B

calc_js_like <- function(x_raw_row, mu, B) {
  x_c <- x_raw_row - mu
  as.numeric(x_c %*% B)
}

set.seed(123)
i <- sample(seq_len(nrow(x_raw)), 5)

eta_js_sim <- t(sapply(i, function(j) {
  calc_js_like(
    x_raw_row = as.numeric(x_raw[j, ]),
    mu        = media_itens,
    B         = B
  )
}))

stopifnot(
  all(
    apply(eta_js_sim / eta_manual_js_base[i, ], 2, function(x)
      sd(x, na.rm = TRUE) < 1e-12
    )
  )
)

colnames(B) <- c("F1", "F2", "F3")

###########################################################################
# Cálculo das normas totais e por idade
###########################################################################

cargas_geral <- standardizedSolution(fit) %>%
  filter(op == "=~", lhs == "Geral") %>%
  select(rhs, est.std)

w_geral <- setNames(
  cargas_geral$est.std,
  cargas_geral$rhs
)

w_geral <- w_geral / sum(abs(w_geral))
names(w_geral) <- c("F1", "F2", "F3")


escores_1ordem <- as.data.frame(eta_manual)

names(w_geral) <- colnames(escores_1ordem)
escore_geral <- as.matrix(escores_1ordem) %*%
  matrix(w_geral[colnames(escores_1ordem)], ncol = 1)

escores <- bind_cols(
  escores_1ordem,
  Geral = as.numeric(escore_geral)
)

normas_totais <- escores %>%
  summarise(across(
    everything(),
    list(media = mean, dp = sd),
    na.rm = TRUE
  ))

dados_escores <- bind_cols(dados, escores)

normas_idade <- dados_escores %>%
  group_by(Idade) %>%
  summarise(across(
    Fator1:Geral,
    list(media = mean, dp = sd),
    na.rm = TRUE
  ))%>%
  rename(
    F1_m = Fator1_media,
    F1_sd = Fator1_dp,
    F2_m = Fator2_media,
    F2_sd = Fator2_dp,
    F3_m = Fator3_media,
    F3_sd = Fator3_dp,
    G_m  = Geral_media,
    G_sd = Geral_dp
  )

normas_idade_json <- normas_idade %>%
  pivot_longer(
    -Idade,
    names_to = c("Fator", "Estat"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = Estat,
    values_from = value
  ) %>%
  group_by(Idade) %>%
  summarise(
    normas = list(pick(everything())),
    .groups = "drop"
  )



percentis <- escores %>%
  reframe(across(
    everything(),
    ~ quantile(.x, probs = c(.05, .10, .25, .50, .75, .90, .95),
               na.rm = TRUE)
  ))


pesos_df <- as.data.frame(B)
pesos_df$Item <- rownames(B)

###########################################################################
# Exportação
###########################################################################

wb <- createWorkbook()

addWorksheet(wb, "Estatisticas_Itens")
writeData(wb, "Estatisticas_Itens", estat_itens)

addWorksheet(wb, "Cargas_Fatoriais")
writeData(wb, "Cargas_Fatoriais", standardizedSolution(fit))

addWorksheet(wb, "Pesos_Fatoriais_1ordem")
writeData(wb, "Pesos_Fatoriais_1ordem", pesos_df)

addWorksheet(wb, "Pesos_Geral")
writeData(wb, "Pesos_Geral", data.frame(Fator = names(w_geral), Peso = w_geral))

addWorksheet(wb, "Normas_Totais")
writeData(wb, "Normas_Totais", normas_totais)

addWorksheet(wb, "Normas_Idade")
writeData(wb, "Normas_Idade", normas_idade)

addWorksheet(wb, "Percentis")
writeData(wb, "Percentis", percentis)

addWorksheet(wb, "Medias_Escore")
writeData(
  wb,
  "Medias_Escore",
  data.frame(Item = names(media_itens), Media = media_itens)
)

saveWorkbook(wb, "Normas_AFC_1ordem.xlsx", overwrite = TRUE)


json_out <- list(
  itens = itens,
  medias_itens = media_itens[itens],
  minimos_itens = min_itens[itens],
  pesos_1ordem = setNames(
    apply(B, 2, function(col) {
      setNames(as.numeric(col), rownames(B))
    }),
    c("F1", "F2", "F3")
  ),
  pesos_geral = w_geral,
  normas_totais = normas_totais,
  normas_idade = normas_idade_json,
  percentis = percentis
)

write_json(
  json_out,
  path = "Normas_AFC_1ordem.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

