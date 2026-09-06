####################################################################
# Autor: Marcelo Mangini Dias
# Data: 27/08/2026
# Última Atualização: 01/09/2026
# Objetivo: Calcular e registrar escores para amostra normativa 
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(readxl)
library(dplyr)
library(openxlsx)
library(lubridate)

####################################################################
# Banco de Dados
####################################################################
dados_originais <- read_xlsx("AFC.xlsx")

if(!"idade" %in% names(dados_originais)) {
  names(dados_originais)[grepl("^idade$", names(dados_originais), ignore.case = TRUE)] <- "idade"
}
if(!"idademeses" %in% names(dados_originais)) {
  names(dados_originais)[grepl("idademeses|idade_meses", names(dados_originais), ignore.case = TRUE)] <- "idademeses"
}


####################################################################
# Parâmetros (conforme normas.js e script.js)
####################################################################
medias_itens <- c(MD_PT1=31.63725, MD_PT2=33.11765, MD_PF=27.16667, MD_P=81.13235, MT_S=35.34804, MT_C=47.92647, MT_F=6.88725, MR_Q=10.96569, MR_C=30.01471, CB_A=3.75, CB_R=3.13725, CP_P=1.76961, CE_P1=14.40196, CE_P2=13.37255, CE_T=7.60294)
minimos <- c(MD_PT1=10, MD_PT2=11, MD_PF=2, MD_P=2, MT_S=0, MT_C=0, MT_F=2, MR_Q=0, MR_C=0, CB_A=0, CB_R=0, CP_P=1, CE_P1=1, CE_P2=1, CE_T=0)
maximos <- c(MD_PT1=40, MD_PT2=44, MD_PF=52, MD_P=121, MT_S=38, MT_C=56, MT_F=10, MR_Q=12, MR_C=34, CB_A=5, CB_R=5, CP_P=8, CE_P1=30, CE_P2=28, CE_T=18)
itens_inv <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_P", "MT_S", "MT_C", "MR_Q", "MR_C")

B <- matrix(c(0.243682762, 0.055050151, 0.005147185, 0.185519042, 0.004695337, 0.010218983, 0.060679405, -0.026269557, -0.011078658, 0.019792749, -0.006182236, -0.000443347, 0.08489407, 0.113128454, 0.013613171, -0.013404282, 0.146136111, 0.012875122, -0.126669243, 0.673403462, -0.116624387, -0.049897279, 0.047963441, 0.029611497, 0.04037321, 0.029091513, 0.010314317, -0.028229955, -0.082544054, 0.458889294, 0.015744167, 0.05753447, 0.226421357, -0.083449355, 0.046385968, 0.386266076, -0.006710357, 0.011419953, 0.092984116, 0.025229091, 0.017406145, 0.113369796, -0.016081933, -0.044730866, 0.139457736), ncol=3, byrow=T)
w_g <- c(F1=0.341121495, F2=0.328528177, F3=0.330350328)

normas_idade <- list(
  "4"=list(F1=c(-3.8752,3.0526), F2=c(-3.3508,2.9208), F3=c(-3.2518,2.0848), G=c(-3.4970,2.2067)),
  "5"=list(F1=c(-0.6435,2.8403), F2=c(-0.0305,1.8860), F3=c(-0.0721,2.4676), G=c(-0.2533,1.9165)),
  "6"=list(F1=c(0.7754,1.8459),  F2=c(0.5533,1.5285),  F3=c(0.0646,1.4379),  G=c(0.4676,1.0552)),
  "7"=list(F1=c(1.5921,1.4916),  F2=c(1.1232,1.2038),  F3=c(0.9581,1.5005),  G=c(1.2286,0.9417)),
  "8"=list(F1=c(2.2299,1.2340),  F2=c(1.7599,1.1876),  F3=c(2.3681,1.2795),  G=c(2.1212,0.8620))
)

####################################################################
# Interpolação para idade
####################################################################
get_params_interpolated <- function(idade, fator) {
  idades_ref <- c(4, 5, 6, 7, 8)
  idade_clip <- max(4, min(8, idade))
  idx_low <- max(which(idades_ref <= idade_clip))
  idx_high <- min(which(idades_ref >= idade_clip))
  p_low <- normas_idade[[as.character(idades_ref[idx_low])]][[fator]]
  if (idx_low == idx_high) return(p_low)
  p_high <- normas_idade[[as.character(idades_ref[idx_high])]][[fator]]
  t <- (idade_clip - idades_ref[idx_low]) / (idades_ref[idx_high] - idades_ref[idx_low])
  return(c(m = p_low[1] + t * (p_high[1] - p_low[1]), d = p_low[2] + t * (p_high[2] - p_low[2])))
}

####################################################################
# Cálculo
####################################################################

dados_final <- dados_originais %>%
  mutate(Idade_Calc = idademeses / 12)

dados_proc_mat <- matrix(0, nrow=nrow(dados_final), ncol=length(medias_itens))
colnames(dados_proc_mat) <- names(medias_itens)

for(it in names(medias_itens)) {
  val <- as.numeric(dados_final[[it]])
  
  val[is.na(val)] <- if(it %in% itens_inv) maximos[it] else minimos[it]
  
  if(it %in% itens_inv) {
    dados_proc_mat[, it] <- (maximos[it] + minimos[it]) - val
  } else {
    dados_proc_mat[, it] <- val
  }
}

Xc <- sweep(dados_proc_mat, 2, medias_itens, "-")
lat <- Xc %*% B
lat_G <- (lat[,1]*w_g[1]) + (lat[,2]*w_g[2]) + (lat[,3]*w_g[3])

for(i in 1:nrow(dados_final)) {
  id <- dados_final$Idade_Calc[i]
  for(f in 1:3) {
    p <- get_params_interpolated(id, paste0("F", f))
    dados_final[i, paste0("Fator", f)] <- round(((lat[i, f] - p[1]) / p[2]) * 10 + 50)
  }
  pg <- get_params_interpolated(id, "G")
  dados_final[i, "Geral"] <- round(((lat_G[i] - pg[1]) / pg[2]) * 10 + 50)
}

####################################################################
# Exportação
####################################################################
write.xlsx(dados_final, "DadosComEscores.xlsx", overwrite = TRUE)
