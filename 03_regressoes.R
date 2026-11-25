# 03_regressoes.R - Regressões, uma etapa por bloco
# Trabalho de Econometria I - Leonardo Soares e Rodrigo Santos

library(lmtest) # bptest e coeftest
library(car)    # hccm (erros-padrão robustos) e linearHypothesis

# Rode com a pasta do projeto como diretório de trabalho (veja o README)
dir.create("saidas", showWarnings = FALSE) # Cria a pasta saidas, se não existir
options(scipen = 999) # Evita notação científica nas saídas
enem <- readRDS("dados/enem2023_limpo.rds") # Base limpa criada no 01_limpeza.R
ordem_mor <- c("1", "2", "3", "4", "5", "6", "7", "8mais") # Ordem natural de moradores_cat (para tabelas)


# ===================== ETAPA 1: regressão simples =====================

ols1 <- lm(nota_media ~ moradores, enem) # Nota média em função do número de moradores
summary(ols1) # Coeficientes, erros-padrão (homocedásticos), R² e teste F

sd(enem$nota_media) # Desvio-padrão da nota média
coef(ols1)["moradores"] / sd(enem$nota_media) # Efeito de +1 morador em desvios-padrão da nota

# Gráfico: médias observadas por número de moradores x reta ajustada da ols1
enem$mor8 <- ifelse(enem$moradores >= 8, 8, enem$moradores) # Agrupa 8 ou mais em 8 (só para este gráfico)
media_mor8 <- tapply(enem$nota_media, enem$mor8, mean) # Nota média em cada número de moradores (1 a 8)
round(media_mor8, 1) # Mostra as médias
png("saidas/figura3.png", width = 1600, height = 1200, res = 200) # Figura 3 do artigo (título fica no Word)
plot(1:8, media_mor8, pch = 19, xaxt = "n",
     ylim = range(media_mor8, coef(ols1)[1] + coef(ols1)[2] * c(1, 8)), # Eixo y cobre pontos e reta
     main = "", xlab = "Moradores no domicílio", ylab = "Nota média")
axis(1, at = 1:8, labels = c("1", "2", "3", "4", "5", "6", "7", "8+")) # Rótulos do eixo x
abline(ols1, col = "red", lwd = 2) # Reta ajustada da ols1
legend("topright", legend = c("Média observada", "Reta da regressão simples (coluna 1)"),
       pch = c(19, NA), lty = c(NA, 1), col = c("black", "red"), lwd = c(NA, 2)) # Legenda
dev.off() # Fecha e salva o arquivo


# ===================== ETAPA 1b: heterocedasticidade =====================

# Teste de Breusch-Pagan (H0: homocedasticidade)
bptest(ols1)

# Teste de White (versão simplificada, como na aula)
resid2 <- resid(ols1)^2 # Resíduos ao quadrado
yhat <- predict(ols1) # Valores preditos
yhat2 <- predict(ols1)^2 # Valores preditos ao quadrado
summary(lm(resid2 ~ yhat + yhat2)) # O teste F desta regressão é o teste de White

# Erros-padrão robustos de White (HC1). Daqui em diante: matriz robusta calculada uma vez por modelo
V1 <- hccm(ols1, type = "hc1") # Matriz de variância-covariância robusta da ols1
coeftest(ols1, vcov. = V1) # Coeficientes com erros-padrão robustos


# ===================== ETAPA 2: controle de renda =====================

ols2 <- lm(nota_media ~ moradores + renda, enem) # Acrescenta as 16 dummies de renda (base: ate1sm)
V2 <- hccm(ols2, type = "hc1") # Matriz robusta da ols2, calculada uma vez
coeftest(ols2, vcov. = V2) # Coeficientes com erros-padrão robustos (HC1)

# Comparação de b1 entre ols1 e ols2 (viés de variável omitida)
coef(ols1)["moradores"] # b1 sem controles
coef(ols2)["moradores"] # b1 controlando pela renda
coef(ols2)["moradores"] / sd(enem$nota_media) # b1 da ols2 em desvios-padrão da nota

# R² e R² ajustado das duas regressões
summary(ols1)$r.squared # R² da ols1
summary(ols1)$adj.r.squared # R² ajustado da ols1
summary(ols2)$r.squared # R² da ols2
summary(ols2)$adj.r.squared # R² ajustado da ols2

# Teste F conjunto: as 16 dummies de renda são todas iguais a zero? (robusto)
h_renda <- grep("renda", names(coef(ols2)), value = TRUE) # Nomes dos coeficientes de renda
h_renda # Mostra os nomes
linearHypothesis(ols2, h_renda, vcov. = V2) # Teste F robusto

# Frisch-Waugh-Lovell: o que significa "controlar pela renda"
aux_renda <- lm(moradores ~ renda, enem) # 1) Regride moradores nas dummies de renda
enem$res_mor <- resid(aux_renda) # Guarda o resíduo: a parte de moradores que a renda não explica
fwl <- lm(nota_media ~ res_mor, enem) # 2) Regride a nota nesse resíduo
coef(fwl)["res_mor"] # 3) Coeficiente da regressão FWL...
coef(ols2)["moradores"] # ...é idêntico ao b1 da ols2
summary(aux_renda)$r.squared # R² da regressão auxiliar: quanto da variação de moradores a renda explica
rm(aux_renda, fwl) # Apaga os modelos auxiliares para liberar memória


# ===================== ETAPA 3: escolaridade do pai e da mãe =====================

ols3 <- lm(nota_media ~ moradores + renda + esc_pai + esc_mae, enem) # Acrescenta as dummies de escolaridade
V3 <- hccm(ols3, type = "hc1") # Matriz robusta da ols3, calculada uma vez
coeftest(ols3, vcov. = V3) # Coeficientes com erros-padrão robustos (HC1)

# Evolução de b1: ols1 -> ols2 -> ols3
b1 <- c(coef(ols1)["moradores"], coef(ols2)["moradores"], coef(ols3)["moradores"]) # b1 nas três regressões
data.frame(modelo = c("ols1", "ols2", "ols3"),           # Nome do modelo
           b1     = round(b1, 3),                         # Em pontos da nota
           b1_dp  = round(b1 / sd(enem$nota_media), 4))   # Em desvios-padrão da nota

# Viés de variável omitida na renda: coeficientes de renda antes e depois da escolaridade
data.frame(ols2 = round(coef(ols2)[h_renda], 2), # Renda sem controlar escolaridade
           ols3 = round(coef(ols3)[h_renda], 2)) # Renda controlando escolaridade

# Multicolinearidade imperfeita
aux3 <- lm(moradores ~ renda + esc_pai + esc_mae, enem) # Regressão auxiliar de moradores nos controles
summary(aux3)$r.squared # R² auxiliar: quanto da variação de moradores os controles explicam
1 / (1 - summary(aux3)$r.squared) # Penalidade na variância de b1: 1/(1 - R²)
rm(aux3) # Apaga o modelo auxiliar para liberar memória
coeftest(ols2, vcov. = V2)["rendamais20sm", "Std. Error"] # EP robusto da renda mais alta na ols2
coeftest(ols3, vcov. = V3)["rendamais20sm", "Std. Error"] # EP robusto da renda mais alta na ols3

# Testes F conjuntos robustos: a escolaridade de cada um importa?
h_pai <- grep("esc_pai", names(coef(ols3)), value = TRUE) # Nomes das dummies do pai
linearHypothesis(ols3, h_pai, vcov. = V3) # H0: todas as dummies do pai = 0
h_mae <- grep("esc_mae", names(coef(ols3)), value = TRUE) # Nomes das dummies da mãe
linearHypothesis(ols3, h_mae, vcov. = V3) # H0: todas as dummies da mãe = 0

# Mãe vs. pai: coeficientes lado a lado (mesma base: nunca_estudou)
data.frame(pai = round(coef(ols3)[h_pai], 2), mae = round(coef(ols3)[h_mae], 2)) # Mesma ordem de níveis

# (a) Mãe = pai nos 6 níveis (sem a base e sem nao_sei, que significa coisas diferentes para pai e mãe)
h_mae_pai <- c("esc_maefund1_incomp = esc_paifund1_incomp",
               "esc_maefund1_comp = esc_paifund1_comp",
               "esc_maefund_comp = esc_paifund_comp",
               "esc_maemedio_comp = esc_paimedio_comp",
               "esc_maesuperior_comp = esc_paisuperior_comp",
               "esc_maepos_grad = esc_paipos_grad") # 6 restrições lineares
linearHypothesis(ols3, h_mae_pai, vcov. = V3) # F conjunto robusto

# (b) Só o nível superior completo
linearHypothesis(ols3, "esc_maesuperior_comp = esc_paisuperior_comp", vcov. = V3) # 1 restrição (F = t²)

# Pós-graduação acrescenta algo além do superior? (dentro de cada um dos pais)
linearHypothesis(ols3, "esc_maepos_grad = esc_maesuperior_comp", vcov. = V3) # Mãe: pós = superior?
linearHypothesis(ols3, "esc_paipos_grad = esc_paisuperior_comp", vcov. = V3) # Pai: pós = superior?

# Conferência da previsão do U: categorias de moradores (base 4) sem e com controles
ols_cat1 <- lm(nota_media ~ moradores_cat, enem) # Só as dummies de moradores
ols_cat3 <- lm(nota_media ~ moradores_cat + renda + esc_pai + esc_mae, enem) # Com os controles da ols3
coeftest(ols_cat1, vcov. = hccm(ols_cat1, type = 'hc1')) # Robusto, sem controles
round(coeftest(ols_cat3, vcov. = hccm(ols_cat3, type = 'hc1'))[1:8, ], 4) # Robusto, com controles (só intercepto e moradores)
h_cat <- grep("moradores_cat", names(coef(ols_cat1)), value = TRUE) # Nomes das dummies de moradores
data.frame(sem_controles = round(coef(ols_cat1)[h_cat], 2), # Diferença em relação a 4 moradores
           com_controles = round(coef(ols_cat3)[h_cat], 2)) # Mesma diferença, controlando renda e escolaridade
rm(ols_cat1, ols_cat3) # Apaga os modelos por categoria para liberar memória

# Evidência para o lado esquerdo do U: % de "nao_sei" (provável pai/mãe ausente) por número de moradores
round(tapply(enem$esc_pai == "nao_sei", enem$moradores_cat, mean)[ordem_mor] * 100, 1) # % pai "nao_sei"
round(tapply(enem$esc_mae == "nao_sei", enem$moradores_cat, mean)[ordem_mor] * 100, 1) # % mãe "nao_sei"


# ===================== ETAPA 4: controles demográficos, UF e tipo de escola =====================
gc() # Libera a memória dos modelos apagados antes das regressões mais pesadas

# 4a: só controles determinados antes da escolha do tamanho da família
ols4a <- lm(nota_media ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V4a <- hccm(ols4a, type = "hc1") # Matriz robusta da ols4a
coeftest(ols4a, vcov. = V4a) # Coeficientes com erros-padrão robustos

# 4b: acrescenta o tipo de escola (em parte mediador do canal de Becker)
ols4b <- lm(nota_media ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf + tipo_escola, enem)
V4b <- hccm(ols4b, type = "hc1") # Matriz robusta da ols4b
coeftest(ols4b, vcov. = V4b) # Coeficientes com erros-padrão robustos

# Evolução de b1 da ols1 à ols4b, com EP robusto, R² e R² ajustado
b1 <- c(coef(ols1)["moradores"], coef(ols2)["moradores"], coef(ols3)["moradores"],
        coef(ols4a)["moradores"], coef(ols4b)["moradores"]) # b1 nos 5 modelos
ep <- c(coeftest(ols1, vcov. = V1)["moradores", "Std. Error"], coeftest(ols2, vcov. = V2)["moradores", "Std. Error"],
        coeftest(ols3, vcov. = V3)["moradores", "Std. Error"], coeftest(ols4a, vcov. = V4a)["moradores", "Std. Error"],
        coeftest(ols4b, vcov. = V4b)["moradores", "Std. Error"]) # EP robusto de b1 nos 5 modelos
r2 <- c(summary(ols1)$r.squared, summary(ols2)$r.squared, summary(ols3)$r.squared,
        summary(ols4a)$r.squared, summary(ols4b)$r.squared) # R² dos 5 modelos
r2aj <- c(summary(ols1)$adj.r.squared, summary(ols2)$adj.r.squared, summary(ols3)$adj.r.squared,
          summary(ols4a)$adj.r.squared, summary(ols4b)$adj.r.squared) # R² ajustado dos 5 modelos
tab_mestra <- data.frame(modelo = c("ols1", "ols2", "ols3", "ols4a", "ols4b"), # Nome do modelo
                         b1     = round(b1, 3),                                 # b1 em pontos
                         ep_rob = round(ep, 3),                                 # Erro-padrão robusto (HC1)
                         b1_dp  = round(b1 / sd(enem$nota_media), 4),           # b1 em desvios-padrão da nota
                         r2     = round(r2, 4),                                 # R²
                         r2_aj  = round(r2aj, 4))                               # R² ajustado
tab_mestra # Mostra a tabela
write.csv(tab_mestra, "saidas/tab_mestra.csv", row.names = FALSE) # Salva para o texto
n_mod <- c(nobs(ols1), nobs(ols2), nobs(ols3), nobs(ols4a), nobs(ols4b)) # n de cada modelo (para a Tabela 3)
rm(ols1, ols2, ols3) # Apaga os modelos que não serão mais usados
gc() # Libera a memória

# Testes F conjuntos robustos na ols4b
h_cor <- grep("cor_raca", names(coef(ols4b)), value = TRUE) # Nomes das dummies de cor/raça
linearHypothesis(ols4b, h_cor, vcov. = V4b) # H0: todas as dummies de cor/raça = 0
h_uf <- grep("^uf", names(coef(ols4b)), value = TRUE) # Nomes das dummies de UF (^ = começa com "uf")
linearHypothesis(ols4b, h_uf, vcov. = V4b) # H0: todas as dummies de UF = 0
h_global <- names(coef(ols4b))[-1] # Todos os coeficientes, menos o intercepto
linearHypothesis(ols4b, h_global, vcov. = V4b) # F global: H0: todos os coeficientes = 0

# Coeficientes de sexo, cor/raça e escola privada na ols4b
round(coeftest(ols4b, vcov. = V4b)[c("sexofeminino", h_cor, "tipo_escolaprivada"), ], 3)

# Bônus: famílias maiores estão menos na escola privada?
round(tapply(enem$moradores, enem$tipo_escola, mean), 2) # Média de moradores por tipo de escola
round(tapply(enem$tipo_escola == "privada", enem$moradores_cat, mean)[ordem_mor] * 100, 1) # % na privada por moradores

b1_4a <- coef(ols4a)["moradores"] # Guarda o b1 da especificação principal (usado na Etapa 5)
rm(ols4a, ols4b) # Apaga os modelos da Etapa 4
gc() # Libera a memória


# ===================== ETAPA 5: não-linearidade (controles da ols4a) =====================

# 5a. Dummies para cada número de moradores (base: 4)
ols5a <- lm(nota_media ~ moradores_cat + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V5a <- hccm(ols5a, type = "hc1") # Matriz robusta da ols5a
ct5a <- coeftest(ols5a, vcov. = V5a) # Coeficientes com EP robusto (guardados para o gráfico)
h_cat <- c("moradores_cat1", "moradores_cat2", "moradores_cat3", "moradores_cat5",
           "moradores_cat6", "moradores_cat7", "moradores_cat8mais") # Dummies de moradores na ordem natural
round(ct5a[h_cat, ], 3) # Coeficientes das categorias (diferença em relação a 4 moradores)

# Teste F de linearidade com as dummies: se o efeito for linear, cada categoria k vale (k - 4) * b
h_lin <- c("moradores_cat3 = -1*moradores_cat5",
           "moradores_cat2 = -2*moradores_cat5",
           "moradores_cat1 = -3*moradores_cat5",
           "moradores_cat6 = 2*moradores_cat5",
           "moradores_cat7 = 3*moradores_cat5") # 5 restrições (8mais fica de fora: junta valores diferentes)
f_lin17 <- linearHypothesis(ols5a, h_lin, vcov. = V5a) # H0: efeito linear de 1 a 7 moradores
f_lin17 # Mostra o teste

# Mesmo teste só de 2 a 7 moradores (sem as categorias 1 e 8mais): a rejeição vem do grupo de 1 morador?
h_lin27 <- c("moradores_cat3 = -1*moradores_cat5",
             "moradores_cat2 = -2*moradores_cat5",
             "moradores_cat6 = 2*moradores_cat5",
             "moradores_cat7 = 3*moradores_cat5") # 4 restrições
f_lin27 <- linearHypothesis(ols5a, h_lin27, vcov. = V5a) # H0: efeito linear de 2 a 7 moradores
f_lin27 # Mostra o teste

# Gráfico fig7: coeficiente de cada categoria com intervalo de 95%
coef_cat <- c(ct5a[h_cat[1:3], "Estimate"], 0, ct5a[h_cat[4:7], "Estimate"]) # 0 na categoria base (4)
ep_cat <- c(ct5a[h_cat[1:3], "Std. Error"], 0, ct5a[h_cat[4:7], "Std. Error"]) # EP robusto (0 na base)
png("saidas/figura4.png", width = 1600, height = 1200, res = 200) # Figura 4 do artigo (título fica no Word)
plot(1:8, coef_cat, pch = 19, xaxt = "n",
     ylim = range(coef_cat - 1.96 * ep_cat, coef_cat + 1.96 * ep_cat), # Eixo y cobre os intervalos
     main = "", xlab = "Moradores no domicílio", ylab = "Diferença em relação a 4 moradores (pontos)")
segments(1:8, coef_cat - 1.96 * ep_cat, 1:8, coef_cat + 1.96 * ep_cat, lwd = 2) # Intervalo de 95%: coef ± 1,96·EP
abline(h = 0, lty = 2) # Linha em zero (sem diferença em relação a 4 moradores)
axis(1, at = 1:8, labels = c("1", "2", "3", "4", "5", "6", "7", "8+")) # Rótulos do eixo x
dev.off() # Fecha e salva o arquivo
r2_5a <- summary(ols5a)$r.squared # R² da ols5a (para a Tabela 4)
n_5a <- nobs(ols5a) # n da ols5a (para a Tabela 4)
rm(ols5a) # Apaga o modelo
gc() # Libera a memória

# 5b. Quadrático
ols5b <- lm(nota_media ~ moradores + I(moradores^2) + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V5b <- hccm(ols5b, type = "hc1") # Matriz robusta da ols5b
names(coef(ols5b))[1:3] # Confere o nome exato do termo quadrático
ct5b <- coeftest(ols5b, vcov. = V5b)[c("moradores", "I(moradores^2)"), ] # b1, b2 e o teste t de H0: b2 = 0
round(ct5b, 4) # Mostra (linearidade: t de I(moradores^2))

# Efeito marginal de +1 morador: b1 + 2*b2*m
me2 <- coef(ols5b)["moradores"] + 2 * coef(ols5b)["I(moradores^2)"] * 2 # Em m = 2
me2 # Mostra
lh2 <- linearHypothesis(ols5b, "moradores + 4*I(moradores^2) = 0", vcov. = V5b) # Teste do efeito em m = 2
lh2 # Mostra
me4 <- coef(ols5b)["moradores"] + 2 * coef(ols5b)["I(moradores^2)"] * 4 # Em m = 4
me4 # Mostra
lh4 <- linearHypothesis(ols5b, "moradores + 8*I(moradores^2) = 0", vcov. = V5b) # Teste do efeito em m = 4
lh4 # Mostra
me6 <- coef(ols5b)["moradores"] + 2 * coef(ols5b)["I(moradores^2)"] * 6 # Em m = 6
me6 # Mostra
lh6 <- linearHypothesis(ols5b, "moradores + 12*I(moradores^2) = 0", vcov. = V5b) # Teste do efeito em m = 6
lh6 # Mostra

# Ponto de mínimo da parábola (só faz sentido se b2 > 0): -b1 / (2*b2)
-coef(ols5b)["moradores"] / (2 * coef(ols5b)["I(moradores^2)"])
r2_5b <- summary(ols5b)$r.squared # R² da ols5b (para a Tabela 4)
n_5b <- nobs(ols5b) # n da ols5b (para a Tabela 4)
rm(ols5b) # Apaga o modelo
gc() # Libera a memória

# 5c. Cúbico: teste de linearidade de grau maior
ols5c <- lm(nota_media ~ moradores + I(moradores^2) + I(moradores^3) + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V5c <- hccm(ols5c, type = "hc1") # Matriz robusta da ols5c
round(coeftest(ols5c, vcov. = V5c)[c("moradores", "I(moradores^2)", "I(moradores^3)"), ], 4) # Termos em moradores
linearHypothesis(ols5c, c("I(moradores^2)", "I(moradores^3)"), vcov. = V5c) # F conjunto: H0: b2 = b3 = 0
rm(ols5c) # Apaga o modelo
gc() # Libera a memória

# 5d. Log-nível: efeito em % da nota
ols5d <- lm(log(nota_media) ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V5d <- hccm(ols5d, type = "hc1") # Matriz robusta da ols5d
coeftest(ols5d, vcov. = V5d)["moradores", ] # b1 do modelo log-nível com EP robusto
100 * coef(ols5d)["moradores"] # Variação % aproximada da nota por morador
100 * b1_4a / mean(enem$nota_media) # Comparação: b1 da ols4a dividido pela nota média, em %
summary(ols5d)$r.squared # R² do log-nível (NÃO comparável com o R² dos modelos em nível)
rm(ols5d) # Apaga o modelo
gc() # Libera a memória

# 5e. Sensibilidade aos extremos: ols4a sem moradores == 1 e sem moradores > 10
sum(enem$moradores == 1 | enem$moradores > 10) # Quantos alunos saem
ols4a_s <- lm(nota_media ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf,
              subset(enem, moradores > 1 & moradores <= 10)) # Mesma especificação, sem os extremos
V4a_s <- hccm(ols4a_s, type = "hc1") # Matriz robusta
coeftest(ols4a_s, vcov. = V4a_s)["moradores", ] # b1 sem os extremos
b1_4a # b1 da ols4a com a amostra completa, para comparar
rm(ols4a_s) # Apaga o modelo
gc() # Libera a memória


# ===================== ETAPA 6: interações (o efeito é igual para todos?) =====================

# 6a. Renda baixa (restrição de crédito): o efeito de +1 morador é maior nas famílias pobres?
enem$renda_baixa <- ifelse(enem$renda %in% c("nenhuma", "ate1sm", "1a1_5sm"), 1, 0) # 1 = até 1,5 SM
round(mean(enem$renda_baixa) * 100, 1) # % da amostra com renda baixa

# Armadilha das dummies: renda_baixa = 1 - (soma das dummies de renda acima de 1,5 SM),
# ou seja, é combinação linear exata do intercepto e das dummies de renda (multicolinearidade perfeita).
# Se entrar sozinha junto com renda, o R não consegue estimar o coeficiente e devolve NA:
armadilha <- lm(nota_media ~ moradores + renda + renda_baixa + moradores:renda_baixa, enem) # Modelo com a armadilha
coef(armadilha)[c("renda_baixa", "moradores:renda_baixa")] # renda_baixa = NA (não identificado)
rm(armadilha) # Apaga o modelo
gc() # Libera a memória

# Por isso entra só a interação; a diferença de nível entre os grupos já está nas dummies de renda
ols6a <- lm(nota_media ~ moradores + moradores:renda_baixa + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V6a <- hccm(ols6a, type = "hc1") # Matriz robusta da ols6a
ct6a <- coeftest(ols6a, vcov. = V6a)[c("moradores", "moradores:renda_baixa"), ] # b1 e b3 (guardados para a Tabela 5)
round(ct6a, 4) # Mostra (teste t de H0: b3 = 0)

coef(ols6a)["moradores"] # Efeito de +1 morador na renda NÃO baixa: b1
coef(ols6a)["moradores"] + coef(ols6a)["moradores:renda_baixa"] # Efeito na renda baixa: b1 + b3
linearHypothesis(ols6a, "moradores + moradores:renda_baixa = 0", vcov. = V6a) # Teste do efeito na renda baixa
rm(ols6a) # Apaga o modelo
gc() # Libera a memória

# Robustez: corte mais baixo, renda_baixa2 = só nenhuma e ate1sm (até 1 SM)
enem$renda_baixa2 <- ifelse(enem$renda %in% c("nenhuma", "ate1sm"), 1, 0) # 1 = até 1 SM
round(mean(enem$renda_baixa2) * 100, 1) # % da amostra com renda até 1 SM
ols6a2 <- lm(nota_media ~ moradores + moradores:renda_baixa2 + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem)
V6a2 <- hccm(ols6a2, type = "hc1") # Matriz robusta da ols6a2
ct6a2 <- coeftest(ols6a2, vcov. = V6a2)[c("moradores", "moradores:renda_baixa2"), ] # b1 e b3 (para a Tabela 5)
round(ct6a2, 4) # Mostra (teste t de H0: b3 = 0)
coef(ols6a2)["moradores"] # Efeito de +1 morador acima de 1 SM: b1
coef(ols6a2)["moradores"] + coef(ols6a2)["moradores:renda_baixa2"] # Efeito até 1 SM: b1 + b3
linearHypothesis(ols6a2, "moradores + moradores:renda_baixa2 = 0", vcov. = V6a2) # Teste do efeito até 1 SM
rm(ols6a2) # Apaga o modelo
gc() # Libera a memória

# 6b. Sexo (tarefas domésticas): o efeito de +1 morador é diferente para meninas?
ols6b <- lm(nota_media ~ moradores * sexo + renda + esc_pai + esc_mae + cor_raca + uf, enem) # moradores * sexo = moradores + sexo + moradores:sexo
V6b <- hccm(ols6b, type = "hc1") # Matriz robusta da ols6b
ct6b <- coeftest(ols6b, vcov. = V6b)[c("moradores", "sexofeminino", "moradores:sexofeminino"), ] # b1, b2 e b3 (para a Tabela 5)
round(ct6b, 4) # Mostra

coef(ols6b)["moradores"] # Efeito de +1 morador para homens: b1
coef(ols6b)["moradores"] + coef(ols6b)["moradores:sexofeminino"] # Efeito para mulheres: b1 + b3
linearHypothesis(ols6b, "moradores + moradores:sexofeminino = 0", vcov. = V6b) # Teste do efeito para mulheres

# F de "regressões idênticas" para homens e mulheres (mesmo intercepto e mesma inclinação em moradores)
linearHypothesis(ols6b, c("sexofeminino", "moradores:sexofeminino"), vcov. = V6b) # H0: b2 = b3 = 0
rm(ols6b) # Apaga o modelo
gc() # Libera a memória


# ===================== ETAPA 7: mecanismo (recursos materiais) =====================
# Computador e internet são MEDIADORES, não controles: a ols7 não substitui a ols4a

round(tapply(enem$tem_computador, enem$moradores_cat, mean)[ordem_mor] * 100, 1) # % com computador por moradores
round(tapply(enem$tem_internet, enem$moradores_cat, mean)[ordem_mor] * 100, 1) # % com internet por moradores

ols7 <- lm(nota_media ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf + tem_computador + tem_internet, enem)
V7 <- hccm(ols7, type = "hc1") # Matriz robusta da ols7
ct7 <- coeftest(ols7, vcov. = V7)[c("moradores", "tem_computador", "tem_internet"), ] # b1 e os bens (para a Tabela 6)
round(ct7, 4) # Mostra
coef(ols7)["moradores"] # b1 controlando pelos bens
b1_4a # b1 da ols4a (especificação principal), para comparar
coef(ols7)["moradores"] - b1_4a # Quanto b1 muda ao incluir os bens
rm(ols7) # Apaga o modelo
gc() # Libera a memória


# ===================== ETAPA 8: efeito por prova (especificação da ols4a) =====================

ols8_cn <- lm(NU_NOTA_CN ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem) # Ciências da Natureza
b8_cn <- coeftest(ols8_cn, vcov. = hccm(ols8_cn, type = "hc1"))["moradores", 1:2] # b1 e EP robusto
rm(ols8_cn) # Apaga o modelo
gc() # Libera a memória

ols8_ch <- lm(NU_NOTA_CH ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem) # Ciências Humanas
b8_ch <- coeftest(ols8_ch, vcov. = hccm(ols8_ch, type = "hc1"))["moradores", 1:2] # b1 e EP robusto
rm(ols8_ch) # Apaga o modelo
gc() # Libera a memória

ols8_lc <- lm(NU_NOTA_LC ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem) # Linguagens e Códigos
b8_lc <- coeftest(ols8_lc, vcov. = hccm(ols8_lc, type = "hc1"))["moradores", 1:2] # b1 e EP robusto
rm(ols8_lc) # Apaga o modelo
gc() # Libera a memória

ols8_mt <- lm(NU_NOTA_MT ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem) # Matemática
b8_mt <- coeftest(ols8_mt, vcov. = hccm(ols8_mt, type = "hc1"))["moradores", 1:2] # b1 e EP robusto
rm(ols8_mt) # Apaga o modelo
gc() # Libera a memória

ols8_red <- lm(NU_NOTA_REDACAO ~ moradores + renda + esc_pai + esc_mae + sexo + cor_raca + uf, enem) # Redação
b8_red <- coeftest(ols8_red, vcov. = hccm(ols8_red, type = "hc1"))["moradores", 1:2] # b1 e EP robusto
rm(ols8_red) # Apaga o modelo
gc() # Libera a memória

# Tabela por prova: só em desvios-padrão da própria prova dá para comparar (escalas diferentes)
b8 <- c(b8_cn[1], b8_ch[1], b8_lc[1], b8_mt[1], b8_red[1]) # b1 de cada prova
ep8 <- c(b8_cn[2], b8_ch[2], b8_lc[2], b8_mt[2], b8_red[2]) # EP robusto de cada prova
dp8 <- c(sd(enem$NU_NOTA_CN), sd(enem$NU_NOTA_CH), sd(enem$NU_NOTA_LC),
         sd(enem$NU_NOTA_MT), sd(enem$NU_NOTA_REDACAO)) # Desvio-padrão de cada prova
tab_prova <- data.frame(prova  = c("CN", "CH", "LC", "MT", "Redacao"), # Prova
                        b1     = round(b8, 3),                         # b1 em pontos da prova
                        ep_rob = round(ep8, 3),                        # EP robusto
                        dp     = round(dp8, 2),                        # Desvio-padrão da prova
                        b1_dp  = round(b8 / dp8, 4),                   # b1 em DP da própria prova
                        row.names = NULL)
tab_prova # Mostra a tabela
write.csv(tab_prova, "saidas/tab_por_prova.csv", row.names = FALSE) # Salva para o texto


# ===================== TABELAS FINAIS DO ARTIGO (3 a 6) =====================
# Montadas só com os números guardados acima (nenhum modelo é rodado de novo).
# write.csv2 = vírgula decimal e ponto e vírgula; latin1 = acentos certos no Excel em português.
# formatC(..., format = "f", digits = 2, decimal.mark = ",") transforma o número em texto com vírgula.

# Tabela 3: especificação principal. Colunas (1) a (5) = ols1, ols2, ols3, ols4a, ols4b (vetores da Etapa 4)
tab3 <- rbind(formatC(b1, format = "f", digits = 2, decimal.mark = ","),                       # Coeficiente de moradores
              paste0("(", formatC(ep, format = "f", digits = 3, decimal.mark = ","), ")"),     # EP robusto
              formatC(b1 / sd(enem$nota_media), format = "f", digits = 3, decimal.mark = ","),  # Em DP da nota
              c("Não", "Sim", "Sim", "Sim", "Sim"),                                            # Renda
              c("Não", "Não", "Sim", "Sim", "Sim"),                                            # Escolaridade dos pais
              c("Não", "Não", "Não", "Sim", "Sim"),                                            # Sexo, cor/raça e UF
              c("Não", "Não", "Não", "Não", "Sim"),                                            # Tipo de escola
              formatC(n_mod, format = "d", big.mark = ".", decimal.mark = ","),                                    # n
              formatC(r2, format = "f", digits = 3, decimal.mark = ","),                       # R²
              formatC(r2aj, format = "f", digits = 3, decimal.mark = ","))                     # R² ajustado
tab3 <- data.frame(c("Moradores", "", "Moradores (em DP da nota)", "Renda familiar", "Escolaridade dos pais",
                     "Sexo, cor/raça e UF", "Tipo de escola", "n", "R²", "R² ajustado"), tab3) # Nomes das linhas
colnames(tab3) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)") # Nomes das colunas
tab3 # Mostra a tabela
write.csv2(tab3, "saidas/tabela3.csv", row.names = FALSE, fileEncoding = "latin1") # Salva

# Tabela 4: forma funcional. Coluna (1) = dummies (ols5a); coluna (2) = quadrático (ols5b)
# c(rbind(coef, EP)) intercala os vetores: coef da 1ª categoria, EP da 1ª, coef da 2ª, EP da 2ª...
dum <- c(rbind(formatC(ct5a[h_cat, "Estimate"], format = "f", digits = 2, decimal.mark = ","),
               paste0("(", formatC(ct5a[h_cat, "Std. Error"], format = "f", digits = 2, decimal.mark = ","), ")")))
p17 <- f_lin17[2, "Pr(>F)"] # p-valor do F de linearidade de 1 a 7
p27 <- f_lin27[2, "Pr(>F)"] # p-valor do F de linearidade de 2 a 7
txt17 <- paste0(formatC(f_lin17[2, "F"], format = "f", digits = 2, decimal.mark = ","),
                ifelse(p17 < 0.001, " (p < 0,001)", paste0(" (p = ", formatC(p17, format = "f", digits = 3, decimal.mark = ","), ")")))
txt27 <- paste0(formatC(f_lin27[2, "F"], format = "f", digits = 2, decimal.mark = ","),
                ifelse(p27 < 0.001, " (p < 0,001)", paste0(" (p = ", formatC(p27, format = "f", digits = 3, decimal.mark = ","), ")")))
me <- c(me2, me4, me6) # Efeitos marginais em m = 2, 4 e 6
ep_me <- abs(me) / sqrt(c(lh2[2, "F"], lh4[2, "F"], lh6[2, "F"])) # EP do efeito marginal: |efeito| / raiz(F), porque F = t²
quad <- c(rbind(formatC(me, format = "f", digits = 2, decimal.mark = ","),
                paste0("(", formatC(ep_me, format = "f", digits = 2, decimal.mark = ","), ")"))) # Efeito e EP intercalados
col1 <- c(dum, rep("", 4), txt17, txt27, "", rep("", 6), "Sim",
          formatC(n_5a, format = "d", big.mark = ".", decimal.mark = ","), formatC(r2_5a, format = "f", digits = 3, decimal.mark = ","))
col2 <- c(rep("", 14),
          formatC(ct5b[1, "Estimate"], format = "f", digits = 2, decimal.mark = ","),
          paste0("(", formatC(ct5b[1, "Std. Error"], format = "f", digits = 2, decimal.mark = ","), ")"),
          formatC(ct5b[2, "Estimate"], format = "f", digits = 3, decimal.mark = ","),
          paste0("(", formatC(ct5b[2, "Std. Error"], format = "f", digits = 3, decimal.mark = ","), ")"),
          "", "", formatC(ct5b[2, "t value"], format = "f", digits = 2, decimal.mark = ","), quad, "Sim",
          formatC(n_5b, format = "d", big.mark = ".", decimal.mark = ","), formatC(r2_5b, format = "f", digits = 3, decimal.mark = ","))
tab4 <- data.frame(c("1 morador", "", "2 moradores", "", "3 moradores", "", "5 moradores", "", "6 moradores", "",
                     "7 moradores", "", "8 ou mais moradores", "", "Moradores", "", "Moradores²", "",
                     "F de linearidade (1 a 7 moradores)", "F de linearidade (2 a 7 moradores)", "t de moradores²",
                     "Efeito marginal em 2 moradores", "", "Efeito marginal em 4 moradores", "",
                     "Efeito marginal em 6 moradores", "", "Controles da coluna (4) da Tabela 3", "n", "R²"),
                   col1, col2) # Junta as três colunas
colnames(tab4) <- c("", "(1) Dummies (base: 4 moradores)", "(2) Quadrático") # Nomes das colunas
tab4 # Mostra a tabela
write.csv2(tab4, "saidas/tabela4.csv", row.names = FALSE, fileEncoding = "latin1") # Salva

# Tabela 5: heterogeneidade (uma linha por interação)
b1_base <- c(ct6a[1, "Estimate"], ct6a2[1, "Estimate"], ct6b[1, "Estimate"]) # Efeito no grupo-base: b1
b3_int <- c(ct6a[2, "Estimate"], ct6a2[2, "Estimate"], ct6b[3, "Estimate"]) # Diferença: b3
ep3_int <- c(ct6a[2, "Std. Error"], ct6a2[2, "Std. Error"], ct6b[3, "Std. Error"]) # EP robusto de b3
p3_int <- c(ct6a[2, "Pr(>|t|)"], ct6a2[2, "Pr(>|t|)"], ct6b[3, "Pr(>|t|)"]) # p-valor do teste t de H0: b3 = 0
tab5 <- data.frame(c("Moradores × renda até 1,5 SM", "Moradores × renda até 1 SM", "Moradores × sexo feminino"), # Interação
                   c("Renda acima de 1,5 SM", "Renda acima de 1 SM", "Homens"),                                  # Grupo-base
                   formatC(b1_base, format = "f", digits = 2, decimal.mark = ","),                               # Efeito no grupo-base
                   c("Renda até 1,5 SM", "Renda até 1 SM", "Mulheres"),                                          # Outro grupo
                   formatC(b1_base + b3_int, format = "f", digits = 2, decimal.mark = ","),                      # Efeito no outro grupo
                   formatC(b3_int, format = "f", digits = 2, decimal.mark = ","),                                # Diferença (b3)
                   paste0("(", formatC(ep3_int, format = "f", digits = 2, decimal.mark = ","), ")"),             # EP de b3
                   ifelse(p3_int < 0.001, "< 0,001", formatC(p3_int, format = "f", digits = 3, decimal.mark = ","))) # p-valor
colnames(tab5) <- c("Interação", "Grupo-base", "Efeito no grupo-base", "Outro grupo", "Efeito no outro grupo",
                    "Diferença", "EP da diferença", "p-valor") # Nomes das colunas
tab5 # Mostra a tabela
write.csv2(tab5, "saidas/tabela5.csv", row.names = FALSE, fileEncoding = "latin1") # Salva

# Tabela 6. Parte A: efeito de +1 morador por prova (especificação da coluna 4)
tab6a <- data.frame(c("Ciências da Natureza", "Ciências Humanas", "Linguagens e Códigos", "Matemática", "Redação"), # Prova
                    formatC(b8, format = "f", digits = 2, decimal.mark = ","),                                    # Coeficiente
                    paste0("(", formatC(ep8, format = "f", digits = 2, decimal.mark = ","), ")"),                 # EP robusto
                    formatC(dp8, format = "f", digits = 2, decimal.mark = ","),                                   # DP da prova
                    formatC(b8 / dp8, format = "f", digits = 3, decimal.mark = ","))                              # Em DP da prova
colnames(tab6a) <- c("Prova", "Coeficiente de moradores", "EP robusto", "DP da prova", "Coeficiente em DP da prova")
tab6a # Mostra a parte A

# Parte B: mecanismo (coluna 4 vs. coluna 4 + computador e internet)
tab6b <- data.frame(c("Moradores", "", "Tem computador", "", "Tem internet", "", "Redução da magnitude do coeficiente de moradores"),
                    c(formatC(b1[4], format = "f", digits = 2, decimal.mark = ","),
                      paste0("(", formatC(ep[4], format = "f", digits = 2, decimal.mark = ","), ")"),
                      "", "", "", "", ""),                                                                         # Coluna (4)
                    c(formatC(ct7[1, "Estimate"], format = "f", digits = 2, decimal.mark = ","),
                      paste0("(", formatC(ct7[1, "Std. Error"], format = "f", digits = 2, decimal.mark = ","), ")"),
                      formatC(ct7[2, "Estimate"], format = "f", digits = 2, decimal.mark = ","),
                      paste0("(", formatC(ct7[2, "Std. Error"], format = "f", digits = 2, decimal.mark = ","), ")"),
                      formatC(ct7[3, "Estimate"], format = "f", digits = 2, decimal.mark = ","),
                      paste0("(", formatC(ct7[3, "Std. Error"], format = "f", digits = 2, decimal.mark = ","), ")"),
                      paste0(formatC((abs(b1[4]) - abs(ct7[1, "Estimate"])) / abs(b1[4]) * 100, format = "f", digits = 1, decimal.mark = ","), "%"))) # Coluna (7): quanto o tamanho do coeficiente encolhe, em %
colnames(tab6b) <- c("", "(4) Especificação principal", "(7) Com computador e internet") # Nomes das colunas
tab6b # Mostra a parte B

write.csv2(tab6a, "saidas/tabela6.csv", row.names = FALSE, fileEncoding = "latin1") # Salva a parte A
write.table(tab6b, "saidas/tabela6.csv", sep = ";", row.names = FALSE, append = TRUE,
            fileEncoding = "latin1") # Acrescenta a parte B embaixo (o aviso "appending column names" é esperado)
