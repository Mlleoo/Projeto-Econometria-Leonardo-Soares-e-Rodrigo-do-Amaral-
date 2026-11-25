# 02_descritivas.R - Etapa 0, parte 2: análise descritiva (seção de Dados)
# Trabalho de Econometria I - Leonardo Soares e Rodrigo Santos

# Rode com a pasta do projeto como diretório de trabalho (veja o README)
options(scipen = 999) # Evita notação científica nas saídas e nos eixos dos gráficos
enem <- readRDS("dados/enem2023_limpo.rds") # Base limpa criada no 01_limpeza.R
dir.create("saidas", showWarnings = FALSE) # Pasta para tabelas e gráficos

# Ordem natural das categorias (nos factors, a categoria-base vem primeiro)
ordem_mor <- c("1", "2", "3", "4", "5", "6", "7", "8mais") # Moradores: de 1 a 8 ou mais
ordem_renda <- levels(enem$renda)[c(2, 1, 3:17)] # Renda: "nenhuma" volta para antes de "ate1sm"

# Rótulos bonitos para os eixos dos gráficos (os da base são só letras, números e _)
rot_mor <- c("1", "2", "3", "4", "5", "6", "7", "8+") # Moradores
rot_renda <- c("Nenhuma renda", "Até 1 SM", "1 a 1,5 SM", "1,5 a 2 SM", "2 a 2,5 SM", "2,5 a 3 SM",
               "3 a 4 SM", "4 a 5 SM", "5 a 6 SM", "6 a 7 SM", "7 a 8 SM", "8 a 9 SM",
               "9 a 10 SM", "10 a 12 SM", "12 a 15 SM", "15 a 20 SM", "Mais de 20 SM") # Renda
rot_esc <- c("Nunca estudou", "Fund. I incompleto", "Fund. I completo", "Fund. completo",
             "Médio completo", "Superior completo", "Pós-graduação", "Não sei") # Escolaridade

# 1. Tabela de estatísticas descritivas
vars <- c("nota_media", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO", "moradores") # Variáveis numéricas
tab_desc <- data.frame(variavel  = vars,                                 # Nome da variável
                       n         = nrow(enem),                           # Número de observações
                       media     = round(sapply(enem[, vars], mean), 2), # sapply aplica mean em cada coluna
                       dp        = round(sapply(enem[, vars], sd), 2),   # Desvio-padrão
                       variancia = round(sapply(enem[, vars], var), 2),  # Variância
                       minimo    = round(sapply(enem[, vars], min), 2),  # Mínimo
                       maximo    = round(sapply(enem[, vars], max), 2),  # Máximo
                       row.names = NULL)                                 # Sem nomes de linha repetidos
tab_desc # Mostra a tabela
write.csv(tab_desc, "saidas/tab_descritivas.csv", row.names = FALSE) # Salva para o texto

# Tabela 2 do artigo. Parte A: estatísticas descritivas formatadas
tab2a <- data.frame(c("Nota média", "Ciências da Natureza", "Ciências Humanas", "Linguagens e Códigos",
                      "Matemática", "Redação", "Moradores no domicílio"),                              # Variável
                    formatC(tab_desc$n, format = "d", big.mark = ".", decimal.mark = ","),                                 # n
                    formatC(tab_desc$media, format = "f", digits = 2, decimal.mark = ",", big.mark = "."),     # Média
                    formatC(tab_desc$dp, format = "f", digits = 2, decimal.mark = ",", big.mark = "."),        # DP
                    formatC(tab_desc$variancia, format = "f", digits = 2, decimal.mark = ",", big.mark = "."), # Variância
                    formatC(tab_desc$minimo, format = "f", digits = 2, decimal.mark = ",", big.mark = "."),    # Mínimo
                    formatC(tab_desc$maximo, format = "f", digits = 2, decimal.mark = ",", big.mark = "."))    # Máximo
colnames(tab2a) <- c("Variável", "n", "Média", "Desvio-padrão", "Variância", "Mínimo", "Máximo") # Nomes das colunas
tab2a # Mostra a parte A

# Parte B: distribuição do número de moradores
cont_mor <- as.numeric(table(enem$moradores_cat)[ordem_mor]) # Contagem em cada categoria, na ordem natural
tab2b <- data.frame(rot_mor,                                                                       # Moradores
                    formatC(cont_mor, format = "d", big.mark = ".", decimal.mark = ","),                               # n
                    formatC(cont_mor / nrow(enem) * 100, format = "f", digits = 2, decimal.mark = ",")) # %
colnames(tab2b) <- c("Moradores no domicílio", "n", "% da amostra") # Nomes das colunas
tab2b # Mostra a parte B

write.csv2(tab2a, "saidas/tabela2.csv", row.names = FALSE, fileEncoding = "latin1") # Salva a parte A
write.table(tab2b, "saidas/tabela2.csv", sep = ";", row.names = FALSE, append = TRUE,
            fileEncoding = "latin1") # Acrescenta a parte B embaixo (o aviso "appending column names" é esperado)

# 2. Histograma da nota média (distribuição de Y)
png("saidas/figura1.png", width = 1600, height = 1200, res = 200) # Figura 1 do artigo (título fica no Word)
hist(enem$nota_media, breaks = 50, col = "gray80", main = "",
     xlab = "Nota média (5 provas)", ylab = "Número de alunos")
abline(v = mean(enem$nota_media), col = "red", lty = 2, lwd = 2) # Linha tracejada na média
legend("topright", legend = "Média", col = "red", lty = 2, lwd = 2, bty = "n") # Legenda da linha
dev.off() # Fecha e salva o arquivo

# 3. Barras do número de moradores (distribuição de X)
png("saidas/fig2_barras_moradores.png", width = 1600, height = 1200, res = 200) # Exploratório (não vai para o artigo)
barplot(table(enem$moradores_cat)[ordem_mor] / 1000, col = "gray80", names.arg = rot_mor,
        main = "Número de moradores no domicílio",
        xlab = "Moradores no domicílio (incluindo o participante)", ylab = "Número de alunos (mil)")
dev.off() # Fecha e salva o arquivo

# 4. Gráfico central: nota média por número de moradores
media_mor <- tapply(enem$nota_media, enem$moradores_cat, mean)[ordem_mor] # Média da nota em cada categoria
round(media_mor, 1) # Mostra as médias
png("saidas/fig3_nota_por_moradores.png", width = 800, height = 600) # Abre o arquivo PNG
plot(1:8, media_mor, type = "b", pch = 19, xaxt = "n",
     main = "Nota média no ENEM por número de moradores", xlab = "Moradores no domicílio", ylab = "Nota média")
axis(1, at = 1:8, labels = rot_mor) # Rótulos 1, 2, ..., 8+ no eixo x
dev.off() # Fecha e salva o arquivo

# 5. Nota média por faixa de renda
media_renda <- tapply(enem$nota_media, enem$renda, mean)[ordem_renda] # Média da nota em cada faixa
round(media_renda, 1) # Mostra as médias
png("saidas/fig4_nota_por_renda.png", width = 900, height = 650) # Abre o arquivo PNG
par(mar = c(9, 4, 3, 1)) # Aumenta a margem de baixo para caber os rótulos
plot(1:17, media_renda, type = "b", pch = 19, xaxt = "n",
     main = "Nota média no ENEM por renda familiar (salários mínimos)", xlab = "", ylab = "Nota média")
axis(1, at = 1:17, labels = rot_renda, las = 2) # las = 2: rótulos na vertical
dev.off() # Fecha e salva o arquivo

# 6. Nota média por escolaridade da mãe
media_mae <- tapply(enem$nota_media, enem$esc_mae, mean) # Já está na ordem natural ("nao_sei" no fim)
round(media_mae, 1) # Mostra as médias
png("saidas/fig5_nota_por_esc_mae.png", width = 900, height = 650) # Abre o arquivo PNG
par(mar = c(9, 4, 3, 1)) # Aumenta a margem de baixo para caber os rótulos
plot(1:8, media_mae, type = "b", pch = 19, xaxt = "n",
     main = "Nota média no ENEM por escolaridade da mãe", xlab = "", ylab = "Nota média")
axis(1, at = 1:8, labels = rot_esc, las = 2) # Rótulos na vertical
dev.off() # Fecha e salva o arquivo

# Figura 2 do artigo: painel com a nota média por renda (esquerda) e por escolaridade da mãe (direita)
png("saidas/figura2.png", width = 1600, height = 1200, res = 200) # Título fica no Word
par(mfrow = c(1, 2), mar = c(9, 4.5, 1, 1)) # 1 linha e 2 colunas de gráficos; margem de baixo maior para os rótulos
plot(1:17, media_renda, type = "b", pch = 19, xaxt = "n", main = "", xlab = "", ylab = "Nota média",
     ylim = range(media_renda, media_mae)) # Mesmo eixo y nos dois painéis, para comparar
axis(1, at = 1:17, labels = rot_renda, las = 2, cex.axis = 0.75) # Faixas de renda na vertical
mtext("Renda familiar (salários mínimos)", side = 1, line = 7.5) # Nome do eixo x, abaixo dos rótulos
plot(1:7, media_mae[1:7], type = "b", pch = 19, xaxt = "n", main = "", xlab = "", ylab = "Nota média",
     xlim = c(1, 8), ylim = range(media_renda, media_mae)) # Linha só até a pós-graduação; mesmo eixo y da esquerda
points(8, media_mae[8], pch = 19) # "Não sei" como ponto solto (não é um nível de escolaridade)
axis(1, at = 1:8, labels = rot_esc, las = 2, cex.axis = 0.75) # Níveis de escolaridade na vertical
mtext("Escolaridade da mãe", side = 1, line = 7.5) # Nome do eixo x, abaixo dos rótulos
dev.off() # Fecha e salva o arquivo

# 7. Média de moradores por renda e por escolaridade da mãe (sinal do viés de variável omitida)
round(tapply(enem$moradores, enem$renda, mean)[ordem_renda], 2) # Moradores médios em cada faixa de renda
round(tapply(enem$moradores, enem$esc_mae, mean), 2) # Moradores médios por escolaridade da mãe

# 8. Matriz de correlação
enem$renda_num <- as.numeric(factor(enem$renda, levels = ordem_renda)) # 1 = nenhuma ... 17 = mais20sm
enem$esc_mae_num <- as.numeric(enem$esc_mae) # 1 = nunca_estudou ... 7 = pos_grad, 8 = nao_sei
enem$esc_mae_num[enem$esc_mae_num == 8] <- NA # "nao_sei" não tem ordem: vira NA
cor_mat <- round(cor(enem[, c("nota_media", "moradores", "renda_num", "esc_mae_num")], use = "complete.obs"), 3) # Correlações
cor_mat # Mostra a matriz
write.csv(cor_mat, "saidas/tab_correlacao.csv") # Salva para o texto
