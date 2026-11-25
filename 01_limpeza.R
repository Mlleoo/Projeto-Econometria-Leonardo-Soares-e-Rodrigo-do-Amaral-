# 01_limpeza.R - Etapa 0, parte 1: leitura e limpeza dos Microdados do ENEM 2023
# Trabalho de Econometria I - Leonardo Soares e Rodrigo Santos

library(data.table) # Só para o fread, que lê o CSV grande (1,7 GB) rápido

# Rode com a pasta do projeto como diretório de trabalho (veja o README)

# CAMINHO DO CSV DO INEP: troque pelo caminho do arquivo MICRODADOS_ENEM_2023.csv no SEU computador
arquivo <- "C:/Program Files/Youtube-Twitch/Econometria/microdados_enem_2023/DADOS/MICRODADOS_ENEM_2023.csv"

# Colunas que vamos usar (lemos só elas para economizar memória)
colunas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO", # Notas
             "Q001", "Q002", "Q005", "Q006",                                        # Pai, mãe, moradores, renda
             "Q024", "Q025",                                                        # Computador e internet (mecanismo)
             "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA", "SG_UF_PROVA",                  # Controles
             "TP_ST_CONCLUSAO", "IN_TREINEIRO", "TP_STATUS_REDACAO",                # Usadas nos filtros
             "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC", "TP_PRESENCA_MT")

enem <- fread(arquivo, sep = ";", encoding = "Latin-1", select = colunas, na.strings = "") # Lê o CSV
enem <- as.data.frame(enem) # Transforma em data.frame comum, como na aula
n_passos <- nrow(enem) # Guarda o n da base completa (para a Tabela 1)

# Filtros (subset já descarta as linhas com NA na condição)
enem <- subset(enem, TP_ST_CONCLUSAO == 2) # Concluintes do Ensino Médio em 2023
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, IN_TREINEIRO == 0) # Exclui treineiros
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) # Presente nas 4 provas
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, TP_STATUS_REDACAO == 1) # Redação sem problemas (anulada, em branco etc. saem)
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, !is.na(Q001) & !is.na(Q002) & !is.na(Q005) & !is.na(Q006)) # Questionário respondido
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, TP_ESCOLA != 1) # Exclui quem não respondeu o tipo de escola (só 5 alunos)
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro
enem <- subset(enem, NU_NOTA_CN > 0 & NU_NOTA_CH > 0 & NU_NOTA_LC > 0 & NU_NOTA_MT > 0) # Exclui nota objetiva zero (prova em branco)
n_passos <- c(n_passos, nrow(enem)) # Guarda o n após o filtro

# Variável dependente: média simples das 5 notas
enem$nota_media <- (enem$NU_NOTA_CN + enem$NU_NOTA_CH + enem$NU_NOTA_LC + enem$NU_NOTA_MT + enem$NU_NOTA_REDACAO) / 5

# Variável de interesse: número de moradores (Q005), incluindo o participante
enem$moradores <- enem$Q005 # Numérica, de 1 a 20
enem$moradores_cat <- ifelse(enem$moradores >= 8, "8mais", enem$moradores) # Agrupa 8 ou mais (poucos casos)
enem$moradores_cat <- factor(enem$moradores_cat, levels = c("1", "2", "3", "4", "5", "6", "7", "8mais")) # Ordem natural
enem$moradores_cat <- relevel(enem$moradores_cat, ref = "4") # Base: 4 moradores (família típica)

# Rótulos só com letras, números e "_": o linearHypothesis lê os nomes dos coeficientes como fórmula

# Renda familiar TOTAL (Q006) em faixas de salário mínimo (SM de 2023 = R$ 1.320)
enem$renda <- factor(enem$Q006, levels = LETTERS[1:17],
                     labels = c("nenhuma", "ate1sm", "1a1_5sm", "1_5a2sm", "2a2_5sm", "2_5a3sm",
                                "3a4sm", "4a5sm", "5a6sm", "6a7sm", "7a8sm", "8a9sm",
                                "9a10sm", "10a12sm", "12a15sm", "15a20sm", "mais20sm")) # Ex.: 1a1_5sm = de 1 a 1,5 SM
enem$renda <- relevel(enem$renda, ref = "ate1sm") # Base: até 1 SM ("nenhuma" continua como categoria)

# Escolaridade do pai (Q001) e da mãe (Q002): mesmos códigos de A a H
niveis_esc <- c("nunca_estudou", "fund1_incomp", "fund1_comp", "fund_comp",
                "medio_comp", "superior_comp", "pos_grad", "nao_sei") # Fund. I = até o 5º ano; fund = até o 9º ano
enem$esc_pai <- factor(enem$Q001, levels = LETTERS[1:8], labels = niveis_esc) # Base: nunca_estudou (1º nível)
enem$esc_mae <- factor(enem$Q002, levels = LETTERS[1:8], labels = niveis_esc) # Base: nunca_estudou (1º nível)

# Controles demográficos
enem$sexo <- factor(enem$TP_SEXO, levels = c("M", "F"), labels = c("masculino", "feminino")) # Base: masculino
enem$cor_raca <- factor(enem$TP_COR_RACA, levels = 0:6,
                        labels = c("nao_declarado", "branca", "preta", "parda", "amarela", "indigena", "nao_declarado")) # Códigos 0 e 6 juntos
enem$cor_raca <- relevel(enem$cor_raca, ref = "branca") # Base: branca
enem$tipo_escola <- factor(enem$TP_ESCOLA, levels = 2:3, labels = c("publica", "privada")) # Base: publica (1º nível)
enem$uf <- factor(enem$SG_UF_PROVA) # UF onde fez a prova
enem$uf <- relevel(enem$uf, ref = "SP") # Base: SP (UF com mais participantes)

# Bens do domicílio: só para a etapa de mecanismo (são mediadores, não controles)
enem$tem_computador <- ifelse(enem$Q024 != "A", 1, 0) # Q024: A = não tem; B a E = tem um ou mais
enem$tem_internet <- ifelse(enem$Q025 == "B", 1, 0) # Q025: A = não; B = sim

# Base final: só as variáveis de análise
enem <- enem[, c("nota_media", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO",
                 "moradores", "moradores_cat", "renda", "esc_pai", "esc_mae", "sexo", "cor_raca", "tipo_escola", "uf",
                 "tem_computador", "tem_internet")]

dir.create("dados", showWarnings = FALSE) # Cria a pasta dados, se não existir
saveRDS(enem, "dados/enem2023_limpo.rds") # Salva a base limpa, como na aula

# Resumo
str(enem) # Estrutura da base final
table(enem$moradores_cat) # Quantos alunos em cada número de moradores
round(prop.table(table(enem$tipo_escola)) * 100, 2) # % de alunos em cada tipo de escola
table(enem$esc_pai) # Escolaridade do pai, inclusive "Não sei"
table(enem$esc_mae) # Escolaridade da mãe, inclusive "Não sei"
round(mean(enem$tem_computador) * 100, 1) # % com computador em casa
round(mean(enem$tem_internet) * 100, 1) # % com internet em casa
file.size("dados/enem2023_limpo.rds") / 1024^2 # Tamanho do arquivo em MB (limite do GitHub: 100 MB)
colSums(is.na(enem)) # NA por variável: deve ser tudo zero (NA = código fora do dicionário)

# Tabela 1 do artigo: construção da amostra (write.csv2 = vírgula decimal; latin1 = acentos certos no Excel)
dir.create("saidas", showWarnings = FALSE) # Cria a pasta saidas, se não existir
tab1 <- data.frame(c("Inscritos no ENEM 2023", "Concluintes do ensino médio em 2023", "Exclui treineiros",
                     "Presentes nas 4 provas objetivas", "Redação sem problemas", "Questionário socioeconômico respondido",
                     "Tipo de escola informado", "Notas objetivas maiores que zero"),       # Etapa
                   formatC(n_passos, format = "d", big.mark = ".", decimal.mark = ","),                         # n após a etapa
                   c("", formatC(-diff(n_passos), format = "d", big.mark = ".", decimal.mark = ",")))           # Quantos saíram
colnames(tab1) <- c("Etapa", "n", "Removidos") # Nomes das colunas
tab1 # Mostra a tabela
write.csv2(tab1, "saidas/tabela1.csv", row.names = FALSE, fileEncoding = "latin1") # Salva
