# Número de moradores no domicílio e desempenho no ENEM 2023

Trabalho final de Econometria I (FGV EESP, 2º semestre de 2026).

Autores: Leonardo César Nascimento Soares e Rodrigo do Amaral de Oliveira Santos.

## Dados

Microdados do ENEM 2023, do INEP:
<https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem>

Baixe o arquivo do ENEM 2023 e descompacte. O CSV usado é `DADOS/MICRODADOS_ENEM_2023.csv` (cerca de 1,7 GB).

**Atalho:** a base limpa `dados/enem2023_limpo.rds` já está neste repositório. Com ela, dá para rodar o `02_descritivas.R` e o `03_regressoes.R` sem baixar o CSV do INEP.

## Como rodar

1. Use a pasta do projeto (onde está este README) como diretório de trabalho do R. Os scripts usam caminhos relativos (`dados/`, `saidas/`).
   - No RStudio: abra a pasta como projeto, ou use *Session > Set Working Directory > To Project Directory*.
   - No VS Code: abra a pasta do projeto como workspace.
   - No terminal: entre na pasta e rode `Rscript 01_limpeza.R`.
2. Rode os scripts nesta ordem:
   1. `01_limpeza.R`: lê o CSV do INEP, aplica os filtros e cria as variáveis. Antes de rodar, troque o caminho do CSV na variável `arquivo`, no início do script.
   2. `02_descritivas.R`: estatísticas descritivas e gráficos da seção de Dados.
   3. `03_regressoes.R`: regressões, testes e as tabelas finais. Leva cerca de 15 minutos e usa até uns 7 GB de memória RAM.

## Versões

- R 4.6.2.
- Pacotes: `data.table` (leitura do CSV), `lmtest` (`bptest`, `coeftest`) e `car` (`hccm`, `linearHypothesis`).

```r
install.packages(c("data.table", "lmtest", "car"))
```

## Pastas

- `dados/enem2023_limpo.rds`: base final limpa, com 1.002.841 participantes e 17 variáveis, criada pelo `01_limpeza.R`.
- `saidas/`: tudo o que os scripts geram.
  - `tabela1.csv` a `tabela6.csv`: tabelas do artigo (separador `;` e vírgula decimal, abrem direto no Excel em português).
  - `figura1.png` a `figura4.png`: figuras do artigo.
  - `tab_*.csv` e `fig*_*.png`: tabelas e gráficos exploratórios, que não entram no artigo.
