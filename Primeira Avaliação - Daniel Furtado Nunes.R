# =============================================================================
# TRABALHO: Integração de Bases, Visualização e Publicação Web
# ANÁLISE: CAGED vs PNAD Contínua - Emprego Formal no Brasil
# =============================================================================

# METADADOS DA ANÁLISE
# =============================================================================
# 
# DISCIPLINA: Introdução ao R Aplicado em Ciência de Dados
# PROFESSOR: Diogo Tavares Robaina
# ALUNO: Daniel Furtado Nunes Rocha da Silva
# DATA: 2025
#
# OBJETIVO: Desenvolver habilidades em integração de bases de dados,
#           visualização de dados e publicação web usando R
#

# INTRODUÇÃO DIDÁTICA
# =============================================================================
# 
# ESTE TRABALHO ORIGINA-SE DE UMA ANÁLISE DESENVOLVIDA PARA A DISCIPLINA DE 
# SÉRIES TEMPORAIS, AGORA AMPLIADA E ADAPTADA PARA INTEGRAÇÃO DE BASES E 
# PUBLICAÇÃO WEB.
#
# POR QUE COMPARAR CAGED E PNAD CONTÍNUA?
# =============================================================================
#
# O CAGED (Cadastro Geral de Empregados e Desempregados) é um registro 
# administrativo que captura TODOS os vínculos formais de trabalho, sendo
# excelente para medir o FLUXO mensal (admissões e demissões).
#
# A PNAD Contínua é uma pesquisa por amostra domiciliar que estima o ESTOQUE
# de pessoas ocupadas, capturando tanto o setor formal quanto informal.
#
# COMPARAR ESTAS BASES PERMITE:
# 1. Entender a relação entre fluxo (CAGED) e estoque (PNAD) do mercado formal
# 2. Validar a consistência entre diferentes metodologias de coleta
# 3. Identificar possíveis defasagens temporais entre os indicadores
# 4. Analisar a qualidade dos dados administrativos versus pesquisas amostrais
#
# Esta análise é crucial para formuladores de política pública, pesquisadores
# e investidores que precisam entender a dinâmica do mercado de trabalho formal.
#
# =============================================================================

# CARREGAMENTO DE PACOTES
# =============================================================================
# 
# OBJETIVO: Carregar todas as bibliotecas necessárias para a análise
# FUNCIONALIDADE AUTOMÁTICA: Se algum pacote não estiver instalado, 
# o código irá INSTALAR AUTOMATICAMENTE antes de carregar
#

# Lista de pacotes necessários
pacotes <- c("ecoseries", "sidrar", "ggplot2", "scales", "vars", "aod", 
             "rstanarm", "brms", "MCMCpack", "forecast", "urca", "dplyr",
             "seasonal", "gridExtra", "patchwork", "readr", "tidyr")

# ⚡ FUNÇÃO IF AUTOMÁTICA PARA INSTALAÇÃO DE PACOTES FALTANTES ⚡
# =============================================================================
# 
# ESTA PARTE DO CÓDIGO VERIFICA AUTOMATICAMENTE:
# 1. Quais pacotes da lista JÁ ESTÃO INSTALADOS na máquina
# 2. Quais pacotes NÃO ESTÃO INSTALADOS (faltantes)
# 3. Se houver pacotes faltantes, INSTALA AUTOMATICAMENTE
# 4. Isso garante que o código funcione em QUALQUER computador
#

cat("🔍 VERIFICANDO PACOTES NECESSÁRIOS...\n")

# Verifica quais pacotes não estão instalados
pacotes_instalar <- pacotes[!pacotes %in% installed.packages()[,"Package"]]

# SE existirem pacotes para instalar (if automático)
if(length(pacotes_instalar) > 0) {
  cat("📦 INSTALANDO PACOTES FALTANTES AUTOMATICAMENTE:\n")
  cat("Os seguintes pacotes serão instalados:", paste(pacotes_instalar, collapse = ", "), "\n")
  cat("Isso pode levar alguns minutos...\n")
  
  # Instala os pacotes faltantes com dependências
  install.packages(pacotes_instalar, dependencies = TRUE)
  
  cat("✅ INSTALAÇÃO CONCLUÍDA! Todos os pacotes necessários estão disponíveis.\n")
} else {
  cat("✅ TODOS OS PACOTES JÁ ESTÃO INSTALADOS! Prosseguindo com a análise...\n")
}

# AGORA CARREGA OS PACOTES (já garantidos que estão instalados)
# =============================================================================
cat("🔄 CARREGANDO BIBLIOTECAS...\n")

library(ecoseries)   # Para acesso aos dados do CAGED via API do IPEA
library(sidrar)      # Para acesso aos dados da PNAD via API do IBGE
library(ggplot2)     # Para criação de visualizações profissionais
library(scales)      # Para formatação de escalas nos gráficos
library(dplyr)       # Para manipulação e transformação de dados
library(tidyr)       # Para organização e limpeza de dados
library(readr)       # Para importação e exportação de arquivos CSV
library(forecast)    # Para análise de séries temporais

cat("✅ BIBLIOTECAS CARREGADAS COM SUCESSO!\n")

# Configurar opções do ambiente R
options(scipen = 999)  # Evita notação científica nos números
theme_set(theme_minimal())  # Define tema minimalista para os gráficos

# 1. COLETA DOS DADOS BRUTOS
# =============================================================================
#
# OBJETIVO: Coletar dados diretamente das fontes oficiais via APIs
# Vantagens: Reprodutibilidade, atualização automática, transparência
#

cat("=== INICIANDO COLETA DOS DADOS BRUTOS ===\n")

## 1.1 Dados do CAGED (Cadastro Geral de Empregados e Desempregados)
## Fonte: Ministério do Trabalho e Emprego - MTE
## Portal: http://pdet.mte.gov.br/microdados-rais-e-caged
cat("Coletando dados brutos do CAGED via IPEA Data...\n")

# A função series_ipeadata acessa a API do IPEA Data usando o código da série
caged_bruto <- series_ipeadata(
  '272844966',  # Código único da série: CAGED - Saldo de empregos formais
  periodicity = 'M'  # Periodicidade mensal
)

## 1.2 Dados da PNAD Contínua (Pesquisa Nacional por Amostra de Domicílios)
## Fonte: IBGE - Instituto Brasileiro de Geografia e Estatística
## Portal: https://sidra.ibge.gov.br/pesquisa/pnadct
cat("Coletando dados brutos da PNAD Contínua via SIDRA/IBGE...\n")

# A função get_sidra acessa a API do IBGE usando a tabela específica
pnad_bruto <- get_sidra(
  api = '/t/6320/n1/all/v/4090/p/all/c11913/allxt'
)

# 2. LIMPEZA E PROCESSAMENTO DOS DADOS
# =============================================================================
#
# OBJETIVO: Transformar os dados brutos em formato adequado para análise
# Processo: Selecionar variáveis relevantes, tratar valores missing, 
#           converter formatos e criar identificadores
#

cat("=== REALIZANDO LIMPEZA E PROCESSAMENTO ===\n")

## 2.1 Limpeza dos dados do CAGED
cat("Processando dados do CAGED...\n")

# O CAGED fornece o SALDO LÍQUIDO mensal (admissões - demissões)
caged_limpo <- caged_bruto$serie_272844966 %>%
  mutate(
    caged_saldo = as.numeric(valor),  # Converte para numérico
    fonte = "CAGED/MTE"  # Identifica a fonte dos dados
  ) %>%
  select(data, caged_saldo, fonte) %>%  # Seleciona apenas colunas relevantes
  arrange(data) %>%  # Ordena por data crescente
  filter(!is.na(caged_saldo))  # Remove valores missing (NA)

cat("Primeiras linhas do CAGED processado:\n")
print(head(caged_limpo))

## 2.2 Limpeza dos dados da PNAD
cat("Processando dados da PNAD...\n")

# A PNAD fornece o ESTOQUE de ocupados com carteira (pesquisa amostral)
pnad_limpo <- pnad_bruto %>%
  # Filtra apenas para ocupados com carteira assinada (código 31722)
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == '31722') %>%
  mutate(
    # Converte o código do trimestre móvel para data
    data = as.Date(paste0(`Trimestre Móvel (Código)`, "01"), format = "%Y%m%d"),
    pnad_ocupados = as.numeric(Valor),  # Converte valor para numérico
    fonte = "PNAD Contínua/IBGE"  # Identifica a fonte
  ) %>%
  select(data, pnad_ocupados, fonte) %>%  # Seleciona colunas relevantes
  arrange(data) %>%  # Ordena por data
  filter(!is.na(pnad_ocupados))  # Remove valores missing

cat("Primeiras linhas da PNAD processada:\n")
print(head(pnad_limpo))

# 3. INTEGRAÇÃO DAS BASES (MERGE/JOIN)
# =============================================================================
#
# OBJETIVO: Unir as duas bases de dados em uma única tabela
# Método: inner_join mantém apenas as datas presentes em AMBAS as bases
# Resultado: Base consolidada pronta para análise comparativa
#

cat("=== INTEGRANDO AS BASES DE DADOS ===\n")

dados_integrados <- inner_join(
  caged_limpo, 
  pnad_limpo, 
  by = "data"  # Critério de junção: mesma data
) %>%
  mutate(
    fonte = "CAGED + PNAD",  # Nova identificação para base integrada
    ano = as.integer(format(data, "%Y")),  # Extrai ano da data
    mes = as.integer(format(data, "%m"))   # Extrai mês da data
  ) %>%
  select(data, ano, mes, caged_saldo, pnad_ocupados, fonte) %>%
  arrange(data)  # Garante ordenação temporal

cat("Resumo da base integrada:\n")
cat("Período:", min(dados_integrados$data), "a", max(dados_integrados$data), "\n")
cat("Total de observações:", nrow(dados_integrados), "\n")
cat("Variáveis:", paste(names(dados_integrados), collapse = ", "), "\n")

# 4. VISUALIZAÇÃO DOS DADOS
# =============================================================================
#
# OBJETIVO: Criar visualizações que facilitem a compreensão da relação
#           entre as duas séries temporais
# Gráfico 1: Evolução temporal comparativa
# Gráfico 2: Dispersão e correlação linear
#

cat("=== CRIANDO VISUALIZAÇÕES ===\n")

## GRÁFICO 1: Evolução Temporal Comparativa
## Mostra como as duas séries se comportam ao longo do tempo
cat("Gerando Gráfico 1: Evolução Temporal...\n")

p1 <- ggplot(dados_integrados, aes(x = data)) +
  # Linha do CAGED (saldo líquido mensal)
  geom_line(aes(y = caged_saldo, colour = 'CAGED - Saldo Empregos Formais'), 
            size = 1.2, alpha = 0.8) +
  # Linha da PNAD (estoque de ocupados) - dividido por 100 para mesma escala
  geom_line(aes(y = pnad_ocupados/100, colour = 'PNAD - Ocupados com Carteira (÷100)'), 
            size = 1.2, alpha = 0.8) +
  scale_colour_manual(
    name = "Indicadores de Emprego",
    values = c(
      'CAGED - Saldo Empregos Formais' = '#E41A1C',  # Vermelho
      'PNAD - Ocupados com Carteira (÷100)' = '#377EB8'  # Azul
    )
  ) +
  labs(
    title = "EVOLUÇÃO DO EMPREGO FORMAL NO BRASIL - CAGED vs PNAD",
    subtitle = "Comparação entre registro administrativo (CAGED) e pesquisa amostral (PNAD)\nCAGED: Saldo líquido mensal | PNAD: Estoque trimestral de ocupados",
    x = "Ano",
    y = "Quantidade de Pessoas",
    caption = "Fontes: MTE/CAGED (registro administrativo) e IBGE/PNAD Contínua (pesquisa amostral)\nNota: Valores da PNAD divididos por 100 para compatibilidade de escalas"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y"
  ) +
  scale_y_continuous(labels = comma)  # Formata números com separador de milhar

print(p1)

## GRÁFICO 2: Dispersão e Correlação
## Mostra a relação linear entre as duas variáveis
cat("Gerando Gráfico 2: Dispersão e Correlação...\n")

# Calcula correlação linear entre as séries
correlacao <- cor(dados_integrados$caged_saldo, dados_integrados$pnad_ocupados, 
                  use = "complete.obs")

p2 <- ggplot(dados_integrados, aes(x = caged_saldo, y = pnad_ocupados)) +
  geom_point(aes(color = ano), alpha = 0.7, size = 3) +  # Pontos coloridos por ano
  # Linha de tendência linear com intervalo de confiança
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    color = "#4DAF4A", 
    linetype = "solid",
    size = 1.2
  ) +
  scale_color_gradientn(
    name = "Ano",
    colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")
  ) +
  labs(
    title = "RELAÇÃO ENTRE FLUXO (CAGED) E ESTOQUE (PNAD) DO EMPREGO FORMAL",
    subtitle = paste("Correlação linear: ", round(correlacao, 4), 
                     "\nCada ponto representa um período de observação"),
    x = "Saldo Mensal de Empregos Formais - CAGED (FLUXO)",
    y = "População Ocupada com Carteira - PNAD (ESTOQUE)",
    caption = "Fonte: Dados integrados CAGED/MTE (registro administrativo) e PNAD Contínua/IBGE (pesquisa amostral)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

print(p2)

# 5. ANÁLISE ESTATÍSTICA BÁSICA
# =============================================================================
#
# OBJETIVO: Quantificar a relação entre as variáveis através de:
# - Estatísticas descritivas (médias, medianas, variabilidade)
# - Modelo de regressão linear (relação funcional)
#

cat("=== REALIZANDO ANÁLISE ESTATÍSTICA ===\n")

## 5.1 Estatísticas Descritivas
## Fornecem um resumo numérico das distribuições de cada variável
estatisticas_descritivas <- dados_integrados %>%
  summarise(
    Periodo = paste(min(ano), "-", max(ano)),
    Observacoes = n(),
    Media_CAGED = mean(caged_saldo, na.rm = TRUE),
    Mediana_CAGED = median(caged_saldo, na.rm = TRUE),
    DesvioPadrao_CAGED = sd(caged_saldo, na.rm = TRUE),
    Media_PNAD = mean(pnad_ocupados, na.rm = TRUE),
    Mediana_PNAD = median(pnad_ocupados, na.rm = TRUE),
    DesvioPadrao_PNAD = sd(pnad_ocupados, na.rm = TRUE),
    Correlacao = round(cor(caged_saldo, pnad_ocupados, use = "complete.obs"), 4)
  )

cat("ESTATÍSTICAS DESCRITIVAS:\n")
print(estatisticas_descritivas)

## 5.2 Modelo de Regressão Simples
## Modela a PNAD como função linear do CAGED: PNAD = β₀ + β₁ × CAGED + ε
modelo_regressao <- lm(pnad_ocupados ~ caged_saldo, data = dados_integrados)

cat("\nMODELO DE REGRESSÃO - PNAD ~ CAGED:\n")
cat("Este modelo testa se o saldo do CAGED ajuda a prever o estoque da PNAD\n")
print(summary(modelo_regressao))

# 6. GLOSSÁRIO DE VARIÁVEIS - VERSÃO APRIMORADA
# =============================================================================
#
# OBJETIVO: Documentar de forma CLARA e DETALHADA o significado de cada variável
#           para garantir compreensão completa e reprodutibilidade da análise
#

cat("=== GLOSSÁRIO DE VARIÁVEIS - VERSÃO DETALHADA ===\n")

glossario_detalhado <- data.frame(
  Variavel = c(
    "data", 
    "ano", 
    "mes", 
    "caged_saldo", 
    "pnad_ocupados", 
    "fonte"
  ),
  
  Descricao_Detalhada = c(
    # data
    "Data de referência da observação no formato Date (AAAA-MM-DD). 
     Para o CAGED: representa o mês de referência do saldo líquido.
     Para a PNAD: representa o início do trimestre móvel da pesquisa.",
    
    # ano
    "Ano extraído da data de referência, utilizado para agrupamentos temporais,
     filtros por período e análises de tendência anual. Valor numérico inteiro.",
    
    # mes  
    "Mês extraído da data de referência, variando de 1 (janeiro) a 12 (dezembro).
     Utilizado para análises de sazonalidade e comportamentos mensais.",
    
    # caged_saldo
    "SALDO LÍQUIDO MENSAL de empregos formais = (Admissões - Demissões).
     Fonte: CAGED/MTE - Registro administrativo universal que captura TODOS os vínculos formais.
     Característica: Mede FLUXO - a variação líquida no estoque de empregos.
     Interpretação: Valores positivos indicam criação líquida de empregos;
     Valores negativos indicam destruição líquida de empregos.",
    
    # pnad_ocupados
    "ESTOQUE de população ocupada com carteira de trabalho assinada.
     Fonte: PNAD Contínua/IBGE - Pesquisa amostral domiciliar expandida.
     Característica: Mede ESTOQUE - o total de pessoas ocupadas em determinado momento.
     Metodologia: Estimativa baseada em amostra representativa, expandida para população.
     Abrangência: Captura tanto setor formal quanto informal (apenas formal nesta análise).",
    
    # fonte
    "Identificação da origem dos dados para rastreabilidade e transparência.
     Valores possíveis: 'CAGED/MTE', 'PNAD Contínua/IBGE', 'CAGED + PNAD'.
     Utilizado para filtrar e identificar a procedência de cada observação."
  ),
  
  Unidade_Medida = c(
    "Data (AAAA-MM-DD)",
    "Ano (ex: 2020, 2021, 2022)",
    "Mês numérico (1-12)",
    "Quantidade de pessoas (unidades)",
    "Quantidade de pessoas (unidades)", 
    "Texto categórico"
  ),
  
  Tipo_Variavel = c(
    "Date (data)",
    "Integer (inteiro)",
    "Integer (inteiro)",
    "Numeric (contínua)",
    "Numeric (contínua)",
    "Character (categórica)"
  ),
  
  Fonte_Original = c(
    "Processado a partir das datas originais das bases", 
    "Derivado da coluna 'data' por extração do ano", 
    "Derivado da coluna 'data' por extração do mês",
    "MTE/CAGED - Ministério do Trabalho e Emprego (registro administrativo)",
    "IBGE/PNAD Contínua - Instituto Brasileiro de Geografia e Estatística (pesquisa amostral)",
    "Processado para identificação das fontes integradas"
  ),
  
  Exemplo_Valor = c(
    "'2022-03-01'",
    "2022",
    "3",
    "150.230 (criação líquida de ~150 mil empregos)",
    "35.648.900 (~35,6 milhões de ocupados com carteira)",
    "'CAGED + PNAD'"
  )
)

cat("GLOSSÁRIO DETALHADO CRIADO:\n")
print(glossario_detalhado)

# 7. EXPORTAÇÃO DOS RESULTADOS
# =============================================================================
#
# OBJETIVO: Salvar todos os resultados para compartilhamento, 
#           documentação e publicação web
#

cat("=== EXPORTANDO RESULTADOS ===\n")

## 7.1 Criar diretório organizado para os resultados
if (!dir.exists("resultados")) {
  dir.create("resultados")
  cat("✓ Diretório 'resultados' criado\n")
}

## 7.2 Salvar dados processados (base integrada)
write_csv(dados_integrados, "resultados/dados_emprego_integrados.csv")
cat("✓ Dados integrados salvos: resultados/dados_emprego_integrados.csv\n")

## 7.3 Salvar gráficos em alta resolução para publicação
ggsave("resultados/grafico_evolucao_temporal.png", p1, width = 12, height = 8, dpi = 300)
ggsave("resultados/grafico_dispersao_correlacao.png", p2, width = 10, height = 8, dpi = 300)
cat("✓ Gráficos salvos em PNG (alta resolução 300dpi)\n")

## 7.4 Salvar estatísticas e documentação
write_csv(estatisticas_descritivas, "resultados/estatisticas_descritivas.csv")
write_csv(glossario_detalhado, "resultados/glossario_variaveis_detalhado.csv")
cat("✓ Estatísticas e glossário detalhado salvos em CSV\n")

# 8. INTERPRETAÇÃO DETALHADA DOS GRÁFICOS
# =============================================================================
#
# OBJETIVO: Fornecer uma análise profunda e interpretação dos gráficos gerados
#           para facilitar o entendimento dos resultados
#

cat("=== INTERPRETAÇÃO DETALHADA DOS GRÁFICOS ===\n")

cat("\n📈 GRÁFICO 1 - EVOLUÇÃO TEMPORAL COMPARATIVA:\n")
cat("   • OBJETIVO: Mostrar o comportamento temporal das duas séries lado a lado\n")
cat("   • LINHA VERMELHA (CAGED): Representa o FLUXO mensal de empregos formais\n")
cat("     - Picos positivos: Períodos de forte criação de empregos\n")
cat("     - Vales negativos: Períodos de destruição líquida de empregos\n")
cat("     - Exemplo: Durante crises econômicas, espera-se valores negativos\n")
cat("   • LINHA AZUL (PNAD): Representa o ESTOQUE trimestral de ocupados com carteira\n")
cat("     - Tendência de crescimento: Expansão do mercado formal ao longo do tempo\n")
cat("     - Estabilidade relativa: Menos volátil que o CAGED (natureza de estoque)\n")
cat("   • COMPARAÇÃO CHAVE:\n")
cat("     - Quando CAGED é positivo por vários meses → PNAD deve crescer\n")
cat("     - Quando CAGED é negativo → PNAD pode estagnar ou cair\n")
cat("     - Defasagem temporal: Mudanças no CAGED levam tempo para refletir no PNAD\n")

cat("\n📊 GRÁFICO 2 - DISPERSÃO E CORRELAÇÃO:\n")
cat("   • OBJETIVO: Quantificar a relação estatística entre fluxo (CAGED) e estoque (PNAD)\n")
cat("   • PONTOS COLORIDOS: Cada ponto é uma observação temporal\n")
cat("     - Cor indica o ano (escala azul→vermelho: anos mais antigos→recentes)\n")
cat("     - Posição no eixo X: Valor do saldo CAGED naquele período\n")
cat("     - Posição no eixo Y: Valor do estoque PNAD no mesmo período\n")
cat("   • LINHA DE TENDÊNCIA VERDE: Mostra a relação linear média entre as variáveis\n")
cat("     - Inclinação positiva: Relação direta (mais CAGED → mais PNAD)\n")
cat("     - Faixa cinza: Intervalo de confiança de 95% para a tendência\n")
cat("   • INTERPRETAÇÃO DA CORRELAÇÃO:", correlacao, "\n")
cat("     - Correlação > 0.7: Forte relação positiva\n")
cat("     - Correlação 0.5-0.7: Relação moderada\n")
cat("     - Correlação < 0.5: Relação fraca\n")
cat("   • PADRÕES IMPORTANTES:\n")
cat("     - Agrupamento por cores: Mostra evolução temporal da relação\n")
cat("     - Dispersão vertical: Variação no PNAD para um mesmo valor de CAGED\n")
cat("     - Outliers: Pontos que fogem do padrão geral (investigar causas)\n")

# 9. COMO REPRODUZIR A ANÁLISE
# =============================================================================
#
# OBJETIVO: Fornecer informações completas para que outros pesquisadores
#           possam reproduzir exatamente a mesma análise
#

cat("=== INFORMAÇÕES PARA REPRODUÇÃO ===\n")

info_reproducao <- list(
  Disciplina = "Introdução ao R Aplicado em Ciência de Dados",
  Professor = "Diogo Tavares Robaina",
  Aluno = "Daniel Furtado Nunes Rocha da Silva",
  R_Version = R.version.string,
  Plataforma = R.version$platform,
  Data_Execucao = Sys.Date(),
  Pacotes_Utilizados = paste(pacotes, collapse = ", "),
  Passos_Reproducao = c(
    "1. Instalar pacotes R listados (serão instalados automaticamente se faltantes)",
    "2. Executar código sequencialmente do início ao fim",
    "3. Dados serão baixados automaticamente das APIs oficiais (requer internet)",
    "4. Processamento inclui: limpeza, integração, análise e visualização",
    "5. Resultados serão salvos automaticamente na pasta 'resultados'"
  ),
  Requisitos_Minimos = c(
    "R versão 4.0.0 ou superior",
    "Conexão com internet para acesso às APIs do IPEA e IBGE",
    "2GB de memória RAM livres",
    "Pacotes listados instalados (instalação automática inclusa)"
  )
)

cat("INFORMAÇÕES DO AMBIENTE DE EXECUÇÃO:\n")
cat("Disciplina:", info_reproducao$Disciplina, "\n")
cat("Professor:", info_reproducao$Professor, "\n")
cat("Aluno:", info_reproducao$Aluno, "\n")
cat("R Version:", info_reproducao$R_Version, "\n")
cat("Plataforma:", info_reproducao$Plataforma, "\n")
cat("Data de Execução:", info_reproducao$Data_Execucao, "\n")

# 10. REFERÊNCIAS E DIVULGAÇÃO DE IA
# =============================================================================
#
# OBJETIVO: Documentar todas as fontes e ferramentas utilizadas,
#           incluindo o uso de IA generativa de forma transparente
#

cat("=== REFERÊNCIAS E CRÉDITOS ===\n")

referencias <- list(
  Fontes_Dados = c(
    "CAGED/MTE: http://pdet.mte.gov.br/microdados-rais-e-caged",
    "PNAD Contínua/IBGE: https://sidra.ibge.gov.br/pesquisa/pnadct",
    "IPEA Data: http://www.ipeadata.gov.br/",
    "SIDRA/IBGE: https://apisidra.ibge.gov.br/"
  ),
  Pacotes_R = c(
    "ecoseries: https://github.com/viniciusoike/ecoseries",
    "sidrar: https://github.com/rpradosiqueira/sidrar", 
    "ggplot2: https://ggplot2.tidyverse.org/",
    "dplyr: https://dplyr.tidyverse.org/"
  ),
  Ferramentas_IA_Utilizadas = c(
    "ChatGPT (OpenAI) para auxílio no desenvolvimento",
    "Propósito: Estruturação de código, documentação e troubleshooting",
    "Prompts principais utilizados:",
    "  - 'Crie código R para integrar dados do CAGED e PNAD de fontes oficiais'",
    "  - 'Ajude a resolver conflitos entre pacotes dplyr e stats'", 
    "  - 'Gere visualizações profissionais com ggplot2 para análise de séries temporais'",
    "  - 'Estruture projeto com seções específicas para publicação web'"
  )
)

cat("REFERÊNCIAS REGISTRADAS COM SUCESSO\n")