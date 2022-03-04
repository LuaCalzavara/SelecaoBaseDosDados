### Seleção para Analista Base dos Dados ######################################

### carreando pacotes

pacman::p_load(basedosdados, tidyverse, data.table,Hmisc, modelsummary, rio)

###  definindo projeto
basedosdados::set_billing_id("<base-dos-dados-selecao>")

query <- bdplyr("br_tse_eleicoes.detalhes_votacao_municipio")
df <- bd_collect(query)


# Tive algum erro de identificação, logo não consegui acessar as bases pelo
# pacote basedosdados. Sendo assim, acessei a base de interesse pelo query no
# próprio google cloud com o seguinte comando:
# SELECT *  FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio_zona` WHERE ano BETWEEN 2000 AND 2020


###### Análise dos dados ####


### importanto a base
base <- read.csv("bq-results-20220303-093156-k324bfwli7wy.csv")


### conhecendo os dados

#sumarizando
describe(base)

#tipos das variáveis

base %>% 
   glimpse()

### observando a distribuição de variáveis de interesse

base %>% 
   select(-c(tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse, zona, secoes,
             secoes_agregadas, secoes_totalizadas, ano)) %>% 
   hist.data.frame()




# proporção de comparecimento: conseguimos observar melhor com esta variável

# selecionando apenas a competição em primeiro turno
base_turno1 <- base %>% 
  filter(turno == 1)


comparecimento <- base_turno1 %>% 
   group_by(ano, cargo) %>% 
   summarise(contagem = n(),
             soma = sum(proporcao_comparecimento),
             media = mean(proporcao_comparecimento),
             mediana = median(proporcao_comparecimento),
             desvio = sd(proporcao_comparecimento),
             minimo = min(proporcao_comparecimento),
             maximo = max(proporcao_comparecimento))
 



prop_comp <- ggplot(comparecimento, aes(ano, media)) + 
   geom_jitter(width = 0.01, height = 0.01) +
   scale_x_continuous(breaks = seq(2000, 2020, 2)) 
p1 <- prop_comp +
   ggtitle("Media de comparecimento nas eleições, por cargo")+ 
   facet_wrap(~ cargo)
p1


### Analisando dados de despesas dos candidatos

# dados obtidos através pelo Cloud da Basedosdados
# SELECT * FROM `basedosdados.br_tse_eleicoes.despesas_candidato` WHERE ano BETWEEN 2000 AND 2020

despesas <- import("bquxjob_54c51b19_17f525d930e.csv")

head(despesas)

describe(despesas) # está faltado os dados da disputa presidencial

hist.data.frame(despesas)

## visivelmente muitos NA nas prestações de conta.
## Mas o que interessa a nós aqui é o acumulado médio por cargo.
## Desejamos verificar a correlação entre comparecimento e custo médio de campanha

names(despesas)

despesas_corte <- despesas %>% 
   select(ano, turno, id_municipio_tse, cargo, valor_despesa)


despesas_sum <- despesas_corte %>% 
   group_by(ano, cargo) %>% 
   summarise(contagem = n(),
             soma = sum(valor_despesa),
             media = mean(valor_despesa),
             mediana = median(valor_despesa),
             desvio = sd(valor_despesa),
             minimo = min(valor_despesa),
             maximo = max(valor_despesa))
despesas_sum


despesas_plot <- ggplot(despesas_sum, aes(ano, media)) + 
   geom_jitter(width = 0.01, height = 0.01) +
   scale_x_continuous(breaks = seq(2000, 2020, 2)) 
p2 <- despesas_plot +
   ggtitle("Media de despesas eleitorais por cargo")+ 
   facet_wrap(~ cargo)
p2


 