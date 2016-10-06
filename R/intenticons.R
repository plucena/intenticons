#pacote necess?rio para calcular a moda
require(functional)

#seta o working directory para os links relativos funcionarem
diretorio <- dirname(parent.frame(2)$ofile)
setwd(diretorio)
rm(diretorio)

#l? os arquivos CSV e atribui o valor NA para respostas vazias
proposta <- read.csv("csv/proposta.csv", na.strings=c(""))
previsao <- read.csv("csv/previsao.csv", na.strings=c(""))
induzimento <- read.csv("csv/induzimento.csv", na.strings=c(""))
afirmacao <- read.csv("csv/afirmacao.csv", na.strings=c(""))
desejo <- read.csv("csv/desejo.csv", na.strings=c(""))
contricao <- read.csv("csv/contricao.csv", na.strings=c(""))
retratacao <- read.csv("csv/retratacao.csv", na.strings=c(""))
valoracao <- read.csv("csv/valoracao.csv", na.strings=c(""))

#coloca o prefixo da ilocu??o no t?tulo das colunas (para poder juntar em uma ?nica tabela e saber o que ? o que)
colnames(proposta) <- paste("PROPOSTA.", colnames(proposta), sep = "")
colnames(previsao) <- paste("PREVISAO.", colnames(previsao), sep = "")
colnames(induzimento) <- paste("INDUZIMENTO.", colnames(induzimento), sep = "")
colnames(afirmacao) <- paste("AFIRMACAO.", colnames(afirmacao), sep = "")
colnames(desejo) <- paste("DESEJO.", colnames(desejo), sep = "")
colnames(contricao) <- paste("CONTRICAO.", colnames(contricao), sep = "")
colnames(retratacao) <- paste("RETRATACAO.", colnames(retratacao), sep = "")
colnames(valoracao) <- paste("VALORACAO.", colnames(valoracao), sep = "")

#junta todas as ilocu??es em uma ?nica tabela
global <- cbind(proposta, previsao, induzimento, afirmacao, desejo, contricao, retratacao, valoracao)

#remove as linhas que possuem ao menos uma resposta NA (em branco) para todas as ilocu??es (an?lise geral)
global <- na.omit(global)

#remove as tabelas que n?o estamos usando por enquanto, para n?o confundir
#rm(proposta,previsao,induzimento,afirmacao,desejo,contricao,retratacao,valoracao)

#remove as linhas que possuem ao menos uma resposta NA (em branco) para uma ?nica ilocu??o (an?lise individual)
proposta <- na.omit(proposta)
previsao <- na.omit(previsao)
induzimento <- na.omit(induzimento)
afirmacao <- na.omit(afirmacao)
desejo <- na.omit(desejo)
contricao <- na.omit(contricao)
retratacao <- na.omit(retratacao)
valoracao <- na.omit(valoracao)

#cria data frames com as m?dias dos valores para o grupo OLD e para o grupo NEW
#provavelmente n?o est? correto usar m?dia, pois os dados vem de uma escala likert, precisamos ver isso
global_meta <- data.frame(
  MEDIANA.OLD=apply(global[ , grep(".OLD", colnames(global))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(global[ , grep(".NEW", colnames(global))], 1, median, na.rm = TRUE)
  #MODA.OLD=apply(global[ , grep(".OLD", colnames(global))], 1, Compose(table, function(i) i==max(i), which, names, function(i) paste0(i, collapse='/'))),
  #MODA.NEW=apply(global[ , grep(".NEW", colnames(global))], 1, Compose(table, function(i) i==max(i), which, names, function(i) paste0(i, collapse='/')))
)

proposta_meta <- data.frame(
  MEDIANA.OLD=apply(proposta[ , grep(".OLD", colnames(proposta))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(proposta[ , grep(".NEW", colnames(proposta))], 1, median, na.rm = TRUE)
)

previsao_meta <- data.frame(
  MEDIANA.OLD=apply(previsao[ , grep(".OLD", colnames(previsao))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(previsao[ , grep(".NEW", colnames(previsao))], 1, median, na.rm = TRUE)
)

induzimento_meta <- data.frame(
  MEDIANA.OLD=apply(induzimento[ , grep(".OLD", colnames(induzimento))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(induzimento[ , grep(".NEW", colnames(induzimento))], 1, median, na.rm = TRUE)
)

afirmacao_meta <- data.frame(
  MEDIANA.OLD=apply(afirmacao[ , grep(".OLD", colnames(afirmacao))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(afirmacao[ , grep(".NEW", colnames(afirmacao))], 1, median, na.rm = TRUE)
)

desejo_meta <- data.frame(
  MEDIANA.OLD=apply(desejo[ , grep(".OLD", colnames(desejo))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(desejo[ , grep(".NEW", colnames(desejo))], 1, median, na.rm = TRUE)
)

contricao_meta <- data.frame(
  MEDIANA.OLD=apply(contricao[ , grep(".OLD", colnames(contricao))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(contricao[ , grep(".NEW", colnames(contricao))], 1, median, na.rm = TRUE)
)

retratacao_meta <- data.frame(
  MEDIANA.OLD=apply(retratacao[ , grep(".OLD", colnames(retratacao))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(retratacao[ , grep(".NEW", colnames(retratacao))], 1, median, na.rm = TRUE)
)

valoracao_meta <- data.frame(
  MEDIANA.OLD=apply(valoracao[ , grep(".OLD", colnames(valoracao))], 1, median, na.rm = TRUE),
  MEDIANA.NEW=apply(valoracao[ , grep(".NEW", colnames(valoracao))], 1, median, na.rm = TRUE)
)

#1 vari?vel dependente (percep??o)
#1 vari?vel independente (grupo do intenticon)
#2 tratamentos (grupo novo e velho)
#dados n?o param?tricos
#teste adequado: Wilcoxon signed ranks test: wilcox.test(write, read, paired = TRUE)

#H0
print(paste("Global H0 (diferentes):", wilcox.test(global_meta$MEDIANA.OLD, global_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Proposta H0 (diferentes):", wilcox.test(proposta_meta$MEDIANA.OLD, proposta_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Previsao H0 (diferentes):", wilcox.test(previsao_meta$MEDIANA.OLD, previsao_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Induzimento H0 (diferentes):", wilcox.test(induzimento_meta$MEDIANA.OLD, induzimento_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Afirmacao H0 (diferentes):", wilcox.test(afirmacao_meta$MEDIANA.OLD, afirmacao_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Desejo H0 (diferentes):", wilcox.test(desejo_meta$MEDIANA.OLD, desejo_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Contricao H0 (diferentes):", wilcox.test(contricao_meta$MEDIANA.OLD, contricao_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Retratacao H0 (diferentes):", wilcox.test(retratacao_meta$MEDIANA.OLD, retratacao_meta$MEDIANA.NEW, paired = TRUE)$p.value))
print(paste("Valoracao H0 (diferentes):", wilcox.test(valoracao_meta$MEDIANA.OLD, valoracao_meta$MEDIANA.NEW, paired = TRUE)$p.value))

#H1
print(paste("Global H1 (maior):", wilcox.test(global_meta$MEDIANA.OLD, global_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Proposta H1 (maior):", wilcox.test(proposta_meta$MEDIANA.OLD, proposta_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Previs?o H1 (maior):", wilcox.test(previsao_meta$MEDIANA.OLD, previsao_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Induzimento H1 (maior):", wilcox.test(induzimento_meta$MEDIANA.OLD, induzimento_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Afirma??o H1 (maior):", wilcox.test(afirmacao_meta$MEDIANA.OLD, afirmacao_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Desejo H1 (maior):", wilcox.test(desejo_meta$MEDIANA.OLD, desejo_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Contri??o H1 (maior):", wilcox.test(contricao_meta$MEDIANA.OLD, contricao_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Retrata??o H1 (maior):", wilcox.test(retratacao_meta$MEDIANA.OLD, retratacao_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))
print(paste("Valora??o H1 (maior):", wilcox.test(valoracao_meta$MEDIANA.OLD, valoracao_meta$MEDIANA.NEW, paired = TRUE, alternative = "greater")$p.value))

#H2
print(paste("Global H2 (menor):", wilcox.test(global_meta$MEDIANA.OLD, global_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Proposta H2 (menor):", wilcox.test(proposta_meta$MEDIANA.OLD, proposta_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Previs?o H2 (menor):", wilcox.test(previsao_meta$MEDIANA.OLD, previsao_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Induzimento H2 (menor):", wilcox.test(induzimento_meta$MEDIANA.OLD, induzimento_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Afirma??o H2 (menor):", wilcox.test(afirmacao_meta$MEDIANA.OLD, afirmacao_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Desejo H2 (menor):", wilcox.test(desejo_meta$MEDIANA.OLD, desejo_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Contri??o H2 (menor):", wilcox.test(contricao_meta$MEDIANA.OLD, contricao_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Retrata??o H2 (menor):", wilcox.test(retratacao_meta$MEDIANA.OLD, retratacao_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
print(paste("Valora??o H2 (menor):", wilcox.test(valoracao_meta$MEDIANA.OLD, valoracao_meta$MEDIANA.NEW, paired = TRUE, alternative = "less")$p.value))
