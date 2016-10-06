library("ggplot2")
library("reshape2")
library("plyr")

png("plots/plot-%d.png", type="cairo", width=1100, height=1200, res=150, bg = "transparent")

global_meta_long <- reshape(global_meta, direction = "long", varying = 1:2)
global_meta_long <- rename(global_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
global_meta_long[global_meta_long=="OLD"]<-"Antigo"
global_meta_long[global_meta_long=="NEW"]<-"Novo"

proposta_meta_long <- reshape(proposta_meta, direction = "long", varying = 1:2)
proposta_meta_long <- rename(proposta_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
proposta_meta_long[proposta_meta_long=="OLD"]<-"Antigo"
proposta_meta_long[proposta_meta_long=="NEW"]<-"Novo"

previsao_meta_long <- reshape(previsao_meta, direction = "long", varying = 1:2)
previsao_meta_long <- rename(previsao_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
previsao_meta_long[previsao_meta_long=="OLD"]<-"Antigo"
previsao_meta_long[previsao_meta_long=="NEW"]<-"Novo"

induzimento_meta_long <- reshape(induzimento_meta, direction = "long", varying = 1:2)
induzimento_meta_long <- rename(induzimento_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
induzimento_meta_long[induzimento_meta_long=="OLD"]<-"Antigo"
induzimento_meta_long[induzimento_meta_long=="NEW"]<-"Novo"

afirmacao_meta_long <- reshape(afirmacao_meta, direction = "long", varying = 1:2)
afirmacao_meta_long <- rename(afirmacao_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
afirmacao_meta_long[afirmacao_meta_long=="OLD"]<-"Antigo"
afirmacao_meta_long[afirmacao_meta_long=="NEW"]<-"Novo"

desejo_meta_long <- reshape(desejo_meta, direction = "long", varying = 1:2)
desejo_meta_long <- rename(desejo_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
desejo_meta_long[desejo_meta_long=="OLD"]<-"Antigo"
desejo_meta_long[desejo_meta_long=="NEW"]<-"Novo"

contricao_meta_long <- reshape(contricao_meta, direction = "long", varying = 1:2)
contricao_meta_long <- rename(contricao_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
contricao_meta_long[contricao_meta_long=="OLD"]<-"Antigo"
contricao_meta_long[contricao_meta_long=="NEW"]<-"Novo"

retratacao_meta_long <- reshape(retratacao_meta, direction = "long", varying = 1:2)
retratacao_meta_long <- rename(retratacao_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
retratacao_meta_long[retratacao_meta_long=="OLD"]<-"Antigo"
retratacao_meta_long[retratacao_meta_long=="NEW"]<-"Novo"

valoracao_meta_long <- reshape(valoracao_meta, direction = "long", varying = 1:2)
valoracao_meta_long <- rename(valoracao_meta_long, c("time"="Grupo", "MEDIANA"="Mediana", "id"="Participante"))
valoracao_meta_long[valoracao_meta_long=="OLD"]<-"Antigo"
valoracao_meta_long[valoracao_meta_long=="NEW"]<-"Novo"

print(ggplot(data = global_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Todas Ilocuções") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = global_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Todas Ilocuções") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = proposta_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Proposta") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = proposta_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Proposta") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = previsao_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Previsão") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = previsao_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Previsão") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = induzimento_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Induzimento") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = induzimento_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Induzimento") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = afirmacao_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Afirmação") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = afirmacao_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Afirmação") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = desejo_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Desejo") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = desejo_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Desejo") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = contricao_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Contrição") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = contricao_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Contrição") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = retratacao_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Retratação") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = retratacao_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Retratação") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = valoracao_meta_long, aes(x=Participante, y=Mediana, colour=Grupo, group=Grupo, fill=Grupo)) +
        #geom_bar(stat = "identity", position = position_dodge()) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        ggtitle("Valoração") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

print(ggplot(data = valoracao_meta_long, aes(x=Grupo, y=Mediana, colour=Grupo, group=Grupo)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.5), size = 3) +
        ggtitle("Valoração") +
        xlab("Participante") + ylab("Mediana da percepção de intenção"))

dev.off()