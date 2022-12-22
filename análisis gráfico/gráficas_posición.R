
#--------------------------------------------------------- POSICION ---------------------------------------------------------

library(ggplot2)
library(grid) # textGrob
library(ggpubr) # ggarrange
library(gtable) # para las leyendas
library(dplyr)


# Variables
base <- read.csv("base.csv", na.strings = "#N/A")

id <- base$paciente_ID
cir <- base$pos_cir

soc1 <- base$pos_SOC_1
soc2 <- base$pos_SOC_2
soc3 <- base$pos_SOC_3
soc4 <- base$pos_SOC_4
ideal1 <- base$pos_IDEAL_1 
ideal2 <- base$pos_IDEAL_2
ideal3 <- base$pos_IDEAL_3 
ideal4 <- base$pos_IDEAL_4 

df1 <- data.frame(id, cir, soc1, soc2, soc3, soc4, ideal1, ideal1, ideal2, ideal3, ideal4)




# --------------- plots Posición IDEAL cv SOC ---------------

# Definir la figura adecuada según el valor máximo
df1 <- df1 %>% mutate(mycol1 = ifelse(soc1 > ideal1, "SOC", "IDEAL"))
df1 <- df1 %>% mutate(mycol2 = ifelse(soc2 > ideal2, "SOC", "IDEAL"))
df1 <- df1 %>% mutate(mycol3 = ifelse(soc3 > ideal3, "SOC", "IDEAL"))
df1 <- df1 %>% mutate(mycol4 = ifelse(soc4 > ideal4, "SOC", "IDEAL"))

p <- ggplot(df1, aes(x=id))

p1 <-  p + geom_point(aes(y=soc1, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal1, color="IDEAL"), shape=1, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc1, yend=ideal1, color=mycol1), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p2 <-  p + geom_point(aes(y=soc2, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal2, color="IDEAL"), shape=1, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc2, yend=ideal2, color=mycol2), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p3 <-  p + geom_point(aes(y=soc3, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal3, color="IDEAL"), shape=1, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc3, yend=ideal3, color=mycol3), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p4 <-  p + geom_point(aes(y=soc4, color="SOC"), shape=2,  stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal4, color="IDEAL"), shape=1, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc4, yend=ideal4, color=mycol4), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())


leyenda <- gtable_filter(ggplotGrob(p1), "guide-box") 

plots1 <- ggarrange(p1, p2, p3, p4, 
                    ncol=1, nrow=4, 
                    heights = c(1, 1, 1, 1.25),
                    common.legend = TRUE, legend="bottom")

annotate_figure(plots1,
                top = textGrob("Posición. Método SOC vs. Método IDEAL", vjust = 1)
)





# --------------- plots Posición SOC cv Cir ---------------

# Definir la figura adecuada según el valor máximo
df2 <- data.frame(id, cir, soc1, soc2, soc3, soc4)

df2 <- df2 %>% mutate(mycol1 = ifelse(soc1 > cir, "SOC", "Cirujano"))
df2 <- df2 %>% mutate(mycol2 = ifelse(soc2 > cir, "SOC", "Cirujano"))
df2 <- df2 %>% mutate(mycol3 = ifelse(soc3 > cir, "SOC", "Cirujano"))
df2 <- df2 %>% mutate(mycol4 = ifelse(soc4 > cir, "SOC", "Cirujano"))

p <- ggplot(df2, aes(x=id))

p1 <-  p + geom_point(aes(y=soc1, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=cir, color="Cirujano"), shape=0, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc1, yend=cir, color=mycol1), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p2 <-  p + geom_point(aes(y=soc2, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=cir, color="Cirujano"), shape=0, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc2, yend=cir, color=mycol2), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p3 <-  p + geom_point(aes(y=soc3, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=cir, color="Cirujano"), shape=0, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc3, yend=cir, color=mycol3), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p4 <-  p + geom_point(aes(y=soc4, color="SOC"), shape=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=cir, color="Cirujano"), shape=0, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=soc4, yend=cir, color=mycol4), linewidth=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())


leyenda <- gtable_filter(ggplotGrob(p1), "guide-box") 

plots2 <- ggarrange(p1, p2, p3, p4, 
                    ncol=1, nrow=4, 
                    heights = c(1, 1, 1, 1.25),
                    common.legend = TRUE, legend="bottom")

annotate_figure(plots2,
                top = textGrob("Posición. Método SOC vs. Cirujano", vjust = 1)
)





# --------------- plots Posición IDEAL cv Cir ---------------

# Definir la figura adecuada según el valor máximo
df3 <- data.frame(id, cir, ideal1, ideal1, ideal2, ideal3, ideal4)

df3 <- df3 %>% mutate(mycol1 = ifelse(cir > ideal1, "Cirujano", "IDEAL"))
df3 <- df3 %>% mutate(mycol2 = ifelse(cir > ideal2, "Cirujano", "IDEAL"))
df3 <- df3 %>% mutate(mycol3 = ifelse(cir > ideal3, "Cirujano", "IDEAL"))
df3 <- df3 %>% mutate(mycol4 = ifelse(cir > ideal4, "Cirujano", "IDEAL"))

p <- ggplot(df3, aes(x=id))

p1 <-  p + geom_point(aes(y=cir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal1, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=cir, yend=ideal1, color=mycol1), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p2 <-  p + geom_point(aes(y=cir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal2, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=cir, yend=ideal2, color=mycol2), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p3 <-  p + geom_point(aes(y=cir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal3, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=cir, yend=ideal3, color=mycol3), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())

p4 <-  p + geom_point(aes(y=cir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ideal4, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=cir, yend=ideal4, color=mycol4), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_discrete(limits=c(-3.1416, -2.618, -2.0944, -1.5708, -1.0472, -0.5236,
                            0, 0.5236, 1.0472, 1.5708, 2.0944, 2.618, 3.1416),
                   labels=c(expression(-pi), "", "",
                            expression(frac(-pi,2)), "", "", 
                            0, 
                            "", "", expression(frac(pi,2)), 
                            "", "", expression(pi))) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank())


leyenda <- gtable_filter(ggplotGrob(p1), "guide-box") 

plots3 <- ggarrange(p1, p2, p3, p4, 
                    ncol=1, nrow=4, 
                    heights = c(1, 1, 1, 1.25),
                    common.legend = TRUE, legend="bottom")

annotate_figure(plots3,
                top = textGrob("Posición. Método IDEAL vs. Cirujano", vjust = 1)
)






