#--------------------------------------------------------- EXTENSION ---------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(gtable)
library(dplyr)


# Variables
base <- read.csv("base.csv", na.strings = "#N/A")

id <- base$paciente_ID
ecir <- base$ext_cir

esoc1 <- base$ext_SOC_1
esoc2 <- base$ext_SOC_2
esoc3 <- base$ext_SOC_3
esoc4 <- base$ext_SOC_4
eideal1 <- base$ext_IDEAL_1 
eideal2 <- base$ext_IDEAL_2
eideal3 <- base$ext_IDEAL_3 
eideal4 <- base$ext_IDEAL_4

df <- data.frame(id, ecir, esoc1, esoc2, esoc3, esoc4, eideal1, eideal2, eideal3, eideal4)


# Definir la figura adecuada según el valor máximo
max(data.frame(max(df$ecir, na.rm =TRUE),
max(df$esoc1, na.rm =TRUE),
max(df$esoc2, na.rm =TRUE),
max(df$esoc3, na.rm =TRUE),
max(df$esoc4, na.rm =TRUE),
max(df$eideal1, na.rm =TRUE),
max(df$eideal2, na.rm =TRUE),
max(df$eideal3, na.rm =TRUE),
max(df$eideal4, na.rm =TRUE)))

df <- df %>% mutate(mycol1_esi = ifelse(esoc1 > eideal1, "SOC", "IDEAL"))
df <- df %>% mutate(mycol2_esi = ifelse(esoc2 > eideal2, "SOC", "IDEAL"))
df <- df %>% mutate(mycol3_esi = ifelse(esoc3 > eideal3, "SOC", "IDEAL"))
df <- df %>% mutate(mycol4_esi = ifelse(esoc4 > eideal4, "SOC", "IDEAL"))

df <- df %>% mutate(mycol1_esc = ifelse(esoc1 > ecir, "SOC", "Cirujano"))
df <- df %>% mutate(mycol2_esc = ifelse(esoc2 > ecir, "SOC", "Cirujano"))
df <- df %>% mutate(mycol3_esc = ifelse(esoc3 > ecir, "SOC", "Cirujano"))
df <- df %>% mutate(mycol4_esc = ifelse(esoc4 > ecir, "SOC", "Cirujano"))

df <- df %>% mutate(mycol1_eic = ifelse(eideal1 > ecir, "IDEAL", "Cirujano"))
df <- df %>% mutate(mycol2_eic = ifelse(eideal2 > ecir, "IDEAL", "Cirujano"))
df <- df %>% mutate(mycol3_eic = ifelse(eideal3 > ecir, "IDEAL", "Cirujano"))
df <- df %>% mutate(mycol4_eic = ifelse(eideal4 > ecir, "IDEAL", "Cirujano"))


# --------------- plots Extensión SOC cv IDEAL ---------------

p <- ggplot(df, aes(x=id))

p1esi <-   p + geom_point(aes(y=esoc1, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=eideal1, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc1, yend=eideal1, color=mycol1_esi), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p2esi <-  p + geom_point(aes(y=esoc2, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=eideal2, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc2, yend=eideal2, color=mycol2_esi), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(size = 7))

p3esi <-  p + geom_point(aes(y=esoc3, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=eideal3, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc3, yend=eideal3, color=mycol3_esi), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p4esi <-  p + geom_point(aes(y=esoc4, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=eideal4, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc4, yend=eideal4, color=mycol4_esi), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))


leyenda <- gtable_filter(ggplotGrob(p1esi), "guide-box") 

plots_esi <- ggarrange(p1esi, p2esi, p3esi, p4esi, 
                    ncol=1, nrow=4, 
                    heights = c(1, 1, 1, 1.25),
                    common.legend = TRUE, legend="bottom")

annotate_figure(plots_esi,
                top = textGrob("Extensión. Método SOC vs. Método IDEAL", vjust = 1)
)





# --------------- plots Extensión SOC cv Cir -----------------

p <- ggplot(df, aes(x=id))

p1esc <-   p + geom_point(aes(y=esoc1, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc1, yend=ecir, color=mycol1_esc), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p2esc <-  p + geom_point(aes(y=esoc2, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc2, yend=ecir, color=mycol2_esc), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p3esc <-  p + geom_point(aes(y=esoc3, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc3, yend=ecir, color=mycol3_esc), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p4esc <-  p + geom_point(aes(y=esoc4, color="SOC"), shape=2, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=esoc4, yend=ecir, color=mycol4_esc), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,4)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))


leyenda <- gtable_filter(ggplotGrob(p1esc), "guide-box") 

plots_esc <- ggarrange(p1esc, p2esc, p3esc, p4esc, 
                       ncol=1, nrow=4, 
                       heights = c(1, 1, 1, 1.25),
                       common.legend = TRUE, legend="bottom")

annotate_figure(plots_esc,
                top = textGrob("Extensión. Método SOC vs. Cirujano", vjust = 1)
)




# --------------- plots Extensión IDEAL cv Cir ---------------

p <- ggplot(df, aes(x=id))

p1eic <-   p + geom_point(aes(y=eideal1, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=eideal1, yend=ecir, color=mycol1_eic), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,2)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 1") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p2eic <-  p + geom_point(aes(y=eideal2, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=eideal2, yend=ecir, color=mycol2_eic), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,2)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 2") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p3eic <-  p + geom_point(aes(y=eideal3, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=eideal3, yend=ecir, color=mycol3_eic), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,2)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente") + 
  ylab("Radiólogo 3") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))

p4eic <- p + geom_point(aes(y=eideal4, color="IDEAL"), shape=1, size=2, stroke=1, na.rm=TRUE) +
  geom_point(aes(y=ecir, color="Cirujano"), shape=0, size=2, stroke=1, na.rm=TRUE) +
  geom_segment(aes(x=id, xend=id, y=eideal4, yend=ecir, color=mycol4_eic), size=.7, na.rm=TRUE) +
  scale_x_discrete(limits=seq(1,87,2)) +
  scale_y_continuous(limits=c(0,  4.7124),
                     breaks = c(0, 0.5236, 1.0472, 
                                1.5708, 2.0944,
                                2.618, 3.1416, 
                                3.6652, 4.1888, 
                                4.7124),
                     labels=c(0, "", expression(frac(2*pi,12)), 
                              "", expression(frac(4*pi,12)), 
                              "", expression(frac(6*pi,12)), 
                              "", expression(frac(8*pi,12)),
                              "")) +
  guides(color=guide_legend(title="Método")) +
  xlab("Paciente ID") + 
  ylab("Radiólogo 4") + 
  theme(axis.ticks.x=element_blank(), axis.text.x = element_text(size = 7)) + 
  theme(axis.ticks.y=element_blank(), axis.text.y = element_text(size = 7))


leyenda <- gtable_filter(ggplotGrob(p1eic), "guide-box") 

plots_eic <- ggarrange(p1eic, p2eic, p3eic, p4eic, 
                       ncol=1, nrow=4, 
                       heights = c(1, 1, 1, 1.25),
                       common.legend = TRUE, legend="bottom")

annotate_figure(plots_eic,
                top = textGrob("Extensión. Método IDEAL vs. Cirujano", vjust = 1)
)




