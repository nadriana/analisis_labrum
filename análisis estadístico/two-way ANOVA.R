
base <- read.csv("base.csv", na.strings = "#N/A")


# factor metodo: nivel 1 = SOC, nivel 2 = IDEAL
# la cirujano no se le incluye porque su diagnóstico no es relevante para la comparación de resultados entre medicos y metodos


################################## TWO WAY ##########################################

#two way (sin cirujano)
metodo_tw <- factor(c(rep(1,87),rep(2,87),rep(1,87),rep(2,87),rep(1,87),rep(2,87),rep(1,87),rep(2,87)))
medico_tw <- factor(c(rep(1,174),rep(2,174),rep(3,174),rep(4,174)))
pos_tw <- c(base$pos_SOC_1, base$pos_IDEAL_1, base$pos_SOC_2, base$pos_IDEAL_2,
            base$pos_SOC_3, base$pos_IDEAL_3, base$pos_SOC_4, base$pos_IDEAL_4)
ext_tw <- c(base$ext_SOC_1, base$ext_IDEAL_1, base$ext_SOC_2, base$ext_IDEAL_2,
            base$ext_SOC_3, base$ext_IDEAL_3, base$ext_SOC_4, base$ext_IDEAL_4)

df_tw <- data.frame(medico_tw, metodo_tw, pos_tw, ext_tw)
df_tw_sin_na <- df_tw[complete.cases(df_tw),]


# Two-way ANOVA: POSICION
tw_anova_pos <- aov(pos_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw)
summary(tw_anova_pos)
#el factor medico tiene un valor p = 3.06e-14
#el factor metodo tiene un valor p = 0.00447 
#el factor de interacción tiene un valor p = 0.49251
#por lo que los factores médico y método son significativos, y el factor interacción no lo es

#sin NAs
tw_anova_pos_sin_na <- aov(pos_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw_sin_na)
summary(tw_anova_pos_sin_na)
#el factor medico tiene un valor p = 1.25e-14
#el factor metodo tiene un valor p = 0.0117 
#el factor de interacción tiene un valor p = 0.6390
#por lo que los factores médico y método son significativos, y el factor interacción no lo es

interaction.plot(df_tw_sin_na$metodo_tw, df_tw_sin_na$medico_tw, df_tw_sin_na$pos_tw, 
                 fun = mean, type = "b")

plot(tw_anova_pos_sin_na)


#quitando outliers
#pos: 140, 344
df_tw_so_pos <- df_tw[-c(144, 344), ]
df_tw_sin_na_so_pos <- df_tw_sin_na_so_pos[complete.cases(df_tw_so_pos),]


tw_anova_pos_sin_na_so <- aov(pos_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw_sin_na_so_pos)
summary(tw_anova_pos_sin_na_so)
#el factor medico tiene un valor p = 2.86e-15
#el factor metodo tiene un valor p = 0.0718 
#el factor de interacción tiene un valor p = 0.56820
#por lo que los factores médico y método son significativos, y el factor interacción no lo es

interaction.plot(df_tw_sin_na_so_pos$metodo_tw, df_tw_sin_na_so_pos$medico_tw, df_tw_sin_na_so_pos$pos_tw, 
                 fun = mean, type = "b")

plot(tw_anova_pos_sin_na_so)







# Two-way ANOVA: EXTENSION
tw_anova_ext <- aov(ext_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw)
summary(tw_anova_ext)
#el factor medico tiene un valor p = <2e-16 
#el factor metodo tiene un valor p = 0.0445 
#el factor de interacción tiene un valor p = 0.9687 
#por lo que los factores médico y método son significativos, y el factor interacción no lo es

#sin NAs
tw_anova_ext_sin_na <- aov(ext_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw_sin_na)
summary(tw_anova_ext_sin_na)
#el factor medico tiene un valor p = <2e-16
#el factor metodo tiene un valor p = 0.0445
#el factor de interacción tiene un valor p = 0.9687
#por lo que los factores médico y método son significativos, y el factor interacción no lo es

interaction.plot(df_tw_sin_na$metodo_tw, df_tw_sin_na$medico_tw, df_tw_sin_na$ext_tw, 
                 fun = mean, type = "b")

plot(tw_anova_ext_sin_na)

#quitando outliers
#ext: 319, 606, 667
df_tw_sin_na_so_ext <- df_tw_sin_na[-c(319, 606, 667), ]

tw_anova_ext_sin_na_so <- aov(ext_tw ~ medico_tw + metodo_tw + medico_tw:metodo_tw, data=df_tw_sin_na_so_ext)
summary(tw_anova_ext_sin_na_so)

plot(tw_anova_ext_sin_na_so)




