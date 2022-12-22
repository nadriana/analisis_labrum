
base <- read.csv("base.csv", na.strings = "#N/A")
base1 <- na.omit(base)

################################## POSICION #########################################


################################## ONE WAY ##########################################
###### one way: SOC (sin cirujano)
medico_soc <- factor(c(rep(1,71), rep(2,71),rep(3,71), rep(4,71)))
pos_soc <- c(base1$pos_SOC_1, base1$pos_SOC_2, base1$pos_SOC_3, base1$pos_SOC_4)
df_pos_soc <- data.frame(medico_soc, pos_soc)

ggboxplot(df_pos_soc, x="medico_soc", y="pos_soc", color="medico_soc", xlab="medicos", ylab="posicion SOC") 

anova_pos_soc <- aov(pos_soc ~ medico_soc, data = df_pos_soc)
summary(anova_pos_soc)
#El valor p es muy pequeño (1.91e-07), por lo que se puede decir que el resultado es significativo, 
#esto implica que se rechaza la hipótesis nula, es decir, que hay diferencias entre al menos una de las mediciones por médico usando SOC
#Sin NAs: valor-p es muy pequeño (8.36e-08 )...

#Análisis de residuales
plot(anova_pos_soc)
#El análisis de residuales muestra que los supuestos del modelo (de regresión) se cumplen

### sin outliers y sin NAs
#so = sin outliers  
df_pos_soc_so <- df_pos_soc[-c(22,45,116), ]

anova_pos_soc_so <- aov(pos_soc ~ medico_soc, data = df_pos_soc_so)
summary(anova_pos_soc_so)
#el valor p es muy pequeño (3.57e-08)
plot(anova_pos_soc_so)
#no hay tanta diferencia si quito los outliers, entonces ME QUEDO CON EL PRIMER RESULTADO



###### one way: IDEAL (sin cirujano)
medico_ideal <- factor(c(rep(1,71), rep(2,71),rep(3,71), rep(4,71)))
pos_ideal <- c(base1$pos_IDEAL_1, base1$pos_IDEAL_2, base1$pos_IDEAL_3, base1$pos_IDEAL_4)
df_pos_ideal <- data.frame(medico_ideal, pos_ideal)

ggboxplot(df_pos_ideal, x="medico_ideal", y="pos_ideal", color="medico_ideal", xlab="medicos", ylab="posicion IDEAL") 

anova_pos_ideal <- aov(pos_ideal ~ medico_ideal, data = df_pos_ideal)
summary(anova_pos_ideal)
#El valor p es muy pequeño (3e-07), por lo que se puede decir que el resultado es significativo, 
#esto implica que se rechaza la hipótesis nula, es decir, que hay diferencias las mediciones por médico usando IDEAL
#Sin NA: valor-p muy pequeño (1.13e-07)

#Análisis de residuales
plot(anova_pos_ideal)
#el supuesto de normalidad no se respeta
#fitted también se ve una bajada muy pronunciada al final (Creo que esta gráfica es de los cuadrados)


### sin outliers y sin NAs
#so = sin outliers  
df_pos_ideal_so <- df_pos_ideal[-c(44,116,136,45,139,281), ] 
anova_pos_ideal_so <- aov(pos_ideal ~ medico_ideal, data = df_pos_ideal_so)
summary(anova_pos_ideal_so)
#el valor p es muy pequeño (1.13e-07)
#quitando las segundas tres observaciones se hace un pco más normal y el valor p = 2.9e-10 
plot(anova_pos_ideal_so)
#no hay tanta diferencia si quito los outliers
#quité otras tres observaciones, me quedo con este resultado (quitando en total seis observaciones)




################################## EXTENSION ########################################

################################## ONE WAY ##########################################

###### one way: SOC (sin cirujano)
medico_soc <- factor(c(rep(1,87), rep(2,87),rep(3,87), rep(4,87)))
ext_soc <- c(base$ext_SOC_1, base$ext_SOC_2, base$ext_SOC_3, base$ext_SOC_4)
df_ext_soc <- data.frame(medico_soc, ext_soc)

ggboxplot(df_ext_soc, x="medico_soc", y="ext_soc", color="medico_soc", xlab="medicos", ylab="extension SOC") 

anova_ext_soc <- aov(ext_soc ~ medico_soc, data = df_ext_soc)
summary(anova_ext_soc)
#El valor p es muy pequeño (4.72e-10), por lo que se puede decir que el resultado es significativo, 
#esto implica que se rechaza la hipótesis nula, es decir, que hay diferencias las mediciones por médico usando SOC

plot(anova_ext_soc)

### sin outliers
#so = sin outliers  
df_ext_soc_so <- df_ext_soc[-c(31, 118, 345), ]
anova_ext_soc_so <- aov(ext_soc ~ medico_soc, data = df_ext_soc_so)
summary(anova_ext_soc_so)
#el valor p es muy pequeño (2.19e-10)
plot(anova_ext_soc_so)
#me quedo con este resultado



######one way: IDEAL (sin cirujano)
medico_ideal <- factor(c(rep(1,87), rep(2,87),rep(3,87), rep(4,87)))
ext_ideal <- c(base$ext_IDEAL_1, base$ext_IDEAL_2, base$ext_IDEAL_3, base$ext_IDEAL_4)
df_ext_ideal <- data.frame(medico_ideal, ext_ideal)

ggboxplot(df_ideal, x="medico_ideal", y="ext_ideal", color="medico_ideal", xlab="medicos", ylab="exticion IDEAL") 

anova_ext_ideal <- aov(ext_ideal ~ medico_ideal, data = df_ext_ideal)
summary(anova_ext_ideal)
#El valor p es muy pequeño (4.16e-09), por lo que se puede decir que el resultado es significativo, 
#esto implica que se rechaza la hipótesis nula, es decir, que hay diferencias las mediciones por médico usando IDEAL

plot(anova_ext_ideal)

### sin outliers
df_ext_ideal_so <- df_ext_ideal[-c(145, 167, 319), ]
anova_ext_ideal_so <- aov(ext_ideal ~ medico_ideal, data = df_ext_ideal_so)
summary(anova_ext_ideal_so)
#el valor p es muy pequeño (7e-12)
plot(anova_ext_ideal_so)
#me quedo con este resultado

