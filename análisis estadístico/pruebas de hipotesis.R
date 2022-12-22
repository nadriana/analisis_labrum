
library(dplyr)

base <- read.csv("base.csv", na.strings = "#N/A")
base1 <- na.omit(base)

# PRUEBAS DE HIPÓTESIS ############################################################

# POSICION ########################################################################
# Medico 1 
t.test(base1$pos_cir, base1$pos_SOC_1, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_cir, base1$pos_IDEAL_1, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_SOC_1, base1$pos_IDEAL_1, alternative = "two.sided", paired = TRUE)

# Medico 2 
t.test(base1$pos_cir, base1$pos_SOC_2, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_cir, base1$pos_IDEAL_2, alternative = "two.sided", paired = TRUE)
t.test(base1$pos_SOC_2, base1$pos_IDEAL_2, alternative = "two.sided", paired = TRUE) 

# Medico 3 
t.test(base1$pos_cir, base1$pos_SOC_3, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_cir, base1$pos_IDEAL_3, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_SOC_3, base1$pos_IDEAL_3, alternative = "two.sided", paired = TRUE) 

# Medico 4 
t.test(base1$pos_cir, base1$pos_SOC_4, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_cir, base1$pos_IDEAL_4, alternative = "two.sided", paired = TRUE) 
t.test(base1$pos_SOC_4, base1$pos_IDEAL_4, alternative = "two.sided", paired = TRUE) 



# EXTENSION ########################################################################
# Medico 1 
t.test(base1$ext_cir, base1$ext_SOC_1, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_cir, base1$ext_IDEAL_1, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_SOC_1, base1$ext_IDEAL_1, alternative = "two.sided", paired = TRUE) 

# Medico 2 
t.test(base1$ext_cir, base1$ext_SOC_2, alternative = "two.sided", paired = TRUE)
t.test(base1$ext_cir, base1$ext_IDEAL_2, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_SOC_2, base1$ext_IDEAL_2, alternative = "two.sided", paired = TRUE) 

# Medico 3 
t.test(base1$ext_cir, base1$ext_SOC_3, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_cir, base1$ext_IDEAL_3, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_SOC_3, base1$ext_IDEAL_3, alternative = "two.sided", paired = TRUE) 

# Medico 4 
t.test(base1$ext_cir, base1$ext_SOC_4, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_cir, base1$ext_IDEAL_4, alternative = "two.sided", paired = TRUE) 
t.test(base1$ext_SOC_4, base1$ext_IDEAL_4, alternative = "two.sided", paired = TRUE) 


