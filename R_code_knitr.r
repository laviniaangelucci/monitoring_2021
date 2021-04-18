# R_code_knitr.r

# inseriamo i dati in R dalla cartella lab 
setwd("C:/lab/")

library(knitr)

# 
stitch("R_code_greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))
