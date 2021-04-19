# R_code_knitr.r

# inseriamo i dati in R dalla cartella lab 
setwd("C:/lab/")

library(knitr)

# generare report partendo da script .r
stitch("R_code_time_series.r", template=system.file("misc", "knitr-template.Rnw", package="knitr"))
