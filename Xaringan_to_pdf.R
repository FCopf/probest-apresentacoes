#install.packages(c("pagedown", "xaringan"))
# make sure you have pagedown >= 0.2 and xaringan >= 0.9; if not, run
# remotes::install_github(c('rstudio/pagedown', 'yihui/xaringan'))

pagedown::chrome_print(input = "Teste_t.Rmd", 
                       output = "pdf_presentations/Teste_t.pdf")

pagedown::chrome_print(input = "Correlacao_Regressao_Linear.Rmd", 
                       output = "pdf_presentations/Correlacao_Regressao_Linear.pdf")

pagedown::chrome_print(input = "Tabulacao_tipo_dados.Rmd", 
                       output = "pdf_presentations/Tabulacao_tipo_dados.pdf")

pagedown::chrome_print(input = "Covariancia_Correlacao_Linear.Rmd", 
                       output = "pdf_presentations/Covariancia_Correlacao_Linear.pdf")



# or just pass the HTML output file path to chrome_print()
#pagedown::chrome_print("Teste_Hipoteses.html")
