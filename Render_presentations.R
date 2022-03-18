#------------------------------------------
# rmarkdown::render para html
rmarkdown::render(input = "Tabulacao_tipo_dados.Rmd")

rmarkdown::render(input = "Covariancia_Correlacao_Linear.Rmd")

rmarkdown::render(input = "Descreve_Populacoes_Amostras.Rmd")

rmarkdown::render(input = "Amostra_Populacao.Rmd")

rmarkdown::render(input = "Distribuicao_normal.Rmd")

rmarkdown::render(input = "Distribuicao_medias_tcl.Rmd")

rmarkdown::render(input = "Estimacao.Rmd")

rmarkdown::render(input = "Teste_Hipoteses.Rmd")

rmarkdown::render(input = "Teste_t.Rmd")

rmarkdown::render(input = "Anova.Rmd")

rmarkdown::render(input = "Regressao_Linear_Simples.Rmd")

#------------------------------------------
# rmarkdown::render para pdf
#install.packages(c("pagedown", "xaringan"))
# make sure you have pagedown >= 0.2 and xaringan >= 0.9; if not, run
# remotes::install_github(c('rstudio/pagedown', 'yihui/xaringan'))

pagedown::chrome_print(input = "Tabulacao_tipo_dados.Rmd", 
                       output = "pdf_presentations/Tabulacao_tipo_dados.pdf")

pagedown::chrome_print(input = "Covariancia_Correlacao_Linear.Rmd", 
                       output = "pdf_presentations/Covariancia_Correlacao_Linear.pdf")

pagedown::chrome_print(input = "Descreve_Populacoes_Amostras.Rmd", 
                       output = "pdf_presentations/Descreve_Populacoes_Amostras.pdf")

pagedown::chrome_print(input = "Amostra_Populacao.Rmd", 
                       output = "pdf_presentations/Amostra_Populacao.pdf")

pagedown::chrome_print(input = "Distribuicao_normal.Rmd", 
                       output = "pdf_presentations/Distribuicao_normal.pdf")

pagedown::chrome_print(input = "Distribuicao_medias_tcl.Rmd",
                       output = "pdf_presentations/Distribuicao_medias_tcl.pdf")

pagedown::chrome_print(input = "Estimacao.Rmd",
                       output = "pdf_presentations/Estimacao.pdf")

pagedown::chrome_print(input = "Teste_Hipoteses.Rmd", 
                       output = "pdf_presentations/Teste_Hipoteses.pdf")

pagedown::chrome_print(input = "Teste_t.Rmd", 
                       output = "pdf_presentations/Teste_t.pdf")

pagedown::chrome_print(input = "Anova.Rmd", 
                       output = "pdf_presentations/Anova.pdf")

pagedown::chrome_print(input = "Regressao_Linear_Simples.Rmd", 
                       output = "pdf_presentations/Regressao_Linear_Simples.pdf")

# or just pass the HTML output file path to chrome_print()
#pagedown::chrome_print("Teste_Hipoteses.html")
#------------------------------------------