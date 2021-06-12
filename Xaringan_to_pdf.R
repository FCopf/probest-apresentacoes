#install.packages(c("pagedown", "xaringan"))
# make sure you have pagedown >= 0.2 and xaringan >= 0.9; if not, run
# remotes::install_github(c('rstudio/pagedown', 'yihui/xaringan'))

pagedown::chrome_print(input = "Teste_t.Rmd", 
                       output = "pdf_presentations/Teste_t.pdf")

# or just pass the HTML output file path to chrome_print()
#pagedown::chrome_print("Teste_Hipoteses.html")
