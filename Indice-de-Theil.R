# Objectivo :
# Ler os dados sobre distribuição de renda global encontrados em:
# https://www.credit-suisse.com/about-us/en/reports-research/global-wealth-report.html
# Calcular, para os anos de 2010 a 2021, o indice de desigualdade de renda de Theil
# utilizando os dados do arquivo Global_Wealth.csv

pacman::p_load("dplyr")


data <- read.csv("Global_Wealth.csv", check.names = FALSE)

anos <- 2010:2021
n_anos <- length(anos)

Theil <- numeric(n_anos)

for (i in 1:n_anos) {
    ano <- anos[i]
    Ri <- unlist(filter(data, `Ano/Categoria` == paste(ano, "renda", sep = "-"))[2:5])
    Ni <- unlist(filter(data, `Ano/Categoria` == paste(ano, "adultos", sep = "-"))[2:5])

    R <- sum(rendas)
    N <- sum(adultos)

    Theil[i] <- round(log2(N) - sum(Ri/R * log2(Ri/Ni/R)), 3)
}










