# Objectivo :
# Ler os dados sobre distribuição de renda global encontrados em:
# https://www.credit-suisse.com/about-us/en/reports-research/global-wealth-report.html
# Calcular, para os anos de 2010 a 2021, o indice de desigualdade de renda de Theil
# utilizando os dados do arquivo Global_Wealth.csv

pacman::p_load("dplyr", "ggplot2")


data <- read.csv("Global_Wealth.csv", check.names = FALSE)

anos <- 2010:2021
n_anos <- length(anos)

Theil <- numeric(n_anos)

for (i in 1:n_anos) {
    ano <- anos[i]
    Ri <- unlist(filter(data, `Ano` == ano & `Renda/Adultos` == "Renda")[3:6])
    Ni <- unlist(filter(data, `Ano` == ano & `Renda/Adultos` == "Adultos")[3:6])

    R <- sum(Ri)
    N <- sum(Ni)

    Theil[i] <- round(log2(N) - sum(Ri/R * log2(Ri/Ni/R)), 3)
}
hist_theil <- data.frame(
    "Anos" = anos,
    "Theil" = Theil
)
ggplot(hist_theil) +
    geom_line(aes(x = `Anos`, y = `Theil`), size = 1.4) +
    geom_point(aes(x = `Anos`, y = `Theil`), size = 3) +
    scale_x_continuous(breaks = anos) +
    ylab("Indíce de Desigualdade de Renda de Theil") +
    theme_bw()
