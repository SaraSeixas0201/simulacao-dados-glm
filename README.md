# simulacao-dados-glm

# Definir semente para reprodutibilidade
set.seed(42)

# Tamanho do conjunto de dados
n <- 1000

# Criar variáveis preditoras com multicolinearidade
x1 <- rnorm(n, mean = 10, sd = 2)  # Variável quantitativa
x2 <- 0.85 * x1 + rnorm(n, mean = 0, sd = 1)  # Alta correlação com x1
x3 <- rnorm(n, mean = 5, sd = 1.5)  # Independente
x4 <- sample(c("A", "B", "C"), n, replace = TRUE)  # Variável categórica

# Criar variáveis de ruído (sem associação com y)
ruido1 <- rnorm(n, mean = 0, sd = 1)
ruido2 <- sample(c("X", "Y", "Z"), n, replace = TRUE)

# Criar a variável resposta (dados de contagem - distribuição binomial negativa)
theta <- 2  # Parâmetro de dispersão (menor valor = mais dispersão)
mu <- exp(0.5 * x1 - 0.3 * x2 + 0.7 * (x4 == "B"))  # Média esperada
y <- rnbinom(n, size = theta, mu = mu)  # Variável resposta Binomial Negativa

# Introduzir valores ausentes (10% de missing values em várias colunas)
percent_missing <- 0.1
for (col in c("x1", "x2", "x3")) {
  missing_indices <- sample(1:n, size = round(n * percent_missing), replace = FALSE)
  y[missing_indices] <- NA  # Removendo y em alguns casos também
}

# Criar valores influentes (outliers em 5% dos dados)
num_outliers <- round(0.05 * n)
outlier_indices <- sample(1:n, num_outliers, replace = FALSE)
y[outlier_indices] <- y[outlier_indices] * 5  # Multiplicando para criar valores extremos

# Criar um dataframe final
df <- data.frame(x1, x2, x3, x4, y, ruido1, ruido2)

# Exibir as primeiras linhas do conjunto de dados
head(df)

# Verificar correlação para confirmar multicolinearidade
cor(df[, c("x1", "x2", "x3")], use = "pairwise.complete.obs")

# Criar modelo de regressão Binomial Negativa
df <- na.omit(df)  # Remover valores ausentes para análise
df$x4 <- as.factor(df$x4)

modelo_nb <- glm.nb(y ~ x1 + x2 + x3 + x4, data = df)
summary(modelo_nb)

# Visualizar distribuição da variável resposta
ggplot(df, aes(x = y)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Distribuição da variável resposta (Binomial Negativa)") +
  theme_minimal()

