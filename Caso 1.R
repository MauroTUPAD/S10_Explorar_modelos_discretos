# Parámetros del problema
n <- 15  # número de ensayos
p <- 0.25  # probabilidad de éxito
k <- 6  # número de éxitos deseados

# ============================================
# PREGUNTA 1: P(X = 6)
# ============================================
prob_exactamente_6 <- dbinom(x = k, size = n, prob = p)

cat("============================================\n")
cat("PREGUNTA 1: Probabilidad de exactamente 6 respuestas\n")
cat("============================================\n")
cat("P(X = 6) =", prob_exactamente_6, "\n")
cat("P(X = 6) =", round(prob_exactamente_6, 6), "\n")
cat("P(X = 6) =", paste0(round(prob_exactamente_6 * 100, 4), "%"), "\n\n")

# ============================================
# PREGUNTA 2: P(X ≤ 4)
# ============================================
prob_maximo_4 <- pbinom(q = 4, size = n, prob = p, lower.tail = TRUE)

cat("============================================\n")
cat("PREGUNTA 2: Probabilidad de como máximo 4 respuestas\n")
cat("============================================\n")
cat("P(X ≤ 4) =", prob_maximo_4, "\n")
cat("P(X ≤ 4) =", round(prob_maximo_4, 6), "\n")
cat("P(X ≤ 4) =", paste0(round(prob_maximo_4 * 100, 4), "%"), "\n\n")

# ============================================
# VERIFICACIÓN MANUAL (OPCIONAL)
# ============================================
cat("============================================\n")
cat("VERIFICACIÓN MANUAL\n")
cat("============================================\n")

# Primero calculamos el coeficiente binomial
coef_binomial <- choose(n, k)
cat("Coeficiente binomial C(15,6) =", coef_binomial, "\n")

# Luego calculamos la probabilidad manualmente
prob_manual <- coef_binomial * (p^k) * ((1-p)^(n-k))
cat("P(X = 6) calculado manualmente =", prob_manual, "\n\n")

# Cálculo manual de P(X ≤ 4) sumando probabilidades individuales
prob_manual_suma <- sum(dbinom(0:4, size = n, prob = p))
cat("P(X ≤ 4) = P(X=0) + P(X=1) + P(X=2) + P(X=3) + P(X=4)\n")
cat("P(X ≤ 4) calculado manualmente =", prob_manual_suma, "\n\n")

# ============================================
# GRÁFICO DE LA DISTRIBUCIÓN
# ============================================
x_vals <- 0:n
probs <- dbinom(x_vals, size = n, prob = p)

# Crear el gráfico
plot(x_vals, probs, type = "h", lwd = 3, col = "steelblue",
     main = "Distribución Binomial: Respuestas a la Circular",
     xlab = "Número de personas que responden (X)",
     ylab = "Probabilidad P(X = k)",
     ylim = c(0, max(probs) * 1.1))

# Agregar grid DESPUÉS de crear el plot
grid()

# Agregar puntos
points(x_vals, probs, pch = 16, col = "steelblue", cex = 1.2)

# Resaltar P(X = 6)
points(6, dbinom(6, n, p), pch = 16, col = "red", cex = 2)
text(6, dbinom(6, n, p) + 0.01, "X = 6", col = "red", font = 2)

# Resaltar región X ≤ 4
segments(x0 = 0:4, y0 = 0, x1 = 0:4, y1 = dbinom(0:4, n, p), 
         col = "darkgreen", lwd = 3)
text(2, max(probs) * 0.9, "X ≤ 4", col = "darkgreen", font = 2)

# Agregar línea horizontal
abline(h = 0, col = "gray")
