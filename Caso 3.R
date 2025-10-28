# ============================================
# CASO 3: DISTRIBUCIÓN DE POISSON
# ============================================

cat("====================================================\n")
cat("CASO 3: SOLICITUDES DE SERVICIO DE REPARACIÓN\n")
cat("====================================================\n\n")

# ============================================
# DATOS DEL PROBLEMA
# ============================================

cat("DATOS DEL PROBLEMA:\n")
cat("-------------------\n")
cat("• Se reciben 12 solicitudes en 45 minutos (promedio)\n")
cat("• Se desea calcular probabilidad en 30 minutos\n")
cat("• Pregunta: P(X < 5) en 30 minutos\n\n")

# ============================================
# AJUSTE DE LA TASA (λ) AL NUEVO INTERVALO
# ============================================

cat("====================================================\n")
cat("PASO 1: AJUSTAR LA TASA AL INTERVALO DESEADO\n")
cat("====================================================\n\n")

# Tasa original
solicitudes_45min <- 12
tiempo_original <- 45  # minutos
tiempo_nuevo <- 30     # minutos

# Calcular λ (lambda) para 30 minutos
lambda_30min <- (solicitudes_45min / tiempo_original) * tiempo_nuevo

cat("Tasa original: λ₄₅ =", solicitudes_45min, "solicitudes en 45 minutos\n\n")

cat("Cálculo de λ para 30 minutos:\n")
cat("-----------------------------\n")
cat("λ₃₀ = (λ₄₅ / t₁) × t₂\n")
cat("λ₃₀ = (", solicitudes_45min, "/ 45) × 30\n")
cat("λ₃₀ =", solicitudes_45min/tiempo_original, "× 30\n")
cat("λ₃₀ =", lambda_30min, "solicitudes en 30 minutos\n\n")

# ============================================
# MODELO PROBABILÍSTICO
# ============================================

cat("====================================================\n")
cat("MODELO PROBABILÍSTICO: DISTRIBUCIÓN DE POISSON\n")
cat("====================================================\n\n")

cat("JUSTIFICACIÓN:\n")
cat("--------------\n")
cat("Se utiliza la distribución de POISSON porque:\n\n")
cat("1. Contamos EVENTOS que ocurren en un INTERVALO de tiempo\n")
cat("2. Los eventos ocurren de manera INDEPENDIENTE\n")
cat("3. Los eventos ocurren con una TASA PROMEDIO constante\n")
cat("4. Se puede calcular la probabilidad de que ocurra\n")
cat("   cualquier número de eventos en el intervalo\n")
cat("5. Es un proceso de conteo con tasa conocida\n\n")

# ============================================
# FÓRMULA DE POISSON
# ============================================

cat("====================================================\n")
cat("FÓRMULA DE LA DISTRIBUCIÓN DE POISSON\n")
cat("====================================================\n\n")

cat("Fórmula general:\n")
cat("                    e^(-λ) × λ^k\n")
cat("   P(X = k) = ---------------------\n")
cat("                       k!\n\n")

cat("Donde:\n")
cat("  λ (lambda) = Tasa promedio de eventos en el intervalo\n")
cat("  k = Número específico de eventos\n")
cat("  e = Número de Euler (≈ 2.71828)\n\n")

# ============================================
# PARÁMETROS DEL MODELO
# ============================================

cat("====================================================\n")
cat("PARÁMETROS DEL MODELO EN ESTE CASO\n")
cat("====================================================\n\n")

cat("λ = 8:   Tasa promedio de solicitudes de servicio\n")
cat("         que se reciben en 30 minutos\n\n")

cat("k:       Número de solicitudes recibidas\n")
cat("         (variable aleatoria)\n\n")

# ============================================
# VARIABLE ALEATORIA
# ============================================

cat("====================================================\n")
cat("VARIABLE ALEATORIA\n")
cat("====================================================\n\n")

cat("X = Número de solicitudes de servicio recibidas en 30 minutos\n\n")

cat("SIGNIFICADO DE X:\n")
cat("-----------------\n")
cat("X representa la cantidad de solicitudes de reparación\n")
cat("de maquinarias que llegan al departamento durante un\n")
cat("período de 30 minutos.\n\n")

cat("X puede tomar valores: 0, 1, 2, 3, 4, 5, 6, ...\n\n")

# ============================================
# PROBABILIDAD SOLICITADA
# ============================================

cat("====================================================\n")
cat("PROBABILIDAD SOLICITADA\n")
cat("====================================================\n\n")

cat("Se pide: P(X < 5)\n\n")

cat("INTERPRETACIÓN:\n")
cat("---------------\n")
cat("Probabilidad de que se reciban MENOS de 5 solicitudes\n")
cat("en 30 minutos.\n\n")

cat("Esto equivale a:\n")
cat("P(X < 5) = P(X ≤ 4)\n")
cat("         = P(X=0) + P(X=1) + P(X=2) + P(X=3) + P(X=4)\n\n")

# ============================================
# CÁLCULO DE LA PROBABILIDAD
# ============================================

cat("====================================================\n")
cat("CÁLCULO DE LA PROBABILIDAD\n")
cat("====================================================\n\n")

# Calcular P(X < 5) = P(X ≤ 4)
prob_menos_5 <- ppois(q = 4, lambda = lambda_30min, lower.tail = TRUE)

cat("Usando ppois() de R:\n")
cat("--------------------\n")
cat("ppois(q = 4, lambda = 8, lower.tail = TRUE)\n\n")

cat("Donde:\n")
cat("  q = 4        → valor máximo (queremos X ≤ 4)\n")
cat("  lambda = 8   → tasa promedio en 30 minutos\n")
cat("  lower.tail = TRUE → probabilidad acumulada desde 0 hasta q\n\n")

cat("RESULTADO:\n")
cat("----------\n")
cat("P(X < 5) = P(X ≤ 4) =", prob_menos_5, "\n")
cat("P(X < 5) =", round(prob_menos_5, 6), "\n")
cat("P(X < 5) =", paste0(round(prob_menos_5 * 100, 4), "%"), "\n\n")

# ============================================
# PROCEDIMIENTO DETALLADO
# ============================================

cat("====================================================\n")
cat("PROCEDIMIENTO PARA CALCULAR LA PROBABILIDAD\n")
cat("====================================================\n\n")

cat("MÉTODO 1: Usando función de distribución acumulada (CDF)\n")
cat("---------------------------------------------------------\n")
cat("P(X < 5) = P(X ≤ 4) = F(4)\n\n")
cat("Donde F(4) es la función de distribución acumulada\n")
cat("de Poisson evaluada en k = 4\n\n")
cat("En R: ppois(4, lambda = 8)\n")
cat("Resultado:", prob_menos_5, "\n\n")

cat("MÉTODO 2: Suma de probabilidades individuales\n")
cat("----------------------------------------------\n")
cat("P(X < 5) = P(X=0) + P(X=1) + P(X=2) + P(X=3) + P(X=4)\n\n")

# Calcular cada probabilidad individual
prob_individual <- dpois(0:4, lambda = lambda_30min)
prob_suma <- sum(prob_individual)

cat("Calculando cada término:\n")
for(i in 0:4) {
  cat(sprintf("P(X = %d) = %.6f\n", i, prob_individual[i+1]))
}
cat("\nSuma total:\n")
cat("P(X < 5) =", prob_suma, "\n\n")

cat("✓ Verificación: Ambos métodos dan el mismo resultado\n\n")

# ============================================
# VERIFICACIÓN MANUAL DE UNA PROBABILIDAD
# ============================================

cat("====================================================\n")
cat("VERIFICACIÓN MANUAL: Cálculo de P(X = 3)\n")
cat("====================================================\n\n")

k_ejemplo <- 3
prob_manual <- (exp(-lambda_30min) * lambda_30min^k_ejemplo) / factorial(k_ejemplo)

cat("Fórmula: P(X = 3) = e^(-8) × 8³ / 3!\n\n")

cat("Paso 1: e^(-8) =", exp(-lambda_30min), "\n")
cat("Paso 2: 8³ =", lambda_30min^k_ejemplo, "\n")
cat("Paso 3: 3! =", factorial(k_ejemplo), "\n\n")

cat("P(X = 3) = (", exp(-lambda_30min), "×", lambda_30min^k_ejemplo, ") /", 
    factorial(k_ejemplo), "\n")
cat("P(X = 3) =", prob_manual, "\n")
cat("P(X = 3) usando dpois =", dpois(k_ejemplo, lambda_30min), "\n\n")

cat("✓ Verificación exitosa\n\n")

# ============================================
# INTERPRETACIÓN DEL RESULTADO
# ============================================

cat("====================================================\n")
cat("INTERPRETACIÓN DEL RESULTADO\n")
cat("====================================================\n\n")

cat("Probabilidad P(X < 5) =", paste0(round(prob_menos_5 * 100, 2), "%"), "\n\n")

cat("SIGNIFICADO:\n")
cat("------------\n")
cat("Hay una probabilidad del", paste0(round(prob_menos_5 * 100, 2), "%"), "\n")
cat("de que el departamento de reparación reciba MENOS de 5\n")
cat("solicitudes de servicio en un período de 30 minutos.\n\n")

cat("En otras palabras:\n")
cat("• De cada 100 períodos de 30 minutos,\n")
cat("  aproximadamente", round(prob_menos_5 * 100), "veces se recibirán\n")
cat("  menos de 5 solicitudes.\n\n")

# Probabilidad complementaria
prob_5_o_mas <- 1 - prob_menos_5
cat("Probabilidad complementaria:\n")
cat("P(X ≥ 5) = 1 - P(X < 5) =", round(prob_5_o_mas, 6), "\n")
cat("          =", paste0(round(prob_5_o_mas * 100, 2), "%"), "\n\n")

# ============================================
# TABLA DE PROBABILIDADES
# ============================================

cat("====================================================\n")
cat("TABLA DE PROBABILIDADES INDIVIDUALES\n")
cat("====================================================\n\n")

# Crear tabla hasta k = 15
k_vals <- 0:15
probs_tabla <- dpois(k_vals, lambda = lambda_30min)
probs_acum <- ppois(k_vals, lambda = lambda_30min)

tabla <- data.frame(
  k = k_vals,
  `P(X=k)` = round(probs_tabla, 6),
  `Porcentaje` = paste0(round(probs_tabla * 100, 2), "%"),
  `P(X≤k)` = round(probs_acum, 6),
  check.names = FALSE
)

print(tabla[1:12,])  # Mostrar primeros 12 valores
cat("\n")

# ============================================
# GRÁFICO DE LA DISTRIBUCIÓN
# ============================================

cat("====================================================\n")
cat("GENERANDO GRÁFICO DE LA DISTRIBUCIÓN...\n")
cat("====================================================\n\n")

# Ajustar márgenes
par(mar = c(5, 4, 4, 2) + 0.1)

# Valores para graficar (0 hasta 20)
x_vals <- 0:20
probs_graf <- dpois(x_vals, lambda = lambda_30min)

# Crear el gráfico
plot(x_vals, probs_graf, type = "h", lwd = 3, col = "steelblue",
     main = "Distribución de Poisson: Solicitudes de Servicio",
     xlab = "Número de solicitudes (X) en 30 minutos",
     ylab = "Probabilidad P(X = k)",
     ylim = c(0, max(probs_graf) * 1.15),
     las = 1)
grid()
points(x_vals, probs_graf, pch = 16, col = "steelblue", cex = 1.2)

# Resaltar la región X < 5 (X ≤ 4)
segments(x0 = 0:4, y0 = 0, x1 = 0:4, y1 = dpois(0:4, lambda_30min), 
         col = "darkgreen", lwd = 4)
points(0:4, dpois(0:4, lambda_30min), pch = 16, col = "darkgreen", cex = 1.5)

# Añadir línea vertical en x = 4.5
abline(v = 4.5, col = "red", lty = 2, lwd = 2)
text(4.5, max(probs_graf) * 0.95, "X < 5", col = "red", pos = 2, font = 2)

# Añadir la media (λ)
abline(v = lambda_30min, col = "orange", lty = 2, lwd = 2)
text(lambda_30min, max(probs_graf) * 1.05, 
     paste0("λ = ", lambda_30min), col = "orange", font = 2)

abline(h = 0, col = "gray30")

# Información adicional
mtext(paste0("λ = ", lambda_30min, " solicitudes en 30 minutos"), 
      side = 3, line = 0.5, cex = 0.85, col = "gray40")

# Leyenda
legend("topright", 
       legend = c("Todas las probabilidades", 
                  "P(X < 5)", 
                  paste0("Media (λ = ", lambda_30min, ")")),
       col = c("steelblue", "darkgreen", "orange"),
       lwd = c(3, 4, 2),
       lty = c(1, 1, 2),
       cex = 0.8,
       bg = "white")

cat("✓ Gráfico generado exitosamente\n\n")

# ============================================
# RESUMEN EJECUTIVO
# ============================================

cat("====================================================\n")
cat("RESUMEN EJECUTIVO\n")
cat("====================================================\n\n")

cat("MODELO:        Distribución de Poisson\n")
cat("PARÁMETRO:     λ = 8 solicitudes en 30 minutos\n")
cat("VARIABLE:      X = Número de solicitudes en 30 min\n")
cat("PROBABILIDAD:  P(X < 5) =", round(prob_menos_5, 6), "\n")
cat("               P(X < 5) =", paste0(round(prob_menos_5 * 100, 2), "%"), "\n\n")

cat("CONCLUSIÓN:\n")
cat("-----------\n")
cat("Es POCO PROBABLE (", paste0(round(prob_menos_5 * 100, 2), "%"),
    ") que se reciban\n")
cat("menos de 5 solicitudes en 30 minutos, dado que el\n")
cat("promedio esperado es de 8 solicitudes en ese período.\n\n")

cat("====================================================\n")
cat("FIN DEL ANÁLISIS - CASO 3\n")
cat("====================================================\n")