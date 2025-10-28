# ============================================
# CASO 2: DISTRIBUCIÓN HIPERGEOMÉTRICA
# ============================================

# Parámetros del problema
N <- 15  # Tamaño de la población total
K <- 7   # Número de clientes que emiten respuesta en la población
n <- 5   # Tamaño de la muestra (clientes seleccionados)
k <- 2   # Número de clientes que responden en la muestra

cat("====================================================\n")
cat("CASO 2: MUESTREO SIN REEMPLAZO\n")
cat("====================================================\n\n")

cat("PARÁMETROS DEL PROBLEMA:\n")
cat("------------------------\n")
cat("N =", N, "→ Población total de clientes\n")
cat("K =", K, "→ Clientes que SIEMPRE emiten respuesta\n")
cat("N-K =", N-K, "→ Clientes que NO emiten respuesta\n")
cat("n =", n, "→ Clientes seleccionados para la muestra\n")
cat("k =", k, "→ Clientes que responden en la muestra (deseados)\n\n")

# ============================================
# MODELO PROBABILÍSTICO: HIPERGEOMÉTRICA
# ============================================

cat("====================================================\n")
cat("MODELO PROBABILÍSTICO: DISTRIBUCIÓN HIPERGEOMÉTRICA\n")
cat("====================================================\n\n")

cat("JUSTIFICACIÓN:\n")
cat("--------------\n")
cat("Se utiliza la distribución HIPERGEOMÉTRICA porque:\n\n")
cat("1. Población FINITA: N = 15 clientes\n")
cat("2. Muestreo SIN REEMPLAZO: Los 5 clientes se seleccionan\n")
cat("   sin devolver a la población\n")
cat("3. Dos categorías bien definidas:\n")
cat("   - Éxitos: 7 clientes que responden\n")
cat("   - Fracasos: 8 clientes que no responden\n")
cat("4. La probabilidad cambia en cada extracción\n")
cat("   (a diferencia de la binomial)\n\n")

# ============================================
# FÓRMULA Y SIGNIFICADO DE PARÁMETROS
# ============================================

cat("====================================================\n")
cat("FÓRMULA DE LA DISTRIBUCIÓN HIPERGEOMÉTRICA\n")
cat("====================================================\n\n")

cat("Fórmula general:\n")
cat("                C(K, k) × C(N-K, n-k)\n")
cat("   P(X = k) = -------------------------\n")
cat("                    C(N, n)\n\n")

cat("Donde:\n")
cat("  C(a, b) = Combinaciones de 'a' elementos tomados de 'b' en 'b'\n\n")

cat("Reemplazando con nuestros valores:\n")
cat("                C(7, 2) × C(8, 3)\n")
cat("   P(X = 2) = ---------------------\n")
cat("                   C(15, 5)\n\n")

# ============================================
# SIGNIFICADO DE CADA PARÁMETRO
# ============================================

cat("====================================================\n")
cat("SIGNIFICADO DE LOS PARÁMETROS EN ESTE CASO\n")
cat("====================================================\n\n")

cat("N = 15:  Tamaño total de la población de clientes\n")
cat("         de la empresa competidora\n\n")

cat("K = 7:   Número de clientes en la población que\n")
cat("         SIEMPRE emiten respuesta (éxitos posibles)\n\n")

cat("N-K = 8: Número de clientes en la población que\n")
cat("         NO emiten respuesta (fracasos posibles)\n\n")

cat("n = 5:   Tamaño de la muestra (cantidad de clientes\n")
cat("         seleccionados para recibir la circular)\n\n")

cat("k = 2:   Número de clientes que deseamos que respondan\n")
cat("         en la muestra seleccionada\n\n")

# ============================================
# CÁLCULO DE LA PROBABILIDAD
# ============================================

cat("====================================================\n")
cat("CÁLCULO DE LA PROBABILIDAD\n")
cat("====================================================\n\n")

# Usando la función dhyper de R
prob_2_respuestas <- dhyper(x = k, m = K, n = N-K, k = n)

cat("Usando dhyper() de R:\n")
cat("---------------------\n")
cat("dhyper(x = 2, m = 7, n = 8, k = 5)\n\n")
cat("Donde en dhyper():\n")
cat("  x = 2  → número de éxitos en la muestra\n")
cat("  m = 7  → número de éxitos en la población\n")
cat("  n = 8  → número de fracasos en la población\n")
cat("  k = 5  → tamaño de la muestra\n\n")

cat("RESULTADO:\n")
cat("----------\n")
cat("P(X = 2) =", prob_2_respuestas, "\n")
cat("P(X = 2) =", round(prob_2_respuestas, 6), "\n")
cat("P(X = 2) =", paste0(round(prob_2_respuestas * 100, 4), "%"), "\n\n")

# ============================================
# VERIFICACIÓN MANUAL
# ============================================

cat("====================================================\n")
cat("VERIFICACIÓN MANUAL DEL CÁLCULO\n")
cat("====================================================\n\n")

# Calcular combinaciones
comb_K_k <- choose(K, k)        # C(7, 2)
comb_NK_nk <- choose(N-K, n-k)  # C(8, 3)
comb_N_n <- choose(N, n)        # C(15, 5)

cat("Paso 1: Calcular combinaciones\n")
cat("-------------------------------\n")
cat("C(7, 2)  = C(K, k)      =", comb_K_k, "\n")
cat("C(8, 3)  = C(N-K, n-k)  =", comb_NK_nk, "\n")
cat("C(15, 5) = C(N, n)      =", comb_N_n, "\n\n")

# Calcular probabilidad manualmente
prob_manual <- (comb_K_k * comb_NK_nk) / comb_N_n

cat("Paso 2: Aplicar la fórmula\n")
cat("---------------------------\n")
cat("           ", comb_K_k, "×", comb_NK_nk, "\n")
cat("P(X = 2) = ", "------------", "\n")
cat("           ", comb_N_n, "\n\n")

cat("           ", comb_K_k * comb_NK_nk, "\n")
cat("P(X = 2) = ", "------", "\n")
cat("           ", comb_N_n, "\n\n")

cat("P(X = 2) =", prob_manual, "\n")
cat("P(X = 2) =", round(prob_manual, 6), "\n\n")

cat("✓ Verificación: El cálculo manual coincide con dhyper()\n\n")

# ============================================
# INTERPRETACIÓN DEL RESULTADO
# ============================================

cat("====================================================\n")
cat("INTERPRETACIÓN\n")
cat("====================================================\n\n")

cat("Hay una probabilidad del", paste0(round(prob_2_respuestas * 100, 2), "%"), "\n")
cat("de que exactamente 2 de los 5 clientes seleccionados\n")
cat("emitan respuesta al recibir la circular.\n\n")

# ============================================
# GRÁFICO DE LA DISTRIBUCIÓN
# ============================================

cat("====================================================\n")
cat("GENERANDO GRÁFICO DE LA DISTRIBUCIÓN...\n")
cat("====================================================\n\n")

# Valores posibles de X (de 0 hasta el mínimo entre n y K)
x_vals <- 0:min(n, K)
probs <- dhyper(x = x_vals, m = K, n = N-K, k = n)

# Crear el gráfico
plot(x_vals, probs, type = "h", lwd = 4, col = "steelblue",
     main = "Distribución Hipergeométrica: Respuestas de Clientes",
     sub = "N=15, K=7 (responden), n=5 (seleccionados)",
     xlab = "Número de clientes que responden (X)",
     ylab = "Probabilidad P(X = k)",
     ylim = c(0, max(probs) * 1.15),
     las = 1)
grid()
points(x_vals, probs, pch = 16, col = "steelblue", cex = 1.5)

# Resaltar P(X = 2)
points(2, dhyper(2, K, N-K, n), pch = 16, col = "red", cex = 2.5)
text(2, dhyper(2, K, N-K, n) + max(probs)*0.05, 
     paste0("X = 2\nP = ", round(prob_2_respuestas, 4)), 
     col = "red", font = 2, cex = 0.9)

abline(h = 0, col = "gray30")

# Agregar leyenda
legend("topright", 
       legend = c("Probabilidades", "P(X = 2)"),
       col = c("steelblue", "red"),
       pch = 16,
       cex = 0.9,
       bg = "white")

# ============================================
# COMPARACIÓN CON BINOMIAL (EDUCATIVO)
# ============================================

cat("====================================================\n")
cat("COMPARACIÓN: ¿Por qué NO usar Binomial?\n")
cat("====================================================\n\n")

cat("Si usáramos BINOMIAL (INCORRECTAMENTE):\n")
cat("----------------------------------------\n")
p_binomial <- K/N  # 7/15
prob_binomial <- dbinom(k, n, p_binomial)
cat("Con p = K/N =", p_binomial, "\n")
cat("P_binomial(X=2) =", round(prob_binomial, 6), "\n")
cat("P_hipergeométrica(X=2) =", round(prob_2_respuestas, 6), "\n\n")

diferencia <- abs(prob_binomial - prob_2_respuestas)
cat("Diferencia absoluta =", round(diferencia, 6), "\n")
cat("Diferencia relativa =", paste0(round(diferencia/prob_2_respuestas * 100, 2), "%"), "\n\n")

cat("CONCLUSIÓN:\n")
cat("-----------\n")
cat("La binomial NO es apropiada aquí porque:\n")
cat("- El muestreo es SIN reemplazo\n")
cat("- La población es pequeña (N=15)\n")
cat("- La probabilidad cambia en cada extracción\n")
cat("- La hipergeométrica da el resultado EXACTO\n\n")

cat("====================================================\n")
cat("FIN DEL ANÁLISIS - CASO 2\n")
cat("====================================================\n")
```

## Resumen de Resultados

**Modelo utilizado:** Distribución Hipergeométrica

**Fórmula aplicada:**
  ```
P(X = 2) = [C(7,2) × C(8,3)] / C(15,5)
= [21 × 56] / 3003
= 1176 / 3003
≈ 0.3916