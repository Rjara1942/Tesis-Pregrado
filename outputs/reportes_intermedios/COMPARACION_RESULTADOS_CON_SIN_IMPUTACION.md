# COMPARACIÓN DE RESULTADOS IAIDS: CON VS SIN IMPUTACIÓN

---

## 1. TAMAÑOS MUESTRALES

| Método | N observaciones | Pérdida vs completo |
|--------|-----------------|---------------------|
| Sin imputación | 90 | 37.5% pérdida |
| Con imputación (genérica) | 140 | 2.8% pérdida |
| Imputación Kalman | 144 | 0% pérdida |

**Hallazgo:** La imputación recupera entre 50-54 observaciones adicionales (56-60% más datos).

**Consideración:** El panel "completo" teórico sería 144 meses (12 años × 12 meses). Sin imputación se pierden 54 meses, concentrados presumiblemente en períodos de veda y baja actividad.

---

## 2. DIAGNÓSTICOS DE AJUSTE

### 2.1. Coeficientes de Determinación (R²)

| Ecuación | Sin Imputación | Con Imputación | Kalman |
|----------|----------------|----------------|--------|
| **Anchoveta** | **0.368** | 0.197 | 0.199 |
| Jurel | 0.104 | **0.133** | 0.115 |
| Sardina | **0.239** | 0.203 | 0.171 |

**Hallazgos:**

1. **Anchoveta pierde poder explicativo con imputación:**
   - R² cae de 0.368 → 0.197 (caída de 46%)
   - Imputación Kalman no mejora: 0.199

2. **Jurel MEJORA marginalmente con imputación:**
   - R² sube de 0.104 → 0.133 (mejora de 28%)
   - Kalman intermedio: 0.115

3. **Sardina también pierde ajuste:**
   - R² cae de 0.239 → 0.171-0.203 (caída 15-28%)

**Duda crítica:** ¿Por qué anchoveta tiene mejor ajuste SIN imputación pero jurel mejora CON imputación? Posible explicación: los meses imputados para jurel aportan información estructural (mercado dual), mientras que para anchoveta los valores imputados introducen ruido.

---

### 2.2. Error Cuadrático Medio (RMSE)

| Ecuación | Sin Imputación | Con Imputación |
|----------|----------------|----------------|
| Anchoveta | **0.130** | 0.169 |
| Jurel | 0.206 | **0.186** |
| Sardina | 0.161 | **0.153** |

**Hallazgo:** Sin imputación genera menor error para anchoveta, pero mayor error para jurel y sardina.

**Consideración:** El RMSE más bajo sin imputación para anchoveta puede ser artefacto de menor varianza en la muestra restringida (sesgo de selección hacia meses "normales").

---

## 3. MATRIZ DE FLEXIBILIDADES PRECIO-CANTIDAD

### 3.1. Flexibilidades Propias (Diagonal)

| Especie | Sin Imputación | Con Imputación | Kalman | Signo esperado |
|---------|----------------|----------------|--------|----------------|
| γ_A,A (Anchoveta) | **+0.0377** | +0.0143 | +0.0144 | **NEGATIVO** ✗ |
| γ_J,J (Jurel) | +0.0238 | **+0.0260** | +0.0233 | **NEGATIVO** ✗ |
| γ_S,S (Sardina) | **-0.0565** | -0.0278 | -0.0249 | **NEGATIVO** ✓ |

**Hallazgos críticos:**

1. **Solo sardina tiene signo correcto en TODAS las especificaciones**
   - Flexibilidad propia negativa: más sardina → menor precio sardina ✓
   - Magnitud mayor sin imputación (-0.057 vs -0.025)

2. **Anchoveta y jurel tienen signos POSITIVOS (viola teoría)**
   - Más cantidad → mayor precio (contraintuitivo)
   - El problema persiste con y sin imputación
   - Imputación REDUCE la magnitud del sesgo pero NO corrige el signo

3. **Imputación atenúa magnitudes en ~50-60%:**
   - γ_A,A: 0.038 → 0.014 (reducción 62%)
   - γ_J,J: 0.024 → 0.026 (sin cambio significativo)
   - γ_S,S: -0.057 → -0.028 (reducción 51%)

**Duda:** ¿La atenuación es corrección de sesgo o introducción de sesgo hacia cero por suavizado Kalman?

---

### 3.2. Flexibilidades Cruzadas (Fuera de diagonal)

| Par | Sin Imputación | Con Imputación | Kalman | Interpretación |
|-----|----------------|----------------|--------|----------------|
| γ_A,J (Anch←Jur) | +0.0005 | -0.0064 | -0.0067 | ¿Complementos? |
| γ_A,S (Anch←Sard) | -0.0343 | -0.0156 | -0.0158 | Sustitutos |
| γ_J,A (Jur←Anch) | -0.0342 | -0.0200 | -0.0193 | Sustitutos |
| γ_J,S (Jur←Sard) | +0.0114 | +0.0110 | +0.0101 | ¿Complementos? |
| γ_S,A (Sard←Anch) | +0.0543 | +0.0342 | +0.0293 | ¿Complementos? |
| γ_S,J (Sard←Jur) | -0.0146 | -0.0143 | -0.0151 | Sustitutos |

**Hallazgos:**

1. **Relación Anchoveta-Sardina:**
   - Sin imputación: γ_A,S = -0.034, γ_S,A = +0.054 (ASIMÉTRICO)
   - Con imputación: magnitudes menores pero misma asimetría

2. **Relación Jurel-Sardina:**
   - γ_J,S positivo (~0.01), γ_S,J negativo (~-0.015)
   - Interpretación ambigua: jurel y sardina ¿sustitutos o complementos?

3. **Imputación reduce TODAS las magnitudes cruzadas:**
   - Patrón consistente de atenuación 30-50%

**Consideración metodológica:** La asimetría persistente (γ_A,S ≠ γ_S,A) sugiere que la restricción de simetría NO debería imponerse. Esto es consistente con heterogeneidad de uso industrial entre especies.

---

## 4. ELASTICIDADES DE TRANSMISIÓN FOB

| Especie | Sin Imputación | Con Imputación | Kalman |
|---------|----------------|----------------|--------|
| Anchoveta | **0.578** | 0.555 | 0.525 |
| Jurel | **-0.051** | 0.026 | 0.043 |
| Sardina | 0.233 | **0.254** | 0.191 |

**Hallazgos críticos:**

1. **Anchoveta tiene transmisión FOB robusta:**
   - γ_FM ≈ 0.52-0.58 en todas las especificaciones
   - Interpretación: Si P_FOB ↑10% → P_anchoveta ↑5.2-5.8%
   - Consistente con integración imperfecta (transmisión parcial)

2. **Jurel tiene transmisión FOB NEGATIVA sin imputación:**
   - γ_FM = -0.051 (sin imputación) → ABSURDO económico
   - Con imputación: γ_FM = +0.026 (signo correcto pero casi cero)
   - Kalman: γ_FM = +0.043

   **Interpretación:** El mercado dual del jurel contamina la señal. Sin imputación, los meses disponibles están sesgados hacia períodos de alto consumo humano (menos vinculados a FOB). La imputación "corrige" parcialmente recuperando meses de mayor reducción.

3. **Sardina intermedia:**
   - γ_FM ≈ 0.19-0.25
   - Menor transmisión que anchoveta (consistente con mayor uso artesanal local)

---

## 5. COMPARACIÓN DE INTERCEPTOS

| Ecuación | Sin Imputación | Kalman | Diferencia |
|----------|----------------|--------|------------|
| Anchoveta | 3.82 | 4.68 | +0.86 |
| Jurel | 13.17 | 11.69 | -1.48 |
| Sardina | 9.03 | 9.53 | +0.50 |

**Hallazgo:** Los interceptos cambian sustancialmente entre métodos.

**Consideración:** En log-levels, diferencias de intercepto implican diferencias en nivel medio de precios. El cambio de -1.48 en jurel (log-scale) equivale a una diferencia de ~23% en el precio base predicho.

---

## 6. ANÁLISIS DE PATRONES

### 6.1. Efecto de Imputación por Especie

| Especie | Efecto en R² | Efecto en γ_ii | Efecto en γ_FM | Diagnóstico |
|---------|--------------|----------------|----------------|-------------|
| Anchoveta | Empeora (-46%) | Atenúa | Estable | Imputación perjudica |
| Jurel | Mejora (+28%) | Sin cambio | Corrige signo | Imputación ayuda |
| Sardina | Empeora (-28%) | Atenúa | Variable | Efecto mixto |

**Hallazgo clave:** La imputación tiene efectos HETEROGÉNEOS por especie. No es uniformemente beneficiosa ni perjudicial.

---

### 6.2. Consistencia Teórica por Método

| Criterio | Sin Imputación | Con Imputación | Kalman |
|----------|----------------|----------------|--------|
| γ_ii < 0 (flexibilidad propia negativa) | 1/3 ✓ | 1/3 ✓ | 1/3 ✓ |
| γ_FM > 0 (transmisión FOB positiva) | 2/3 ✓ | 3/3 ✓ | 3/3 ✓ |
| Consistencia global | 50% | 67% | 67% |

**Hallazgo:** La imputación mejora consistencia teórica de γ_FM (corrige signo jurel) pero NO corrige el problema de γ_ii positivos.

---

## 7. DUDAS Y CUESTIONES ABIERTAS

### 7.1. Sobre la Validez de los Resultados

1. **¿Por qué γ_anchoveta,anchoveta es POSITIVO en todas las especificaciones?**
   - Posibilidades: 
     - (a) Endogeneidad no resuelta (Q afecta P pero también P afecta Q)
     - (b) Variable omitida correlacionada con Q y P
     - (c) Problema de mercado dual extendiéndose a anchoveta

2. **¿Es la corrección de signo de γ_FM jurel real o artefacto?**
   - Sin imputación: -0.051 (negativo, absurdo)
   - Con imputación: +0.026 (positivo, plausible)
   - ¿La imputación está recuperando información o fabricándola?

3. **¿Qué meses se están imputando exactamente?**
   - Se recuperan 50-54 observaciones
   - Probablemente concentrados en vedas (ago-sep) y períodos de transición
   - ¿Estos meses son informativos o atípicos por definición?

### 7.2. Sobre la Metodología

4. **¿Es apropiado comparar R² entre muestras de distinto tamaño?**
   - R² sin imputación (N=90) vs con imputación (N=140)
   - Mayor N → más variabilidad → menor R² esperado
   - La caída de R² podría ser mecánica, no sustantiva

5. **¿Por qué Kalman produce resultados casi idénticos a imputación genérica?**
   - Matriz flexibilidades Kalman ≈ matriz con imputación
   - Si el filtro Kalman solo está interpolando linealmente, pierde sus propiedades teóricas

6. **¿Se debería usar el modelo de 2 especies (anchoveta-sardina) en lugar de 3?**
   - Jurel introduce ruido sistemático
   - Sistema 2×2 tendría menos parámetros y mayor poder estadístico

### 7.3. Sobre la Interpretación Económica

7. **¿Los coeficientes atenuados con imputación son "mejores"?**
   - Argumento a favor: Magnitudes más plausibles, menos extremas
   - Argumento en contra: Suavizado artificial reduce señal real

8. **¿La transmisión FOB de 52-58% para anchoveta es creíble?**
   - Literatura sugiere ratio P_exvessel/P_FOB ≈ 11-13% (Dresdner et al.)
   - Elasticidad 0.55 implicaría alta respuesta a shocks internacionales
   - Verificar: ¿Es consistente con estructura de costos de plantas?

9. **¿Por qué sardina es la única especie con γ_SS negativo?**
   - ¿Mercado más homogéneo (menos consumo humano directo)?
   - ¿Menor poder de mercado de compradores?
   - ¿Datos de mejor calidad para esta especie?

---

## 8. TABLAS RESUMEN

### 8.1. Comparación Completa de Coeficientes

```
                        SIN IMPUTACIÓN    CON IMPUTACIÓN    KALMAN
────────────────────────────────────────────────────────────────────
ANCHOVETA
  Intercepto                 3.820             ---           4.676
  ln_Q_Anchoveta            +0.038            +0.014        +0.014
  ln_Q_Jurel                +0.001            -0.006        -0.007
  ln_Q_Sardina              -0.034            -0.016        -0.016
  ln_P_HARINA               +0.578            +0.555        +0.525
  R²                         0.368             0.197         0.199
────────────────────────────────────────────────────────────────────
JUREL
  Intercepto                13.167             ---          11.693
  ln_Q_Anchoveta            -0.034            -0.020        -0.019
  ln_Q_Jurel                +0.024            +0.026        +0.023
  ln_Q_Sardina              +0.011            +0.011        +0.010
  ln_P_HARINA               -0.051            +0.026        +0.043
  R²                         0.104             0.133         0.115
────────────────────────────────────────────────────────────────────
SARDINA
  Intercepto                 9.026             ---           9.532
  ln_Q_Anchoveta            +0.054            +0.034        +0.029
  ln_Q_Jurel                -0.015            -0.014        -0.015
  ln_Q_Sardina              -0.056            -0.028        -0.025
  ln_P_HARINA               +0.233            +0.254        +0.191
  R²                         0.239             0.203         0.171
────────────────────────────────────────────────────────────────────
N observaciones                90               140           144
```

### 8.2. Cambios Porcentuales por Imputación

| Variable | Cambio Sin→Con Imputación | Cambio Sin→Kalman |
|----------|---------------------------|-------------------|
| γ_A,A | -62% (atenuación) | -62% |
| γ_J,J | +9% (leve aumento) | -2% |
| γ_S,S | -51% (atenuación) | -56% |
| γ_FM Anchoveta | -4% (estable) | -9% |
| γ_FM Jurel | +151% (cambio signo) | +184% |
| γ_FM Sardina | +9% (leve aumento) | -18% |

---

## 9. CONSIDERACIONES PARA DECISIÓN METODOLÓGICA

### 9.1. Argumentos para USAR imputación

1. Mayor tamaño muestral (140-144 vs 90)
2. Corrige signo de γ_FM jurel (de negativo a positivo)
3. Mejor consistencia teórica global (67% vs 50%)
4. Reduce sesgo de selección temporal (incluye meses atípicos)

### 9.2. Argumentos para NO usar imputación

1. R² anchoveta cae drásticamente (36.8% → 19.9%)
2. Atenúa coeficientes uniformemente (¿sesgo hacia cero?)
3. Valores imputados son fabricados, no observados
4. Kalman puede introducir autocorrelación artificial

### 9.3. Posición intermedia

- Usar datos sin imputación como especificación principal
- Reportar resultados con imputación como análisis de sensibilidad
- Reconocer que γ_FM jurel solo es interpretable con imputación
- Considerar excluir jurel del sistema (modelo 2 especies)

---

## 10. VERIFICACIONES PENDIENTES

1. [ ] Examinar distribución temporal de observaciones imputadas
2. [ ] Test de Hausman: ¿Coeficientes estadísticamente diferentes entre métodos?
3. [ ] Autocorrelación de residuos post-imputación Kalman
4. [ ] Análisis por subperíodo (pre/post cambio estructural 2022)
5. [ ] Comparar con estimación 2SLS/3SLS (actualmente solo OLS)
6. [ ] Validación cruzada: ajuste out-of-sample por método
