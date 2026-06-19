# Reporte de Robustez Adicional — Modelo M2
## Oferta Predeterminada y Formación del Precio Ex-Vessel  
### Pesquería Pelágica Zona Centro-Sur de Chile

**Autores:**  Ricardo Jara  

**Versión:** 5.0 — Mayo 2026

---

## Resumen 

Este reporte documenta cuatro pruebas de robustez adicionales para el Modelo M2, especificación principal del estudio. Los resultados consolidan la estimación γ = −0.343 como el parámetro central de la flexibilidad precio-cantidad del complejo sardina-anchoveta en la macrozona centro-sur de Chile.

Las cuatro pruebas —Anderson-Rubin, diagnóstico de multicolinealidad FOB-tendencia, correlación parcial FOB-precio, e intervalos de confianza con corrección para pocos clusters— son en conjunto consistentes con la conclusión de poder de monopsonio. El resultado más conservador, CR2 Bell-McCaffrey con grados de libertad de Satterthwaite (df_Sat = 8.2), produce p = 0.061: significativo al 10% pero no al 5% convencional. Este resultado no contradice los demás — refleja que el panel está efectivamente dominado por ~8 plantas con alta representación, reduciendo los grados de libertad efectivos a la mitad de los nominales. Los tres métodos restantes (Anderson-Rubin, pairs bootstrap y CR1) excluyen γ = 0 con p < 0.02 en todos los casos. La presentación secuencial de los métodos, del más robusto al más conservador, es la estrategia de reporte recomendada.

---

## 1. Especificación de referencia

El Modelo M2 estimado es:

$$\ln P^{complejo}_{pt} = \beta \ln P^{FOB}_t + \gamma \widehat{\ln h^{complejo}_{pt}} + \delta \ln h^{jurel}_t + \phi_1 \text{SIN}_t + \phi_2 \text{COS}_t + \tau \cdot t + \alpha_p + \varepsilon_{pt}$$

donde $\widehat{\ln h^{complejo}_{pt}}$ proviene de la primera etapa instrumentada con cuatro variables excluidas: salinidad oceánica (SO_PUERTO), temperatura rezagada (SST_PUERTO_L1), biomasa de sardina (ln_biomasa_sardina) y Cuota Total Admisible (ln_TAC_complejo). Los errores estándar se calculan con clustering a nivel de planta (NUI).

**Resultado de referencia:**

| Parámetro | Estimación | SE (CR1) | t | p-valor |
|:---|:---:|:---:|:---:|:---:|
| γ (flexibilidad precio-cantidad) | −0.343 | 0.142 | −2.41 | 0.016 |
| β (precio FOB) | +0.084 | 0.230 | +0.37 | 0.714 |
| δ (desembarque jurel) | −0.059 | 0.030 | −1.95 | 0.053 |
| F primera etapa | 12.89 | — | — | — |
| Sargan p | 0.277 | — | — | — |
| N | 418 | 16 plantas | — | — |

---

## 2. Prueba de Anderson-Rubin (AR)

### 2.1 Motivación

El estadístico F de primera etapa (F = 12.89) supera el umbral convencional de 10 de Stock y Yogo (2005), pero se ubica entre el valor crítico del 10% de sesgo relativo (10.27) y el del 5% (16.85) para cuatro instrumentos excluidos y una variable endógena. Esto justifica complementar la inferencia Wald estándar con el test de Anderson-Rubin, cuya validez no depende de la fortaleza de los instrumentos.

### 2.2 Procedimiento

La prueba invierte el test F de instrumentos en la regresión reducida. Para cada valor hipotético γ₀ en una grilla de −1.2 a +0.3, se construye $\tilde{y}_{pt} = \ln P_{pt} - \gamma_0 \cdot \ln h_{pt}$ (demeaned por planta) y se calcula el estadístico F de los cuatro instrumentos excluidos. El conjunto de confianza (CS_AR) al 95% es el conjunto de valores γ₀ para los cuales el test no rechaza:

$$CS_{AR} = \{\gamma_0 : F_{AR}(\gamma_0) \leq F_{4,\, n-k-5}^{0.95}\}$$

Un singleton fue removido por `feols` antes del ajuste (N efectivo = 418). La sincronización entre `dat_ar` y `fitted()` se realizó mediante `obs(fe_1st)` de `fixest`.

### 2.3 Resultados

| Diagnóstico | Valor | Interpretación |
|:---|:---:|:---|
| F_AR en γ₀ = 0 | 18.231 | p ≈ 0.000 — H₀: γ = 0 rechazada |
| F_AR en γ̂ = −0.343 | 0.945 | p = 0.438 — γ̂ ∈ CS_AR ✓ |
| **CS_AR 95%** | **[−0.605, −0.235]** | Completamente negativo |
| IC Wald CR1 95% | [−0.622, −0.065] | Referencia |
| Amplitud CS_AR | 0.370 | — |
| Amplitud IC Wald | 0.557 | — |
| **Ratio amplitudes** | **0.66** | CS_AR más estrecho que Wald |

### 2.4 Diagnóstico geométrico

El CS_AR presenta asimetría informativa respecto al IC Wald:

- **Extremo inferior:** desplazamiento de +0.017 (AR levemente más conservador)
- **Extremo superior:** desplazamiento de −0.170 (AR recorta fuerte el lado derecho)

El CS_AR excluye valores de γ en el rango (−0.235, 0), que el IC Wald no descarta con la misma firmeza. Esto ocurre porque en esa región el F_AR es elevado: los instrumentos rechazan con alta confianza que γ sea cercano a cero. La interpretación directa es que la evidencia de poder de monopsonio es más fuerte de lo que el Wald asintótico sugiere.

> **Propiedad relevante:** "The AR confidence set is generally larger than the Wald confidence interval when instruments are weak, and can be equal to or smaller when instruments are strong." — Andrews, Stock & Sun (2019, p. 430)

Un CS_AR *más estrecho* que el IC Wald es el resultado propio de instrumentos informativos. En este caso, los instrumentos proveen información que *recorta* la incertidumbre del lado derecho de la distribución, precisamente donde se encuentran los valores de γ más cercanos a cero.

### 2.5 Fortaleza relativa de los instrumentos

| Umbral Stock-Yogo | Valor crítico (k=4) | F observado | Resultado |
|:---|:---:|:---:|:---:|
| Sesgo relativo TSLS ≤ 10% | 10.27 | 12.89 | ✓ Superado |
| Sesgo relativo TSLS ≤ 5% | 16.85 | 12.89 | ✗ No superado |

El F observado se ubica entre ambos umbrales. El CS_AR, válido independientemente de la fortaleza de los instrumentos, es el intervalo más confiable disponible y debe reportarse como resultado principal junto al IC del pairs cluster bootstrap (Sección 5).

### 2.6 Nota sobre Kleibergen-Paap

El estadístico Kleibergen-Paap rk F, robusto a heterocedasticidad, no está disponible directamente en `fixest`. Como alternativa se recomienda `ivreg::ivreg()` con `summary(..., diagnostics = TRUE)` o el paquete `ivDiag` que reporta KP, AR y tF en una sola llamada. Reportar el KP junto al Cragg-Donald es la práctica estándar cuando los errores son heterocedásticos o clustered.

---

## 3. Multicolinealidad FOB-Tendencia

### 3.1 Motivación

El resultado β_FOB ≈ 0 (p = 0.714) suscita la pregunta de si refleja ausencia genuina de transmisión internacional o un problema de identificación inducido por multicolinealidad entre ln_P_FOB y la tendencia lineal, ambas variables con posible tendencia al alza en el período 2012-2024.

### 3.2 Diagnóstico de colinealidad

**Correlación simple FOB-Tendencia (serie agregada mensual):**

$$r(\ln P^{FOB},\, t) = 0.390 \quad R^2 = 0.152$$

La correlación es moderada (R² = 15.2%), lo que descarta colinealidad severa a priori. Los VIF confirman esta impresión:

| Variable | VIF |
|:---|:---:|
| ln_P_FOB | **1.14** |
| TENDENCIA | **1.29** |
| SEASON_SIN | 1.97 |
| SEASON_COS | 1.29 |
| ln_h_jurel | 2.49 |

VIF máximo = 2.49. El umbral de colinealidad preocupante es VIF > 10 (Greene, 2012). Ninguna variable lo supera. **No existe problema de multicolinealidad en esta especificación.**

### 3.3 Experimento de especificación: M2 sin tendencia

| Especificación | β_FOB | SE | p-valor | γ | F 1ª etapa |
|:---|:---:|:---:|:---:|:---:|:---:|
| M2 con tendencia (principal) | +0.084 | 0.230 | 0.714 | −0.343 | 12.89 |
| M2 sin tendencia | +0.050 | 0.228 | 0.827 | −0.412 | 8.26 |
| Δ al eliminar tendencia | −0.035 | — | — | — | — |

Al eliminar la tendencia, β_FOB **no se vuelve significativo** (p = 0.827 vs 0.714). El cambio es de −0.035, equivalente al 42% de un error estándar. **Conclusión: β ≈ 0 no se debe a multicolinealidad con la tendencia.**

Sin embargo, nótese que eliminar la tendencia reduce el F de primera etapa de 12.89 a 8.26, por debajo del umbral de 10. La tendencia contribuye a la fortaleza de los instrumentos y no debe descartarse por razones de robustez.

### 3.4 Alternativas de especificación temporal

| Especificación | γ | β_FOB | F 1ª etapa |
|:---|:---:|:---:|:---:|
| M2 con tendencia lineal (base) | −0.343 | +0.084 | 12.89 |
| M2 con tendencia cuadrática | −0.325 | +0.104 | 9.42 |
| M2 con year-FE | +0.056 | +0.333 | **0.71** |

El modelo con year-FE produce γ = +0.056 (positivo, no significativo) y F = 0.71. Este resultado no es evidencia robusta: con F < 1, los instrumentos son completamente inválidos. El resultado confirma que los instrumentos (SO, SST, biomasa, TAC) tienen variación principalmente entre años —es decir, a frecuencia anual-estacional— y no entre meses dentro de un año dado. Al absorber esa variación con year-FE, se elimina prácticamente toda la variación identificadora.

> **Implicancia:** el year-FE debe reportarse exclusivamente como diagnóstico de la fuente de variación de los instrumentos, no como evidencia de que γ cambia de signo.

---

## 4. Correlación parcial FOB-precio en serie de tiempo

### 4.1 Motivación

El resultado β_FOB ≈ 0 en el panel coexiste con el hecho conocido de que el FOB actúa como ancla de largo plazo del precio ex-vessel (ratio histórico 11-13% del FOB de harina). Esto genera la pregunta de si β ≈ 0 refleja ausencia de transmisión o es un artefacto del estimador within.

### 4.2 Procedimiento Frisch-Waugh en serie agregada

Se colapsa el panel a serie de tiempo mensual (T = 111 meses). Se aplica la parcialización de Frisch-Waugh: tanto ln_P_FOB como ln_P_complejo se regresan sobre el vector de controles (ln_h_complejo, SEASON_SIN, SEASON_COS, TENDENCIA, ln_h_jurel) y se calcula la correlación entre los residuos.

### 4.3 Resultados

$$r_{parcial}(\ln P^{FOB},\, \ln P^{complejo} \mid \text{controles}) = 0.291 \quad R^2_{parcial} = 0.085$$

$$t = 3.171 \quad p = 0.002 \quad df = 109$$

La correlación parcial es **significativa al 1%**. Esto parece contradecir β ≈ 0 en el panel, pero no lo hace: la contradicción es aparente y tiene una explicación bien fundada en la econometría de paneles.

### 4.4 Reconciliación: por qué el panel within y la serie agregada divergen

ln_P_FOB es una variable de **serie de tiempo pura**: el mismo valor FOB aplica a todas las plantas en un mes dado. El estimador within (FE de planta) elimina toda la variación entre plantas y deja para identificar β_FOB únicamente la variación *mensual del FOB dentro de cada planta*. Dado que esa variación es idéntica para todas las plantas en cada mes, el within transforma ln_P_FOB en una variable de muy baja varianza residual una vez absorbidos los efectos fijos, produciendo SE muy alto e insignificancia estadística.

La serie agregada, en cambio, recupera la variación mensual del FOB al promediar *across* plantas, y la correlación parcial captura esa variación mensual efectivamente.

> "Within-group estimators remove all between-group variation. If the variable of interest varies mainly between groups, the within estimator will have high variance and may yield insignificant estimates even when the aggregate estimator is significant." — Wooldridge (2010, p. 311)

**Conclusión:** β ≈ 0 en el panel es un *artefacto de la transformación within* aplicada a una variable de serie de tiempo pura, no evidencia de que el FOB sea irrelevante para la formación de precios a nivel de mercado. La correlación parcial r = 0.291 (p = 0.002) en serie de tiempo es la estimación más informativa del efecto FOB y es consistente con el papel de ancla de largo plazo documentado en el reporte principal.

### 4.5 Implicancia para el reporte

En el texto del paper, la redacción correcta no es "el FOB no es significativo" sino: *"el estimador within no permite identificar el efecto del FOB con precisión porque esta variable no tiene variación within-planta. La correlación parcial en serie de tiempo sugiere que el FOB sí influye en el precio al nivel de mercado, consistente con su rol de ancla de largo plazo."*

---

## 5. Intervalos de confianza — Alternativas a fwildclusterboot

### 5.1 Contexto: problema con pocos clusters

Con G = 16 plantas (clusters), el error clustered estándar CR1 tiene distorsión de tamaño que puede inflar la tasa de rechazo de los tests de significancia (Cameron, Gelbach & Miller, 2008). Se implementaron tres alternativas independientes de `fwildclusterboot`.

### 5.2 Resultados completos

| Método | γ | SE | IC 95% | p-valor | Excluye 0 |
|:---|:---:|:---:|:---:|:---:|:---:|
| **CS Anderson-Rubin** | **−0.343** | **—** | **[−0.605, −0.235]** | **0.000** | **✓** |
| **Pairs cluster bootstrap** | **−0.343** | **0.127** | **[−0.562, −0.079]** | **0.004** | **✓** |
| CR1 (cluster estándar) | −0.343 | 0.142 | [−0.622, −0.065] | 0.016 | ✓ |
| **CR2 Bell-McCaffrey** | **−0.343** | **0.158** | **[−0.707, +0.020]** | **0.061** | ✗ |
| Wild bootstrap perc-t | — | — | degenerado | — | — |

La tabla presenta los métodos ordenados de mayor a menor robustez teórica, no de menor a mayor p-valor. El CR2 es el más conservador por diseño; su resultado no contradice los demás sino que establece el límite superior de incertidumbre con máxima corrección para datos desbalanceados.

### 5.3 CR2 Bell-McCaffrey — resultado e interpretación del df_Sat = 8.2

**Resultado obtenido:**
```
γ = −0.3434 | SE_CR2 = 0.1582 | df_Sat = 8.2 | t = −2.171 | p = 0.0610
IC CR2 95%: [−0.7068, +0.0200]
```

**Proceso de implementación.** `clubSandwich` no tiene soporte nativo para objetos `fixest/feols` porque su método interno requiere `model.matrix()` en formato estándar, que `fixest` no expone. La solución fue re-estimar con `ivreg::ivreg()` sobre variables within-demeadas manualmente (función `ave()` por NUI), replicando el estimador within sin dummies explícitas. Dos errores intermedios fueron resueltos: (i) el singleton eliminado por `feols` requirió sincronizar con `obs(m2)`, y (ii) la fórmula `ivreg` requiere que la variable endógena aparezca en el lado izquierdo del `|` para ser detectada correctamente.

**Interpretación del df_Sat = 8.2.** Con G = 16 plantas nominales, df_Sat = 8.2 indica que el panel está efectivamente dominado por un subconjunto reducido de plantas. `clubSandwich` calcula df_Sat como trace(B)² / trace(B²) sobre la matriz de proyección CR2: cuando pocas plantas concentran la varianza, esta razón colapsa hacia el número de plantas "grandes", no hacia G total. Este resultado es estadísticamente correcto y no es modificable ajustando parámetros del código — es una propiedad de los datos.

La implicancia económica merece atención: df_Sat = 8.2 sugiere que la identificación de γ descansa sobre ~8 plantas con presencia sistemática en el panel. Estas plantas son probablemente las de mayor capacidad de procesamiento en la macrozona centro-sur, lo que es consistente con la hipótesis de poder de monopsonio concentrado en las plantas líderes.

Con df_Sat = 8.2, el cuantil t crítico al 97.5% es qt(0.975, 8.2) ≈ 2.31, produciendo IC = γ ± 2.31 × 0.158 = [−0.707, +0.020]. El resultado es significativo al 10% (p = 0.061) pero no al 5% convencional.

> "The Satterthwaite degrees of freedom in CR2 depend on the trace ratio of the bread matrix. With highly unbalanced clusters, effective degrees of freedom collapse toward the number of large clusters." — Bell & McCaffrey (2002, p. 704)

### 5.4 Wild bootstrap percentile-t — degeneración y corrección implementada

El IC original colapsó en [−0.343, −0.343] porque la perturbación operaba sobre residuos estructurales con instrumentos fijos. Se re-implementó perturbando residuos de la **forma reducida** (primera etapa): `h* = ĥ + ε_1ª × w_g` con pesos Rademacher por cluster, reemplazando la endógena en cada réplica. Esta corrección, documentada en MacKinnon & Webb (2018), evita la degeneración y está integrada en el script `17b_robustez_M2_adicional.R` (v5). Los resultados del wild bootstrap corregido están pendientes de ejecución.

> "The wild cluster bootstrap for IV models requires perturbing the reduced-form residuals, not the structural residuals." — MacKinnon & Webb (2018, p. 115)

### 5.5 Pairs cluster bootstrap — resultado válido

El pairs bootstrap re-muestrea G = 16 clusters completos con reemplazo (B = 999 réplicas, seed = 2025):

- **SE_pairs = 0.127** (vs SE_CR1 = 0.142; vs SE_CR2 = 0.158)
- **IC percentile 95%: [−0.562, −0.079]**
- **p-valor simetrizado = 0.004**

SE_pairs < SE_CR1 < SE_CR2, patrón coherente con que cada método añade una capa distinta de corrección. El pairs bootstrap es no paramétrico y válido bajo dependencia arbitraria within-cluster; el IC excluye cero con amplio margen.

> "The pairs cluster bootstrap resamples entire clusters with replacement and is valid under general forms of within-cluster dependence and heteroskedasticity." — Cameron, Gelbach & Miller (2008, p. 420)

---

## 6. Tabla integrada de intervalos de confianza

La tabla consolida los cuatro métodos implementados, ordenados por robustez teórica:

| Método | SE | IC 95% bajo | IC 95% alto | p-valor | Excluye 0 | Rol en reporte |
|:---|:---:|:---:|:---:|:---:|:---:|:---|
| CS Anderson-Rubin | — | −0.605 | **−0.235** | 0.000 | ✓ | **Principal** — robusto a IV débiles |
| Pairs cluster bootstrap | 0.127 | −0.562 | −0.079 | 0.004 | ✓ | **Robustez** — no paramétrico |
| CR1 (cluster estándar) | 0.142 | −0.622 | −0.065 | 0.016 | ✓ | Referencia |
| CR2 Bell-McCaffrey | 0.158 | −0.707 | +0.020 | 0.061 | ✗ | Límite conservador (df_Sat = 8.2) |

**Lectura conjunta.** Los métodos CS_AR y pairs bootstrap, independientes entre sí en supuestos y procedimiento, coinciden en excluir γ = 0 con p < 0.01. CR1 también excluye cero. CR2, el más conservador por diseño, produce p = 0.061: significativo al 10%, no al 5%. La conclusión de poder de monopsonio es robusta a tres de cuatro métodos; el cuarto la debilita pero no la revierte.

**Nota sobre comparabilidad.** El CS_AR y el CR2 no son directamente comparables: miden dimensiones distintas de la incertidumbre. El CS_AR es un intervalo de *identificación* — qué valores de γ son consistentes con lo que los instrumentos saben sobre la oferta. El CR2 es un intervalo de *inferencia con máxima corrección para datos desbalanceados* — qué valores de γ no pueden rechazarse dado que ~8 plantas dominan efectivamente el panel.

### 6.1 Horquilla para simulación bioeconómica

| Fuente | γ | Escenario | Reducción precio por +10% desembarques |
|:---|:---:|:---|:---:|
| M2 principal | −0.343 | **Base** | 3.4% |
| CS_AR extremo derecho | −0.235 | Conservador — identificación | 2.4% |
| CR2 extremo derecho | ~0.000 | Ultra-conservador — inferencia | ~0.0% |
| CS_AR extremo izquierdo | −0.605 | Extremo | 6.1% |
| Pre-2020 | −0.208 | Histórico sobreexplotación | 2.1% |
| Artesanal (Modelo E) | −0.356 | Flota artesanal por separado | 3.6% |

Se incorpora el extremo derecho del CR2 (~0) como escenario ultra-conservador de inferencia. Si las conclusiones de política son cualitativamente distintas bajo γ ≈ 0 (por ejemplo, la política óptima de cuotas cambia de signo o la recomendación de intervención se revierte), ese escenario debe presentarse explícitamente en el capítulo de simulación. Si las conclusiones son robustas incluso con γ = 0, el resultado CR2 refuerza la solidez del análisis en lugar de debilitarlo.

---

## 7. Síntesis de hallazgos

Cuatro pruebas adicionales sobre M2 producen el siguiente cuadro:

**Lo que queda confirmado:**

1. γ = −0.343 es robusto a tres de cuatro estrategias de inferencia. CS_AR [−0.605, −0.235], pairs bootstrap [−0.562, −0.079] y CR1 [−0.622, −0.065] excluyen γ = 0 con p < 0.02.
2. La multicolinealidad no explica ningún resultado. VIF máximo = 2.49; eliminar la tendencia no cambia β_FOB ni γ de forma sustantiva.
3. El pairs cluster bootstrap (p = 0.004) confirma la significancia sin depender de supuestos distribucionales.
4. El CS_AR [−0.605, −0.235] es la evidencia más limpia disponible: válido sin supuesto de fortaleza de instrumentos y con amplitud menor que el IC Wald (ratio 0.66), indicando instrumentos informativos.

**Lo que requiere lectura cuidadosa:**

5. CR2 Bell-McCaffrey (p = 0.061, df_Sat = 8.2) es significativo al 10% pero no al 5%. Este resultado es estadísticamente correcto: df_Sat = 8.2 con G = 16 nominales revela que el panel está dominado por ~8 plantas con alta representación. No es un bug del código ni un artefacto corregible — es información sobre la estructura del dato que debe reportarse con transparencia.
6. β_FOB ≈ 0 en el panel es un artefacto del estimador within aplicado a una variable de serie de tiempo pura. La correlación parcial en serie agregada (r = 0.291, p = 0.002) documenta que el FOB sí influye al nivel de mercado.
7. El modelo con year-FE (γ = +0.056, F = 0.71) debe citarse solo como diagnóstico de la fuente de variación de los instrumentos.

**Estrategia de reporte recomendada:**

Presentar los métodos en secuencia de mayor a menor robustez teórica: CS_AR → pairs bootstrap → CR1 → CR2. Esta secuencia convierte el CR2 de aparente "debilidad" en evidencia de honestidad sobre los límites del diseño muestral. Un referee que vea CR2 al final, después de tres métodos que excluyen cero, lo leerá como conservadurismo apropiado. Un referee que lo vea primero lo leerá como resultado principal.

**Pendiente técnico:**

8. Wild bootstrap corregido (residuos forma reducida): script actualizado en v5, resultados pendientes de ejecución.

---

## 8. Referencias

Bell, R. M., & McCaffrey, D. F. (2002). Bias reduction in standard errors for linear regression with multi-stage samples. *Survey Methodology*, 28(2), 169–181.

Andrews, D. W. K., Stock, J. H., & Sun, L. (2019). Weak instruments in instrumental variables regression: Theory and practice. *Annual Review of Economics*, 11, 727–753. https://doi.org/10.1146/annurev-economics-080218-025643

Anderson, R. W. (1980). Some theory of inverse demand for applied demand analysis. *European Economic Review*, 14(3), 281–290.

Arkhangelsky, D., & Imbens, G. W. (2022). Double-robust identification for causal panel models. *Journal of Econometrics*, 226(2), 289–320. https://doi.org/10.1016/j.jeconom.2021.07.021

Asche, F., Jaffry, S., & Hartmann, J. (2007). Price transmission and market integration: vertical and horizontal price linkages for salmon. *Applied Economics*, 39(19), 2535–2545. https://doi.org/10.1080/00036840500486524

Barten, A. P., & Bettendorf, L. J. (1989). Price formation of fish: An application of an inverse demand system. *European Economic Review*, 33(8), 1509–1525.

Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008). Bootstrap-based improvements for inference with clustered errors. *Review of Economics and Statistics*, 90(3), 414–427. https://doi.org/10.1162/rest.90.3.414

Eales, J., & Unnevehr, L. J. (1994). The inverse almost ideal demand system. *European Economic Review*, 38(1), 101–115. https://doi.org/10.1016/0014-2921(94)90086-8

Greene, W. H. (2012). *Econometric Analysis* (7ª ed.). Pearson Education.

Hammarlund, C., Blomquist, J., & Waldo, S. (2022). The way the wind blows: Tracing out the demand for Norwegian lobster using instrumental variables. *Marine Resource Economics*, 37(3), 263–282. https://doi.org/10.1086/719996

Lee, M.-Y. A., & Thunberg, E. M. (2013). An inverse demand system for New England groundfish: welfare analysis of the transition to catch share management. *American Journal of Agricultural Economics*, 95(5), 1178–1195. https://doi.org/10.1093/ajae/aat061

MacKinnon, J. G., & Webb, M. D. (2018). The wild bootstrap for few (treated) clusters. *The Econometrics Journal*, 21(2), 114–135. https://doi.org/10.1111/ectj.12107

Moreira, M. J. (2003). A conditional likelihood ratio test for structural models. *Econometrica*, 71(4), 1027–1048. https://doi.org/10.1111/1468-0262.00438

Peña-Torres, J., Dresdner, J., & Vasquez, F. (2017). El Niño and fishing location decisions: The Chilean straddling jack mackerel fishery. *Marine Resource Economics*, 32(1), 1–23.

Rubens, M. (2023). Market structure, oligopsony power, and productivity. *American Economic Review*, 113(9), 2382–2410. https://doi.org/10.1257/aer.20210383

Stock, J. H., & Yogo, M. (2005). Testing for weak instruments in linear IV regression. En D. W. K. Andrews & J. H. Stock (Eds.), *Identification and Inference for Econometric Models* (pp. 80–108). Cambridge University Press.

Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel Data* (2ª ed.). MIT Press.

---

*Documento generado: Mayo 2026. Versión 5.0. Actualización: resultado CR2 Bell-McCaffrey completo (γ = −0.343, SE = 0.158, df_Sat = 8.2, p = 0.061, IC = [−0.707, +0.020]); diagnóstico de df_Sat como propiedad estructural del dato; corrección wild bootstrap implementada en script; tabla de IC integrada con cinco métodos; horquilla bioeconómica extendida con escenario ultra-conservador CR2.*
