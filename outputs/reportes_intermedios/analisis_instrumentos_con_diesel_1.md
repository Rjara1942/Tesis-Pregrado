# ANALISIS DETALLADO: INSTRUMENTOS Y ENDOGENEIDAD (CON DIESEL)

## Modelo de Precios Ex-Vessel para Pequenos Pelagicos Centro-Sur de Chile

**Autor:** Ricardo Jara Valencia  
**Colaborador:** Felipe Quezada-Escalona  
**Fecha:** Marzo 2026

---

## INDICE

1. [Diagnostico de Autocorrelacion](#1-diagnostico-de-autocorrelacion)
2. [Instrumentos Utilizados](#2-instrumentos-utilizados)
3. [Diagnosticos de Primera Etapa](#3-diagnosticos-de-primera-etapa)
4. [El Problema de Instrumentos Debiles y el Sesgo Hacia Cero](#4-el-problema-de-instrumentos-debiles-y-el-sesgo-hacia-cero)
5. [Analisis del Test de Wu-Hausman](#5-analisis-del-test-de-wu-hausman)
6. [Test de Sargan para Validez de Instrumentos](#6-test-de-sargan-para-validez-de-instrumentos)
7. [Test de Hausman para Exogeneidad del FOB (Paso 6)](#7-test-de-hausman-para-exogeneidad-del-fob-paso-6)
8. [Test de Wald para Integracion Imperfecta (Paso 7)](#8-test-de-wald-para-integracion-imperfecta-paso-7)
9. [Modelo Final y Coeficientes](#9-modelo-final-y-coeficientes)
10. [Discusion: Contradiccion entre Wu-Hausman y Hausman FOB](#10-discusion-contradiccion-entre-wu-hausman-y-hausman-fob)
11. [Interpretacion Economica](#11-interpretacion-economica)

---

## 1. DIAGNOSTICO DE AUTOCORRELACION

### 1.1 Test de Breusch-Godfrey

El test de Breusch-Godfrey evalua la presencia de autocorrelacion serial en los residuos del modelo panel.

**Resultados:**

| Test | Orden | p-value | Interpretacion |
|------|-------|---------|----------------|
| Breusch-Godfrey | AR(1) | 0.0000 | Autocorrelacion de orden 1 presente |
| Breusch-Godfrey | AR(2) | 0.0000 | Autocorrelacion de orden 2 presente |

**Conclusion:** Existe autocorrelacion severa en los residuos. Los errores estandar convencionales son invalidos.

### 1.2 Solucion Aplicada

Se utilizan errores estandar de Driscoll-Kraay (funcion `vcovSCC` en R) con `maxlag = 3`. Este estimador es robusto a:

- Autocorrelacion serial de orden arbitrario
- Heterocedasticidad
- Correlacion espacial entre unidades (plantas)

```r
vcov_dk <- vcovSCC(fe_ols, maxlag = 3)
coef_ols <- coeftest(fe_ols, vcov = vcov_dk)
```

### 1.3 Grafico ACF/PACF

Se exporto el grafico de autocorrelacion de residuos como `acf_pacf_residuos.png` para inspeccion visual de la estructura de dependencia temporal.

---

## 2. INSTRUMENTOS UTILIZADOS

### 2.1 Ecuacion Estructural

La ecuacion a estimar es:

```
ln(P_complejo) = beta * ln(P_FOB) + gamma_1 * ln(h_complejo) + gamma_2 * ln(h_jurel) 
                 + efectos_fijos_mes + efectos_fijos_planta + epsilon
```

Donde:
- `P_complejo`: Precio ex-vessel del complejo sardina-anchoveta (CLP/ton)
- `P_FOB`: Precio FOB de harina de pescado (CLP/ton)
- `h_complejo`: Desembarques del complejo sardina-anchoveta (toneladas)
- `h_jurel`: Desembarques de jurel (toneladas)

### 2.2 Variables Endogenas Sospechosas

| Variable | Fuente de Endogeneidad |
|----------|------------------------|
| `ln_P_FOB` | Chile podria afectar el precio mundial si tiene poder de mercado |
| `ln_h_complejo` | Pescadores podrian ajustar captura en respuesta al precio |

### 2.3 Conjunto Completo de Instrumentos (Incluyendo Diesel)

#### A. Instrumentos para el Precio FOB

| Instrumento | Variable | Justificacion de Relevancia | Justificacion de Exclusion |
|-------------|----------|-----------------------------|-----------------------------|
| **Tipo de cambio** | `ln_tipo_cambio` | FOB en CLP = FOB en USD x TC. Relacion mecanica directa. | El tipo de cambio afecta el precio local solo via el FOB, no directamente. |
| **Desembarques Peru** | `ln_h_peru` | Peru produce 60% del mercado mundial. Su oferta afecta el precio FOB. | La produccion peruana no afecta el precio ex-vessel chileno excepto via el FOB. |

#### B. Instrumentos para los Desembarques

| Instrumento | Variable | Justificacion de Relevancia | Justificacion de Exclusion |
|-------------|----------|-----------------------------|-----------------------------|
| **Temperatura superficial del mar** | `SST_MACRO` | Afecta distribucion de cardumenes y disponibilidad biologica. | No afecta la disposicion a pagar de las plantas procesadoras. |
| **Clorofila-a** | `CHL_A_MACRO` | Proxy de productividad primaria. Determina alimento para pelagicos. | No afecta el valor del pescado para la industria reductora. |
| **Precio del diesel** | `ln_DIESEL` | Afecta costos de operacion de la flota (30-40% de costos variables). Determina esfuerzo pesquero. | Posible violacion: podria afectar costos de procesamiento en planta. |
| **Rezago de desembarques (t-3)** | `ln_h_complejo_L3` | Persistencia en patrones de captura. Predeterminado. | Exogeno respecto a epsilon_t bajo no autocorrelacion de errores. |
| **Rezago de desembarques (t-4)** | `ln_h_complejo_L4` | Persistencia en patrones de captura. Predeterminado. | Exogeno respecto a epsilon_t bajo no autocorrelacion de errores. |

### 2.4 Nota sobre el Diesel como Instrumento

El precio del diesel es un instrumento potencialmente problematico:

**Argumento a favor (relevancia):**
- El combustible representa 30-40% de los costos variables de la flota.
- Un aumento en el precio del diesel reduce el esfuerzo pesquero y, por ende, los desembarques.

**Argumento en contra (exclusion):**
- El diesel tambien afecta los costos de procesamiento en planta (generadores, transporte).
- Si el diesel afecta el precio ex-vessel directamente (via costos), viola la restriccion de exclusion.

Esta preocupacion se refleja en el test de Sargan, que muestra validez marginal (p = 0.08).

### 2.5 Especificacion del Modelo IV en R

```r
iv_final <- ivreg(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + MES_fact + NUI_fact |
    ln_h_peru + ln_tipo_cambio + SST_MACRO + CHL_A_MACRO + ln_DIESEL + 
    ln_h_complejo_L3 + ln_h_complejo_L4 + ln_h_jurel + MES_fact + NUI_fact,
  data = df_clean
)
```

**Diferencia con especificacion anterior:** Se anade `ln_DIESEL` a los instrumentos excluidos.

---

## 3. DIAGNOSTICOS DE PRIMERA ETAPA

### 3.1 Resultados del Diagnostico

```
                                 df1 df2 statistic      p-value
Weak instruments (ln_P_FOB)        7 327 45.166428 1.688219e-44
Weak instruments (ln_h_complejo)   7 327  2.247248 3.030697e-02
Wu-Hausman                         2 330  7.322278 7.735980e-04
Sargan                             5  NA  9.797507 8.118039e-02
```

### 3.2 Comparacion con Especificacion Sin Diesel

| Test | Sin Diesel | Con Diesel | Cambio |
|------|------------|------------|--------|
| Weak IV (FOB) | F = 38.98 | F = 45.17 | Mejora (mas fuerte) |
| Weak IV (h_complejo) | F = 2.48 | F = 2.25 | Empeora (mas debil) |
| Wu-Hausman | p = 0.328 | p = 0.0008 | Cambio drastico |
| Sargan | p = 0.187 | p = 0.081 | Empeora (menos validos) |

### 3.3 Interpretacion de Cada Linea

#### Linea 1: Weak instruments (ln_P_FOB)

| Elemento | Valor | Significado |
|----------|-------|-------------|
| Variable endogena | ln_P_FOB | Precio FOB |
| F-statistic | 45.17 | Poder predictivo de los IVs excluidos |
| Regla de Stock-Yogo | F > 10 | Umbral para instrumentos fuertes |
| Conclusion | **IV FUERTE** | Los instrumentos predicen bien el FOB |

El F-statistic aumento de 38.98 a 45.17 con la inclusion del diesel. Esto sugiere que el diesel tiene correlacion con el FOB (posiblemente via costos de produccion global de harina).

#### Linea 2: Weak instruments (ln_h_complejo)

| Elemento | Valor | Significado |
|----------|-------|-------------|
| Variable endogena | ln_h_complejo | Desembarques |
| F-statistic | 2.25 | Poder predictivo de los IVs excluidos |
| Regla de Stock-Yogo | F > 10 | Umbral para instrumentos fuertes |
| Conclusion | **IV DEBIL** | Los instrumentos NO predicen bien los desembarques |

El F-statistic disminuyo de 2.48 a 2.25. Agregar el diesel no mejoro la primera etapa para h_complejo, y de hecho la empeoro marginalmente.

#### Linea 3: Wu-Hausman

| Elemento | Valor | Significado |
|----------|-------|-------------|
| F-statistic | 7.32 | Diferencia entre coeficientes IV y OLS |
| p-value | 0.0008 | Significativo al 1% |
| Conclusion | **SE RECHAZA H0** | Hay evidencia de endogeneidad |

**CAMBIO CRITICO:** Con la inclusion del diesel, el test de Wu-Hausman ahora rechaza la exogeneidad (p = 0.0008 vs. p = 0.328 sin diesel).

#### Linea 4: Sargan

| Elemento | Valor | Significado |
|----------|-------|-------------|
| chi2-statistic | 9.80 | Test de sobreidentificacion |
| df | 5 | Grados de libertad (num. IVs excluidos - num. endogenas) |
| p-value | 0.081 | Marginalmente significativo al 10% |
| Conclusion | **VALIDEZ MARGINAL** | Posible violacion de exclusion |

El p-value cayo de 0.187 a 0.081. Esto sugiere que la inclusion del diesel introduce dudas sobre la validez del conjunto de instrumentos.

---

## 4. EL PROBLEMA DE INSTRUMENTOS DEBILES Y EL SESGO HACIA CERO

### 4.1 Que es el Sesgo por Instrumentos Debiles

Cuando los instrumentos son debiles (F < 10), el estimador de Variables Instrumentales (IV) o 2SLS sufre un sesgo sistematico hacia el estimador OLS, y en casos extremos, hacia cero.

### 4.2 Mecanismo del Sesgo

El estimador IV tiene la forma:

```
beta_IV = (Z'X)^(-1) * (Z'Y)
```

Donde:
- Z = matriz de instrumentos
- X = matriz de regresores endogenos
- Y = variable dependiente

Cuando Z tiene correlacion debil con X:

1. **Z'X es pequeno:** La matriz de momentos cruzados instrumento-endogena tiene valores cercanos a cero.

2. **Inversion inestable:** (Z'X)^(-1) se vuelve muy grande e inestable numericamente.

3. **Amplificacion de errores:** Cualquier correlacion espuria entre Z y epsilon (incluso por muestreo aleatorio) se amplifica.

4. **Sesgo hacia OLS:** El estimador IV "colapsa" hacia el estimador OLS cuando los instrumentos son irrelevantes.

5. **Atenuacion hacia cero:** Con multiples endogenas y weak IV para alguna de ellas, los coeficientes tienden a atenuarse hacia cero.

### 4.3 Manifestacion en los Coeficientes IV

Los coeficientes IV con errores CR2 fueron:

```
         Coef. Estimate     SE   t-stat  p-val (Satt) 
(Intercept)   17.279  8.2666   2.090      0.341     
ln_P_FOB      -0.189  0.4501  -0.421      0.687     
ln_h_complejo -0.391  0.2791  -1.399      0.198     
ln_h_jurel    -0.018  0.0244  -0.737      0.490     
```

**Observaciones criticas:**

1. **ln_P_FOB tiene signo negativo (-0.189):** Esto es economicamente absurdo. Un mayor precio FOB deberia aumentar, no disminuir, el precio ex-vessel. El signo incorrecto es sintoma clasico de sesgo por weak IV.

2. **Todos los coeficientes son no significativos:** Los errores estandar son enormes (SE de ln_P_FOB = 0.45).

3. **Comparacion con OLS-FE:**

| Variable | Coef IV | SE IV | Coef OLS-FE | SE OLS-FE |
|----------|---------|-------|-------------|-----------|
| ln_P_FOB | -0.189 | 0.450 | 0.466 | 0.168 |
| ln_h_complejo | -0.391 | 0.279 | -0.062 | 0.018 |
| ln_h_jurel | -0.018 | 0.024 | -0.009 | 0.016 |

El coeficiente IV de ln_P_FOB no solo esta sesgado hacia cero, sino que tiene el signo incorrecto. Esto invalida cualquier inferencia basada en el estimador IV.

### 4.4 Por que Usar OLS-FE a Pesar del Wu-Hausman

Aunque el test de Wu-Hausman rechaza exogeneidad (p = 0.0008), el estimador IV no es utilizable porque:

1. **Weak IV (F = 2.25 << 10):** Los instrumentos para h_complejo son demasiado debiles.
2. **Coeficientes sin sentido:** ln_P_FOB negativo es economicamente imposible.
3. **Sargan marginal (p = 0.08):** Hay dudas sobre la validez del conjunto de instrumentos.

En esta situacion, el "mal menor" es usar OLS-FE, reconociendo que puede haber sesgo por endogeneidad, pero al menos los coeficientes tienen interpretacion economica coherente.

---

## 5. ANALISIS DEL TEST DE WU-HAUSMAN

### 5.1 Cambio Drastico con la Inclusion del Diesel

| Especificacion | Wu-Hausman F | p-value | Decision |
|----------------|--------------|---------|----------|
| Sin diesel | 1.12 | 0.328 | No rechaza H0 (exogeneidad) |
| Con diesel | 7.32 | 0.0008 | Rechaza H0 (endogeneidad) |

### 5.2 Posibles Explicaciones

**Hipotesis 1: El diesel viola la restriccion de exclusion**

Si el diesel afecta el precio ex-vessel directamente (via costos de procesamiento), entonces:
- El diesel es un instrumento invalido.
- Los coeficientes IV estan sesgados.
- El test de Wu-Hausman detecta la diferencia entre IV sesgado y OLS.

**Hipotesis 2: El diesel introduce multicolinealidad**

Si el diesel esta correlacionado con otras variables (FOB, tendencia temporal), puede:
- Desestabilizar la primera etapa.
- Generar coeficientes IV erraticos.
- Hacer que el test de Wu-Hausman sea poco confiable.

**Hipotesis 3: Cambio en la muestra efectiva**

Al requerir `drop_na()` incluyendo ln_DIESEL, la muestra puede cambiar, alterando los resultados.

### 5.3 Evidencia del Test de Sargan

El test de Sargan (p = 0.081) proporciona evidencia adicional:

- Con p < 0.10, hay indicios de que al menos un instrumento viola la restriccion de exclusion.
- El diesel es el candidato mas probable, dado que no estaba en la especificacion original.

### 5.4 Conclusion sobre Wu-Hausman

El rechazo del test de Wu-Hausman con diesel (p = 0.0008) probablemente refleja un problema con el diesel como instrumento, no endogeneidad genuina de h_complejo. Esta interpretacion es consistente con:

1. El test de Sargan marginal (p = 0.08).
2. Los coeficientes IV absurdos (ln_P_FOB negativo).
3. El resultado opuesto sin diesel (p = 0.328).

---

## 6. TEST DE SARGAN PARA VALIDEZ DE INSTRUMENTOS

### 6.1 Hipotesis

| Hipotesis | Descripcion |
|-----------|-------------|
| H0 | Todos los instrumentos son validos (cumplen restriccion de exclusion) |
| H1 | Al menos un instrumento es invalido |

### 6.2 Resultado

```
Sargan: chi2 = 9.797, df = 5, p-value = 0.0812
```

### 6.3 Interpretacion

| Criterio | Valor | Decision |
|----------|-------|----------|
| p-value | 0.0812 | < 0.10 (marginalmente) |
| Decision | Rechazo marginal de H0 al 10% | |
| Conclusion | **VALIDEZ CUESTIONABLE** | Posible violacion de exclusion |

### 6.4 Implicacion para el Diesel

El test de Sargan sugiere que el conjunto de instrumentos tiene problemas de validez. Dado que la unica diferencia con la especificacion anterior (donde Sargan p = 0.187) es la inclusion del diesel, es razonable concluir que:

**El precio del diesel no cumple la restriccion de exclusion.**

El diesel probablemente afecta el precio ex-vessel a traves de dos canales:
1. Via desembarques (canal valido): mas diesel caro = menos esfuerzo = menos captura.
2. Via costos de planta (canal invalido): mas diesel caro = mayores costos de procesamiento = mayor precio pagado para mantener margenes.

---

## 7. TEST DE HAUSMAN PARA EXOGENEIDAD DEL FOB (PASO 6)

### 7.1 Proposito

Este test evalua especificamente si el precio FOB internacional es exogeno, es decir, si Chile es un tomador de precios en el mercado mundial de harina de pescado.

### 7.2 Metodologia: Control Function Approach

**Paso 1: Primera Etapa**

Regresar ln_P_FOB sobre todos los instrumentos (incluyendo diesel):

```r
first_stage_fob <- lm(
  ln_P_FOB ~ ln_h_jurel + SST_MACRO + CHL_A_MACRO + ln_DIESEL + 
    ln_h_complejo_L2 + ln_h_complejo_L3 + 
    ln_h_peru + ln_tipo_cambio + MES_fact,
  data = df_clean
)
```

**Paso 2: Obtener Residuos**

```r
df_clean$resid_fob <- residuals(first_stage_fob)
```

**Paso 3: Incluir Residuos en Ecuacion Estructural**

```r
hausman_eq <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + 
    resid_fob + MES_fact,
  data = pdata_clean, 
  model = "within"
)
```

### 7.3 Resultado

**Errores CR2 (robustos para muestras pequenas):**
```
     Coef. Estimate   SE   t-stat  p-val (Satt)
resid_fob    0.335  0.26    1.29      0.235     
```

### 7.4 Interpretacion

| Elemento | Valor | Significado |
|----------|-------|-------------|
| Coeficiente | 0.335 | Correlacion entre residuos y error estructural |
| Error estandar | 0.26 | Incertidumbre del coeficiente |
| t-statistic | 1.29 | No significativo |
| p-value | 0.235 | > 0.10 |
| Decision | No se rechaza H0 | |
| Conclusion | **FOB es EXOGENO** | Chile es tomador de precios |

### 7.5 Comparacion con Wu-Hausman

Existe una aparente contradiccion:

| Test | p-value | Conclusion |
|------|---------|------------|
| Wu-Hausman (conjunto) | 0.0008 | Rechaza exogeneidad |
| Hausman FOB (individual) | 0.235 | No rechaza exogeneidad de FOB |

**Interpretacion:**

El test de Wu-Hausman evalua la exogeneidad conjunta de (ln_P_FOB, ln_h_complejo). Si rechaza, significa que al menos una de las dos es endogena.

El test de Hausman FOB evalua solo la exogeneidad de ln_P_FOB.

Si Wu-Hausman rechaza pero Hausman FOB no rechaza, la implicacion es que **ln_h_complejo es la variable endogena**, no ln_P_FOB.

Sin embargo, dado que los instrumentos para h_complejo son debiles (F = 2.25), el rechazo del Wu-Hausman puede ser espurio.

---

## 8. TEST DE WALD PARA INTEGRACION IMPERFECTA (PASO 7)

### 8.1 Proposito

Este test evalua si las cantidades locales afectan el precio ex-vessel, controlando por el precio FOB.

### 8.2 Hipotesis

| Hipotesis | Descripcion |
|-----------|-------------|
| H0 | gamma_complejo = gamma_jurel = 0 (plantas son price-takers puros) |
| H1 | Al menos un gamma diferente de cero (cantidades afectan precio) |

### 8.3 Implementacion

```r
library(car)

wald_test <- linearHypothesis(
  fe_ols,
  c("ln_h_complejo = 0", "ln_h_jurel = 0"),
  vcov. = vcov_dk,
  test = "F"
)
```

### 8.4 Resultado

```
F-statistic: 5.698
p-value: 0.0036
```

### 8.5 Interpretacion

| Criterio | Valor | Decision |
|----------|-------|----------|
| p-value | 0.0036 | < 0.01 |
| Decision | Se RECHAZA H0 al 1% | |
| Conclusion | Las cantidades procesadas afectan el precio | |
| Implicacion | Plantas locales NO son price-takers puros | |

---

## 9. MODELO FINAL Y COEFICIENTES

### 9.1 Justificacion del Modelo OLS-FE

A pesar del rechazo del Wu-Hausman (p = 0.0008), se adopta OLS-FE porque:

1. **Weak IV (F = 2.25):** Los instrumentos para h_complejo son demasiado debiles para generar estimaciones confiables.

2. **Coeficientes IV absurdos:** ln_P_FOB negativo no tiene interpretacion economica.

3. **Sargan marginal (p = 0.08):** El diesel probablemente viola la restriccion de exclusion.

4. **Hausman FOB (p = 0.235):** El FOB es exogeno, que es la variable de mayor interes.

### 9.2 Especificacion

```r
fe_ols <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + MES_fact,
  data = pdata,
  model = "within"
)

vcov_dk <- vcovSCC(fe_ols, maxlag = 3)
coef_ols <- coeftest(fe_ols, vcov = vcov_dk)
```

### 9.3 Coeficientes Finales

```
COEFICIENTES OLS-FE (Errores Driscoll-Kraay):
------------------------------------------------------------
                  Estimate Std. Error    t value     Pr(>|t|)
ln_P_FOB       0.466482665 0.16759975  2.7833136 0.0056326525
ln_h_complejo -0.061794380 0.01830548 -3.3757311 0.0008073094
ln_h_jurel    -0.009251307 0.01639958 -0.5641185 0.5729857502

R-squared within: 0.1242
```

### 9.4 Tabla de Resultados

| Variable | Coeficiente | Error Estandar | t-value | p-value | Significancia |
|----------|-------------|----------------|---------|---------|---------------|
| ln_P_FOB | 0.466 | 0.168 | 2.78 | 0.006 | *** |
| ln_h_complejo | -0.062 | 0.018 | -3.38 | 0.001 | *** |
| ln_h_jurel | -0.009 | 0.016 | -0.56 | 0.573 | |

Nota: *** p < 0.01, ** p < 0.05, * p < 0.10

---

## 10. DISCUSION: CONTRADICCION ENTRE WU-HAUSMAN Y HAUSMAN FOB

### 10.1 El Problema

Los tests arrojan resultados aparentemente contradictorios:

| Test | p-value | Conclusion |
|------|---------|------------|
| Wu-Hausman | 0.0008 | Endogeneidad presente |
| Hausman FOB | 0.235 | FOB exogeno |
| Sargan | 0.081 | IVs marginalmente validos |

### 10.2 Resolucion

La contradiccion se resuelve reconociendo que:

1. **Wu-Hausman es un test conjunto:** Evalua si (FOB, h_complejo) son conjuntamente exogenas. Si rechaza, al menos una es endogena.

2. **Hausman FOB es un test individual:** Evalua solo FOB. Si no rechaza, FOB es exogena.

3. **Implicacion logica:** Si Wu-Hausman rechaza y Hausman FOB no rechaza, entonces h_complejo es la variable endogena.

4. **Pero los IVs para h_complejo son debiles (F = 2.25):** Esto hace que el test de Wu-Hausman sea poco confiable para h_complejo.

5. **Y el Sargan es marginal (p = 0.08):** Sugiere que el diesel es un instrumento invalido.

### 10.3 Conclusion

El rechazo del Wu-Hausman probablemente refleja:
- Un instrumento invalido (diesel).
- Instrumentos debiles para h_complejo.
- NO endogeneidad genuina.

La evidencia mas confiable es el test de Hausman FOB (p = 0.235), que indica que Chile es tomador de precios en el mercado internacional.

---

## 11. INTERPRETACION ECONOMICA

### 11.1 Transmision del Precio Internacional (Pass-through)

**Coeficiente:** beta = 0.466 (p = 0.006)

**Interpretacion:**
- Por cada 10% de aumento en el precio FOB, el precio ex-vessel aumenta 4.7%.
- La transmision del precio internacional es del 47%.
- El 53% restante es absorbido por margenes o fricciones de mercado.

### 11.2 Efecto de los Desembarques Locales (Flexibilidad de Precio)

**Coeficiente:** gamma = -0.062 (p = 0.001)

**Interpretacion:**
- Por cada 10% de aumento en los desembarques, el precio ex-vessel disminuye 0.6%.
- El signo negativo es coherente con la teoria de demanda inversa.
- La magnitud pequena indica demanda relativamente inelastica.

### 11.3 Chile como Tomador de Precios

**Evidencia:** Test de Hausman para FOB (p = 0.235)

**Interpretacion:**
- Chile produce aproximadamente 15% del mercado mundial de harina.
- Peru domina con aproximadamente 60%.
- La produccion chilena no afecta el precio FOB internacional.

### 11.4 Integracion Imperfecta

**Evidencia:** Test de Wald (F = 5.70, p = 0.004)

**Interpretacion:**
- Las plantas NO son price-takers puros.
- Las condiciones locales de oferta generan desviaciones del precio que implicaria el arbitraje perfecto.
- FOB es el ancla de largo plazo; desembarques generan desviaciones de corto plazo.

---

## RESUMEN DE TESTS Y CONCLUSIONES

| Paso | Test | Estadistico | p-value | Conclusion |
|------|------|-------------|---------|------------|
| - | Breusch-Godfrey AR(1) | - | 0.000 | Autocorrelacion presente |
| - | Weak IV (FOB) | F = 45.17 | <0.001 | IV fuerte para FOB |
| - | Weak IV (h_complejo) | F = 2.25 | 0.030 | IV DEBIL para h |
| 8a | Wu-Hausman | F = 7.32 | 0.0008 | Rechaza (pero IV invalido) |
| 8b | Sargan | chi2 = 9.80 | 0.081 | IVs marginalmente validos |
| 6 | Hausman FOB | t = 1.29 | 0.235 | FOB exogeno |
| 7 | Wald (gamma = 0) | F = 5.70 | 0.004 | Cantidades afectan precio |

**Modelo Final:** OLS con Efectos Fijos y errores Driscoll-Kraay

**Coeficientes Clave:**
- Pass-through FOB: 0.47 (47% de transmision)
- Flexibilidad de precio: -0.06 (demanda inelastica)

---

## ARCHIVOS EXPORTADOS

| Archivo | Contenido |
|---------|-----------|
| `acf_pacf_residuos.png` | Grafico de autocorrelacion de residuos |
| `resumen_tests_678.csv` | Resultados de todos los tests de especificacion |
| `coeficientes_finales.csv` | Coeficientes OLS-FE con errores robustos |

---

## NOTA METODOLOGICA SOBRE EL DIESEL

La inclusion del diesel como instrumento genera resultados problematicos:

1. **Wu-Hausman cambia de p = 0.33 a p = 0.0008:** Un cambio tan drastico sugiere sensibilidad a la especificacion.

2. **Sargan empeora de p = 0.19 a p = 0.08:** Evidencia de que el diesel viola la restriccion de exclusion.

3. **Coeficientes IV se vuelven absurdos:** ln_P_FOB negativo.

**Recomendacion:** En futuras estimaciones, excluir el diesel del conjunto de instrumentos o usarlo solo en la primera etapa para FOB (no para h_complejo).

---

## REFERENCIAS

- Dresdner, J., Chavez, C., Quiroga, M., et al. (2014). Evaluacion socio-economica de la aplicacion de medidas de administracion sobre la pesqueria mixta de pequenos pelagicos de la zona centro sur. SUBPESCA.

- Stock, J. H., & Yogo, M. (2005). Testing for weak instruments in linear IV regression. In Identification and Inference for Econometric Models. Cambridge University Press.

- Driscoll, J. C., & Kraay, A. C. (1998). Consistent covariance matrix estimation with spatially dependent panel data. Review of Economics and Statistics, 80(4), 549-560.

- Bound, J., Jaeger, D. A., & Baker, R. M. (1995). Problems with instrumental variables estimation when the correlation between the instruments and the endogeneous explanatory variable is weak. Journal of the American Statistical Association, 90(430), 443-450.
