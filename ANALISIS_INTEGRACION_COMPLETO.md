---
output:
  pdf_document: default
  html_document: default
---
# ANÁLISIS EXHAUSTIVO: INTEGRACIÓN PRECIOS + DESEMBARQUES

## RESUMEN EJECUTIVO

Tu script de integración (`03_integracion_base_final.R`) es **EXCELENTE** y demuestra comprensión profunda del problema. Generaste 3 bases estratégicamente diseñadas, cada una con un propósito específico.

### Bases Generadas

| Base | Observaciones | Nivel | Panel | Uso Recomendado |
|------|---------------|-------|-------|-----------------|
| **Regional** | 341 | Mes-Región-Especie | Desbalanceado | Heterogeneidad espacial |
| **Macro Completa** | 204 | Mes-Especie | Desbalanceado | Análisis general |
| **Macro Balanceada** | 54 | Mes-Especie | **PERFECTAMENTE BALANCEADO** | **IAIDS** |

---

##  HALLAZGOS CRÍTICOS

### 1. PROBLEMA DE COMPLETITUD: SOLO 18 MESES BALANCEADOS

**HALLAZGO MÁS IMPORTANTE:**

De 107 meses únicos con datos, **SOLO 18 meses (16.8%)** tienen las 3 especies simultáneamente.

#### Distribución Temporal de Meses Balanceados

```
Año    Meses Balanceados
----   -----------------
2012   1  (Marzo)
2013   0  
2014   0  
2015   0  
2016   1  (Marzo)
2017   1  (Mayo)
2018   1  (Febrero)
2019   0  
2020   5  (Feb, Mar, Abr, May, Jun)  ← Mejor año
2021   0  
2022   2  (Nov, Dic)
2023   4  (Mar, Abr, May, Jun)
2024   3  (Mar, Abr, May)
----   -----------------
TOTAL  18 meses en 13 años
```

** Implicaciones:**

1. **Base balanceada es PEQUEÑA:** Solo 54 observaciones (18 meses × 3 especies)
2. **Periodo NO es continuo:** Hay brechas de años completos (2013-2015, 2019, 2021)
3. **Sesgo temporal:** 78% de datos están en 2020-2024 (período reciente)
4. **Sesgo estacional:** Concentración en marzo-junio (temporada alta)

---

### 2. CALIDAD DEL MATCH PRECIO-CANTIDAD

#### Match Regional (nivel más desagregado)

```
Match Rate: 341 / 1,468 = 23.2%

Obs con PRECIO y CANTIDAD: 341  
Obs solo con PRECIO:       0
Obs solo con CANTIDAD:     1,127
Total:                     1,468
```

**Interpretación:**
- **Excelente:** No hay precios huérfanos (todas las obs de precio tienen cantidad)
- **Esperado:** Muchos desembarques sin precio (encuestas de precio son muestrales)

#### Match Macrozonal

```
Precios macro:       204 obs
Desembarques macro:  464 obs
Match:               204 obs (100% de precios)
```

**Conclusión:** El match es **perfecto por diseño**. Los 204 meses-especie con precio SIEMPRE tienen desembarques.

---

### 3. CONSISTENCIA DE AGREGACIÓN

#### Precios: Regional → Macrozonal

```
Diferencia media:   0.00%  
Diferencia máxima:  0.00%  
```

**Perfecto:** La ponderación por `Q_MUESTRA_PLANTAS` funciona correctamente.

#### Cantidades: Regional → Macrozonal

```
Diferencia media:   19.39%  
Diferencia máxima:  99.88%  
```

**PROBLEMA DETECTADO:**

Esta discrepancia se debe a que:
- **Base regional:** Solo tiene los 341 mes-región-especie que TAMBIÉN tienen precio
- **Base macro:** Suma TODOS los desembarques (incluso sin precio)

**Ejemplo del problema:**

```
Mes 2020-03, ANCHOVETA:
 Regional completo suma: 50,000 ton (de regiones con precio)
 Macro suma:            100,000 ton (de TODAS las regiones)

Diferencia: 50% (porque faltan regiones sin datos de precio)
```

** Implicación:** 
- Para análisis de **mercado de precios**, usa base regional o macro (son coherentes)
- Para análisis de **oferta total**, la macro completa captura MÁS desembarques

---

### 4. DISTRIBUCIÓN DE MESES POR COMPLETITUD

#### Base Macrozonal Completa (204 obs en 107 meses)

```
Meses con:
 1 especie:  28 meses (26.2%)
2 especies: 61 meses (57.0%)
 3 especies: 18 meses (16.8%)   Solo estos en base balanceada
```

**Problema IAIDS:**
- El modelo IAIDS requiere que **TODAS** las especies estén presentes
- El 83.2% de tus datos NO califica para IAIDS
- Alternativas:
  1. **Usar base balanceada** (54 obs, limitado pero correcto)
  2. **Imputar precios faltantes** (riesgoso, introduce supuestos)
  3. **Modelo AIDS parcial** (estimar con especies disponibles en cada período)

---

### 5. ANÁLISIS DE PRECIOS

#### Estadísticas Descriptivas (Base Balanceada)

| Especie | N | Media ($/ton) | SD | CV | Mín | Máx |
|---------|---|---------------|----|----|-----|-----|
| **Anchoveta** | 18 | 151,781 | 36,592 | 24.1% | 62,695 | 215,570 |
| **Jurel** | 18 | 219,157 | 76,694 | **35.0%** | 59,000 | 300,000 |
| **Sardina** | 18 | 150,271 | 35,260 | 23.5% | 63,621 | 215,570 |

**Hallazgos:**
1. **Jurel es MÁS CARO:** +45% vs anchoveta/sardina (mayor contenido graso)
2. **Jurel es MÁS VOLÁTIL:** CV=35% (mayor variabilidad de mercado)
3. **Anchoveta y sardina tienen precios similares** (sustitutos en reducción)

#### Evolución Temporal de Precios

```
Año    Anchoveta   Jurel    Sardina
----   ---------   -------  --------
2012   $62,695     $59,000  $63,621  ← Precios bajos (referencia)
2016   $120,801    $114,000 $121,408 ← Dobla (↑90%)
2017   $93,863     $120,000 $103,010 ← Caída
2018   $215,570    $230,000 $215,570 ← Pico histórico (↑245%)
2020   $149,826    $174,365 $142,255 ← Normalización
2022   $140,000    $245,000 $140,000
2023   $160,000    $290,000 $160,000 ← Jurel sube solo
2024   $190,000    $300,000 $190,000 ← Máximo jurel
```

**Tendencias:**
- **2012-2018:** Crecimiento explosivo (+245%)
- **2018:** Pico sincronizado (¿boom de harina internacional?)
- **2019-2024:** Divergencia (jurel sube, sardina/anchoveta estables)

---

### 6. ANÁLISIS DE CANTIDADES

#### Desembarques por Año (Base Balanceada)

```
Año    Anchoveta  Jurel     Sardina
----   ---------  --------  ---------
2012   6,730      25,882    246,996   ← Boom sardina
2016   30,622     23,188    40,715
2017   8,768      70,295    30,950
2018   2,142      50,864    15,462    ← Colapso sardina
2020   118,707    359,308   191,763   ← Boom todo
2022   23,647     58,053    33,226
2023   156,126    360,130   358,354   ← Máximo histórico
2024   97,533     312,394   83,439
```

**Hallazgos:**
1. **Sardina colapsa:** 247k (2012) → 15k (2018) = -94%
2. **Jurel explota:** 26k (2012) → 360k (2023) = +1,290%
3. **Anchoveta crece:** 7k (2012) → 156k (2023) = +2,130%
4. **2020 es excepcional:** Boom simultáneo en todas las especies

---

### 7. CORRELACIÓN PRECIO-CANTIDAD ⚠

#### Correlaciones por Especie (Base Balanceada)

```
Especie          Corr(P,Q)   Corr(log P, log Q)
--------------   ---------   ------------------
ANCHOVETA        +0.357      +0.282
JUREL            +0.577      +0.295
SARDINA COMÚN    -0.419      -0.377  ← ¡NEGATIVA!
```

** HALLAZGO CRÍTICO:**

Las correlaciones son **OPUESTAS a la teoría económica clásica**:

**Esperado (Curva de oferta hacia arriba):**
```
↑ Cantidad → ↓ Precio (exceso de oferta)
Correlación negativa
```

**Observado:**
```
Anchoveta: ↑ Cantidad → ↑ Precio (+0.36)  
Jurel:     ↑ Cantidad → ↑ Precio (+0.58)  
Sardina:   ↑ Cantidad → ↓ Precio (-0.42)  ✓ (coherente)
```

**Posibles Explicaciones:**

1. **Variable omitida: Precio internacional de harina**
   - Si precio mundial ↑ → incentiva captura ↑ Y precio local ↑
   - Ambos efectos van en la misma dirección

2. **Demanda muy elástica**
   - Mercado de reducción puede absorber toda la oferta
   - Precio está anclado al mercado internacional

3. **Efectos temporales confundidos**
   - Tendencia al alza en ambas variables (spurious correlation)
   - Necesitas controlar por tendencia temporal

4. **Calidad del recurso**
   - Años con alto reclutamiento → + cantidad + mejor calidad → + precio
   - % grasa varía por temporada

**Recomendación:**
- Incluir **precio internacional de harina** como variable de control
- Usar **diferencias primeras** o **tendencia temporal**
- Modelo con **efectos fijos por año**

---

### 8. DISPERSIÓN REGIONAL DE PRECIOS

#### Alta Dispersión (CV > 30%)

**Frecuencia:** 8 de 204 observaciones (3.9%)

**Top casos:**

```
Año-Mes  Especie    CV    N_Regiones  Precio_Medio
-------- ---------  ----  ----------  ------------
2019-07  Sardina    84.5% 3           $131,934
2019-11  Sardina    78.7% 3           $144,046
2019-12  Sardina    78.2% 3           $142,656
2019-07  Anchoveta  42.8% 3           $144,208
```

**Patrón:**
- **Concentrado en 2019** (¿evento específico?)
- **Sardina es más volátil regionalmente**
- **3 regiones con precios muy diferentes** (segmentación de mercado)

**Interpretación:**
- Posible **calidad diferencial** por región
- O **mercados parcialmente segmentados**
- O **error de medición** (encuestas pequeñas)

---

### 9. ANÁLISIS DE OUTLIERS

#### Frecuencia General

```
Base Macro Completa (204 obs):
├─ Outliers de PRECIO:    1  (0.5%)   ✓ Muy pocos
└─ Outliers de CANTIDAD:  150 (73.5%)  ¡Mayoría!
```

**💡 Hallazgo Importante:**

- **Precios son muy estables** (1 outlier en 204 obs)
- **Cantidades son muy volátiles** (150 outliers en 204 obs)

**Implicación:**
- Los "outliers" de cantidad son **EVENTOS REALES** (pulsos de captura, temporada excepcional)
- **NO eliminarlos** (son informativos)
- Pero usar **modelos robustos** (quantile regression, GLM gamma)

#### Outliers por Especie (Base Balanceada)

```
Especie          Outliers_Precio  Outliers_Cantidad
-------------   ---------------  -----------------
ANCHOVETA       0                20 / 18 obs (111%)
JUREL           0                13 / 18 obs (72%)
SARDINA COMÚN   0                21 / 18 obs (117%)
```

**Nota:** El número de outliers puede exceder N obs porque es suma acumulada de outliers regionales.

---

### 10. COMPOSICIÓN DE FLOTAS

#### Participación Industrial Promedio

```
Especie          % Industrial
--------------   ------------
ANCHOVETA        2.7%   (97.3% artesanal)
JUREL            93.8%  (6.2% artesanal)
SARDINA COMÚN    5.3%   (94.7% artesanal)
```

**Conclusión:**
- **Jurel = Flota industrial**
- **Sardina y Anchoveta = Flota artesanal**

#### Evolución Temporal (% Industrial)

```
Año   Anchoveta  Jurel  Sardina
----  ---------  -----  -------
2012  15.7%      98.8%  5.7%
2016  2.2%       99.2%  33.7%  ← Sardina tuvo pico industrial
2017  4.1%       99.6%  0.2%
2018  17.8%      97.2%  44.7%  ← Otro pico sardina
2020  0.0%       98.9%  0.0%   ← Artesanal puro
2022  1.2%       55.1%  0.1%   ← ¡Jurel cae a 55%!
2023  0.9%       98.8%  1.8%   ← Jurel recupera
2024  0.9%       97.8%  1.4%
```

**Hallazgos:**
1. **2022 es ANÓMALO:** Jurel industrial cae a 55% (¿regulación, veda, crisis?)
2. **Sardina fluctúa:** 0.2% → 44.7% → 0.1% (muy variable)
3. **Anchoveta estable:** Siempre <18% industrial

---

##  EVALUACIÓN METODOLÓGICA DEL CÓDIGO

### Fortalezas del Script

1. **Estrategia dual bien pensada:**
   - Regional para heterogeneidad espacial
   - Macrozonal para IAIDS

2. **Ponderación correcta:**
   ```r
   PRECIO_MACRO = weighted.mean(PRECIO_PONDERADO, w = Q_MUESTRA_PLANTAS)
   ```
   Pondera por volumen de muestra, no por número de regiones.

3. **Indicadores de calidad incluidos:**
   - `CV_PRECIO_REGIONAL`: Detecta segmentación
   - `N_OUTLIERS_PRECIO/CANTIDAD`: Flaggea eventos extremos
   - `N_REGIONES_PRECIO/DESEMB`: Documenta cobertura

4. **Tres outputs diferenciados:**
   - Regional (análisis espacial)
   - Macro completa (máxima data)
   - Macro balanceada (IAIDS)

5. **Validaciones exhaustivas:**
   - Diferencias regional vs macro
   - Rangos razonables
   - Casos especiales documentados

###  Áreas de Mejora

#### 1. Imputación de Precios Faltantes (Opcional)

**Problema:** Solo 18/107 meses tienen 3 especies.

**Solución posible:**
```r
# Imputar con media móvil regional
df_precios_imputado <- df_precios %>%
  group_by(RG, NM_RECURSO) %>%
  arrange(ANIO, MES) %>%
  mutate(
    PRECIO_IMPUTADO = zoo::na.approx(PRECIO_PONDERADO, na.rm = FALSE),
    PRECIO_ORIGINAL = !is.na(PRECIO_PONDERADO)
  )
```

**Ventajas:**
- Crea panel más completo (maybe 50-60 meses balanceados)

**Desventajas:**
- Introduce supuestos (interpolación lineal)
- Debes reportar sensibilidad con/sin imputación

#### 2. Deflactar Precios

**Problema:** Serie 2012-2024 (12 años, inflación acumulada ~60% en Chile).

**Solución:**
```r
df_macro <- df_macro %>%
  left_join(ipc_data, by = "ANIO") %>%
  mutate(
    PRECIO_REAL = PRECIO_MACRO * (IPC_2024 / IPC),
    PRECIO_NOMINAL = PRECIO_MACRO
  )
```

#### 3. Agregar Precio Internacional

**Fuente:** IFFO (harina de pescado Perú super prime)

```r
# Merge con serie internacional
df_final <- df_macro %>%
  left_join(precio_harina_mundial, by = c("ANIO", "MES"))
```

**Importancia:** El precio ex-vessel está **fuertemente correlacionado** con precio de harina.

#### 4. Variables Derivadas

```r
# Rezagos
df_final <- df_final %>%
  group_by(NM_RECURSO) %>%
  arrange(FECHA) %>%
  mutate(
    P_LAG1 = lag(PRECIO_MACRO, 1),
    Q_LAG1 = lag(Q_MACRO, 1),
    
    # Promedio móvil
    P_MA3 = (PRECIO_MACRO + lag(PRECIO_MACRO,1) + lag(PRECIO_MACRO,2)) / 3,
    
    # Variación mensual
    P_CRECIMIENTO = (PRECIO_MACRO / lag(PRECIO_MACRO, 1) - 1) * 100
  )
```

---

## PROBLEMAS IDENTIFICADOS Y SOLUCIONES

### Problema 1: Base Balanceada MUY PEQUEÑA (54 obs)

**Impacto:**
- IAIDS requiere estimar múltiples parámetros
- Con solo 54 obs, poder estadístico es bajo
- Intervalos de confianza serán amplios

**Soluciones:**

**Opción A: Imputar precios faltantes**
```r
# Pros: Sube a ~150-180 obs
# Contras: Introduce supuestos
# Recomendación: Hacer y reportar robustez
```

**Opción B: Ampliar ventana temporal**
```r
# Incluir 2010-2011 si hay datos
# Pros: Más observaciones
# Contras: Puede haber cambios estructurales
```

**Opción C: Modelo parcial**
```r
# Estimar con pares de especies cuando solo 2 disponibles
# Pros: Usa más data (107 meses)
# Contras: Más complejo, resultados menos directos
```

**Recomendación:** 
- **Principal:** Usa base balanceada (54 obs) con resultados cautelosos
- **Robustez:** Re-estima con base imputada

---

### Problema 2: Correlaciones Precio-Cantidad Positivas

**Diagnóstico:** Variable omitida (precio internacional).

**Solución:**
```r
# Modelo con precio harina
lm(log(PRECIO_MACRO) ~ log(Q_MACRO) + log(PRECIO_HARINA_INTL) + 
                       factor(NM_RECURSO) + factor(MES))
```

**Esperado:** 
- `log(Q_MACRO)` se vuelve negativo (correcto)
- `log(PRECIO_HARINA_INTL)` captura tendencia común

---

### Problema 3: Diferencias Cantidad Regional vs Macro

**Causa:** Base regional solo suma regiones CON precio.

**Solución:** Documenta claramente en tu tesis:

> "La base macrozonal agrega desembarques de TODAS las regiones del centro-sur, mientras que la base regional solo incluye regiones con datos de precio disponibles. Por tanto, los desembarques macrozonas son sistemáticamente mayores (promedio +19%) que la suma regional reportada en base_regional.csv. Esta diferencia es esperada y no constituye un error."

---

## CHECKLIST ANTES DE ESTIMAR IAIDS

### Datos
- [x] Base balanceada con 3 especies × N meses
- [x] Precios y cantidades positivos
- [x] Sin valores faltantes
- [ ] Precios deflactados (PENDIENTE - recomendado)
- [ ] Precio internacional incluido (PENDIENTE - crítico)

### Variables Derivadas
- [ ] Shares de gasto calculados
- [ ] Índice de precios Stone
- [ ] Variables de control (tendencia, estacionalidad)

### Validaciones
- [x] Consistencia agregación regional-macro
- [x] Rangos razonables de precios/cantidades
- [x] Outliers identificados
- [ ] Multicolinealidad verificada
- [ ] Estacionariedad testada

---

## ESTADÍSTICAS FINALES: BASE MACROZONAL BALANCEADA

### Periodo y Cobertura

```
Total observaciones: 54 (18 meses × 3 especies)
Período: 2012-2024 (13 años, pero solo 18 meses con datos completos)
Distribución temporal:
  - 2012-2018: 4 meses (22%)
  - 2020-2024: 14 meses (78%)  ← Sesgo reciente
```

### Precios ($/tonelada)

| Especie | Media | Mediana | Min | Max | CV |
|---------|-------|---------|-----|-----|----|
| Anchoveta | 151,781 | 149,826 | 62,695 | 215,570 | 24% |
| Jurel | 219,157 | 217,696 | 59,000 | 300,000 | 35% |
| Sardina | 150,271 | 150,636 | 63,621 | 215,570 | 23% |

### Cantidades (toneladas/mes)

| Especie | Media | Mediana | Min | Max | CV |
|---------|-------|---------|-----|-----|----|
| Anchoveta | 24,682 | 15,957 | 1,274 | 118,707 | 152% |
| Jurel | 70,006 | 54,458 | 23,188 | 360,130 | 140% |
| Sardina | 55,606 | 33,728 | 15,462 | 358,354 | 158% |

**Nota:** Alta variabilidad en cantidades (CV >140%) vs baja en precios (CV <35%).

---

##  RECOMENDACIONES FINALES

### Para Estimación IAIDS

1. **Usa base_macrozonal_balanceada.csv**
   - Es perfectamente balanceada (requisito IAIDS)
   - Tiene 54 obs (suficiente pero justo)

2. **Deflacta precios ANTES de estimar**
   - Fundamental para serie 2012-2024
   - Usa IPC Chile o Índice de Precios al Productor

3. **Agrega precio internacional harina**
   - Variable de control crítica
   - Explica tendencia común en precios

4. **Especificación recomendada:**
   ```
   Share_i = α_i + Σ γ_ij log(P_j) + β_i log(Y/P*) + 
             δ_i PRECIO_HARINA + θ_i TENDENCIA + ε_i
   ```

5. **Análisis de robustez:**
   - Con/sin outliers
   - Con/sin controles temporales
   - Ventana móvil (últimos 5 años vs serie completa)

### Para tu Tesis

1. **Documenta limitaciones:**
   - Solo 18 meses balanceados de 107 disponibles (16.8%)
   - Sesgo temporal hacia 2020-2024 (78% de datos)
   - Correlaciones precio-cantidad contraintuitivas

2. **Explica decisiones metodológicas:**
   - Por qué macrozonal vs regional
   - Por qué base balanceada (requisito técnico IAIDS)
   - Cómo trataste outliers (flaggeados, no eliminados)

3. **Reporta tabla comparativa:**
   ```
   | Modelo | Base | N | R² | Elasticidades |
   |--------|------|---|----|--------------| 
   | ...
   ```

4. **Análisis de sensibilidad:**
   - Tabla con resultados usando las 3 bases
   - Muestra robustez (o falta de ella)

---



Tu trabajo de integración es **EXCELENTE**. El código está bien estructurado, las validaciones son exhaustivas, y las decisiones metodológicas son correctas.


