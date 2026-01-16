****************************************************
* ÍNDICE DE RIQUEZA (PCA) - STATA 17
* Versión: paredes, techo, piso, agua, baño,
* alumbrado, combustible + habitaciones
* (sin seguros de salud)
****************************************************

*---------------------------------------------------
* 1. Abrir base de datos
*---------------------------------------------------
clear all
cls
log using "C:\Users\RODRIGO\Downloads\indice_riqueza_resultados.txt", text replace
use "D:\Academic\Nutritional epidemiology projects\ferritin-anemia_diagnosis\data\base_de_curvas.dta", clear

* Revisión rápida
describe
count

*---------------------------------------------------
* 2. Mantener solo observaciones con info completa
*    de vivienda/servicios básicos
*---------------------------------------------------
drop if missing(P018, P019, P020, P021, P023, P024, P025, P026, P027)
count
*---------------------------------------------------
* Limpieza previa de códigos "Otro" o no válidos
*---------------------------------------------------
recode P018 (77 = .)
recode P019 (77 = .)
recode P020 (77 = .)
recode P021 (77 = .)
recode P023 (77 = .)
recode P024 (77 = .)
recode P025 (77 = .)

*---------------------------------------------------
* 3. RECODIFICACIÓN ORDENADA DE VARIABLES
*    (0 = mejor, valores mayores = más precario)
*---------------------------------------------------

*---------------------------------------------------
* 3.1 Paredes (P018)
*    labels12:
*    1 Ladrillo/bloque cemento
*    2 Piedra/sillar con cal o cemento
*    3 Adobe/tapia
*    4 Quincha (caña con barro)
*    5 Madera
*    6 Piedra con barro
*    7 Triplay
*    8 Estera
*    9 Otro
*---------------------------------------------------

recode P018 ///
    (1                    = 0 "Ladrillo/bloque cemento (mejor)") ///
    (2 5 7                = 1 "Piedra/sillar con cal, madera, triplay") ///
    (3                    = 2 "Adobe/tapia") ///
    (4 6 8 9              = 3 "Quincha/estera/piedra con barro/otro (peor)"),    gen(walls_ord)

label var walls_ord "Calidad paredes (0=mejor,3=peor)"

*---------------------------------------------------
* 3.2 Techos (P019)
*    labels13:
*    1 Concreto armado
*    2 Madera
*    3 Tejas
*    4 Calamina/fibrocemento
*    5 Caña/estera con torta de barro
*    6 Otro
*---------------------------------------------------

recode P019 ///
    (1 3                 = 0 "Concreto/tejas (mejor)") ///
    (4                   = 1 "Calamina/fibrocemento") ///
    (2                   = 2 "Madera") ///
    (5 6                 = 3 "Caña/estera/otro (peor)"), ///
    gen(roof_ord)

label var roof_ord "Calidad techo (0=mejor,3=peor)"

*---------------------------------------------------
* 3.3 Pisos (P020)
*    labels14:
*    1 Parquet/madera pulida
*    2 Vinílico/laminas asfálticas
*    3 Losetas/terrazos
*    4 Cemento/ladrillo
*    5 Madera entablado
*    6 Pona
*    7 Otro
*---------------------------------------------------

recode P020 ///
    (1 2 3              = 0 "Parquet/loseta/vinílico (mejor)") ///
    (4                  = 1 "Cemento/ladrillo") ///
    (5                  = 2 "Madera entablada") ///
    (6 7                = 3 "Pona/otro (peor)"), ///
    gen(floor_ord)

label var floor_ord "Calidad piso (0=mejor,3=peor)"

*---------------------------------------------------
* 3.4 Agua (P021)  *** REVISADA ***
*    labels15 (según tu codebook/tab):
*    1 Dentro de la vivienda
*    2 Fuera de la vivienda pero dentro del edificio
*    3 Pilón/grifo público
*    4 Pozo en casa/patio/lote
*    5 Pozo público
*    6 Manantial (puquio)
*    7 Río/acequia/laguna
*    9 Camión/aguatero
*    10 Agua embotellada
*    77 Otro
*
* LÓGICA:
*   0: conexión en vivienda/edificio (1, 2)
*   1: pilón, camión, embotellada, otro (3, 9, 10, 77)
*   2: pozos, manantial, río (4, 5, 6, 7)
*---------------------------------------------------

recode P021 ///
    (1 2           = 0 "Conexión vivienda/edificio (mejor)") ///
    (3 9 10      = 1 "Pilón/camión/embotellada") ///
    (4 5 6 7       = 2 "Pozo/manantial/río (peor)"), ///
    gen(water_ord)

label var water_ord "Calidad acceso agua (0=mejor,2=peor)"

* (Opcional) Chequear recodificación
* tab P021 water_ord, m

*---------------------------------------------------
* 3.5 Baño (P023)
*    labels17:
*    1 Dentro de la vivienda
*    2 Fuera de la vivienda
*    3 Letrina (pautas técnicas)
*    4 Letrina (sin pautas)
*    5 Pozo ciego/negro/silo
*    6 No hay servicio (matorral/campo)
*    7 Otro
*
* LÓGICA:
*   0: 1  (dentro vivienda)
*   1: 2  (fuera vivienda, pero servicio)
*   2: 3 4 5 (letrinas/pozo ciego)
*   3: 6 7 (no hay servicio/otro muy precario)
*---------------------------------------------------

recode P023 ///
    (1             = 0 "Baño dentro vivienda (mejor)") ///
    (2             = 1 "Baño fuera vivienda") ///
    (3 4 5         = 2 "Letrinas/pozo ciego") ///
    (6 7           = 3 "Sin servicio/otro (peor)"), ///
    gen(toilet_ord)

label var toilet_ord "Tipo de baño (0=mejor,3=peor)"

*---------------------------------------------------
* 3.6 Alumbrado (P024)
*    labels18:
*    0 categoría residual rara
*    1 Electricidad
*    2 Mechero/lamparín/lámpara
*    3 Otro
*    4 Ninguno
*
* LÓGICA:
*   0: electricidad
*   1: mechero/otro
*   2: ninguno
*   (puedes fusionar 0 con 1 o tratarlo como 'otro')
*---------------------------------------------------

recode P024 ///
    (1             = 0 "Electricidad (mejor)") ///
    (2 3 0         = 1 "Mechero/otro") ///
    (4             = 2 "Ninguno (peor)"), ///
    gen(light_ord)

label var light_ord "Tipo de alumbrado (0=mejor,2=peor)"

*---------------------------------------------------
* 3.7 Combustible para cocinar (P025)
*    labels19:
*    1 Electricidad
*    2 Gas
*    3 Carbón
*    4 Leña
*    5 Bosta
*
* LÓGICA:
*   0: electricidad/gas
*   1: carbón
*   2: leña/bosta
*---------------------------------------------------

recode P025 ///
    (1 2          = 0 "Electricidad/gas (mejor)") ///
    (3            = 1 "Carbón") ///
    (4 5          = 2 "Leña/bosta (peor)"), ///
    gen(fuel_ord)

label var fuel_ord "Combustible cocinar (0=mejor,2=peor)"

*---------------------------------------------------
* 3.8 Habitaciones (P026, P027) - continuas
*---------------------------------------------------
label var P026 "Nº habitaciones (sin baño/cocina)"
label var P027 "Nº habitaciones solo dormir"

drop if missing(walls_ord , roof_ord , floor_ord , water_ord , toilet_ord , light_ord , fuel_ord , P026 , P027)
count
*---------------------------------------------------
* 4. Definir varlist para el PCA
*---------------------------------------------------
local assets ///
    walls_ord ///
    roof_ord ///
    floor_ord ///
    water_ord ///
    toilet_ord ///
    light_ord ///
    fuel_ord ///
    P026 P027

di "Variables incluidas en el PCA:"
di "`assets'"

* Resumen rápido
summ `assets'

*---------------------------------------------------
* 5. PCA (Filmer & Pritchett style)
*---------------------------------------------------
* Stata 17: no usar components()
pca `assets'

* Opcional: ver eigenvalues y cargas
estat loadings

pca `assets', correlation

matrix list e(L)
*---------------------------------------------------
* 6. Construir índice de riqueza a partir de Comp1
*---------------------------------------------------

* Puntaje del primer componente
predict wealth_pc1_raw, score    // primer componente (por defecto)

label var wealth_pc1_raw "Puntaje Comp1 (bruto, mayor = más precario si cargas positivas en 'peor')"

* Invertir signo para que mayor índice = MÁS RICO
gen wealth_pc1 = -1 * wealth_pc1_raw
label var wealth_pc1 "Índice riqueza (Comp1 invertido: mayor = más rico)"

* Estandarizar (media 0, DE 1)
egen wealth_std = std(wealth_pc1)
label var wealth_std "Índice riqueza estandarizado (media 0, DE 1)"

summ wealth_pc1 wealth_std

*---------------------------------------------------
* 7. Quintiles y grupos 40–20–40
*---------------------------------------------------

* 7.1 Quintiles del índice
xtile wealth_q = wealth_std, n(5)
label define wealthq 1 "Q1 (más pobre)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (más rico)"
label values wealth_q wealthq

tab wealth_q

* 7.2 Tres grupos: 40% pobre, 20% medio, 40% rico
gen wealth_3 = .
replace wealth_3 = 1 if wealth_q <= 2    // Q1 + Q2
replace wealth_3 = 2 if wealth_q == 3    // Q3
replace wealth_3 = 3 if wealth_q >= 4    // Q4 + Q5

label define wealth3 1 "Bajo (40% pobre)" 2 "Medio (20%)" 3 "Alto (40% rico)"
label values wealth_3 wealth3

tab wealth_3

*---------------------------------------------------
* 8. Validación básica del índice
*---------------------------------------------------

* Agua por quintil (ordinal)
tab wealth_q water_ord, row

* Paredes por quintil
tab wealth_q walls_ord, row

* Baño por quintil
tab wealth_q toilet_ord, row

* Combustible por quintil
tab wealth_q fuel_ord, row

* Habitaciones (media por quintil)
table wealth_q, ///
    stat(mean P026 ) ///
    stat(mean P027 )

* Gráfico de sedimentación (para decidir cuántos componentes dejar).
screeplot

* Muestra la matriz de pesos (la "receta" del índice).
estat loadings

* Visualización de las contribuciones de los activos.
loadingplot

* Genera el índice de activos (usualmente el PC1).
predict index, score

****************************************************
* FIN DEL DO-FILE
****************************************************
log close