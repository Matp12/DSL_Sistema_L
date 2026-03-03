# DSL: Sistemas-L Composicionales

## Idea general
El presente lenguaje de dominio específico (DSL) permite definir, componer y visualizar Sistemas-L (Lindenmayer Systems), así como observar su evolución a partir de un axioma inicial.

El objetivo principal es proveer una herramienta declarativa para modelar procesos de crecimiento estructural (fractales, plantas, patrones recursivos) sin necesidad de implementar manualmente el mecanismo de derivación paralela.

El lenguaje permite:

Definir Sistemas-L determinísticos

Especificar axioma y reglas de producción

Ejecutar derivaciones paralelas

Interpretar el resultado mediante Turtle Graphics

Componer sistemas mediante operadores formales

## Fundamento teórico

Un Sistema-L extendido se define como:

**G = ⟨Σ, ω, P, I⟩**

Donde:

Σ es un conjunto finito de símbolos.

ω es el axioma (cadena inicial).

P: Σ → Σ* es la función de producción.

I = (angle, step, iterations) son parámetros de interpretación gráfica.

La derivación es paralela, es decir, en cada iteración todos los símbolos de la cadena son reemplazados simultáneamente según sus reglas de producción.

Formalmente:

ωₙ₊₁ = P(ωₙ)

## Alcances

Este DSL permite:

- Definir Sistemas-L determinísticos.

- Especificar parámetros de interpretación gráfica.

- Ejecutar múltiples iteraciones.

- Componer sistemas mediante:

  - Unión

  - Intercalación

  - Encapsulamiento

- Visualizar el resultado mediante Turtle Graphics.


## Operadores de composición

Sean:
G₁ = ⟨Σ₁, ω₁, P₁, I₁⟩
G₂ = ⟨Σ₂, ω₂, P₂, I₂⟩

- Unión :
Combina ambos sistemas unificando alfabetos y reglas:
G = G₁ ∪ G₂
Σ = Σ₁ ∪ Σ₂
ω = ω₁ ω₂
P = P₁ ∪ P₂

Los parámetros de interpretación (angle, step, iterations) son definidos por el sistema resultante.

- Intercalación :

Intercala los axiomas símbolo a símbolo:
w1 = a1a2a3....
w2 = b1b2b3....
ω = interleave(ω₁, ω₂) = a1b1a2b2a3b3....

Las reglas se unifican como en la unión.

- Encapsulamiento : 

Permite ejecutar un sistema dentro de otro:

A -> encaps(G₂)

Cuando aparece el símbolo A, se ejecuta el sistema G₂ con sus propios parámetros y el resultado reemplaza a A.

### Gramática
La siguiente gramatica define formalmente el lenguaje:
```
LSystem ::= BaseSystem
  | Lsystem union LSystem
  | Lsystem interleave LSystem

BaseSystem ::= lsystem Id {Body}

Body ::= axiom : Word

rules : RuleList

angle : Number

step : Number

iterations : Number

RuleList ::= Rule
  | RuleList Rule

Rule ::= Symbol -> Replace;

Replace ::= Word
  | encap(Lsystem)

Word ::= Word Symbol
| Symbol
```
donde Symbol es cualquier caracter en mayúscula, Id es una string sin espacios y number es
cualquier número
### Aclaraciones
Para el apartado gráfico en la palabra resultante el símbolo ’F’ significa dibujar hacia adelante, ’+’
significa vuelta hacia la izquierda, ’-’ significa vuelta hacia la derecha , ’[’ significa guardar el punto
actual , y ’]’ retomar el último punto. Cualquier otro símbolo para el apartado gráfico se
ignora por completo.

## Dependencias
El proyecto fue desarrollado en Haskell y requiere las siguientes librerías:
- base (>= 4.7 && < 5) (biblioteca estándar del lenguaje)
- containers (estructuras de datos como Map y Set)
- array (manejo de arreglos)
- directory (operaciones sobre el sistema de archivos)
- filepath (manipulación de rutas de archivos)
- mtl (soporte para transformadores de mónadas)
- gloss (renderizado gráfico utilizado para la visualización del sistema L)
- parallel (soporte para evaluación paralela)
El proyecto se compila utilizando Stack como herramienta de construcción.

## Manual de Uso
### Instrucciónes de compilación
Stack se encarga de instalar la versión correcta de GHC, instalar los paquetes necesarios y compilar el
proyecto. Para las primeras dos, basta con abrir una terminal en el directorio del proyecto y ejecutar:
```shell
stack setup
```
una vez listo se puede compilar con:
```shell
stack build
```
### Instrucciones de ejecución
Para ejecutar el programa basta con ejecutar:
```shell
stack run -- -[FLAGS] programa.lsys
```
Donde el programa se encuentra dentro de la carpeta Ejemplos y las opciones de flags son:
- -ext (exit del axioma luego de las iteraciones)
- -trz (traza de la evolución del sistema iteración por iteración)
- -grf (gráfica visual de la evolución del sistema)
### Manejo del programa
- Esc para salir de la visualización o en su defecto doble CTRL+C en la terminal
- Se puede desplazar arrastrando el click izquierdo del ratón
- Con la rueda realiza zoom

## Bibliografía
Para mayor conocimiento, puede consultar los siguientes articulos de Wikipedia que sirvieron de ayuda
e inspiracion en este trabajo:

- Wikipedia Sistema-L : https://es.wikipedia.org/wiki/Sistema-L
- Introducción a Sistema-L y Fractales: https://es.scribd.com/document/650879180/L-SYSTEM-Trabajo-Final-profesorado
- Copo de nieve de Koch: https://es.wikipedia.org/wiki/Copo_de_nieve_de_Koch
- Triángulo de Sierpinski: https://es.wikipedia.org/wiki/Tri%C3%A1ngulo_de_Sierpinski
