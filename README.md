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

- Encapsulamiento

Permite ejecutar un sistema dentro de otro:

A -> encaps(G₂)

Cuando aparece el símbolo A, se ejecuta el sistema G₂ con sus propios parámetros y el resultado reemplaza a A.

En este caso, el sistema encapsulado conserva sus propios valores de angle, step e iterations.
