

# Proyecto de Simulación y Programación Declarativa

#																						   Agentes





## 								Andy A. Castañeda Guerra C412





### Problema

​	El ambiente en el cual intervienen los agentes es discreto y tiene la forma de un rectángulo de N × M . El ambiente es de información completa, por tanto todos los agentes conocen toda la información sobre el agente. El ambiente puede
variar aleatoriamente cada t unidades de tiempo. El valor de t es conocido. Las acciones que realizan los agentes ocurren por turnos. En un turno, los agentes realizan sus acciones, una sola por cada agente, y modifican el medio sin que este varı́e a no ser que cambie por una acción de los agentes. En el siguiente, el ambiente puede variar. Si es el momento de cambio del ambiente,
ocurre primero el cambio natural del ambiente y luego la variación aleatoria. En una unidad de tiempo ocurren el turno del agente y el turno de cambio del ambiente.

​	Los elementos que pueden existir en el ambiente son obstáculos, suciedad, niños, el corral y los agentes que son llamados Robots de Casa. A continuación se precisan las caracterı́sticas de los elementos del ambiente:
**Obstáculos**: estos ocupan una única casilla en el ambiente. Ellos pueden ser movidos, empujándolos, por los niños, una única casilla. El Robot de Casa sin embargo no puede moverlo. No pueden ser movidos ninguna de las casillas ocupadas por cualquier otro elemento del ambiente.
**Suciedad**: la suciedad es por cada casilla del ambiente. Solo puede aparecer en casillas que previamente estuvieron vacı́as. Esta, o aparece en el estado inicial o es creada por los niños.
**Corral**: el corral ocupa casillas adyacentes en número igual al del total de niños presentes en el ambiente. El corral no puede moverse. En una casilla del corral solo puede coexistir un niño. En una casilla del corral, que esté vacı́a, puede entrar un robot. En una misma casilla del corral pueden coexistir un niño y un robot solo si el robot lo carga, o si acaba de dejar al niño.
**Niño**: los niños ocupan solo una casilla. Ellos en el turno del ambiente se mueven, si es posible (si la casilla no está ocupada: no tiene suciedad, no está el corral, no hay un Robot de Casa), y aleatoriamente (puede que no ocurra movimiento), a una de las casilla adyacentes. Si esa casilla está ocupada por un obstáculo este es empujado por el niño, si en la dirección hay más de un obstáculo, entonces se desplazan todos. Si el obstáculo está en una posición donde no puede ser empujado y el niño lo intenta, entonces el obstáculo no se mueve y el niño ocupa la misma posición. Los niños son los responsables de que aparezla suciedad. Si en una cuadrícula de 3 por 3 hay un solo niño, entonces, luego de que él se mueva aleatoriamente, una de las casillas de la cuadrı́cula anterior que esté vacı́a puede haber sido ensuciada. Si hay dos niños se pueden ensuciar hasta 3. Si hay tres niños o más pueden resultar sucias hasta 6. Los niños cuando están en una casilla del corral, ni se mueven ni ensucian. Si un niño es capturado por un Robot de Casa tampoco se mueve ni ensucia.
**Robot de Casa**: El Robot de Casa se encarga de limpiar y de controlar a los niños. El Robot se mueve a una de las casillas adyacentee, las que decida. Solo se mueve una casilla sino carga un niño. Si carga un niño pude moverse hasta dos casillas consecutivas. También puede realizar las acciones de limpiar y cargar niños. Si se mueve a una casilla con suciedad, en el próximo turno puede decidir limpiar o moverse. Si se mueve a una casilla donde está un niño, inmediatamente lo carga. En ese momento, coexisten en la casilla Robot y niño. Si se mueve a una casilla del corral que está vacı́a, y carga un niño, puede decidir si lo deja esta casilla o se sigue moviendo. El Robot puede dejar al niño que carga en cualquier casilla. En ese momento cesa el movimiento del Robot en el turno, y coexisten hasta el próximo turno, en la misma casilla, Robot y niño.





### Objetivos

El objetivo del Robot de Casa es mantener la casa limpia. Se considera la casa limpia si el 60 % de las casillas vacias no están sucias.









### Principales Ideas

El problema planteado consiste en mantener un tablero libre de suciedad, el cual evoluciona con el tiempo. Una primera idea, bastante básica, para diseñar un robot que sea capaz de mantener la suciedad a raya, consiste en ir secuencialmente lidiando con los problemas más cercanos al robot. Esto es, buscar un camino válido que le permita llegar al robot a un niño para que lo cargue y posteriormente dejarlo en el corral, o la vía hasta la casilla llena de suciedad más cercana al robot en cuestión. Este agente sería un agente reactivo, con la particularidad de que, debido a especificaciones del problema, conoce todo el entorno, pero solamente tiene en cuenta aquellas "amenazas" más cercanas para deshacerse de ellas cuanto antes, consecutivamente.



Luego de pensar unos segundos, está claro que esta solución es sencilla, pero está lejos de ser óptima. El caso es que los niños suponen un problema muchísimo más grande que la simple suciedad, los niños son al final, la principal fuente de suciedad en el sistema, así que ocuparnos de ellos debería ser la prioridad, pero el agente descrito anteriormente ataca el problema más cercano, y en caso de tener una casilla sucia a la misma distancia que una casilla con un bebé, no discernirá entre sus opciones y lidiará con cualquiera de ellas, arbitrariamente. Una mejora a esto sería entonces primero deshacernos de los bebés, y una vez hecho esto preocuparnos por la suciedad restante, la cual ya no se incrementará al ritmo de antes. Y si pensamos un poco más allá, sería incluso mejor si elegimos la secuencia de pasos que "maximice" la cantidad de problemas con los que lidiamos. Es decir, si tenemos 3 caminos para llegar a un bebé, será mejor elegir aquel que tenga mas suciedad en su recorrido para limpiarla según nos acercamos. Para ello, a cada posible recorrido en el tablero le podemos asignar un costo, donde, movernos a una casilla vacía es lo peor(aunque muchas veces sea obligatorio para llegar al objetivo), utilizar casillas sucias es bueno, y llegar a casillas ocupadas por un bebé es evidentemente lo mejor. Para esto se puede hacer un recorrido en profundidad por el tablero hasta alcanzar un bebé, dándole a cada paso que damos un costo, mayor para movimientos infructuosos, mediano para casillas con basura, y el menor para el movimiento en el cual llegamos a un bebé. Finalmente, nos quedamos con el mejor de todos estos recorridos según los vamos descubriendo. Este ya sería un *goal-based agent*, donde las decisiones tomadas no son tan triviales como encargarse del problema más cercano, y se trabaja mucho más enfocado al objetivo planteado, que es mantener la casa tan limpia como sea posible.



La idea anterior parece perfecta en papel, pero en la práctica es fácil darse cuenta de cuan ineficiente resulta, pues si bien nos da la solución óptima, su coste computacional es exponencial, para tableros de dimensiones mínimamente considerables nuestro algoritmo no terminaría en un tiempo razonable. De modo que es necesario mejorar esta idea para disminuir su tiempo de ejecución, mientras se mantiene la esencia, utilizar el camino que maximice la cantidad de problemas que podemos resolver durante el recorrido. Para esto, se utilizó la idea del algoritmo de caminos de costos mínimos desde un origen *s* de Dijkstra. Queremos hallar