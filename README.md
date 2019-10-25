# Haskell_Skyline
Versión Haskell del proyecto Skyline

La línea de horizonte que dibujan los edificios de una ciudad se conoce como Skyline. 

En esta aplicació vamos a implementar un algoritmo que partiendo de los datos de situación y altura de una serie de edificios, nos devuelva la línea de horizonte (consistente en una lista de coordenadas) o Skyline que forman dichos edificios.

Se trata de un algoritmo de tipo Divide y Vencerás, que divide el problema en dos (o más) subproblemas similares (pero de menor tamaño), los resuelve y combina las soluciones obtenidas en la solución final. Cuando el problema es de un tamaño cuya solución se puede realizar de forma trivial, ésta se devuelve directamente, sin necesidad de volver a dividirlo y combinar las soluciones.

En el caso que nos ocupa, el problema se dividirá en dos subproblemas similares: calcular el skyline de la mitad de los edificios por un lado y el skyline de la otra mitad por otro. La función de combinación recibirá, por tanto, dos líneas de horizonte que deberán ser combinadas en una única. El caso trivial consistirá en calcular la línea de horizonte de un único edificio.
