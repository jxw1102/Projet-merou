## Reconnaissance de motif

Toute action de type X (appel de fonction, déclaration…) est suivie d’une action de type Y dans tout chemin.

<u>Note :</u>

Dans un premier temps, les motifs X et Y seront limités par l'absence de quantificateurs dans les expressions.
Ainsi, f(x + y) sera indiscernable de f(x) si la formule ne spécifiait pas explicitement l'expression à l'intérieur de
l'appel à f.

<u>Intérêt :</u>

* vérifier qu’une variable allouée dynamiquement est toujours libérée
* vérifier que les préconditions d’appel d’une fonction sont toujours vérifiées

<u>Expression sous forme de proposition de la logique temporelle :</u>

AG(not(action(X)) or (action(X) and AX(not(action(X) U action(Y))))) 