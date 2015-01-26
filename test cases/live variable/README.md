## Variable vivante

**La variable x est vivante au noeud N** *(x est utilisée dans au moins un chemin partant de N ne contenant pas d’affectation d’une valeur à x)*
	
<u>Intérêt :</u>

* si une variable est morte à un certain point, il n’est pas utile de la conserver dans un registre
* si une variable est morte lors d’une assignation, l’assignation peut être retirée du programme sans changer son comportement

<u>Expression sous forme de proposition de la logique temporelle :</u>

* __not__(assignment(x)) __U__ use(x)
