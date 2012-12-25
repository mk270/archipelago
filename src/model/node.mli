
type _ connectivity =
	| Tree : 'mudobject	-> 'mudobject Container.container connectivity
	| Digraph : 'mudobject -> 'mudobject Digraph.digraph connectivity

val create : 's connectivity -> 's
