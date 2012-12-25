
type _ connectivity =
	| Tree : 'mudobject	-> 'mudobject Container.container connectivity
	| Digraph : 'mudobject -> 'mudobject Digraph.digraph connectivity

let create : type s . s connectivity -> s = function
	| Digraph d -> Digraph.create d
	| Tree t -> Container.create t
