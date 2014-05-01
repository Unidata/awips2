/*
	File:		List.h

	Date:		2/18/94

	Org:		NOAA NWS/OH/HRL

	Purpose:	The List library provides a set of
			functions to assist the programmer
			in management of doubly-linked lists.
			The programmer must take responsibility
			for allocation and deallocation of the 
			memory for the container structures that
			own the Node and List structures.

			The programmer should first initialize
			the List, by using the ListInit() function,
			before proceeding to other list management
			functions for addition, deletion, counting,
			etc...

			NOTE:  When defining a "C" struct containing
			using the Node and List constructs, the Node
			construct MUST be the first member of the 
			defined struct.

			NOTE:  The ListAdd() and ListDelete() 
			functions DO NOT allocate OR deallocate
			memory.  These functions place this
			responsibility on the programmer.
*/


#ifndef List_h
#define List_h


/*
	Adjacent nodes.
*/
typedef struct _Node {
	struct _Node	*next;
	struct _Node	*prev;
} Node;


/*
	Entire list.
*/
typedef struct _List {
	Node 	*first;
	Node	*last;
	int	count;
} List;



/*
	Symbolic Constants
*/
#define FOUND		 1
#define	NOTFOUND	-1



/*
	List management functions.
*/

void	ListInit(List *lstPtr);
int	ListCount(List *lstPtr);

Node	*ListFirst(const List *lstPtr);
Node	*ListLast(List *lstPtr);
inline Node	*ListNext(const Node *nodePtr);
Node	*ListPrev(const Node *nodePtr);

void	ListAdd(List *lstPtr, Node *nodePtr);
void	ListDelete(List *lstPtr, Node *nodePtr);
void	ListInsert(List *lstPtr, Node *nodePtr, Node *newnodePtr);

Node	*ListDelHead(List *lstPtr);
Node	*ListNth(List *lstPtr, int pos);
int	ListFind(List *lstPtr, Node *nodePtr);
void	ListConcat(List *dstPtr, List *srcPtr);


#endif
