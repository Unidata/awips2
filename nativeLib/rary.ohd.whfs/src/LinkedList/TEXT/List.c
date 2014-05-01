/*
	File:		List.c

	Date:		2/18/94

	Purpose:	The list management functions provided
			allow the programmer to easily manage 
			a doubly-linked list.  The programmer 
			must take responsibility for allocating
			and deallocating the memory for the 
			container structures that own the Node and 
			List structures. 

			The programmer must first initialize the
			list, using ListInit(), before proceeding
			to other list management functions for
			addition, deletion, counting, etc...
*/


#include <memory.h>
#include "List.h"


/*
	Initialize the list by setting all memory to NULL.
*/
void	ListInit(List *lstPtr)
{
	(void) memset ( lstPtr , 0 , sizeof ( List ) ) ;
}


/*
	Returns a count of the number of nodes in the list.
*/
int	ListCount(List *lstPtr)
{
	if (lstPtr)
		return(lstPtr->count);
	return(0);
}


/*
	Returns a pointer to the first node in the list.
*/
Node	*ListFirst(const List *lstPtr)
{
	Node	*node;

	node = (Node *) NULL;
	if (lstPtr)
		node = lstPtr->first;
	return(node);
}


/*
	Returns a pointer to the last node in the list.
*/
Node	*ListLast(List *lstPtr)
{
	Node	*node;

	node = (Node *) NULL;
	if (lstPtr)
		node = lstPtr->last;
	return(node);
}


/*
	Returns a pointer to the next node in the list.
*/
inline Node	*ListNext( const Node *nodePtr)
{
	return(nodePtr->next);
}


/*
	Returns a pointer to the previous node in the list.
*/
Node	*ListPrev(const Node *nodePtr)
{
	Node	*node;

	node = nodePtr->prev;
	return(node);
}


/*
	Adds a node to the list, increments count.
*/	
void	ListAdd(List *lstPtr, Node *nodePtr)
{

	/*
		nodePtr is not first in the list.
	*/
	if (lstPtr->first)
	{
		lstPtr->last->next = nodePtr;
		nodePtr->prev = lstPtr->last;
	}
	else
	{
		lstPtr->first = nodePtr;
		nodePtr->prev = NULL;
	}

	lstPtr->last = nodePtr;
	nodePtr->next = NULL;

	lstPtr->count++;
}



/*
	Deletes a node from the list, decrements count.
	The user is responsible for deallocation of the
	memory associated with the deleted node.
*/
void	ListDelete(List *lstPtr, Node *nodePtr)
{

	/*
		Not the last node in the list.
	*/
	if (nodePtr->next)
		nodePtr->next->prev = nodePtr->prev;
	else
		lstPtr->last = nodePtr->prev;


	if (nodePtr->prev)
		nodePtr->prev->next = nodePtr->next;
	else
		lstPtr->first = nodePtr->next;

	lstPtr->count--;
}



/*
	Inserts newnodePtr in the List before nodePtr;
*/
void	ListInsert(List *lstPtr, Node *nodePtr, Node *newnodePtr)
{

	/*
		Node before which to insert is first node.	
	*/
	if (nodePtr == lstPtr->first)
	{
		lstPtr->first = newnodePtr;
		newnodePtr->prev = NULL;
	}
	else
	{
		newnodePtr->prev = nodePtr->prev;
		nodePtr->prev->next = newnodePtr;
	}
	
	newnodePtr->next = nodePtr;	
	nodePtr->prev = newnodePtr;
	lstPtr->count++;
}



/*
	Retrieves the first element from the list,
	and removes it.
*/
Node	*ListDelHead(List *lstPtr)
{
	Node	*nodePtr;

	if ( ( nodePtr = ListFirst(lstPtr) ) )
		ListDelete(lstPtr, nodePtr);

	return(nodePtr);
}



/*
	Find the specified element in the list and return
	the numerical offset into the list.  If the desired
	node is not found, a negative integer is returned 
	(NOTFOUND) to indicate failure.  This value should
	checked upon function return.
*/
int	ListFind(List *lstPtr, Node *nodePtr)
{
	Node	*nPtr;
	int	rtn = NOTFOUND;
	int	i;


	/*
		Loop through list and find the one we're 
		looking for, and then return offset.
	*/
	for (i=1, nPtr = ListFirst(lstPtr); nPtr; nPtr = ListNext(nPtr), i++)
	{
		if (nPtr == nodePtr)
		{
			rtn = i;
			break;
		}
	}

	return(rtn);
}


/*
	Returns the Node at the List offset specified the 
	parameter pos.  Programmer should check return value
	prior to use to ensure NULL Node * was not returned.
	This condition may only occur when the pos parameter
	falls outside of the list bounds, i.e (pos < 0) or
	(pos > ListCount()).
*/
Node	*ListNth(List *lstPtr, int pos)
{
	Node	*nodePtr;
	int	cnt;
	int	i;


	/*
		Set cnt variable to List len.
	*/
	cnt = ListCount(lstPtr);


	/*
		Check pos against List bounds.
	*/
	if ((pos < 1) || (pos > cnt))
		return((Node *)NULL);


	/*
		Loop through list and get node at pos.
	*/
	nodePtr = ListFirst(lstPtr);
	for (i = 1; i <= cnt; i++)
	{
		if (i == pos)
			break;

		nodePtr = ListNext(nodePtr);
	}

	return(nodePtr);
}


/*
	Concatenates two lists, by appending the srcPtr
	List to the tail of the dstPtr.
*/
void	ListConcat(List *dstPtr, List *srcPtr)
{
	Node	*nodePtr;
	
	while ( ( nodePtr = ListDelHead(srcPtr) ) )
		ListAdd(dstPtr, nodePtr);

	return;
}

