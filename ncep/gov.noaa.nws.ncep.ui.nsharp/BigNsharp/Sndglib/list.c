#include <string.h>
#include "list.h"

ListElmt *list_head(List *list)
{
	if (!list)
	  return NULL;

	return list->head;
}

ListElmt *list_tail(List *list)
{
	if (!list)
	  return NULL;

	return list->tail;
}

ListElmt *list_next(ListElmt *el)
{
	if (!el)
	  return NULL;

	return el->next;
}

void *list_data(ListElmt *el)
{
	if (!el)
	  return NULL;

	return el->data;
}

int list_size(List *list)
{
	if (!list)
	  return -1;

	return list->size;
}

void list_init(List *list, void (*destroy)(void *data))
{
	memset(list, 0, sizeof(List));
	list->destroy = destroy;
}

void list_destroy(List *list)
{
	void *data;

	if (!list)
	  return;

	while (list_size(list) > 0) {
	  if ((list_rem_next(list, NULL, &data) == 0) && list->destroy) {
	    list->destroy(data);
	  }
	}

	memset(list, 0, sizeof(List));
}

int list_ins_next(List *list, ListElmt *el, const void *data)
{
	ListElmt *new;

	if (!list)
	  return -1;

	new = calloc(1, sizeof(ListElmt));
	if (!new)
	  return -1;

	if (!el) {
	  /* Insert at head of the list */
	  if (list_size(list) == 0)
	    list->tail = new;

	  new->next = list->head;
	  list->head = new;
	}
	else {
	  if (!el->next)
	    list->tail = new;

	  new->next = el->next;
	  el->next = new;
	}

	list->size++;

	return 0;
}

int list_rem_next(List *list, ListElmt *el, void **data)
{
	ListElmt *old;

	if (!list)
	  return -1;

	if (list_size(list) == 0)
	  return -1;

	if (!el) {
	  /* Remove from head */
	  *data = list->head->data;
	  old = list->head;
	  list->head = list->head->next;

	  if (list_size(list) == 0)
	    list->tail = NULL;
	}
	else {
	  if (!el->next)
	    return -1;

	  *data = el->next->data;
	  old = el->next;
	  el->next = el->next->next;

	  if (!el->next)
	    list->tail = el;	  
	}

	free(old);

	list->size--;

	return 0;
}

DListElmt *dlist_head(DList *list)
{
	if (!list)
	  return NULL;

	return list->head;
}

DListElmt *dlist_tail(DList *list)
{
	if (!list)
	  return NULL;

	return list->tail;
}

DListElmt *dlist_next(DListElmt *el)
{
	if (!el)
	  return NULL;

	return el->next;
}

DListElmt *dlist_prev(DListElmt *el)
{
	if (!el)
	  return NULL;

	return el->prev;
}

int dlist_size(DList *list)
{
	if (!list)
	  return -1;

	return list->size;
}

void dlist_init(DList *list, void (*destroy)(void *data))
{
	memset(list, 0, sizeof(DList));
	list->destroy = destroy;
}

void dlist_destroy(DList *list)
{
	void *data;

	while (dlist_size(list) > 0) {
	  if (dlist_remove(list, dlist_tail(list), &data) == 0 && 
	      list->destroy) {
	    list->destroy(data);
	  }
	}

	memset(list, 0, sizeof(DList));
}

int dlist_ins_next(DList *list, DListElmt *el, const void *data)
{
	DListElmt *new;

	if (!list)
	  return -1;

	if (!el && dlist_size(list) != 0)
	  return -2;

	if (!(new = (DListElmt *)calloc(1, sizeof(DListElmt)))) 
	  return -3;

	new->data = data;

	if (dlist_size(list) == 0) {
	  /* empty list */

	  list->head = new;
	  list->head->prev = NULL;
	  list->head->next = NULL;
	  list->tail = new;
	}
	else {
	  new->next = el->next;
	  new->prev = el;

	  if (!el->next)
	    list->tail = new;
	  else
	    el->next->prev = new;

	  el->next = new;
	}

	list->size++;

	return 0;
}

int dlist_ins_prev(DList *list, DListElmt *el, const void *data)
{
	DListElmt *new;

	if (!list)
	  return -1;

	if (!el && dlist_size(list) != 0)
	  return -2;

	if (!(new = calloc(1, sizeof(DListElmt))))
	  return -3;

	new->data = data;

	if (dlist_size(list) == 0) {
	  list->head = new;
	  list->head->prev = NULL;
	  list->head->next = NULL;
	  list->tail = new;
	}
	else {
	  new->next = el;
	  new->prev = el->prev;

	  if (!el->prev)
	    list->head = new;
	  else
	    el->prev->next = new;

	  el->prev = new;
	}

	list->size++;

	return 0;
}

int dlist_remove(DList *list, DListElmt *el, void **data)
{
	if (!list)
	  return -1;

	if (!el || dlist_size(list) == 0)
	  return -1;

	*data = el->data;

	if (el == list->head) {
	  list->head = el->next;

	  if (!list->head)
	    list->tail = NULL;
	  else
	    el->next->prev = NULL;
	}
	else {
	  el->prev->next = el->next;

	  if (!el->next)
	    list->tail = el->prev;
	  else
	    el->next->prev = el->prev;
	}

	free(el);

	list->size--;

	return 0;
}

void *dlist_data(DListElmt *el)
{
	if (!el)
	  return NULL;

	return el->data;
}

void stack_init(Stack *stack, void (*destroy)(void *data))
{
	list_init(stack, destroy);
}

void stack_destroy(Stack *stack)
{
	list_destroy(stack);
}

int stack_push(Stack *stack, const void *data)
{
	return list_ins_next(stack, NULL, data);
}

int stack_pop(Stack *stack, void **data)
{
	return list_rem_next(stack, NULL, data);
}

int stack_size(Stack *stack)
{
	if (!stack)
	  return -1;

	return stack->size;
}

void *stack_peek(Stack *stack)
{
	if (!stack)
	  return NULL;

	return (stack->head) ? stack->head->data : NULL;
}

void queue_init(Queue *queue, void (*destroy)(void *data))
{
	list_init(queue, destroy);
}

void queue_destroy(Queue *queue)
{
	list_destroy(queue);
}

int queue_enqueue(Queue *queue, const void *data)
{
	return list_ins_next(queue, list_tail(queue), data);
}

int queue_dequeue(Queue *queue, void **data)
{
	return list_rem_next(queue, NULL, data);
}

int queue_size(Queue *queue)
{
	if (!queue)
	  return -1;

	return queue->size;
}

void *queue_peek(Queue *queue)
{
	if (!queue)
	  return NULL;

	return (queue->head) ? queue->head->data : NULL;
}
