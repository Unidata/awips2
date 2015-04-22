#ifndef LIST_H
#define LIST_H

#include <stdlib.h>

typedef struct ListElmt_ {
	void   *data;
	struct  ListElmt_ *next;
} ListElmt;

typedef struct List_ {
	int       size;
	int     (*match)(const void *key1, const void *key2);
	void    (*destroy)(void *data);
	ListElmt *head;
	ListElmt *tail;
} List;

typedef struct DListElmt_ {
	void   *data;
	struct  DListElmt_ *next;
	struct  DListElmt_ *prev;
} DListElmt;

typedef struct DList_ {
	int        size;
	int      (*match)(const void *key1, const void *key2);
	void     (*destroy)(void *data);
	DListElmt *head;
	DListElmt *tail;
} DList;

typedef List Stack;

typedef List Queue;

/* Singly-linked list functions */
ListElmt  *list_head(List *list);
ListElmt  *list_tail(List *list);
ListElmt  *list_next(ListElmt *el);
int        list_size(List *list);
int        list_ins_next(List *list, ListElmt *el, const void *data);
int        list_rem_next(List *list, ListElmt *el, void **data);
void       list_init(List *list, void (*destroy)(void *data));
void       list_destroy(List *list);
void      *list_data(ListElmt *el);

/* Doubly-linked list functions */
DListElmt *dlist_head(DList *list);
DListElmt *dlist_tail(DList *list);
DListElmt *dlist_next(DListElmt *el);
DListElmt *dlist_prev(DListElmt *el);
int        dlist_size(DList *list);
void       dlist_init(DList *list, void (*destroy)(void *data));
void       dlist_destroy(DList *list);
int        dlist_ins_next(DList *list, DListElmt *el, const void *data);
int        dlist_ins_prev(DList *list, DListElmt *el, const void *data);
int        dlist_remove(DList *list, DListElmt *el, void **data);
void      *dlist_data(DListElmt *el);

/* Stack functions */
void       stack_init(Stack *stack, void (*destroy)(void *data));
void       stack_destroy(Stack *stack);
int        stack_push(Stack *stack, const void *data);
int        stack_pop(Stack *stack, void **data);
int        stack_size(Stack *stack);
void      *stack_peek(Stack *stack);

/* Queue functions */
void       queue_init(Queue *queue, void (*destroy)(void *data));
void       queue_destroy(Queue *queue);
int        queue_enqueue(Queue *queue, const void *data);
int        queue_dequeue(Queue *queue, void **data);
int        queue_size(Queue *queue);
void      *queue_peek(Queue *queue);

#endif
