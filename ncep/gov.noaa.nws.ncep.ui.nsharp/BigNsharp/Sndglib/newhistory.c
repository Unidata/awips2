#include <string.h>
#include "newhistory.h"

void history_init(History *h, int size, void (*destroy)(void *))
{
	memset(h, 0, sizeof(History));
	history_setsize(h, size);
	h->list.destroy = destroy;
}

void history_destroy(History *h)
{
	dlist_destroy(&h->list);
	memset(h, 0, sizeof(History));
}

int history_setsize(History *h, int size)
{
	void *data;

	if (size <= 0)
	  return -1;

	if (size < h->maxsize) {
	  while (dlist_size(&h->list) > size) {
	    if (dlist_remove(&h->list, dlist_tail(&h->list), &data) == 0 &&
	        h->list.destroy) {
	      h->list.destroy(data);
	    }
	  }
	}

	h->maxsize = size;
}

int history_add(History *h, void *data)
{
	void *tmpdata;

	if (dlist_size(&h->list) == h->maxsize) {
	  if (dlist_remove(&h->list, dlist_tail(&h->list), &tmpdata) == 0 &&
	      h->list.destroy) {
	    h->list.destroy(tmpdata);
	  }
	}

	return dlist_ins_prev(&h->list, dlist_head(&h->list), data);
}

HistElmt *history_first(History *h)
{
	return dlist_head(&h->list);
}

HistElmt *history_last(History *h)
{
	return dlist_tail(&h->list);
}

HistElmt *history_next(HistElmt *he)
{
	return dlist_next(he);
}

int history_size(History *h)
{
	return dlist_size(&h->list);
}

void *history_data(HistElmt *he)
{
	return dlist_data(he);
}

HistElmt *history_prev(HistElmt *he)
{
	return dlist_prev(he);
}

int history_remove(History *h, HistElmt *el, void **data)
{
	return dlist_remove(&h->list, el, data);
}

#ifdef USEMAIN
int main(int argc, char *argv[])
{
	int      *a, *b, i, j;
	History   h;
	HistElmt *he;

	history_init(&h, 10, myfree);

	for (i=0;i<10;i++) {
	  a = malloc(sizeof(int));
	  *a = i;
	  printf("Adding %d... ", *a);
	  j = history_add(&h, a);
	  printf("Return value: %d.\n", j);
	}

	he = history_first(&h);
	while (he) {
	  b = (int *)history_data(he);
	  printf("%d\n", *b);
	  he = history_next(he);
	}

	history_setsize(&h, 5);

	history_setsize(&h, 15);

	he = history_first(&h);
	while (he) {
	  b = (int *)history_data(he);
	  printf("%d\n", *b);
	  he = history_next(he);
	}

	history_destroy(&h);

	return 0;
}
#endif
