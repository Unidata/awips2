#include <stdio.h>
#include <stdlib.h>
#include "history.h"

static short         maxitems=0;
static short         nitems=0;
static short         curindex=0;
static historyItem **history = NULL;

historyItem *newHistoryItem(void *data)
{
	historyItem *h;
	int *a;

	h = (historyItem *)calloc(1, sizeof(historyItem));
	if (!h)
	  return h;

	h->data = data;

	return h;
}

/*
 * Newest item always gets added to index 0
 */
short addHistoryItem(historyItem *h)
{
	/* In case we get called before being initialized */
	if (maxitems == 0) {
	  setHistoryLimit(32);
	}

	if (h == NULL || history == NULL)
	  return 1;

	/* Delete first item to make room for this one */
	if (nitems == maxitems) {
	  delHistoryItem(nitems-1);
	}
	else {
	  nitems++;
	}

	printf("addHistoryItem: I am now holding %d (max=%d) items\n",
	  nitems, maxitems);

	memmove(history+1, history, ((nitems-1)*sizeof(historyItem *)));
	history[0] = h;

	return 0;
}

short delHistoryItem(short index)
{
	historyItem *h;

	if (nitems == 0)
	  return 1;

	h = history[index];

	if (h == NULL)
	  return 1;

	/* Free things */
	free(h);

	return 0;
}

/* Wrap determines whether or not we loop back to index 0 or not */
historyItem *nextHistoryItem(short wrap)
{
	short index;

	if (nitems == 0)
	  return NULL;

	index = getHistoryIndex();
	setHistoryIndex(++index);
	index = getHistoryIndex();

	/* We're at the last element and we don't want to wrap forward */
	if (index == nitems && wrap == 0)
	  return NULL;

	return history[index];
}

historyItem *prevHistoryItem(short wrap)
{
	short index;

	if (nitems == 0)
	  return NULL;

	index = getHistoryIndex();

	/* We're at the first element and we don't want to wrap backwards */
	if (index == 0 && wrap == 0)
	  return NULL;

	setHistoryIndex(--index);
	index = getHistoryIndex();

	return history[index];
}

historyItem *getHistoryItem(short index)
{
	if (!history || index >= nitems)
	  return NULL;

	return history[index];
}

short getHistoryIndex()
{
	return curindex;
}

void setHistoryIndex(short index)
{
	short n = numHistoryItems();

	if (index > n)
	  curindex = 0;
	else if (index < 0)
	  curindex = n-1;
	else
	  curindex = index;
}

short numHistoryItems()
{
	return nitems;
}

int setHistoryLimit(int num)
{
	int i;
	historyItem **tmp=NULL;

	if (num <= 0)
	  return 1;

	tmp = (historyItem **)realloc(history, (num * sizeof(historyItem *)));
	if (!tmp)
	  return 1;

	history = tmp;

	if (num > maxitems) {
	  for (i=maxitems;i<num;i++) {
	    history[i] = NULL;
	  }
	}

	maxitems = num;

	return 0;
}

#ifdef USEMAIN

int main(int argc, char *argv[])
{
	int i, *a;
	historyItem *h;

	setHistoryLimit(15);

	for (i=0;i < 6; i++) {
	  a = (int *)malloc(sizeof(int));
	  *a = i;
	  h = newHistoryItem(a);
	  addHistoryItem(h);
	}

	/* Start at the beginning */
	setHistoryIndex(numHistoryItems());
	while ((h = nextHistoryItem(0)) != NULL) {
	  printf("h[%d]->a is %d\n", getHistoryIndex(), *(int *)h->data);
	}

	printf("Doing the reverse now...\n");
	setHistoryIndex(3);
	for (i=0;i<6;i++) {
	  h = prevHistoryItem(1);
	  if (h)
	    printf("h[%d]->a is %d\n", getHistoryIndex(), *(int *)h->data);
	}

	return 0;
}

#endif

#ifdef NEW
short addHistoryItem2(historyItem *h)
{
	if (maxitems == 0) {
	  setHistoryLimit(32);
	}

	if (h == NULL || history == NULL)
	  return 1;

	while (nitems >= maxitems) {
	  // Delete oldest item(s)
	  delHistoryItem2(0);
	  nitems--;
	}

	history[(nitems % maxitems)] = h;
	nitems++;

	return 0;
}

short delHistoryItem2(short index)
{
	historyItem *h = NULL;

	if (index >= nitems || history == NULL)
	  return 1;

	h = history[index];

	if (h) {
	  if (h->data)
	    free(h->data);
	  free(h);
	}
	else
	  return 1;

	history[index] = NULL;

	return 0;
}

historyItem *nextHistoryItem2(void)
{
}

historyItem *prevHistoryItem2(void)
{
}
#endif
