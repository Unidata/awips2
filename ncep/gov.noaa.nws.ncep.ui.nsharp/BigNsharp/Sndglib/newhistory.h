#ifndef _HISTORY_H
#define _HISTORY_H

#include "list.h"

typedef struct History_ {
  DList list;
  int   maxsize;
} History;
typedef DListElmt HistElmt;

void history_init(History *h, int size, void (*destroy)(void *));
void history_destroy(History *h);
int  history_setsize(History *h, int size);
int  history_add(History *h, void *data);
int  history_remove(History *h, HistElmt *el, void **data);
int  history_size(History *h);
HistElmt *history_first(History *h);
HistElmt *history_last(History *h);
HistElmt *history_next(HistElmt *he);
HistElmt *history_prev(HistElmt *he);
void *history_data(HistElmt *he);

#endif /* _HISTORY_H */
