#ifndef _HISTORY_H
#define _HISTORY_H

typedef struct historyItem {
	void *data;
} historyItem;

historyItem *newHistoryItem(void *data);
historyItem *prevHistoryItem(short wrap);
historyItem *nextHistoryItem(short wrap);
historyItem *getHistoryItem(short index);
short        delHistoryItem(short index);
short        addHistoryItem(historyItem *h);
short        getHistoryIndex();
void         setHistoryIndex(short index);
short        numHistoryItems();
int          setHistoryLimit(int num);

#endif
