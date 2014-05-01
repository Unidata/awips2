
#ifndef setup_tree_h
#define setup_tree_h


typedef struct _SetupTree {
	char		*name;
	void		*dataPtr;
	char		*field;
	void		(*load)();
	int		(*delete)(const char *where);
	int		(*insert)(void *dataPtr);
	int		(*update)(void *dataPtr, const char *where);
} SetupTree;


#endif
