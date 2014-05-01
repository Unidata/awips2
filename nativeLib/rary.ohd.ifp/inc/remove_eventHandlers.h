/* remove_eventHandlers.h */

#ifndef remove_eventHandlers_h
#define remove_eventHandlers_h

typedef struct
	{
	EventMask               mask;
	Boolean                 boolean_flag;
	void                    (*eventHandler_func) ();
	caddr_t                 *data;
	}       eventHandler_struct;
	
#endif
