#ifndef NAMED_COLOR_SET_GROUP_H
#define NAMED_COLOR_SET_GROUP_H

#include "NamedColorUseSet.h"

typedef struct _NamedColorSetGroup
{
  NamedColorUseSet * color_group_array;
  long set_count;
} NamedColorSetGroup;


NamedColorSetGroup * addNamedColorUseSet (  
                             NamedColorSetGroup * pNamedColorSetGroup, 
                             NamedColorUseSet * pNamedColorUseSet );

void FreeColorSetGroupMemory ( NamedColorSetGroup * pColorSetGroup );

#endif /* #ifndef NAMED_COLOR_SET_GROUP_H */
