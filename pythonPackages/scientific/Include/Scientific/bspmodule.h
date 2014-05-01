/*
 * Include file for BSPlib interface.
 *
 * Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
 * last revision: 2001-11-20
 */

#ifndef Py_BSPMODULE_H
#define Py_BSPMODULE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "Scientific/arrayobject.h"
#include "bsp.h"

/* Include the automatically generated API definitions */
#include "Scientific/PyBSP_API.h"

/* Tag type and size for sending Python objects */
typedef enum {PyBSP_StringTag,
	      PyBSP_ArrayTypeTag,
	      PyBSP_ArrayDataTag}
        PyBSP_ObjectType;

typedef struct {PyBSP_ObjectType type;
                char number;
                int source_pid;}
        PyBSP_Tag;

const int PyBSP_TAGSIZE = sizeof(PyBSP_Tag);

/* Message queue for receiving Python objects */
typedef struct {PyBSP_Tag *tag_ptr;
                void *payload_ptr;
                int length;}
        PyBSP_Message;

#ifdef __cplusplus
}
#endif
#endif /* Py_BSPMODULE_H */
