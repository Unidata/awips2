/*
 * The Python Imaging Library
 * $Id: QuantHeap.h 2134 2004-10-06 08:55:20Z fredrik $
 *
 * image quantizer
 *
 * Written by Toby J Sargeant <tjs@longford.cs.monash.edu.au>.
 *
 * See the README file for information on usage and redistribution.
 */

#ifndef __HEAP_H__
#define __HEAP_H__

#include "QuantTypes.h"

void ImagingQuantHeapFree(Heap);
int ImagingQuantHeapRemove(Heap,void **);
int ImagingQuantHeapAdd(Heap,void *);
int ImagingQuantHeapTop(Heap,void **);
Heap *ImagingQuantHeapNew(HeapCmpFunc);

#endif
