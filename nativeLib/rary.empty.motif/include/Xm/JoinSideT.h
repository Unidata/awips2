/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/JoinSideT.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#ifndef _XM_JOINSIDET_H
#define _XM_JOINSIDET_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif


typedef void (*XmJoinSideSetValueProc)(
              Widget,
              unsigned char,
              Dimension);

typedef unsigned char (*XmJoinSideGetValueProc)(
                      Widget,
                      Dimension);

typedef struct {
      int version;
      XmJoinSideSetValueProc setValue;
      XmJoinSideGetValueProc getValue;
} XmJoinSideTraitRec, *XmJoinSideTrait;

enum {
  XmLEFT, XmRIGHT, XmBOTTOM, XmTOP};

#ifdef __cplusplus
}
#endif

#endif /* _XM_JOINSIDET_H */
