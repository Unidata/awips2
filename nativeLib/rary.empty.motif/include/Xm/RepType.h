/**
 *
 * $Id: RepType.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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

#ifndef _XM_REPTYPE_H
#define _XM_REPTYPE_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmREP_TYPE_INVALID = 0x1FFF
};

typedef short XmRepTypeId;

typedef struct {
    String rep_type_name;
    String *value_names;
    unsigned char *values;
    unsigned char num_values;
    Boolean reverse_installed;
    XmRepTypeId rep_type_id;
} XmRepTypeEntryRec, *XmRepTypeEntry, XmRepTypeListRec, *XmRepTypeList;

XMLIBEXPORT void XmRepTypeAddReverse(XmRepTypeId rep_type_id);
XMLIBEXPORT XmRepTypeId XmRepTypeGetId(String rep_type);
XMLIBEXPORT String *XmRepTypeGetNameList(XmRepTypeId rep_type_id,
			     Boolean use_uppercase_format);
XMLIBEXPORT XmRepTypeEntry XmRepTypeGetRecord(XmRepTypeId rep_type_id);
XMLIBEXPORT XmRepTypeList XmRepTypeGetRegistered(void);
XMLIBEXPORT void XmRepTypeInstallTearOffModelConverter(void);
XMLIBEXPORT XmRepTypeId XmRepTypeRegister(String rep_type,
			      String *value_names,
			      unsigned char *values,
			      unsigned char num_values);
XMLIBEXPORT Boolean XmRepTypeValidValue(XmRepTypeId rep_type_id,
			    unsigned char test_value,
			    Widget enable_default_warning);

#ifdef __cplusplus
}
#endif

#endif /* _XM_REPTYPE_H */
