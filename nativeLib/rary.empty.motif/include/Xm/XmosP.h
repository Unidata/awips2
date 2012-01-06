/**
 *
 * $Id: XmosP.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#ifndef _XM_XMOSP_H
#define _XM_XMOSP_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

#if XmVERSION == 1

XmString 
_XmOSGetLocalizedString(char *reserved,
                        Widget w,
                        String resourceName,
                        String value);

void _XmOSQualifyFileSpec(String dirSpec,
                          String filterSpec,
                          String *pQualifiedDir,
                          String *pQualifiedPattern);
 
String _XmOSFindPatternPart(String fileSpec);

void _XmOSGetDirEntries(String qualifiedDir,
                        String matchPattern,
                        unsigned char fileType,
			Boolean matchDotsLiterally,
			Boolean listWithFullPath,
			String **pEntries,
			unsigned int *pNumEntries,
			unsigned int *pNumAlloc);

void _XmOSBuildFileList(String dirPath,
			String pattern,
			unsigned char typeMask,
			String **pEntries,
			unsigned int *pNumEntries,
			unsigned int *pNumAlloc);

int _XmOSFileCompare(XmConst void *sp1, 
                     XmConst void *sp2);

void _XmSleep(unsigned int secs);

int _XmMicroSleep(long secs);

String _XmOSGetHomeDirName(void);

String _XmOSGetCurrentDirName(void);

String _XmOSInitPath(String file_name,
                     String env_pathname,
                     Boolean *user_path);

int _XmOSPutenv(String env_string);

String _XmOSBuildFileName(String, String);

#else /* #if XmVERSION == 1 */

XMLIBEXPORT extern String XmeGetHomeDirName(void);

#endif /* #if XmVERSION != 1 */

#ifdef __cplusplus
}
#endif

#endif /* _XM_XMOSP_H */
