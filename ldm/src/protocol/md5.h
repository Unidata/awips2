/* MD5.H - header file for MD5C.C
 */
#ifndef __MD5_H
#define __MD5_H
/* Copyright (C) 1991, RSA Data Security, Inc. All rights reserved.

   License to copy and use this software is granted provided that it
   is identified as the "RSA Data Security, Inc. MD5 Message-Digest
   Algorithm" in all material mentioning or referencing this software
   or this function.

   License is also granted to make and use derivative works provided
   that such works are identified as "derived from the RSA Data
   Security, Inc. MD5 Message-Digest Algorithm" in all material
   mentioning or referencing the derived work.  
                                                                    
   RSA Data Security, Inc. makes no representations concerning either
   the merchantability of this software or the suitability of this
   software for any particular purpose. It is provided "as is"
   without express or implied warranty of any kind.  
                                                                    
   These notices must be retained in any copies of any part of this
   documentation and/or software.  
 */

typedef struct  MD5_CTX  MD5_CTX; /* private, implemented in md5c.c */

void MD5Init (MD5_CTX *);
void MD5Update (MD5_CTX *, const unsigned char *, unsigned int);
void MD5Final (unsigned char [16], MD5_CTX *);

MD5_CTX *new_MD5_CTX(void);
void free_MD5_CTX(MD5_CTX *);
#endif
