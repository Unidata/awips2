/*
 * error.h
 *
 *  Created on: Feb 6, 2009
 *      Author: jelkins
 */

#ifndef ERROR_H_
#define ERROR_H_

/**
 * Output an error message indicating the given library is no longer supported
 *
 * A stack trace from which the call was made is also given.
 */
void nolibError(char * libName);

#endif /* ERROR_H_ */
