#ifndef STRING_BUF_H
#define STRING_BUF_H


typedef struct StringBuf	StringBuf;


/*
 * Returns an allocated StringBuf.
 *
 * Arguments:
 *	initMax		Initial capacity of the StringBuf, in characters,
 *			excluding any terminating NUL.
 * Returns:
 *	NULL		Failure.
 *	else		Pointer to an allocated StringBuf.
 */
StringBuf*
strBuf_new(
    size_t	initMax	);


/*
 * Appends a string to the contents of a StringBuf.  If the StringBuf is valid
 * and the string cannot be appended, then the StringBuf is rendered invalid.
 *
 * Arguments:
 *	strBuf		Pointer to a StringBuf or NULL.
 *	string		String to be appended.
 * Returns:
 *	0		Success.
 *	EINVAL		"strBuf" is NULL or "*strBuf" is invalid.
 *	ENOMEM		Out-of-memory.
 */
int
strBuf_appendString(
    StringBuf* const	strBuf,
    const char* const	string);


/*
 * Clears a StringBuf.  On success, a subsequent strBuf_toString() will return
 * the empty string.
 *
 * Arguments:
 *	strBuf		Pointer to the StringBuf to be cleared.
 * Returns:
 *	0		Success.
 *	EINVAL		"strBuf" is NULL or "*strBuf" is invalid.
 */
int
strBuf_clear(
    StringBuf* const	strBuf);


/*
 * Sets a StringBuf to a string.
 *
 * Arguments:
 *	strBuf		Pointer to the StringBuf.
 *	string		Pointer to the string to which "strBuf" will be set.
 *			Caller may free upon return.
 * Returns:
 *	0		Success.
 *	EINVAL		"strBuf" is NULL or "*strBuf" is invalid.
 *	ENOMEM		Out-of-memory.
 */
int
strBuf_setToString(
    StringBuf* const	strBuf,
    const char* const	string);


/*
 * Returns the contents of a StringBuf as a NUL-terminated string.
 *
 * Arguments:
 *	strBuf		The StringBuf.
 * Returns:
 *	NULL		"strBuf" is NULL or "*strbuf" is invalid.
 *	else		The contents of "strBuf" as a NUL-terminated string.
 */
const char*
strBuf_toString(
    const StringBuf* const	strBuf);


/*
 * Returns an error-message string corresponding to the state of a StringBuf.
 *
 * Arguments:
 *	strBuf		The StringBuf.  It must have been returned by a
 *			previous call to strBuf_new().
 * Returns:
 *	Pointer to a string describing the state of "strBuf".
 */
const char*
strBuf_strerror(
    const StringBuf* const	strBuf);


/*
 * Releases any resouces associated with a StringBuf.
 *
 * Arguments:
 *	strBuf		Point to the SringBuf or NULL.
 */
void
strBuf_free(
    StringBuf* const	strBuf);


#endif
