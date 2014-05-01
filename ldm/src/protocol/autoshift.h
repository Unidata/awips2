#ifndef AUTOSHIFT_H
#define AUTOSHIFT_H

#include "error.h"

#include <stddef.h>

/*
 * Initializes the logic of this module.
 *
 * Arguments:
 *	isUpstream	Whether or not the process is an upstream (i.e., 
 *			sending) LDM.  True or false.
 *	isPrimary	Whether or not the transmission-mode is primary or
 *			alternate (i.e., uses HEREIS or COMINGSOON/BLKDATA
 *			messages).
 *	socket		The socket on which data-products are exchanged.
 * Returns:
 *	NULL		Success
 *	else		Failure.  Error codes:
 *		EBADF		The socket argument is not a valid file 
 *				descriptor.
 *		ENOTSOCK	The socket argument does not refer to a socket.
 *		EINVAL		The socket has been shut down.
 *		ENOBUFS		Insufficient resources are available in the 
 *				system to complete the call.
 *		ENOSR		There were insufficient STREAMS resources 
 *				available for the operation to complete.
 */
ErrorObj*
as_init(
    const int	isUpstream,
    const int	isPrimary,
    const int	socket);


/*
 * Sets the number of LDM-s exchanging data.  If the number doesn't equal
 * the previous number, then this module is reset.
 *
 * Arguments:
 *      count   The number of LDM-s exchanging data.
 * Returns:
 *      0       Success.  The number will be used to determine when to switch.
 *      EINVAL  "count" is zero.  The previous number will still be used.
 *              The default number is 2.
 */
int
as_setLdmCount(
    unsigned    count);

/*
 * Processes the status of a data-product exchanged via the HEREIS protocol.
 *
 * Arguments:
 *	success		Whether or not the data-product was accepted.
 *			True or false.
 *	size		Size of the data-product in bytes.
 * Returns:
 *	0		Success.
 *	ENOMEM		Out of memory.
 */
int
as_hereis(
    const int		success,
    const size_t	size);

/*
 * Processes the status of a data-product exchanged via the COMINGSOON/BLKDATA
 * protocols.
 *
 * Arguments:
 *	success		Whether or not the data-product was accepted.
 *			True or false.
 *	size		Size of the data-product in bytes.
 * Returns:
 *	0		Success.
 *	ENOMEM		Out of memory.
 */
int
as_comingsoon(
    const int		success,
    const size_t	size);

/*
 * Indicates whether or not to switch between primary and alternate
 * data-product exchange modes.
 *
 * Returns:
 *	0	Don't switch.
 *	1	Do switch
 */
int
as_shouldSwitch(void);

#endif
