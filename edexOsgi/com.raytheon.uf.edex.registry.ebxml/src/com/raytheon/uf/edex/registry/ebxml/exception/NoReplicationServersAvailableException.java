/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.registry.ebxml.exception;

/**
 * 
 * Exception for when no registry synchronization servers are available
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/19/2013     1692        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NoReplicationServersAvailableException extends
        EbxmlRegistryException {
    /**
     * 
     */
    private static final long serialVersionUID = 228063108298169960L;

    /**
     * Creates a new exception with the given message
     * 
     * @param message
     *            The message to attach to the exception
     */
    public NoReplicationServersAvailableException(String message) {
        super(message);
    }

    /**
     * Creates a new exception with the given message and cause
     * 
     * @param message
     *            The message to attach to the exception
     * @param cause
     *            The underlying cause of the exception
     */
    public NoReplicationServersAvailableException(String message,
            Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates a new exception with the given cause
     * 
     * @param cause
     *            The underlying cause of the exception
     */
    public NoReplicationServersAvailableException(Throwable cause) {
        super(cause);
    }
}
