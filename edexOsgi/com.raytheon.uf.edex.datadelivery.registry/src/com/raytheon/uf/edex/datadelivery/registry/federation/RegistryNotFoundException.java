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

package com.raytheon.uf.edex.datadelivery.registry.federation;

import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Exception class used by the registry federation manager for when a registry
 * is not found in the federation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/24/2014    2769        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryNotFoundException extends EbxmlRegistryException {

    private static final long serialVersionUID = -8058778534952899091L;

    /**
     * Creates a new exception with the given message
     * 
     * @param message
     *            The message to attach to the exception
     */
    public RegistryNotFoundException(String message) {
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
    public RegistryNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates a new exception with the given cause
     * 
     * @param cause
     *            The underlying cause of the exception
     */
    public RegistryNotFoundException(Throwable cause) {
        super(cause);
    }
}
