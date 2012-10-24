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
package com.raytheon.openfire.plugin.configuration.collaboration.exception;

/**
 * Implements an exception that is thrown whenever it is not possible to
 * determine whether or not httpd-collaboration is currently running.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2012            bkowal     Initial creation
 *
 * </pre>
 *
 * @author bkowal
 * @version 1.0	
 */

public class HttpdCollaborationStatusException extends Throwable {

    private static final long serialVersionUID = -2446532142660776343L;
    private static final String MESSAGE_PREFIX = "Unable to determine the status of the httpd-collaboration server; ";

    /**
     * 
     */
    public HttpdCollaborationStatusException() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param message
     */
    public HttpdCollaborationStatusException(String message) {
        super(MESSAGE_PREFIX + message);
    }

    /**
     * @param cause
     */
    public HttpdCollaborationStatusException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public HttpdCollaborationStatusException(String message, Throwable cause) {
        super(MESSAGE_PREFIX + message, cause);
    }

}
