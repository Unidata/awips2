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
package com.raytheon.uf.viz.collaboration.comm.identity;

/**
 * Exception class for collaboration component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class CollaborationException extends Exception {

    private static final long serialVersionUID = 2897604473798379699L;

    /**
     * 
     */
    public CollaborationException() {

    }

    /**
     * @param message
     */
    public CollaborationException(String message) {
        super(message);
    }

    /**
     * 
     * @param message
     * @param cause
     */
    public CollaborationException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public CollaborationException(Throwable cause) {
        super(cause);
    }

}
