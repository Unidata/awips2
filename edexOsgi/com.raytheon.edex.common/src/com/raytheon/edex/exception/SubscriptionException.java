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

package com.raytheon.edex.exception;

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 	
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class SubscriptionException extends Exception {
    /**
     * Default serial verion UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Create a Subscription Exception with only a message.
     * 
     * @param message the exception messge
     */
    public SubscriptionException(String message) {
        super(message);
    }

    /**
     * Create a Subscription Exception with a message and cause.
     * 
     * @param message the exception message
     * @param cause the exception cause
     */
    public SubscriptionException(String message, Throwable cause) {
        super(message, cause);
    }

}
