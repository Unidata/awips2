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
package com.raytheon.uf.edex.plugin.text.subscription.services;

/**
 * 
 * Trigger Exception class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2016  5203       tjensen     Initial creation
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
public class TriggerException extends Exception {

    /**
     * Default serial version UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Create a Trigger Exception instance from only a message
     * 
     * @param message
     */
    public TriggerException(String message) {
        super(message);
    }

    /**
     * Create a Trigger Exception instance from both a message and a cause
     * 
     * @param message
     * @param cause
     */
    public TriggerException(String message, Throwable cause) {
        super(message, cause);
    }
}