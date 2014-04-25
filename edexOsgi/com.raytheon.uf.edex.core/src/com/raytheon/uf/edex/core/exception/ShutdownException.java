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
package com.raytheon.uf.edex.core.exception;

import com.raytheon.uf.edex.core.EdexException;

/**
 * Exception thrown during shutdown to allow for easy restart of transacted
 * tasks.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ShutdownException extends EdexException {

    /**
     * Default serial verion UID
     */
    private static final long serialVersionUID = 1L;

    public ShutdownException() {
        super("Aborting process, EDEX shutting down");
    }

    /**
     * Create a Shutdown Exception instance from only a message
     * 
     * @param message
     */
    public ShutdownException(String message) {
        super(message);
    }

    /**
     * Create a Shutdown Exception instance from both a message and a cause
     * 
     * @param message
     * @param cause
     */
    public ShutdownException(String message, Throwable cause) {
        super(message, cause);
    }

}
