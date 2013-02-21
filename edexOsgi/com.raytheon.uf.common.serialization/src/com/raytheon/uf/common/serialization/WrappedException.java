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
package com.raytheon.uf.common.serialization;

/**
 * Wrapped exception, prints the original exception
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 09, 2010           mschenke     Initial creation
 * Sep 14, 2012 1169      djohnson     Moved to com.raytheon.uf.common.serialization
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WrappedException extends Exception {

    private static final long serialVersionUID = 1L;

    private final String exceptionClass;

    /**
     * @param message
     * @param cause
     */
    public WrappedException(String exceptionClass, String message,
            Throwable cause) {
        super(message, cause);
        this.exceptionClass = exceptionClass;
    }

    /**
     * Override toString() copied from Throwable and used the wrapped exception
     * class
     */
    @Override
    public String toString() {
        String s = exceptionClass != null ? exceptionClass : getClass()
                .getName();
        String message = getLocalizedMessage();
        return (message != null) ? (s + ": " + message) : s;
    }

}
