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
package com.raytheon.uf.edex.decodertools.core;

/**
 * Thrown by methods in the LatLonPoint class to indicate that an input range
 * error has occurred.
 * <p>
 * This class is a sub-class of RuntimeException whose exceptions can be thrown
 * during the normal operation of the Java Virtual Machine. Methods are not
 * required to declare a throws for an exception that may be thrown during the
 * execution of the method but not caught.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080103            384 jkorman     Ported from JET JUTL classes.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class LatLonRangeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a <code>LatLonRangeException</code> with no detail message.
     */
    public LatLonRangeException() {
        super();
    }

    /**
     * Constructs a <code>LatLonRangeException</code> with the specified
     * detail message.
     * 
     * @param theErrorMessage
     *            The detail message.
     */
    public LatLonRangeException(String theErrorMessage) {
        super(theErrorMessage);
    }
}
