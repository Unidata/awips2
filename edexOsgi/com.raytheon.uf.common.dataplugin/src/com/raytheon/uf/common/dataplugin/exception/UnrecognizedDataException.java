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
package com.raytheon.uf.common.dataplugin.exception;

/**
 * An exception for when the data may be valid but the code does not recognize
 * it and/or doesn't know to handle it.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2014 2581       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class UnrecognizedDataException extends BadDataException {

    private static final long serialVersionUID = 1L;

    /**
     * Default Constructor
     * 
     */
    public UnrecognizedDataException() {
        super();
    }

    /**
     * @param message
     */
    public UnrecognizedDataException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public UnrecognizedDataException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public UnrecognizedDataException(Throwable cause) {
        super(cause);
    }

}
