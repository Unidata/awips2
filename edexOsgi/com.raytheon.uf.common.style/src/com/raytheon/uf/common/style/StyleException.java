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

package com.raytheon.uf.common.style;

/**
 * Exception for styles.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2007             njensen     Initial creation
 * Sep 5, 2013  2051       mnash       Moved style rules to common and made 
 *                                     this extend Exception instead of VizException
 * </pre>
 * 
 * @author njensen
 */
public class StyleException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * Default Constructor
     * 
     */
    public StyleException() {
        super();
    }

    /**
     * @param message
     */
    public StyleException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public StyleException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public StyleException(Throwable cause) {
        super(cause);
    }

}
