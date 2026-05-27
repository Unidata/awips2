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
package com.raytheon.uf.viz.archive.ui;

/**
 * Exception to use when problem creating a case.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2013 2225       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CaseCreateException extends Exception {
    static final long serialVersionUID = 0L;

    /**
     * 
     */
    public CaseCreateException() {
    }

    /**
     * @param message
     */
    public CaseCreateException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public CaseCreateException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public CaseCreateException(String message, Throwable cause) {
        super(message, cause);
    }
}
