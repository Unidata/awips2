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
package com.raytheon.uf.edex.plugin.bufrobs;

/**
 * Exception thrown when BUFR obs is missing required data fields
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2014 2906       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class MissingRequiredDataException extends BufrObsDecodeException {

    private static final long serialVersionUID = -7605144038224407338L;

    /**
     * 
     */
    public MissingRequiredDataException() {
    }

    /**
     * @param message
     */
    public MissingRequiredDataException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public MissingRequiredDataException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public MissingRequiredDataException(String message, Throwable cause) {
        super(message, cause);
    }

}
