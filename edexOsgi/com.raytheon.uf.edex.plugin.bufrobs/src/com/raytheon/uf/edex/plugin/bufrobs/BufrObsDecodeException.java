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
 * Exception thrown when a BUFR obs cannot be decoded
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2014  2906      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrObsDecodeException extends Exception {

    private static final long serialVersionUID = 530575366878300860L;

    /**
     * 
     */
    public BufrObsDecodeException() {
    }

    /**
     * @param message
     */
    public BufrObsDecodeException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public BufrObsDecodeException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public BufrObsDecodeException(String message, Throwable cause) {
        super(message, cause);
    }

}
