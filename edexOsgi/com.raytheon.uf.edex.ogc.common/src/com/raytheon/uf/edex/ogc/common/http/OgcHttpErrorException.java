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
package com.raytheon.uf.edex.ogc.common.http;

/**
 * Exception for when an OGC request fails at the HTTP level. Contains HTTP
 * error code for response.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2013 2539       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcHttpErrorException extends Exception {

    private static final long serialVersionUID = -452797331391106559L;

    private final int code;

    /**
     * 
     */
    public OgcHttpErrorException(int code) {
        super();
        this.code = code;
    }

    /**
     * @param message
     * @param cause
     */
    public OgcHttpErrorException(int code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
    }

    /**
     * @param message
     */
    public OgcHttpErrorException(int code, String message) {
        super(message);
        this.code = code;
    }

    /**
     * @param cause
     */
    public OgcHttpErrorException(int code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    /**
     * @return the error status code
     */
    public int getCode() {
        return code;
    }

}
