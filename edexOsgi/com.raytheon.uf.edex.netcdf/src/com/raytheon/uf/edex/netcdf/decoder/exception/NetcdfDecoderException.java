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
package com.raytheon.uf.edex.netcdf.decoder.exception;

/**
 * Generic exception for issues encountered while decoding Netcdf Files.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2015 4699       nabowle     Initial creation
 * Jun 16, 2016 5584       nabowle     Finished writing the javadoc.
 *
 * </pre>
 *
 * @author nabowle
 */

public class NetcdfDecoderException extends Exception {

    /**
     *
     */
    public NetcdfDecoderException() {
    }

    /**
     * @param message
     */
    public NetcdfDecoderException(String message) {
        super(message);

    }

    /**
     * @param cause
     */
    public NetcdfDecoderException(Throwable cause) {
        super(cause);

    }

    /**
     * @param message
     * @param cause
     */
    public NetcdfDecoderException(String message, Throwable cause) {
        super(message, cause);

    }

    /**
     * @param message
     * @param cause
     * @param enableSuppression
     * @param writableStackTrace
     */
    public NetcdfDecoderException(String message, Throwable cause,
            boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);

    }

}
