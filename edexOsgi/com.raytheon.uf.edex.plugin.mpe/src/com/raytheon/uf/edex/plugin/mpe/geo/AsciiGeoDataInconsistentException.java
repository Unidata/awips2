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
package com.raytheon.uf.edex.plugin.mpe.geo;

/**
 * Indicates an inconsistent/unexpected structure was encountered while
 * attempting to read an ascii geo data file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class AsciiGeoDataInconsistentException extends Exception {

    private static final long serialVersionUID = -8169930719882081195L;

    public AsciiGeoDataInconsistentException(String message) {
        super(message);
    }

    public AsciiGeoDataInconsistentException(Throwable cause) {
        super(cause);
    }

    public AsciiGeoDataInconsistentException(String message, Throwable cause) {
        super(message, cause);
    }
}