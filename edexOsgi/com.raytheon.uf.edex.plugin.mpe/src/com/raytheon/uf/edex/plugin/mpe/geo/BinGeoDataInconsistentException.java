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
 * attempting to read a binary geo data file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BinGeoDataInconsistentException extends Exception {

    private static final long serialVersionUID = -5239417230961746780L;

    private static final String MSG_FMT = "Insufficient bytes remain for the binary geo data %s. Required %d bytes; available = %d bytes.";

    public static final String SECTION_ID = "id";

    public static final String SECTION_NAME = "name";

    public static final String SECTION_ORDER = "order";

    public static final String SECTION_NUMBER_POINTS = "number of points";
    
    public static final String SECTION_HRAP = "hrap";

    public BinGeoDataInconsistentException(final String sectionName,
            final int bytesRequired, final int bytesAvailable) {
        super(String
                .format(MSG_FMT, sectionName, bytesRequired, bytesAvailable));
    }
}