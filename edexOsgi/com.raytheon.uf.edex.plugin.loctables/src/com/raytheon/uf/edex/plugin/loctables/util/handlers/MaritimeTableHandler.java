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
package com.raytheon.uf.edex.plugin.loctables.util.handlers;

import java.util.regex.Pattern;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;

/**
 * Parses station data from maritime and CMAN station files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2010             jkorman     Initial creation
 * Oct 12, 2015 4911       rjpeter     Refactored.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MaritimeTableHandler extends AbstractTableHandler {
    private final Pattern NUMERIC = Pattern.compile("\\d{5}");

    /**
     * 
     */
    public MaritimeTableHandler() {
        super("MaritimeTable");
    }

    /**
     * @see com.raytheon.uf.edex.plugin.loctables.util.TableHandler#parseLine(java.lang.String)
     */
    @Override
    public ObStationRow parseLine(String data) {
        /**
         * <pre>
         *           11111111112222222222333333333344444444445555555555666666666677777777778888888888
         * 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
         * 0000000000|41001| 34.679| -72.637|    0|6N13 /D E HATTERAS                  |US|BOY
         * 0000000000|41002| 32.281| -75.202|    0|6N35 /D S HATTERAS                  |US|BOY
         * </pre>
         */
        ObStationRow row = null;

        if ((data != null) && (data.length() > 79)) {
            String s = data.substring(11, 16).trim();
            String t = data.substring(80).trim();
            if ((s.length() > 0) && (t.length() > 0)) {
                if ("BOY".equals(t)) {
                    row = new ObStationRow(ObStation.CAT_TYPE_BUOY_FXD);
                    if (NUMERIC.matcher(s).matches()) {
                        row.setWmoIndex(new Integer(s));
                    }
                } else if ("CMAN".equals(t)) {
                    row = new ObStationRow(ObStation.CAT_TYPE_CMAN);
                }
                row.setStationId(s);
                if (row != null) {
                    s = data.substring(17, 24).trim();
                    double lat = Double.parseDouble(s);

                    s = data.substring(25, 33).trim();
                    double lon = Double.parseDouble(s);

                    row.setLocation(ObStationRow.getPoint(lat, lon));

                    s = data.substring(34, 39).trim();
                    if ("-0".equals(s)) {
                        // Unknown station height. we'll deal with this later.
                    } else if (!s.isEmpty()) {
                        Integer elev = new Integer(s);
                        row.setElevation(elev);
                    }

                    s = data.substring(40, 76).trim();
                    if (!s.isEmpty()) {
                        row.setName(s);
                    }

                    s = data.substring(77, 79).trim();
                    if (!s.isEmpty()) {
                        row.setCountry(s);
                    }
                }
            }
        }
        return row;
    }
}
