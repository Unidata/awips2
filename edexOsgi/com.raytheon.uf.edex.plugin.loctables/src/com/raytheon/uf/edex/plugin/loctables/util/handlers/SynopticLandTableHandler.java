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
 * Parses station data from the synoptic station file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2010            jkorman     Initial creation
 * Oct 12, 2015 4911       rjpeter     Refactored.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SynopticLandTableHandler extends AbstractTableHandler {
    public static final String FILE = "synopticStationTable.txt";

    private static final String UNKN_ICAO = "9999";

    // ENJA 010010 JAN_MAYEN(NOR-NAVY) NO 7093 -867 9 046
    private static final int POS_ICAO = 0;

    private static final int POS_WMO = 9;

    private static final int POS_NAME = 16;

    private static final int POS_COUNTRY = 52;

    private static final int POS_LAT = 55;

    private static final int POS_LON = 60;

    private static final int POS_ELEV = 67;

    private static final int POS_WMO_REGION = 77;

    private static final int END_ICAO = 4;

    private static final int END_WMO = 14;

    private static final int END_NAME = 52;

    private static final int END_COUNTRY = 54;

    private static final int END_LAT = 60;

    private static final int END_LON = 67;

    private static final int END_ELEV = 73;

    private static final int END_WMO_REGION = 78;

    private final Pattern NUMERIC = Pattern.compile("\\d{5}");

    /**
     * 
     */
    public SynopticLandTableHandler() {
        super("SynopticLandTable");
    }

    @Override
    public ObStationRow parseLine(String data) {
        /**
         * <pre>
         *           11111111112222222222333333333344444444445555555555666666666677777777778888888888
         * 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
         * ENJA     010010 JAN_MAYEN(NOR-NAVY)                 NO  7093   -867     9  046  
         * 9999     010020 VERLEGENHUKEN                       NO  8002   1625     8  046
         * </pre>
         */
        ObStationRow row = null;

        if ((data != null) && (data.length() > 79)) {
            String s = data.substring(POS_WMO, END_WMO).trim();
            if (!s.isEmpty()) {
                if (NUMERIC.matcher(s).matches()) {
                    row = new ObStationRow(ObStation.CAT_TYPE_SFC_FXD);
                    row.setWmoIndex(getInt(s));
                    row.setStationId(s);

                    // Check for and set an associated ICAO identifier
                    s = data.substring(POS_ICAO, END_ICAO).trim();
                    if (!UNKN_ICAO.equals(s)) {
                        row.setIcao(s);
                    }

                    Double lat = getDouble(data.substring(POS_LAT, END_LAT)
                            .trim(), null);
                    Double lon = getDouble(data.substring(POS_LON, END_LON)
                            .trim(), null);
                    if ((lat != null) && (lon != null)) {
                        row.setLocation(ObStationRow.getPoint(lat / 100.0,
                                lon / 100.0));

                        Integer elev = getInt(data
                                .substring(POS_ELEV, END_ELEV).trim());
                        if (elev != null) {
                            if (elev == -9999) {

                            }
                            row.setElevation(elev);
                        } else {

                        }

                        s = data.substring(POS_NAME, END_NAME).trim();
                        if (!s.isEmpty()) {
                            row.setName(s);
                        }

                        s = data.substring(POS_COUNTRY, END_COUNTRY).trim();
                        if (!s.isEmpty()) {
                            row.setCountry(s);
                        }

                        s = data.substring(POS_WMO_REGION, END_WMO_REGION);
                        int i = "0123456789".indexOf(s);
                        if (i > -1) {
                            row.setWmoRegion(i);
                        }
                    } else {
                        row = null;
                    }
                }
            }
        }
        return row;
    }
}
