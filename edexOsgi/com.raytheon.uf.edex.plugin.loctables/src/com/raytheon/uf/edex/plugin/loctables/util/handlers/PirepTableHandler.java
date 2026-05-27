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

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;

/**
 * Parses station data from the pirep station file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2010             jkorman     Initial creation
 * Oct 12, 2015 4911       rjpeter     Refactored.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PirepTableHandler extends AbstractTableHandler {
    /**
     * 
     */
    public PirepTableHandler() {
        super("PirepTable");
    }

    /**
     * 
     * @param data
     * @return
     */
    @Override
    public ObStationRow parseLine(String data) {
        /**
         * <pre>
         *           1111111111222222222233333333334444444444555555555566666666667777777777
         * 01234567890123456789012345678901234567890123456789012345678901234567890123456789
         * AAO 0 WICHITA KS US 3775 -9722 0 0
         * </pre>
         */
        ObStationRow row = null;

        if ((data != null) && (data.length() > 67)) {
            String s = data.substring(0, 9).trim();
            if (!s.isEmpty()) {
                row = new ObStationRow(ObStation.CAT_TYPE_ACFT_PIREP);
                row.setStationId(s);

                s = data.substring(16, 48).trim();
                if (!s.isEmpty()) {
                    row.setName(s);
                }

                s = data.substring(49, 51).trim();
                if (!s.isEmpty()) {
                    row.setState(s);
                }

                s = data.substring(52, 54).trim();
                if (!s.isEmpty()) {
                    row.setCountry(s);
                }

                s = data.substring(55, 60).trim();
                double lat = Double.parseDouble(s) / 100;

                s = data.substring(60, 67).trim();
                double lon = Double.parseDouble(s) / 100;

                row.setLocation(ObStationRow.getPoint(lat, lon));
            }
        }
        return row;
    }
}
