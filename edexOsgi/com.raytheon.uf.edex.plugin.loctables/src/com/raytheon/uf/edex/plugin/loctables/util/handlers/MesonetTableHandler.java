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
 * Parses station data from the mesonet station file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2011            skorolev    Initial creation
 * Oct 12, 2015 4911       rjpeter     Refactored.
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class MesonetTableHandler extends AbstractTableHandler {

    private static final String DELIMITER = "\\|";

    /**
     * @param name
     * @param storeStrategy
     */
    public MesonetTableHandler() {
        super("MesonetTable");
    }

    @Override
    public ObStationRow parseLine(String data) {
        /**
         * <pre>
         * AALND|AALND|Austin Academy For Excellen Garland, TX| 107.620| 33.5108| -94.5753|CST6CDT |||1|||TX
         * </pre>
         */
        ObStationRow row = null;
        if (data != null) {
            row = new ObStationRow(ObStation.CAT_TYPE_MESONET);
            String[] s = data.split(DELIMITER);
            row.setStationId(s[1]);
            double lat = Double.parseDouble(s[4].trim());
            double lon = Double.parseDouble(s[5].trim());

            row.setLocation(ObStationRow.getPoint(lat, lon));
            Integer elev = Float.valueOf(s[3].trim()).intValue();
            row.setElevation(elev);
            row.setName(s[2].trim());
            row.setState(s[12]);
            row.setCountry("US");
        }
        return row;
    }
}
