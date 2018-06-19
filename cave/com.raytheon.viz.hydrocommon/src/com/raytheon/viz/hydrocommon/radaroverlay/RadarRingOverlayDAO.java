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
package com.raytheon.viz.hydrocommon.radaroverlay;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Radar Rings Overlay Data Access Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010 1783       mpduff      Initial creation.
 * Jan 22, 2010 4356       mpduff      Added getRadarAvailable method.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarRingOverlayDAO {
    /** Radar Query */
    private static final String RADAR_QUERY = "select radid, lat, lon from radarloc";

    /** HashMap of RadarRingOverlayData objects */
    private Map<String, RadarRingOverlayData> radarMap = null;

    /**
     * Get the radar data.
     * 
     * @return HashMap of radid->RadarRingOverlayData objects
     * @throws VizException
     */
    public Map<String, RadarRingOverlayData> getData() throws VizException {
        // Query for the id and location of the radar
        List<Object[]> rs = null;

        if (radarMap == null) {
            radarMap = new HashMap<String, RadarRingOverlayData>();
            rs = DirectDbQuery.executeQuery(RADAR_QUERY, HydroConstants.IHFS,
                    QueryLanguage.SQL);

            for (Object[] oa : rs) {
                RadarRingOverlayData data = new RadarRingOverlayData();
                data.setRadId((String) oa[0]);
                data.setLat((Double) oa[1]);
                data.setLon((Double) oa[2] * -1);
                radarMap.put(data.getRadId(), data);
            }
        }
        return radarMap;
    }

    /**
     * Sets the radar available flag in the RadarRingOverlayData object.
     * 
     * @param data
     *            The RadarRingOverlayData object
     * @param displayDate
     *            The currently displayed MPE date
     * @throws VizException
     */
    public void getRadarAvailable(RadarRingOverlayData data, Date displayDate)
            throws VizException {
        String sql = "select radid, rad_avail from rwradarresult";
        String where = " where radid = '" + data.getRadId()
                + "' and obstime = '"
                + HydroConstants.DATE_FORMAT.format(displayDate) + "'";

        List<Object[]> rs = null;

        // Query for the ids and availability of radars in IHFS
        rs = DirectDbQuery.executeQuery(sql + where, HydroConstants.IHFS,
                QueryLanguage.SQL);

        if (rs.size() > 0) {
            Object[] oa = rs.get(0);
            String radAvail = (String) oa[1];
            if (radAvail.equalsIgnoreCase("Y")
                    || radAvail.equalsIgnoreCase("Z")) {
                data.setRadAvail(true);
            } else {
                data.setRadAvail(false);
            }
        } else {
            data.setRadAvail(false);
        }
    }
}
