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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * GeoData Data Manager class.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Sep 12, 2009  2772        mpduff       Initial creation
 * Jun 30, 2015  17360       xwei         Fixed : basins.dat import failed if
 *                                        the first line does not have Lat Lon
 * Aug 18, 2015  4763        rjpeter      Use Number in blind cast.
 * Mar 22, 2016  5217        mduff        Must ensure longitude values are > 0.
 * Jun 28, 2018  6748        randerso     Code cleanup
 * Aug 28, 2018  6979        mduff        Added SSHP Basin methods.
 * </pre>
 *
 * @author mpduff
 */

public class GeoDataManager extends HydroDataManager {
    private static GeoDataManager instance = null;

    private GeoDataManager() {

    }

    /**
     * Get an instance of this class
     *
     * @return The instance
     */
    public static synchronized GeoDataManager getInstance() {
        if (instance == null) {
            instance = new GeoDataManager();
        }

        return instance;
    }

    /**
     * Get the GeoAreas.
     *
     * @param type
     *            The type of area looking for
     * @return List of GeoAreaData objects
     * @throws VizException
     */
    public List<GeoAreaData> getGeoArea(ArealTypeSelection type)
            throws VizException {
        StringBuilder query = new StringBuilder();
        query.append(
                "select area_id, name, boundary_type, interior_lat, interior_lon ");
        query.append("from geoarea where boundary_type = '");
        query.append(type.getDataName()).append("' order by area_id");

        List<Object[]> results = runQuery(query.toString());

        List<GeoAreaData> returnList = new ArrayList<>(results.size());
        for (Object[] item : results) {
            GeoAreaData data = new GeoAreaData();
            data.setAreaId((String) item[0]);
            data.setName((String) item[1]);
            data.setBoundaryType((String) item[2]);
            data.setInteriorLat((Double) item[3]);
            data.setInteriorLon((Double) item[4]);

            returnList.add(data);
        }

        return returnList;
    }
}
