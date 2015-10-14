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
package com.raytheon.viz.hydrobase.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants.ArealTypeSelection;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;
import com.raytheon.viz.hydrocommon.util.DbUtils;

/**
 * GeoData Data Manager class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2009 2772       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GeoDataManager extends HydroDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoDataManager.class);
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
    public ArrayList<GeoAreaData> getGeoArea(ArealTypeSelection type)
            throws VizException {
        ArrayList<GeoAreaData> returnList = new ArrayList<GeoAreaData>();
        StringBuilder query = new StringBuilder();
        query.append("select area_id, name, boundary_type, interior_lat, interior_lon from geoarea ");
        query.append(" where boundary_type = '"
                + HydroConstants.GEOAREA_DATANAMES[type.ordinal()]
                + "' order by area_id");

        List<Object[]> results = runQuery(query.toString());

        returnList.ensureCapacity(results.size());
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

    /**
     * Delete data from the linesegs table.
     * 
     * @param type
     *            The type of data to delete
     * @return The number of lines modified
     * @throws VizException
     */
    public int deleteLineSegs(String type) throws VizException {
        String query = "DELETE FROM LineSegs WHERE area_id IN ("
                + "SELECT area_id FROM GeoArea WHERE boundary_type='" + type
                + "')";

        int status = DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                QueryLanguage.SQL);

        return status;
    }

    /**
     * Delete data from the geoarea table.
     * 
     * @param type
     *            The type of data to delete
     * @return The number of lines modified
     * @throws VizException
     */
    public int deleteGeoArea(String type) throws VizException {
        String query = "DELETE FROM GeoArea WHERE boundary_type = '" + type
                + "'";

        int status = DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                QueryLanguage.SQL);

        return status;
    }

    /**
     * Write the GeoAreaData data to the IHFS
     * 
     * @param data
     *            The GeoAreaData object to write
     * @return The number of rows modified
     * @throws VizException
     */
    public int putGeoArea(GeoAreaData data) throws VizException {
        int status = 1;
        double[] lat;
        double[] lon;
        double intLat;
        double intLon;
        double latTotal = 0;
        double lonTotal = 0;
        double posWeight = 0;
        double weightTotal = 0;

        if (data == null) {
            status = -1;

            return status;
        }

        DbUtils.escapeSpecialCharforData(data);

        /*
         * if the interior lat, lon were provided from the input file, then use
         * them. otherwise compute them.
         */
        lat = data.getLat();
        lon = data.getLon();

        if (data.getInteriorLat() != HydroConstants.UNASSIGNED) {
            intLat = data.getInteriorLat();
            intLon = data.getInteriorLon();
        } else {
            /*
             * compute the interior lat-lon using weighted method; subtract
             * numbers to reduce magnitude out of paranoia
             */
            for (int i = 0; i < data.getNumberPoints(); i++) {
                if (i > 0) {
                    posWeight = Math.sqrt(Math.pow((lat[i] - lat[i - 2]), 2)
                            + Math.pow((lon[i] - lon[i - 1]), 2));
                }

                weightTotal += posWeight;
                latTotal += posWeight * lat[i];
                lonTotal += posWeight * lon[i];
            }

            intLat = latTotal / weightTotal;
            intLon = lonTotal / weightTotal;

            data.setInteriorLat(intLat);
            data.setInteriorLon(intLon);
        }

        StringBuilder query = new StringBuilder();
        query.append("insert into geoarea (area_id,");
        query.append(" name, boundary_type, interior_lat, interior_lon)");
        query.append(" values ('" + data.getAreaId() + "', '");
        query.append(data.getName() + "', '");
        query.append(data.getBoundaryType() + "', ");
        query.append(data.getInteriorLat() + ", ");
        query.append(data.getInteriorLon() + ")");

        status = DirectDbQuery.executeStatement(query.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (status != 1) {
            System.err.println("Error inserting " + data.getBoundaryType()
                    + " data to GeoArea for: " + data.getAreaId() + ":"
                    + data.getName() + ":" + data.getNumberPoints());
            status = -1;
        }

        return status;
    }

    /**
     * Write thte line segments to the linesegs table
     * 
     * @param areaId
     *            The area id
     * @param binList
     *            The HrapBinList
     */
    public void putLineSegs(String areaId, HrapBinList binList) {
        int status = 0;
        long count = 0;
        double area = binList.getArea();
        StringBuilder query = new StringBuilder();
        StringBuilder where = new StringBuilder();

        for (int i = 0; i < binList.getNumRows(); i++) {
            long hrapRow = binList.getRows().get(i);
            long hrapBegCol = binList.getBeginCols().get(i);
            long hrapEndCol = binList.getEndCols().get(i);

            where.setLength(0);
            query.setLength(0);

            /* check if the record exists in the database */
            where.append(" where area_id = '" + areaId + "' and ");
            where.append(" hrap_row = " + hrapRow);
            where.append(" and hrap_beg_col = " + hrapBegCol);

            query.append("select count(*) from linesegs ");
            query.append(where.toString());
            ArrayList<Object[]> rs = runQuery(query.toString());
            count = (Long) (rs.get(0)[0]);
            if (count == 0) {
                // insert the record
                query.setLength(0);
                query.append("insert into linesegs (area_id, hrap_row, ");
                query.append("hrap_beg_col, hrap_end_col, area) values ");
                query.append("('" + areaId + "', " + hrapRow + ", ");
                query.append(hrapBegCol + ", " + hrapEndCol + ", ");
                query.append(area + ")");

                try {
                    status = DirectDbQuery.executeStatement(query.toString(),
                            HydroConstants.IHFS, QueryLanguage.SQL);
                    if (status != 1) {
                        throw new VizException();
                    }
                } catch (VizException e) {
                    status = -1;
                    statusHandler.handle(Priority.PROBLEM,
                            "Error putting data into LineSegs for area_id:  "
                                    + areaId);
                }
            } else {
                /* delete the record and insert the new record */
                String delete = "delete from linesegs " + where.toString();

                try {
                    runStatement(delete);
                    status = DirectDbQuery.executeStatement(query.toString(),
                            HydroConstants.IHFS, QueryLanguage.SQL);
                    if (status != 1) {
                        throw new VizException();
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating LineSegs for area_id:  " + areaId);
                }
            }
        }
    }
}
