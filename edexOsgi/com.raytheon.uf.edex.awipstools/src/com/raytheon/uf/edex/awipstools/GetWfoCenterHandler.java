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
package com.raytheon.uf.edex.awipstools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handler for retrieving center points for WFOs. Checks a localization file
 * first, if none, looks up in maps database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GetWfoCenterHandler implements IRequestHandler<GetWfoCenterPoint> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetWfoCenterHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Coordinate handleRequest(GetWfoCenterPoint request) throws Exception {

        // Lookup in localization file
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContextForSite(
                LocalizationType.COMMON_STATIC, request.getWfoId());
        File file = pm.getFile(context, Constants.TOOLS_DIRECTORY
                + File.separator + Constants.CENTER_POINT_FILE);
        Coordinate loc = loadPoint(file);

        if (loc == null) {
            loc = lookupInWfoCenter(request.getWfoId());
            if (loc != null) {
                storePoint(file, loc);
            }
        }

        return loc;
    }

    /**
     * @return
     */
    private static Coordinate lookupInWfoCenter(String wfoId) {
        Coordinate loc = null;
        try {
            String query = String.format(
                    "SELECT lat,lon FROM mapdata.cwa WHERE wfo = '%s' LIMIT 1",
                    wfoId);
            QueryResult result = (QueryResult) new CoreDao(
                    DaoConfig.forDatabase("maps")).executeNativeSql(query);

            if (result.getRows().length == 0) {
                query = String
                        .format("select ST_Y(theCentroid) as lat, ST_X(theCentroid) as lon from (select ST_CENTROID(theUnion) as theCentroid from (select ST_Union(the_geom) as theUnion from mapdata.rfc where site_id = '%s') as dummyAlias) as dummyAlias;",
                                wfoId);
                result = (QueryResult) new CoreDao(
                        DaoConfig.forDatabase("maps")).executeNativeSql(query);

            }

            if (result.getRows().length > 0) {
                QueryResultRow row = result.getRows()[0];
                Double lat = ((Number) row.getColumn(0)).doubleValue();
                Double lon = ((Number) row.getColumn(1)).doubleValue();
                loc = new Coordinate(lon, lat);
            }

            if (loc == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "No location information found for wfo: " + wfoId);
            }
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error executing query for wfo center point", e);
        }

        return loc;
    }

    /**
     * Load the point from the
     * 
     * @param file
     * @return
     */
    private static Coordinate loadPoint(File file) {
        if (file.exists() == false) {
            return null;
        }
        Coordinate point = null;
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));
            String line = in.readLine();
            line = line.trim();
            int p = line.indexOf(' ');
            double lat = Double.parseDouble(line.substring(0, p));
            double lon = Double.parseDouble(line.substring(p));

            if (lat > 90.0 || lat < -90.0 || lon > 180.0 || lon < -180.0) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Invalid lat/lon in wfo center point file, using default");
            } else {
                point = new Coordinate(lon, lat);
            }
        } catch (NumberFormatException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Invalid number in wfo center point file, using default",
                            e);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.EVENTA,
                    "No wfo center point file found, creating default.");
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading wfo center point file, using default", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    // nothing to do
                }
            }
        }
        return point;
    }

    /**
     * Store the point to the file passed in
     * 
     * @param file
     * @param point
     */
    private static void storePoint(File file, Coordinate point) {
        // create the local directory if necessary
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(file));
            out.write(String.format("%f %f\n", point.y, point.x));
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error writing to file: "
                    + file.getAbsolutePath(), e);
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error writing to file: " + file.getAbsolutePath(),
                            e);
                }
            }
        }
    }
}
