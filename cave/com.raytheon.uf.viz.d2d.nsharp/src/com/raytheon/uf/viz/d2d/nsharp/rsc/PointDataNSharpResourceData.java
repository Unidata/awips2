/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import org.locationtech.jts.geom.Coordinate;

/**
 * 
 * A base class for nsharp resources for data types that share common
 * functionality through the point data API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 28, 2018  6800     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public abstract class PointDataNSharpResourceData
        extends D2DNSharpResourceData {

    protected static final String LONGITUDE = "location.longitude";

    protected static final String LATITUDE = "location.latitude";

    protected static final String STATION_ID = "location.stationId";

    public PointDataNSharpResourceData() {
        super();
    }

    public PointDataNSharpResourceData(String soundingType) {
        super(soundingType);
    }

    @Override
    protected void preparePointInfo() throws VizException {
        String stationNameKey = getPointNameKey();
        if (pointName != null && pointName.startsWith("Point")) {
            /*
             * The resource data has been altered through IPointsToolContainer.
             * Normally the coordinate must be very close to the actual location
             * for a query to work but more flexibility is allowed for alter
             * bundle, so query the database for a more precise location and
             * name.
             */
            DbQueryRequest request = new DbQueryRequest();
            Map<String, RequestConstraint> constraints = new HashMap<>(
                    getMetadataMap());
            constraints.remove(STATION_ID);
            constraints.put(LONGITUDE,
                    new RequestConstraint(Double.toString(coordinate.x - 1),
                            Double.toString(coordinate.x + 1)));
            constraints.put(LATITUDE,
                    new RequestConstraint(Double.toString(coordinate.y - 1),
                            Double.toString(coordinate.y + 1)));
            request.setConstraints(constraints);
            request.addRequestField(LONGITUDE);
            request.addRequestField(LATITUDE);
            request.addRequestField(STATION_ID);
            if (stationNameKey != null && !STATION_ID.equals(stationNameKey)) {
                request.addRequestField(stationNameKey);
            }
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);

            double bestDist = Double.POSITIVE_INFINITY;
            Map<String, Object> bestResult = null;
            for (Map<String, Object> result : response.getResults()) {
                Coordinate c = unpackResultLocation(result, LONGITUDE,
                        LATITUDE);
                double dist = this.coordinate.distance(c);
                if (dist < bestDist) {
                    bestDist = dist;
                    bestResult = result;
                }
            }
            if (bestResult != null) {
                this.coordinate = unpackResultLocation(bestResult, LONGITUDE,
                        LATITUDE);
                this.pointName = (String) bestResult.get(stationNameKey);
                this.metadataMap.put(STATION_ID, new RequestConstraint(
                        (String) bestResult.get(STATION_ID)));
            }
        } else if (coordinate == null || pointName == null) {
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints(getMetadataMap());
            request.addRequestField(LONGITUDE);
            request.addRequestField(LATITUDE);
            request.addRequestField(STATION_ID);
            if (stationNameKey != null) {
                request.addRequestField(stationNameKey);
            }
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);

            for (Map<String, Object> result : response.getResults()) {
                if (stationNameKey != null && pointName == null) {
                    pointName = (String) result.get(stationNameKey);
                }
                if (coordinate == null) {
                    coordinate = unpackResultLocation(result, LONGITUDE,
                            LATITUDE);
                }
            }
        }
    }

    /**
     * Subclasses should implement this to indicate a hibernate key that should
     * be used to populate the pointName field. STATION_ID is often a good
     * choice, however for some types this may be meaningless or too technical
     * for the user . Subclasses may return a more user friendly database column
     * or simply return null in which case the raw lat/lon will be displayed to
     * the user in place of a name.
     * 
     * @return a hibernate key or null.
     */
    protected abstract String getPointNameKey();

}
