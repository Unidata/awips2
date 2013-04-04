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
package com.raytheon.uf.common.hydro.dataaccess;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.geom.IGeometryDataFactory;
import com.raytheon.uf.common.dataaccess.geom.IGeometryRequest;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.impl.FactoryUtil;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * A data factory for getting data from the IHFS database. Requires that a
 * request have a table identifier that corresponds to the table it should
 * retrieve data from. Only works against tables that follow the SHEF PEDTSEP
 * pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class HydroGeometryFactory extends AbstractDataFactory implements
        IGeometryDataFactory {

    // TODO always require at least one PE
    // TODO possibly take care of it for them and add value
    // TODO potentially limit big requests so that if too big of a result set,
    // throw error to
    // let the user know they need to reduce amount of data they request
    // test out on live data how slow it gets to determine max number
    // TODO add support for envelopes bounding the request
    private static final String[] REQUIRED = { HydroQueryAssembler.TABLE };

    private GeometryFactory gisFactory = new GeometryFactory();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public DataTime[] getAvailableTimes(IGeometryRequest request)
            throws TimeAgnosticDataException {
        validateRequest(request);
        String query = HydroQueryAssembler.assembleGetTimes(request);
        List<Object[]> result = sendServerRequest(query);
        DataTime[] dts = new DataTime[result.size()];
        for (int i = 0; i < dts.length; i++) {
            dts[i] = new DataTime((Timestamp) result.get(i)[0]);
        }
        return dts;
    }

    @Override
    public DataTime[] getAvailableTimes(IGeometryRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        return FactoryUtil.getAvailableTimes(this, request, binOffset);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    public IGeometryData[] getData(IGeometryRequest request, DataTime... times) {
        validateRequest(request);
        String query = HydroQueryAssembler.assembleGetData(request, times);
        List<Object[]> result = sendServerRequest(query);
        return makeGeometries(result, request.getParameters(),
                request.getIdentifiers());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public IGeometryData[] getData(IGeometryRequest request, TimeRange timeRange) {
        validateRequest(request);
        String query = HydroQueryAssembler.assembleGetData(request, timeRange);
        List<Object[]> result = sendServerRequest(query);
        return makeGeometries(result, request.getParameters(),
                request.getIdentifiers());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.geom.IGeometryDataFactory#
     * getAvailableLocationNames
     * (com.raytheon.uf.common.dataaccess.geom.IGeometryRequest)
     */
    @Override
    public String[] getAvailableLocationNames(IGeometryRequest request) {
        String query = "select lid from location;";
        List<Object[]> results = sendServerRequest(query);
        int size = results.size();
        String[] locations = new String[size];
        for (int i = 0; i < size; i++) {
            locations[i] = (String) results.get(i)[0];
        }

        return locations;
    }

    @Override
    public String[] getRequiredIdentifiers() {
        return REQUIRED;
    }

    /**
     * Sends the query to the server and returns the results
     * 
     * @param query
     *            the query to run
     * @return the results of the query
     */
    private List<Object[]> sendServerRequest(String query) {
        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();

        rcMap.put("query", new RequestConstraint(query));
        rcMap.put("database", new RequestConstraint("ihfs"));
        rcMap.put("mode", new RequestConstraint("sqlquery"));
        QlServerRequest qsr = new QlServerRequest(rcMap);
        AbstractResponseMessage response = null;
        try {
            response = (AbstractResponseMessage) RequestRouter.route(qsr);
        } catch (Exception e) {
            throw new DataRetrievalException("Error retrieving IHFS data", e);
        }

        QueryResult result = null;
        if (response instanceof ResponseMessageError) {
            throw new DataRetrievalException("Error retrieving IHFS data: "
                    + response.toString());
        } else if (response instanceof ResponseMessageGeneric) {
            result = (QueryResult) ((ResponseMessageGeneric) response)
                    .getContents();
        } else {
            throw new DataRetrievalException(
                    "Unable to process response of type" + response.getClass());
        }

        List<Object[]> unmappedResults = new ArrayList<Object[]>();
        for (QueryResultRow row : result.getRows()) {
            unmappedResults.add(row.getColumnValues());
        }
        return unmappedResults;
    }

    /**
     * Builds the data objects that will be returned by calls to getData() on
     * the factory
     * 
     * @param serverResult
     *            the results from the query run on the server
     * @param paramNames
     *            the names of the parameters that were requested
     * @param identifiers
     *            the identifiers from the data request
     * @return the IGeometryData based on the results of the query
     */
    private IGeometryData[] makeGeometries(List<Object[]> serverResult,
            String[] paramNames, Map<String, Object> identifiers) {
        List<IGeometryData> resultList = new ArrayList<IGeometryData>();
        Map<String, Object> attrs = Collections.unmodifiableMap(identifiers);

        // loop over each db row
        for (Object[] row : serverResult) {
            DefaultGeometryData geom = new DefaultGeometryData();
            // order is lid, producttime, lat, lon, other params
            String lid = (String) row[0];
            Timestamp date = (Timestamp) row[1];
            double lat = (Double) row[2];
            double lon = (Double) row[3];
            if (row.length > 4) {
                for (int i = 4; i < row.length; i++) {
                    String name = paramNames[i - 4];
                    geom.addData(name, row[i]);
                }
            }
            geom.setLocationName(lid);
            geom.setDataTime(new DataTime(date));
            // intentionally setting level as null until hydrologists determine
            // something better
            geom.setLevel(null);
            geom.setGeometry(gisFactory.createPoint(new Coordinate(lon, lat)));
            geom.setAttributes(attrs);
            resultList.add(geom);
        }

        return resultList.toArray(new DefaultGeometryData[0]);
    }
}
