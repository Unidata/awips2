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
package com.raytheon.uf.common.pointdata.dataaccess;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.UnsupportedOutputTypeException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData.Type;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataServerRequest;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Data Access Factory for retrieving point data as a geometry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 31, 2013  2502     bsteffen    Initial creation
 * Nov 26, 2013  2537     bsteffen    Minor code cleanup.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointDataAccessFactory extends AbstractDataPluginFactory {

    private static class TwoDimensionalParameterGroup {

        public final String countParameter;

        public final String levelParameter;

        public final String levelType;

        public final String[] parameters;

        public TwoDimensionalParameterGroup(String countParameter,
                String levelParameter, String levelType, String[] parameters) {
            super();
            this.countParameter = countParameter;
            this.levelParameter = levelParameter;
            this.levelType = levelType;
            this.parameters = parameters;
        }

    }

    private String locationDatabaseKey = "location.stationId";

    private String locationPointDataKey = PointDataConstants.DATASET_STATIONID;

    private String latitudePointDataKey = "latitude";

    private String longitudePointDataKey = "longitude";

    private String refTimePointDataKey = PointDataConstants.DATASET_REFTIME;

    private String fcstHrPointDataKey = PointDataConstants.DATASET_FORECASTHR;

    private Map<String, TwoDimensionalParameterGroup> parameters2D = new HashMap<String, TwoDimensionalParameterGroup>();

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        return getAvailableLocationNames(request, locationDatabaseKey);
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        /*
         * Point data uses PointDataServerRequest instead of the DbQueryRequest
         * that is used in AbstractDataPluginFactory. Override this method so
         * the DbQueryRequest can be converted to a PointDataServerRequest
         */
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this
                .buildDbQueryRequest(request, times);
        return getGeometryData(request, dbQueryRequest);
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        /*
         * Point data uses PointDataServerRequest instead of the DbQueryRequest
         * that is used in AbstractDataPluginFactory. Override this method so
         * the DbQueryRequest can be converted to a PointDataServerRequest
         */
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request,
                timeRange);
        return getGeometryData(request, dbQueryRequest);
    }

    @Override
    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        /*
         * Since the public getGeometryData methods have been overriden, this is
         * now unreachable code, but since it is an abstract method in the super
         * class it must be implemented.
         */
        throw new UnsupportedOperationException(
                "This method should be unreachable");
    }

    @Override
    protected IGridData[] getGridData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        /*
         * Point data cannot be gridded, so don't even try.
         */
        throw new UnsupportedOutputTypeException(request.getDatatype(), "grid");
    }

    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
        String[] locations = request.getLocationNames();
        if (locations != null && locations.length != 0) {
            rcMap.put(locationDatabaseKey, new RequestConstraint(locations));
        }
        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null) {
            for (Entry<String, Object> entry : identifiers.entrySet()) {
                rcMap.put(entry.getKey(), new RequestConstraint(entry
                        .getValue().toString()));
            }
        }
        return rcMap;
    }

    /**
     * 
     * Request point data from the server and convert to {@link IGeometryData}
     * 
     * @param request
     *            the original request from the {@link DataAccessLayer}
     * @param dbQueryRequest
     *            the request generated by {@link AbstractDataPluginFactory},
     *            this will be converted into a {@link PointDataServerRequest}.
     * @return {@link IGeometryData}
     */
    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryRequest dbQueryRequest) {
        PointDataServerRequest serverRequest = convertRequest(request,
                dbQueryRequest);

        PointDataContainer pdc = null;
        try {
            pdc = (PointDataContainer) RequestRouter.route(serverRequest);
        } catch (Exception e) {
            throw new DataRetrievalException(
                    "Unable to complete the PointDataRequestMessage for request: "
                            + request, e);
        }
        if(pdc == null){
            return new IGeometryData[0];
        }
        LevelFactory lf = LevelFactory.getInstance();
        /* Convert the point data container into a list of IGeometryData */
        List<IGeometryData> result = new ArrayList<IGeometryData>(
                pdc.getAllocatedSz());
        for (int i = 0; i < pdc.getCurrentSz(); i += 1) {
            PointDataView pdv = pdc.readRandom(i);
            DefaultGeometryData data = createNewGeometryData(pdv);
            try {
                data.setLevel(lf.getLevel(LevelFactory.UNKNOWN_LEVEL, 0.0));
            } catch (CommunicationException e) {
                throw new DataRetrievalException(
                        "Unable to retrieve level data for request: " + request,
                        e);
            }
            Set<TwoDimensionalParameterGroup> parameters2D = new HashSet<TwoDimensionalParameterGroup>();
            for (String parameter : request.getParameters()) {
                if (pdc.getParameters().contains(parameter)) {
                    int dim = pdc.getDimensions(parameter);
                    if (dim == 1) {
                        Unit<?> unit = pdv.getUnit(parameter);
                        PointDataDescription.Type type = pdv.getType(parameter);
                        if (type == PointDataDescription.Type.STRING) {
                            data.addData(parameter, pdv.getString(parameter),
                                    Type.STRING, unit);
                        } else {
                            data.addData(parameter, pdv.getNumber(parameter),
                                    unit);
                        }
                    } else if (this.parameters2D.containsKey(parameter)) {
                        parameters2D.add(this.parameters2D.get(parameter));
                    } else {
                        throw new DataRetrievalException(
                                "PointDataAccessFactory cannot handle " + dim
                                        + "D parameters: " + parameter);
                    }
                }
            }
            for (TwoDimensionalParameterGroup p2d : parameters2D) {
                result.addAll(make2DData(request, p2d, pdv));
            }
            if (!data.getParameters().isEmpty()) {
                result.add(data);
            }
        }
        return result.toArray(new IGeometryData[0]);
    }

    /**
     * Pull the constraints ouf of a {@link DbQueryRequest} and combine the
     * information with an {@link IDataRequest} to build a
     * {@link PointDataServerRequest}. This is done because
     * {@link AbstractDataPluginFactory} makes really nice DbQueryRequests but
     * we can't use them for point data.
     * 
     * @param request
     * @param dbQueryRequest
     * @return
     */
    private PointDataServerRequest convertRequest(IDataRequest request,
            DbQueryRequest dbQueryRequest) {
        Map<String, RequestConstraint> constraints = dbQueryRequest
                .getConstraints();
        constraints.put(PointDataServerRequest.REQUEST_MODE_KEY,
                new RequestConstraint(PointDataServerRequest.REQUEST_MODE_2D));
        /*
         * Figure out what parameters we actually need.
         */
        Set<String> parameters = new HashSet<String>();
        Set<TwoDimensionalParameterGroup> parameters2D = new HashSet<TwoDimensionalParameterGroup>();

        for (String parameter : request.getParameters()) {
            /*
             * Make sure that any 2D parameters also have the count parameter
             * requested.
             */
            TwoDimensionalParameterGroup p2d = this.parameters2D.get(parameter);
            if (p2d != null) {
                parameters.add(p2d.countParameter);
                parameters.add(p2d.levelParameter);
                parameters2D.add(p2d);
            }
            parameters.add(parameter);
        }
        /* Always request location parameters */
        parameters.add(locationPointDataKey);
        parameters.add(latitudePointDataKey);
        parameters.add(longitudePointDataKey);
        parameters.add(refTimePointDataKey);
        if (fcstHrPointDataKey != null) {
            parameters.add(fcstHrPointDataKey);
        }

        constraints.put(PointDataServerRequest.REQUEST_PARAMETERS_KEY,
                new RequestConstraint(parameters));

        return new PointDataServerRequest(constraints);
    }

    /**
     * Pull out location and time data from a {@link PointDataView} to build a
     * {@link DefaultGeometryData}.
     * 
     * @param pdv
     *            view for a single record
     * @return {@link DefaultGeometryData} with locationName, time, and geometry
     *         set.
     */
    private DefaultGeometryData createNewGeometryData(PointDataView pdv) {
        DefaultGeometryData data = new DefaultGeometryData();
        data.setLocationName(pdv.getString(locationPointDataKey));
        long refTime = pdv.getNumber(refTimePointDataKey).longValue();
        if (fcstHrPointDataKey != null) {
            int fcstTime = pdv.getNumber(fcstHrPointDataKey).intValue();
            data.setDataTime(new DataTime(new Date(refTime), fcstTime));
        } else {
            data.setDataTime(new DataTime(new Date(refTime)));
        }
        Coordinate c = new Coordinate(pdv.getFloat(longitudePointDataKey),
                pdv.getFloat(latitudePointDataKey));
        data.setGeometry(new GeometryFactory().createPoint(c));
        return data;
    }

    /**
     * Make a {@link IGeometryData} object for each level in a 2 dimensional
     * data set.
     * 
     * @param request
     *            the original request
     * @param p2d
     *            The 2d Parameter group
     * @param pdv
     *            pdv contining data.
     * @return One IGeometryData for each valid level in the 2d group.
     */
    private List<IGeometryData> make2DData(IDataRequest request,
            TwoDimensionalParameterGroup p2d, PointDataView pdv) {
        List<String> requestParameters = Arrays.asList(request.getParameters());
        LevelFactory lf = LevelFactory.getInstance();
        int count = pdv.getInt(p2d.countParameter);
        List<IGeometryData> result = new ArrayList<IGeometryData>(count);
        for (int j = 0; j < count; j += 1) {
            /* Clone the data, not level or parameters though */
            DefaultGeometryData leveldata = createNewGeometryData(pdv);
            double levelValue = pdv.getNumberAllLevels(p2d.levelParameter)[j]
                    .doubleValue();
            String levelUnit = UnitFormat.getUCUMInstance().format(
                    pdv.getUnit(p2d.levelParameter));
            try {
                leveldata.setLevel(lf.getLevel(p2d.levelType, levelValue,
                        levelUnit));
            } catch (CommunicationException e) {
                throw new DataRetrievalException(
                        "Unable to retrieve level data for request: " + request,
                        e);
            }
            for (String parameter : p2d.parameters) {
                if (requestParameters.contains(parameter)) {
                    Unit<?> unit = pdv.getUnit(parameter);
                    PointDataDescription.Type type = pdv.getType(parameter);
                    if (type == PointDataDescription.Type.STRING) {
                        leveldata.addData(parameter,
                                pdv.getStringAllLevels(parameter)[j],
                                Type.STRING, unit);
                    } else {
                        leveldata.addData(parameter,
                                pdv.getNumberAllLevels(parameter)[j], unit);
                    }
                }
            }
            result.add(leveldata);
        }
        return result;
    }

    /**
     * Point data types with 2 dimensions need to register so the 2d parameters
     * can be grouped appropriately
     * 
     * @param countParameter
     *            parameter name of an integer parameter identifying the number
     *            of valid levels.
     * @param levelParameter
     *            parameter which should be used to build the level object in
     *            IGeometryData, for example "pressure"
     * @param levelType
     *            {@link MasterLevel} name for the levelParameter, for example
     *            "MB"
     * @param parameters
     *            all the parameters that are valid on the same 2D levels.
     * @return countParameter is returned so spring can have a bean.
     */
    public String register2D(String countParameter, String levelParameter,
            String levelType, String[] parameters) {
        TwoDimensionalParameterGroup td = new TwoDimensionalParameterGroup(
                countParameter, levelParameter, levelType, parameters);
        for (String parameter : parameters) {
            parameters2D.put(parameter, td);
        }
        return countParameter;
    }

    /**
     * @param locationDatabaseKey
     *            The hibernate field name of the field that is used to identify
     *            location names. Default values is "location.stationId"
     */
    public void setLocationDatabaseKey(String locationDatabaseKey) {
        this.locationDatabaseKey = locationDatabaseKey;
    }

    /**
     * @param locationPointDataKey
     *            The point data key that matches the location database key.
     *            Defaults to "stationId"
     */
    public void setLocationPointDataKey(String locationPointDataKey) {
        this.locationPointDataKey = locationPointDataKey;
    }

    /**
     * @param latitudePointDataKey
     *            The point data key of the station latitude. Default value is
     *            "latitude"
     */
    public void setLatitudePointDataKey(String latitudePointDataKey) {
        this.latitudePointDataKey = latitudePointDataKey;
    }

    /**
     * @param longitudePointDataKey
     *            The point data key of the station longitude. Default value is
     *            "longitude"
     */
    public void setLongitudePointDataKey(String longitudePointDataKey) {
        this.longitudePointDataKey = longitudePointDataKey;
    }

    /**
     * @param refTimePointDataKey
     *            The point data key of the reference time. Default value is
     *            "refTime"
     */
    public void setRefTimePointDataKey(String refTimePointDataKey) {
        this.refTimePointDataKey = refTimePointDataKey;
    }

    /**
     * @param fcstHrPointDataKey
     *            The point data key of the forecast hour. Default value is
     *            "forecastHr". For live data with no forecast times this can be
     *            set to null so that it is not retrieved.
     */
    public void setFcstHrPointDataKey(String fcstHrPointDataKey) {
        this.fcstHrPointDataKey = fcstHrPointDataKey;
    }

}
