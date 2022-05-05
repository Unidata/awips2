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
package com.raytheon.uf.common.dataplugin.radar.dataaccess;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.EnvelopeProjectionException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.filter.FillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;

/**
 *
 * A data factory for getting radar data from the metadata database. There are
 * currently not any required identifiers.
 *
 * This class handles requests for both grid data (i.e., radial and raster
 * products) and geometry data (i.e., graphics products).
 *
 * Radar does not return subgrids for request envelopes like other gridded
 * types. Instead data for only icaos within the request envelope are returned
 * and all data for the product is used. This is done because subgridding radial
 * products is complex and this is not often what a caller actually wants.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 23, 2013           bsteffen    Initial creation
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Feb 04, 2014  2672     bsteffen    Enable requesting icaos within envelope.
 * Jul 30, 2014  3184     njensen     Overrode optional identifiers
 * Oct 28, 2014  3755     nabowle     Implement getAvailableParameters, handle
 *                                    empty parameters, fix error message, and
 *                                    handle dataless radial radars.
 * Dec 18, 2014  3600     nabowle     Implement getAvailableLevels and add
 *                                    optional identifiers to indicate what
 *                                    fields are used for the level one and two
 *                                    values.
 * Feb 13, 2015  4124     mapeters    Inherits IDataFactory.
 * Feb 27, 2015  4179     mapeters    Use AbstractDataPluginFactory.getAvailableValues().
 * Apr 18, 2016  5587     tgurney     Implement getIdentifierValues()
 * Jun 07, 2016  5587     tgurney     Change get*Identifiers() to take
 *                                    IDataRequest
 * Jun 08, 2016  5574     mapeters    Add advanced query support, throw exception for
 *                                    invalid level field identifiers
 * Aug 01, 2016  2416     tgurney     Add dataURI as optional identifier
 * Aug 29, 2016  2671     tgurney     Add support for melting layer graphics
 * Aug 31, 2016  2671     tgurney     Add mesocyclone support
 * Sep 09, 2016  2671     tgurney     Add storm track (STI) support
 * Sep 27, 2016  2671     tgurney     Add hail index support
 * Sep 28, 2016  2671     tgurney     Add tornado vortex sig (TVS) support
 * Mar 06, 2017  6142     bsteffen    Remove dataURI as optional identifier
 * Aug 14, 2017  6390     njensen     Support unsigned data in getDataSource()
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarDataAccessFactory extends AbstractGridDataPluginFactory {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDataAccessFactory.class);

    private static final String PRODUCT_CODE = "productCode";

    private static final String PRIMARY_ANGLE = "primaryElevationAngle";

    private static final String TRUE_ANGLE = "trueElevationAngle";

    private static final String ELEVATION_NUMBER = "elevationNumber";

    private static final String ICAO = "icao";

    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";

    private static final String FORMAT = "format";

    private static final String RADIAL_FORMAT = "Radial";

    private static final String RASTER_FORMAT = "Raster";

    private static final String GRAPHIC_FORMAT = "Graphic";

    private static final String LEVEL_ONE = "level.one.field";

    private static final String LEVEL_TWO = "level.two.field";

    private static final List<String> SUPPORTED_GRID_FORMATS = Arrays
            .asList(RADIAL_FORMAT, RASTER_FORMAT);

    private static final List<String> SUPPORTED_GEOMETRY_FORMATS = Arrays
            .asList(GRAPHIC_FORMAT);

    private static final List<String> SUPPORTED_FORMATS = Arrays
            .asList(RADIAL_FORMAT, RASTER_FORMAT, GRAPHIC_FORMAT);

    private static final List<String> SUPPORTED_LEVELS = Arrays
            .asList(PRIMARY_ANGLE, TRUE_ANGLE, ELEVATION_NUMBER);

    private static final String LEVEL_ERROR = " must be " + PRIMARY_ANGLE + ", "
            + TRUE_ANGLE + ", or " + ELEVATION_NUMBER;

    private static RadarInfoDict radarInfo = null;

    private static MasterLevel tiltLevel = null;

    @Override
    protected IGridData constructGridDataResponse(IDataRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            DataSource dataSource) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        RadarInfo radarInfo = getRadarInfo()
                .getInfo(radarRecord.getProductCode());
        if (!SUPPORTED_GRID_FORMATS.contains(radarInfo.getFormat())) {
            throw new DataRetrievalException(radarInfo.getFormat()
                    + " data cannot be requested as grid");
        }
        DefaultGridData defaultGridData = new DefaultGridData(dataSource,
                gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        // reverse map parameter to match request.
        List<String> requestedParameters = Arrays
                .asList(request.getParameters());
        if (requestedParameters.contains(radarInfo.getName())) {
            defaultGridData.setParameter(radarInfo.getName());
        } else if (requestedParameters.contains(radarInfo.getMnemonic())) {
            defaultGridData.setParameter(radarRecord.getMnemonic());
        } else {
            defaultGridData
                    .setParameter(radarRecord.getProductCode().toString());
        }
        defaultGridData.setUnit(radarRecord.getDataUnit());
        defaultGridData.setLevel(getLevel(radarRecord, request));
        defaultGridData.setLocationName(generateLocationName(radarRecord));

        Map<String, Object> attributes = new HashMap<>();
        attributes.put(ICAO, radarRecord.getIcao());
        attributes.put(FORMAT, radarRecord.getFormat());

        defaultGridData.setAttributes(attributes);

        return defaultGridData;
    }

    @Override
    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {

        Map<File, List<RadarRecord>> results = unpackResults(dbQueryResponse);
        List<IGeometryData> rval = new ArrayList<>();

        for (Entry<File, List<RadarRecord>> resultEntry : results.entrySet()) {
            IDataStore ds = DataStoreFactory.getDataStore(resultEntry.getKey());
            for (RadarRecord radarRecord : resultEntry.getValue()) {
                RadarInfo radarInfo = getRadarInfo()
                        .getInfo(radarRecord.getProductCode());

                if (!SUPPORTED_GEOMETRY_FORMATS
                        .contains(radarInfo.getFormat())) {
                    throw new DataRetrievalException(radarInfo.getFormat()
                            + " data cannot be requested as geometry");
                }

                try {
                    RadarDataRetriever.populateRadarRecord(ds, radarRecord);
                } catch (FileNotFoundException | StorageException e) {
                    throw new DataRetrievalException(e.getLocalizedMessage(),
                            e);
                }

                DefaultGeometryData[] geomDatas = null;
                // TODO: Update this when more radar graphics are supported
                if (radarInfo.getProductCode() == 166) {
                    geomDatas = RadarGeometryDataUtil
                            .makeMeltingLayerGeom(radarRecord);
                } else if (radarInfo.getProductCode() == 141) {
                    geomDatas = RadarGeometryDataUtil
                            .makeMesocycloneGeom(radarRecord);
                } else if (radarInfo.getProductCode() == 58) {
                    geomDatas = RadarGeometryDataUtil
                            .makeStormTrackGeom(radarRecord);
                } else if (radarInfo.getProductCode() == 59) {
                    geomDatas = RadarGeometryDataUtil
                            .makeHailIndexGeom(radarRecord);
                } else if (radarInfo.getProductCode() == 61) {
                    geomDatas = RadarGeometryDataUtil.makeTVSGeom(radarRecord);
                } else {
                    throw new DataRetrievalException(
                            "Product code " + radarInfo.getProductCode()
                                    + " is not yet supported.");
                }
                // reverse map parameter to match request.
                List<String> requestedParameters = Arrays
                        .asList(request.getParameters());
                String productCode = radarRecord.getProductCode().toString();
                for (DefaultGeometryData geomData : geomDatas) {
                    if (requestedParameters.contains(radarInfo.getName())) {
                        geomData.addData(radarInfo.getName(),
                                radarInfo.getName());
                    } else if (requestedParameters
                            .contains(radarInfo.getMnemonic())) {
                        geomData.addData(radarInfo.getMnemonic(),
                                radarInfo.getMnemonic());
                    } else {
                        geomData.addData(productCode, productCode);
                    }

                    // set remaining metadata
                    geomData.setDataTime(radarRecord.getDataTime());
                    geomData.setLevel(getLevel(radarRecord, request));
                    geomData.setLocationName(generateLocationName(radarRecord));
                    geomData.addAttribute(ICAO, radarRecord.getIcao());
                    geomData.addAttribute(FORMAT, radarRecord.getFormat());
                    rval.add(geomData);
                }
            }
        }
        return rval.toArray(new IGeometryData[rval.size()]);
    }

    /**
     * Unpack records from response and group by HDF5 file
     *
     * @param dbQueryResponse
     * @return
     */
    private static Map<File, List<RadarRecord>> unpackResults(
            DbQueryResponse dbQueryResponse) {
        // Bin up requests to the same hdf5
        Map<File, List<RadarRecord>> fileMap = new HashMap<>();

        for (Map<String, Object> result : dbQueryResponse.getResults()) {
            Object object = result.get(null);
            if (object == null || !(object instanceof RadarRecord)) {
                statusHandler.warn(
                        "Unexpected DB query result for radar: " + object);
                continue;
            }
            RadarRecord record = (RadarRecord) object;
            File hdf5File = PDOUtil.getHDF5File(record);
            List<RadarRecord> recList = fileMap.get(hdf5File);
            if (recList == null) {
                recList = new ArrayList<>();
                fileMap.put(hdf5File, recList);
            }
            recList.add(record);
        }
        return fileMap;
    }

    /**
     * Get the level for the radar record. If the request specifies
     * {@value #LEVEL_ONE} or {@value #LEVEL_TWO} identifiers, those fields will
     * be used. If {@value #LEVEL_ONE} is not specified, {@value #PRIMARY_ANGLE}
     * will be used.
     *
     * @param radarRecord
     *            The radar record.
     * @param request
     *            The request.
     * @return The created level.
     */
    private Level getLevel(RadarRecord radarRecord, IDataRequest request) {
        String levelOneField = getLevelField(request, LEVEL_ONE);
        String levelTwoField = getLevelField(request, LEVEL_TWO);

        if (levelOneField == null) {
            levelOneField = PRIMARY_ANGLE;
        }

        Level level;
        if (PRIMARY_ANGLE.equals(levelOneField)) {
            level = getTiltLevel(radarRecord.getPrimaryElevationAngle());
        } else if (TRUE_ANGLE.equals(levelOneField)) {
            level = getTiltLevel(radarRecord.getTrueElevationAngle());
        } else { // elevationNumber
            level = new Level();
            level.setMasterLevel(new MasterLevel("OSEQD"));
            level.setLevelonevalue(radarRecord.getElevationNumber());
        }

        if (levelTwoField != null) {
            if (PRIMARY_ANGLE.equals(levelTwoField)) {
                level.setLeveltwovalue(radarRecord.getPrimaryElevationAngle());
            } else if (TRUE_ANGLE.equals(levelTwoField)) {
                level.setLeveltwovalue(radarRecord.getTrueElevationAngle());
            } else { // elevationNumber
                level.setLeveltwovalue(radarRecord.getElevationNumber());
            }
        }

        return level;
    }

    /**
     * Get the level field corresponding to the given key from the request's
     * identifiers.
     *
     * @param request
     * @param levelFieldKey
     *            key indicating level one or level two
     * @return the level field (may be null)
     */
    private String getLevelField(IDataRequest request, String levelFieldKey) {
        Object levelField = request.getIdentifiers().get(levelFieldKey);
        if (levelField == null) {
            return null;
        } else if (levelField instanceof String
                && SUPPORTED_LEVELS.contains(levelField)) {
            return (String) levelField;
        }

        // Nothing else is valid since the value is a DB field name
        throw new IncompatibleRequestException(
                "'" + levelField.toString() + "' is not a valid level field");
    }

    /**
     * Get a unique name describing the location of the radar data. The name
     * always includes icao, elevation angle, num bins and num radials. For
     * radial data it also includes the first and last angle of the radial data.
     * Theoretically two radial geometries could have the same name but
     * internally different angleData but this is very unlikely and the points
     * would be very nearly identical.
     *
     *
     * @param radarRecord
     *            a record.
     * @return A unique location name
     */
    protected String generateLocationName(RadarRecord radarRecord) {
        StringBuilder locationName = new StringBuilder(32);
        locationName.append(radarRecord.getIcao());
        locationName.append("_");
        locationName.append(radarRecord.getTrueElevationAngle());
        locationName.append("_");
        locationName.append(radarRecord.getNumBins());
        locationName.append("_");
        locationName.append(radarRecord.getNumRadials());
        float[] angleData = radarRecord.getAngleData();
        if (angleData != null) {
            locationName.append("_");
            locationName.append(angleData[0]);
            locationName.append("_");
            locationName.append(angleData[angleData.length - 1]);
        }
        return locationName.toString();
    }

    protected RadarRecord asRadarRecord(PluginDataObject pdo) {
        if (pdo instanceof RadarRecord) {
            return (RadarRecord) pdo;
        }
        throw new DataRetrievalException(this.getClass().getSimpleName()
                + " cannot handle " + pdo.getClass().getSimpleName());
    }

    @Override
    protected GridGeometry2D getGridGeometry(PluginDataObject pdo) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        if (radarRecord.getFormat().equals(RADIAL_FORMAT)) {
            try {
                float[] angleData = radarRecord.getAngleData();
                if (angleData == null) {
                    populateRecord(radarRecord);
                    angleData = radarRecord.getAngleData();
                }
                if (angleData == null) {
                    return null;
                }

                // NOTE: do not set swapXY=true even though it matches the raw
                // data better because there is lots of code, especially on the
                // Viz side that does not correctly handle the resulting
                // GridGeometry.
                return RadarProjectionFactory.constructGridGeometry(
                        new Coordinate(radarRecord.getLongitude(),
                                radarRecord.getLatitude()),
                        angleData, radarRecord.getGateResolution(),
                        radarRecord.getTrueElevationAngle(),
                        radarRecord.getNumBins(), false);
            } catch (FactoryException e) {
                throw new DataRetrievalException(e);
            }
        } else if (radarRecord.getFormat().equals(RASTER_FORMAT)) {
            double maxExtent = RadarUtil.calculateExtent(radarRecord);
            return RadarUtil.constructGridGeometry(radarRecord.getCRS(),
                    maxExtent, Math.max(radarRecord.getNumBins(),
                            radarRecord.getNumRadials()));

        } else {
            return super.getGridGeometry(pdo);
        }
    }

    @Override
    protected SubGridGeometryCalculator calculateSubGrid(
            ReferencedEnvelope envelope, GridGeometry2D gridGeometry)
            throws EnvelopeProjectionException {
        /*
         * The SubGridGeometryCalculator cannot accurately calculate subgrids
         * into RadialBin projections. For this factory the request envelope is
         * only used to limit the sites, not to subgrid. Returning null causes
         * the super class to request a full grid.
         */
        return null;
    }

    @Override
    protected DataSource getDataSource(PluginDataObject pdo,
            SubGridGeometryCalculator subGrid) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        DataSource dataSource = getDataSource(radarRecord);
        if (dataSource == null) {
            /*
             * Radial data prepopulates the record to get the gridGeometry but
             * raster data waits until now.
             */
            populateRecord(radarRecord);
            dataSource = getDataSource(radarRecord);
            if (dataSource == null) {
                throw new DataRetrievalException(
                        "No grid data found for " + radarRecord);
            }
        }
        if (radarRecord.getFormat().equals(RADIAL_FORMAT)) {
            /*
             * The raw data is in bin,radial format but the grid geometries we
             * use are radial,bin so need to do some swapping.
             */
            dataSource = new AxisSwapDataSource(dataSource,
                    radarRecord.getNumBins());
        }

        return dataSource;
    }

    /**
     * Populate a DataSource from the raw data(byte or short) in the provided
     * record.
     *
     * @param radarRecord
     * @return a DataSource or null if the record is not populated or has no
     *         grid data.
     */
    private DataSource getDataSource(RadarRecord radarRecord) {
        int nx = radarRecord.getNumBins();
        int ny = radarRecord.getNumRadials();
        byte[] bytes = radarRecord.getRawData();
        if (bytes != null) {
            ByteBufferWrapper wrapper = new ByteBufferWrapper(bytes, nx, ny);
            DataSource unsigned = UnsignedFilter.apply(wrapper);
            return FillValueFilter.apply(unsigned, 0);
        }
        short[] shorts = radarRecord.getRawShortData();
        if (shorts != null) {
            ShortBufferWrapper wrapper = new ShortBufferWrapper(shorts, nx, ny);
            DataSource unsigned = UnsignedFilter.apply(wrapper);
            return FillValueFilter.apply(unsigned, 0);
        }
        return null;

    }

    protected void populateRecord(RadarRecord radarRecord)
            throws DataRetrievalException {
        try {
            RadarDataRetriever.populateRadarRecord(
                    PDOUtil.getDataStore(radarRecord), radarRecord);
        } catch (Exception e) {
            throw new DataRetrievalException(e);
        }
    }

    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> constraints = new HashMap<>();
        if (request.getParameters() != null
                && request.getParameters().length > 0) {
            Set<Integer> codes = new HashSet<>();
            for (String parameter : request.getParameters()) {
                codes.addAll(getProductCodesFromParameter(parameter));
            }
            RequestConstraint pcConstraint = new RequestConstraint(null,
                    ConstraintType.IN);
            for (Integer code : codes) {
                pcConstraint.addToConstraintValueList(code.toString());
            }
            constraints.put(PRODUCT_CODE, pcConstraint);
        }
        Level[] levels = request.getLevels();
        if (levels != null && levels.length > 0) {
            RequestConstraint angleConstraint = new RequestConstraint(null,
                    ConstraintType.IN);
            RequestConstraint levelTwoConstraint = new RequestConstraint(null,
                    ConstraintType.IN);

            String levelOneField = getLevelField(request, LEVEL_ONE);
            String levelTwoField = getLevelField(request, LEVEL_TWO);

            if (levelOneField == null) {
                levelOneField = PRIMARY_ANGLE;
            }
            for (Level level : levels) {
                angleConstraint.addToConstraintValueList(
                        level.getLevelOneValueAsString());
                if (levelTwoField != null
                        && level.getLeveltwovalue() != Level.INVALID_VALUE) {
                    levelTwoConstraint.addToConstraintValueList(
                            level.getLevelTwoValueAsString());
                }
            }
            constraints.put(levelOneField, angleConstraint);
            if (levelTwoConstraint.getConstraintValue() != null) {
                constraints.put(levelTwoField, levelTwoConstraint);
            }
        }

        String[] locations = request.getLocationNames();
        if (locations != null && locations.length > 0) {
            RequestConstraint rc = new RequestConstraint(locations);
            constraints.put(ICAO, rc);
        }

        if (request.getEnvelope() != null) {
            Envelope envelope = request.getEnvelope();

            String minLon = Double.toString(envelope.getMinX());
            String maxLon = Double.toString(envelope.getMaxX());
            constraints.put(LONGITUDE, new RequestConstraint(minLon, maxLon));
            String minLat = Double.toString(envelope.getMinY());
            String maxLat = Double.toString(envelope.getMaxY());
            constraints.put(LATITUDE, new RequestConstraint(minLat, maxLat));
        }

        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null && identifiers.containsKey(ICAO)) {
            Object value = identifiers.get(ICAO);
            if (value instanceof RequestConstraint) {
                constraints.put(ICAO, (RequestConstraint) value);
            } else {
                constraints.put(ICAO, new RequestConstraint(value.toString()));
            }
        }

        return constraints;
    }

    private Set<Integer> getProductCodesFromParameter(String parameter) {
        String exception = null;
        Set<Integer> codes = new HashSet<>();
        for (RadarInfo info : getRadarInfo()) {
            if (parameter.equals(info.getName())
                    || parameter.equals(info.getMnemonic()) || parameter
                            .equals(Integer.toString(info.getProductCode()))) {

                if (SUPPORTED_FORMATS.contains(info.getFormat())) {
                    codes.add(info.getProductCode());
                } else {
                    exception = info.getFormat() + " is not supported";
                }
            }
        }
        if (codes.isEmpty()) {
            // If any valid product codes are found then don't complain.
            if (exception != null) {
                throw new DataRetrievalException(exception);
            } else {
                throw new DataRetrievalException(
                        parameter + " is not a valid radar parameter.");
            }
        }
        return codes;
    }

    private static synchronized Level getTiltLevel(double angle) {
        if (tiltLevel == null) {
            tiltLevel = new MasterLevel("TILT");
            tiltLevel.setUnitString("°");
            tiltLevel.setType("INC");
            tiltLevel.setDescription("Tilt angle of a radar scan.");
        }
        Level level = new Level();
        level.setMasterLevel(tiltLevel);
        level.setLevelonevalue(angle);
        return level;
    }

    private static synchronized RadarInfoDict getRadarInfo() {
        if (radarInfo == null) {
            File file = PathManagerFactory.getPathManager()
                    .getStaticFile("radarInfo.txt");
            if (file != null) {
                radarInfo = RadarInfoDict.getInstance(file.getParent());
            }
        }
        return radarInfo;
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        return getAvailableValues(request, ICAO, String.class);
    }

    /**
     * Get the available parameters for {@link #SUPPORTED_FORMATS supported
     * formats}.
     */
    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        DbQueryRequest dbQueryRequest = buildDbQueryRequest(request);
        dbQueryRequest.addConstraint(FORMAT,
                new RequestConstraint(SUPPORTED_FORMATS));
        dbQueryRequest.setDistinct(Boolean.TRUE);
        dbQueryRequest.addRequestField(PRODUCT_CODE);

        DbQueryResponse dbQueryResponse = this
                .executeDbQueryRequest(dbQueryRequest, request.toString());
        Set<Integer> productCodes = new TreeSet<>();
        for (Map<String, Object> result : dbQueryResponse.getResults()) {
            productCodes.add((Integer) result.get(PRODUCT_CODE));
        }
        Set<String> parameters = new HashSet<>();
        for (RadarInfo info : getRadarInfo()) {
            if (productCodes.contains(Integer.valueOf(info.getProductCode()))) {
                parameters.add(info.getName());
                parameters.add(info.getMnemonic());
                parameters.add(Integer.toString(info.getProductCode()));
            }
        }

        return parameters.toArray(new String[0]);
    }

    /**
     * Get the available levels. The optional identifiers, {@value #LEVEL_ONE}
     * and {@value #LEVEL_TWO} can be supplied to choose which level fields are
     * returned, otherwise {@value #PRIMARY_ANGLE} will be returned as the level
     * one value.
     */
    @Override
    public Level[] getAvailableLevels(IDataRequest request) {
        DbQueryRequest dbQueryRequest = buildDbQueryRequest(request);
        dbQueryRequest.setDistinct(Boolean.TRUE);

        String levelOneField = getLevelField(request, LEVEL_ONE);
        String levelTwoField = getLevelField(request, LEVEL_TWO);

        if (levelOneField == null) {
            levelOneField = PRIMARY_ANGLE;
        }
        dbQueryRequest.addRequestField(levelOneField);
        if (levelTwoField != null) {
            dbQueryRequest.addRequestField(levelTwoField);
        }

        DbQueryResponse dbQueryResponse = this
                .executeDbQueryRequest(dbQueryRequest, request.toString());
        Level level;
        List<Level> levels = new ArrayList<>();
        for (Map<String, Object> result : dbQueryResponse.getResults()) {
            if (PRIMARY_ANGLE.equals(levelOneField)
                    || TRUE_ANGLE.equals(levelTwoField)) {
                level = getTiltLevel(
                        Double.valueOf(result.get(levelOneField).toString()));
            } else {
                level = new Level();
                level.setMasterLevel(new MasterLevel("OSEQD"));
                level.setLevelonevalue(
                        Double.valueOf(result.get(levelOneField).toString()));
            }
            if (levelTwoField != null) {
                level.setLeveltwovalue(
                        Double.valueOf(result.get(levelTwoField).toString()));
            }
            levels.add(level);
        }

        return levels.toArray(new Level[0]);
    }

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return new String[] { ICAO, LEVEL_ONE, LEVEL_TWO };
    }

    @Override
    protected DbQueryRequest buildDbQueryRequest(IDataRequest request) {
        validateLevelIdentifiers(request);
        return super.buildDbQueryRequest(request);
    }

    /**
     * Validates that, if specified, the {@value #LEVEL_ONE} and
     * {@value #LEVEL_TWO} identifier values are supported.
     *
     * @param request
     */
    private void validateLevelIdentifiers(IDataRequest request) {
        String levelOneField = getLevelField(request, LEVEL_ONE);
        String levelTwoField = getLevelField(request, LEVEL_TWO);

        if (levelOneField != null
                && !SUPPORTED_LEVELS.contains(levelOneField)) {
            throw new DataRetrievalException(LEVEL_ONE + LEVEL_ERROR);
        }

        if (levelTwoField != null
                && !SUPPORTED_LEVELS.contains(levelTwoField)) {
            throw new DataRetrievalException(LEVEL_TWO + LEVEL_ERROR);
        }
    }

    /**
     * Get allowed values for a specified identifier
     *
     * @param request
     *            Request including the name of the datatype
     * @param identifierKey
     *            The identifier to get allowed values for
     */
    @Override
    public String[] getIdentifierValues(IDataRequest request,
            String identifierKey) {
        if (!Arrays.asList(getRequiredIdentifiers(request))
                .contains(identifierKey)
                && !Arrays.asList(getOptionalIdentifiers(request))
                        .contains(identifierKey)) {
            throw new InvalidIdentifiersException(request.getDatatype(), null,
                    Arrays.asList(new String[] { identifierKey }));
        }
        List<String> idValStrings;
        if (identifierKey.equals(LEVEL_ONE)
                || identifierKey.equals(LEVEL_TWO)) {
            idValStrings = SUPPORTED_LEVELS;
        } else {
            Object[] idValues = getAvailableValues(request, identifierKey,
                    Object.class);
            idValStrings = new ArrayList<>(idValues.length);
            for (Object idValue : idValues) {
                idValStrings.add(idValue.toString());
            }
        }
        return idValStrings.toArray(new String[idValStrings.size()]);
    }

    /**
     *
     * This is used to convert data from bin,radial format to radial bin format.
     *
     * <pre>
     *
     * SOFTWARE HISTORY
     *
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jan 25, 2013            bsteffen     Initial creation
     * Feb 14, 2013 1614       bsteffen    refactor data access framework to use
     *                                          single request.
     *
     * </pre>
     *
     * @author bsteffen
     * @version 1.0
     */
    private static class AxisSwapDataSource implements DataSource {

        private final DataSource realData;

        private final int numBins;

        public AxisSwapDataSource(DataSource realData, int numBins) {
            this.realData = realData;
            this.numBins = numBins;
        }

        @Override
        public double getDataValue(int x, int y) {
            return realData.getDataValue(numBins - 1 - y, x);
        }

    }
}