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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.grid.IGridDataFactory;
import com.raytheon.uf.common.dataaccess.grid.IGridRequest;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A data factory for getting radar data from the metadata database. There are
 * currently not any required identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarGridFactory extends AbstractGridDataPluginFactory implements
        IGridDataFactory {

    private static final String PRODUCT_CODE = "productCode";

    private static final String PRIMARY_ANGLE = "primaryElevationAngle";

    private static final String ICAO = "icao";

    private static final String FORMAT = "format";

    private static final String RADIAL_FORMAT = "Radial";

    private static final String RASTER_FORMAT = "Raster";

    private static final List<String> SUPPORTED_FORMATS = Arrays.asList(
            RADIAL_FORMAT, RASTER_FORMAT);

    private static RadarInfoDict radarInfo = null;

    private static MasterLevel tiltLevel = null;

    public RadarGridFactory() {

    }

    @Override
    public GridGeometry2D getGeometry(IGridRequest request) {
        // Radial will almost always throw an exception for radial data since
        // every volume scan starts at a different angle. Raster will work as
        // long as only one product is requested.
        GridGeometry2D gridGeom = null;
        DbQueryRequest dbRequest = buildDbQueryRequest(request);
        DbQueryResponse dbResonse = executeDbQueryRequest(dbRequest,
                request.toString());
        for (Map<String, Object> resultMap : dbResonse.getResults()) {

            if ((resultMap.get(null) instanceof RadarRecord) == false) {
                throw new DataRetrievalException(
                        "The PluginDataObject objects returned by the DbQueryRequest are not of type PluginDataObject as expected.");
            }

            RadarRecord radarRecod = (RadarRecord) resultMap.get(null);
            if (radarRecod.getFormat().equals(RADIAL_FORMAT)) {
                // TODO need angle data but avoid requesting the rest? Or at
                // least cache it?
                getDataRecord(radarRecod, request.getStorageRequest());
            }
            GridGeometry2D potentialGridGeom = getGridGeometry(radarRecod);
            if (gridGeom == null) {
                gridGeom = potentialGridGeom;
            } else if (!gridGeom.equals(potentialGridGeom)) {
                // It would have been nice to throw this before requesting all
                // the data
                throw new IncompatibleRequestException(
                        "Multiple radar geometries match the given request().");
            }
        }
        return gridGeom;

    }

    @Override
    protected IGridData constructGridDataResponse(IGridRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            IDataRecord dataRecord) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        DataSource source = DataWrapperUtil.constructArrayWrapper(dataRecord,
                false);
        if (radarRecord.getFormat().equals(RADIAL_FORMAT)) {
            // The raw data is in bin,radial format but the grid geometries we
            // use are radial,bin so need to do some swapping.
            source = new AxisSwapDataSource(source, radarRecord.getNumBins());

        }
        DefaultGridData defaultGridData = new DefaultGridData(source,
                gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        // reverse map parameter to match request.
        RadarInfo radarInfo = getRadarInfo().getInfo(
                radarRecord.getProductCode());
        List<String> requestedParameters = Arrays.asList(request
                .getParameters());
        if (requestedParameters.contains(radarInfo.getName())) {
            defaultGridData.setParameter(radarInfo.getName());
        } else if (requestedParameters.contains(radarInfo.getMnemonic())) {
            defaultGridData.setParameter(radarRecord.getMnemonic());
        } else {
            defaultGridData.setParameter(radarRecord.getProductCode()
                    .toString());
        }
        defaultGridData.setUnit(radarRecord.getDataUnit());
        defaultGridData.setLevel(getTiltLevel(radarRecord
                .getPrimaryElevationAngle()));

        Map<String, Object> attributes = new HashMap<String, Object>(
                request.getIdentifiers());
        attributes.put(ICAO, radarRecord.getIcao());
        attributes.put(FORMAT, radarRecord.getFormat());

        defaultGridData.setAttributes(attributes);

        return defaultGridData;
    }

    protected RadarRecord asRadarRecord(PluginDataObject pdo) {
        if (pdo instanceof RadarRecord == false) {
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + pdo.getClass().getSimpleName());
        }
        return (RadarRecord) pdo;
    }

    @Override
    protected GridGeometry2D getGridGeometry(PluginDataObject pdo) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        if (radarRecord.getFormat().equals(RADIAL_FORMAT)) {
            try {
                // NOTE: do not set swapXY=true even though it matches the raw
                // data better because there is lots of code, especially on the
                // Viz side that does not correctly handle the resulting
                // GridGeometry.
                return RadarProjectionFactory.constructGridGeometry(
                        new Coordinate(radarRecord.getLongitude(), radarRecord
                                .getLatitude()), radarRecord.getAngleData(),
                        radarRecord.getGateResolution(), radarRecord
                                .getTrueElevationAngle(), radarRecord
                                .getNumBins(), false);
            } catch (FactoryException e) {
                throw new DataRetrievalException(e);
            }
        } else if (radarRecord.getFormat().equals(RASTER_FORMAT)) {
            double maxExtent = RadarUtil.calculateExtent(radarRecord);
            return RadarUtil.constructGridGeometry(
                    radarRecord.getCRS(),
                    maxExtent,
                    Math.max(radarRecord.getNumBins(),
                            radarRecord.getNumRadials()));

        } else {
            return super.getGridGeometry(pdo);
        }
    }

    @Override
    protected IDataRecord getDataRecord(PluginDataObject pdo,
            Request storageRequest) {
        RadarRecord radarRecord = asRadarRecord(pdo);
        try {
            RadarDataRetriever.populateRadarRecord(
PDOUtil.getDataStore(pdo),
                    radarRecord);
        } catch (Exception e) {
            throw new DataRetrievalException(e);
        }
        IDataRecord rec = new ByteDataRecord(RadarStoredData.RAW_DATA_ID,
                radarRecord.getDataURI(), radarRecord.getRawData(), 2,
                new long[] { radarRecord.getNumBins(),
                        radarRecord.getNumRadials() });
        return rec;
    }

    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IGridRequest request) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        if (request.getParameters() != null) {
            Set<Integer> codes = new HashSet<Integer>();
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
        if (request.getLevels() != null) {
            RequestConstraint angleConstraint = new RequestConstraint(null,
                    ConstraintType.IN);
            for (Level level : request.getLevels()) {
                angleConstraint.addToConstraintValueList(level
                        .getLevelOneValueAsString());
            }
            constraints.put(PRIMARY_ANGLE, angleConstraint);
        }

        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null && identifiers.containsKey(ICAO)) {
            constraints.put(ICAO, new RequestConstraint(identifiers.get(ICAO)
                    .toString()));
        }

        return constraints;
    }

    private Set<Integer> getProductCodesFromParameter(String parameter) {
        String exception = null;
        Set<Integer> codes = new HashSet<Integer>();
        for (RadarInfo info : getRadarInfo()) {
            if (parameter.equals(info.getName())
                    || parameter.equals(info.getMnemonic())
                    || parameter
                            .equals(Integer.toString(info.getProductCode()))) {

                if (SUPPORTED_FORMATS.contains(info.getFormat())) {
                    codes.add(info.getProductCode());
                } else {
                    exception = info.getFormat()
                            + " cannot be requested as grid";
                }
            }
        }
        if (codes.isEmpty()) {
            // If any valid produt codes are founf then don't complain.
            if (exception == null) {
                throw new DataRetrievalException(exception);
            } else {
                throw new DataRetrievalException(parameter
                        + " is not a valid radar parameter.");
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
            File file = PathManagerFactory.getPathManager().getStaticFile(
                    "radarInfo.txt");
            if (file != null) {
                radarInfo = RadarInfoDict.getInstance(file.getParent());
            }
        }
        return radarInfo;
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