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
package com.raytheon.uf.common.dataplugin.gfe.dataaccess;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import si.uom.NonSI;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.OffsetDataSource;
import com.raytheon.uf.common.util.StringUtil;

/**
 * A data factory for getting gfe data from the metadata database. There are
 * currently not any required identifiers.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 04, 2013           bsteffen  Initial creation
 * Feb 14, 2013  1614     bsteffen  Refactor data access framework to use single
 *                                  request.
 * May 02, 2013  1949     bsteffen  Update GFE data access in Product Browser,
 *                                  Volume Browser, and Data Access Framework.
 * Oct 31, 2013  2508     randerso  Change to use DiscreteGridSlice.getKeys()
 * Feb 04, 2014  2672     bsteffen  Enable requesting subgrids.
 * Jul 30, 2014  3184     njensen   Renamed valid identifiers to optional
 * Feb 10, 2015  2866     nabowle   Overwrite subgrid size estimation.
 * Feb 26, 2015  4179     mapeters  Overrode getAvailableParameters(), added
 *                                  getAvailableValues(), inherits IDataFactory.
 * Feb 27, 2015  4179     mapeters  Promoted getAvailableValues() to
 *                                  AbstractDataPluginFactory.
 * May 23, 2016  5637     bsteffen  Return 2 IGridData for vectors
 * May 31, 2016  5587     tgurney   Implement getIdentifierValues()
 * Jun 07, 2016  5587     tgurney   Change get*Identifiers() to take
 *                                  IDataRequest
 * Jun 13, 2016  5574     mapeters  Add advanced query support
 * Aug 01, 2016  2416     tgurney   Add dataURI as optional identifier
 * Dec 15, 2016  6040     tgurney   Add dbType as optional identifier
 * Mar 06, 2017  6142     bsteffen  Remove dataURI as optional identifier
 * Oct 19, 2017  6491     tgurney   dbType default to empty string
 * Oct 19, 2017  6491     tgurney   Remove short identifiers
 *
 *
 * </pre>
 *
 * @author bsteffen
 */

public class GFEGridFactory extends AbstractGridDataPluginFactory {

    private static final String KEYS = "keys";

    private static final String[] OPTIONAL_IDENTIFIERS = {
            GFEDataAccessUtil.MODEL_NAME, GFEDataAccessUtil.MODEL_TIME,
            GFEDataAccessUtil.SITE_ID, GFEDataAccessUtil.DB_TYPE };

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return OPTIONAL_IDENTIFIERS;
    }

    @Override
    protected IGridData[] getGridData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        IGridData[] data = super.getGridData(request, dbQueryResponse);
        List<IGridData> result = new ArrayList<>(data.length);
        for (IGridData gridData : data) {
            result.add(gridData);
            if (gridData instanceof VectorGridData) {
                VectorGridData vectorData = (VectorGridData) gridData;
                DataSource dirSource = vectorData.getData().getDirSource();
                DefaultGridData dirData = new DefaultGridData(dirSource,
                        vectorData.getGridGeometry());
                dirData.setDataTime(vectorData.getDataTime());
                dirData.setLocationName(vectorData.getLocationName());
                dirData.setLevel(vectorData.getLevel());
                Map<String, Object> attrs = new HashMap<>();
                for (String attribute : vectorData.getAttributes()) {
                    attrs.put(attribute, vectorData.getAttribute(attribute));
                }
                dirData.setAttributes(attrs);
                dirData.setParameter(vectorData.getParameter() + "Direction");
                dirData.setUnit(NonSI.DEGREE_ANGLE);

                result.add(dirData);
            }
        }
        return result.toArray(data);
    }

    @Override
    protected DefaultGridData constructGridDataResponse(IDataRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            DataSource dataSource) {
        GFERecord gfeRecord = asGFERecord(pdo);

        DefaultGridData defaultGridData;
        if (dataSource instanceof VectorDataSource) {
            defaultGridData = new VectorGridData((VectorDataSource) dataSource,
                    gridGeometry);
        } else {
            defaultGridData = new DefaultGridData(dataSource, gridGeometry);
        }
        defaultGridData.setDataTime(pdo.getDataTime());
        defaultGridData.setParameter(gfeRecord.getParmName());
        Level level = new Level();
        level.setMasterLevel(new MasterLevel(gfeRecord.getParmLevel()));
        defaultGridData.setLevel(level);
        defaultGridData.setUnit(gfeRecord.getGridInfo().getUnitObject());
        defaultGridData.setLocationName(gfeRecord.getDbId().getSiteId());
        Map<String, Object> attrs = new HashMap<>();
        attrs.put(GFEDataAccessUtil.MODEL_NAME,
                gfeRecord.getDbId().getModelName());
        attrs.put(GFEDataAccessUtil.MODEL_TIME,
                gfeRecord.getDbId().getModelTime());
        attrs.put(GFEDataAccessUtil.SITE_ID, gfeRecord.getDbId().getSiteId());
        attrs.put(GFEDataAccessUtil.DB_TYPE, gfeRecord.getDbId().getDbType());

        Object messageData = gfeRecord.getMessageData();
        if (messageData instanceof Object[]) {
            attrs.put(KEYS, StringUtil.join((Object[]) messageData, ','));
        }
        defaultGridData.setAttributes(attrs);

        return defaultGridData;
    }

    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        HashMap<String, RequestConstraint> constraints = new HashMap<>();

        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null) {
            for (Entry<String, Object> entry : identifiers.entrySet()) {
                Object value = entry.getValue();
                RequestConstraint constraint;
                if (value instanceof RequestConstraint) {
                    constraint = (RequestConstraint) value;
                } else {
                    constraint = new RequestConstraint(value.toString());
                }
                constraints.put(entry.getKey(), constraint);
            }
        }

        String[] parameters = request.getParameters();
        if (parameters != null && parameters.length > 0) {
            RequestConstraint paramNameConstraint = new RequestConstraint(
                    parameters);
            constraints.put(GFEDataAccessUtil.PARM_NAME, paramNameConstraint);
        }

        Level[] levels = request.getLevels();
        if (levels != null && levels.length > 0) {
            String[] masterLevelNames = new String[levels.length];
            for (int i = 0; i < levels.length; ++i) {
                masterLevelNames[i] = levels[i].getMasterLevel().getName();
            }
            RequestConstraint paramLevelConstraint = new RequestConstraint(
                    masterLevelNames);
            constraints.put(GFEDataAccessUtil.PARM_LEVEL, paramLevelConstraint);
        }

        String[] locationNames = request.getLocationNames();
        if (locationNames != null && locationNames.length > 0) {
            RequestConstraint siteConstraint = new RequestConstraint(
                    locationNames);
            constraints.put(GFEDataAccessUtil.SITE_ID, siteConstraint);
        }

        // default to the operational forecast DB if no DB type is specified
        if (constraints.get(GFEDataAccessUtil.DB_TYPE) == null) {
            constraints.put(GFEDataAccessUtil.DB_TYPE,
                    new RequestConstraint(""));
        }

        return constraints;
    }

    /**
     * Estimates the subgrid memory size using the grid geometry's size because
     * {@link #getDataSource(PluginDataObject, SubGridGeometryCalculator)} uses
     * an {@link OffsetDataSource} that holds the full grid data in memory.
     *
     * @param gridGeom
     * @param subGrid
     * @return
     */
    @Override
    protected long estimateSubgridSize(GridGeometry2D gridGeom,
            SubGridGeometryCalculator subGrid) {
        long size = gridGeom.getGridRange().getSpan(0)
                * gridGeom.getGridRange().getSpan(1);
        return size;
    }

    @Override
    protected DataSource getDataSource(PluginDataObject pdo,
            SubGridGeometryCalculator subGrid) {
        GFERecord gfeRecord = asGFERecord(pdo);

        IGridSlice slice = null;
        try {
            slice = GFEDataAccessUtil.getSlice(gfeRecord);
        } catch (Exception e) {
            throw new DataRetrievalException(e);
        }
        GridParmInfo info = slice.getGridInfo();
        GridLocation loc = info.getGridLoc();
        gfeRecord.setGridInfo(slice.getGridInfo());
        DataSource dataSource = null;
        Object[] keys = null;
        if (slice instanceof VectorGridSlice) {
            dataSource = new VectorDataSource((VectorGridSlice) slice);
        } else if (slice instanceof ScalarGridSlice) {
            Grid2DFloat data = ((ScalarGridSlice) slice).getScalarGrid();
            dataSource = new FloatBufferWrapper(data.getFloats(), loc.getNx(),
                    loc.getNy());
        } else if (slice instanceof DiscreteGridSlice) {
            DiscreteGridSlice discreteSlice = (DiscreteGridSlice) slice;
            Grid2DByte data = discreteSlice.getDiscreteGrid();
            keys = discreteSlice.getKeys();
            dataSource = new ByteBufferWrapper(data.getBytes(), loc.getNx(),
                    loc.getNy());
        } else if (slice instanceof WeatherGridSlice) {
            WeatherGridSlice weatherSlice = (WeatherGridSlice) slice;
            Grid2DByte data = weatherSlice.getWeatherGrid();
            keys = weatherSlice.getKeys();
            dataSource = new ByteBufferWrapper(data.getBytes(), loc.getNx(),
                    loc.getNy());
        } else {
            throw new DataRetrievalException("Unknown slice of type "
                    + slice.getClass().getSimpleName());
        }
        if (subGrid != null && !subGrid.isFull()) {
            int[] offsets = subGrid.getGridRangeLow(true);
            dataSource = new OffsetDataSource(dataSource, offsets[0],
                    offsets[1]);
        }
        gfeRecord.setMessageData(keys);
        return dataSource;
    }

    @Override
    protected GridGeometry2D getGridGeometry(PluginDataObject pdo) {
        GFERecord gfeRecord = asGFERecord(pdo);
        GridParmInfo info = gfeRecord.getGridInfo();
        if (info == null) {
            try {
                info = GFEDataAccessUtil.getGridParmInfo(gfeRecord.getParmId());
            } catch (Exception e) {
                throw new DataRetrievalException(e);
            }
            gfeRecord.setGridInfo(info);
        }
        return MapUtil.getGridGeometry(info.getGridLoc());
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        return getAvailableValues(request, GFEDataAccessUtil.SITE_ID,
                String.class);
    }

    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        return getAvailableValues(request, GFEDataAccessUtil.PARM_NAME,
                String.class);
    }

    private GFERecord asGFERecord(Object obj) {
        if (!(obj instanceof GFERecord)) {
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + obj.getClass().getSimpleName());
        }

        return (GFERecord) obj;
    }

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
        Object[] idValues = getAvailableValues(request, identifierKey,
                Object.class);
        idValStrings = new ArrayList<>(idValues.length);
        for (Object idValue : idValues) {
            idValStrings.add(idValue.toString());
        }
        return idValStrings.toArray(new String[0]);
    }
}
