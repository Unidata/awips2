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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.quantity.Time;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.cs.CoordinateSystemAxis;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.numeric.sparse.SparseArray;
import com.raytheon.uf.common.numeric.sparse.SparseShortArray;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.DataScale;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.style.level.Level.LevelType;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.ScalarGridData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.lightning.LightningResourceData.DisplayType;
import com.raytheon.viz.lightning.cache.LightningFrame;
import com.raytheon.viz.lightning.cache.LightningFrameMetadata;
import com.raytheon.viz.lightning.cache.LightningFrameRetriever;

import si.uom.SI;
import tec.uom.se.AbstractUnit;
import tec.uom.se.unit.Units;

/**
 * Resource to render lightning point data as contours and images
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 07, 2014  3333     bclement  Initial creation
 * Jul 22, 2014  3333     bclement  ignores strikes that aren't on map
 * Jul 28, 2014  3451     bclement  uses intended range min
 * Jul 29, 2014  3463     bclement  uses sparse data source
 * Mar 05, 2015  4233     bsteffen  include source in cache key.
 * Jul 02, 2015  4606     bclement  added getDisplayParameterName()
 * Sep 25, 2015  4605     bsteffen  repeat binning
 * Apr 06, 2018  6931     njensen   Use existing colormap if available in
 *                                  createColorMapParameters(GeneralGridData)
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363. Added
 *                                  Quantity type casting for Length unit.
 * Aug 29, 2019  67962    tjensen   Update for GeneralGridData refactor

 * Sep 10, 2019  7922     bsteffen  Better mixing of persisted colormap parameters.
 *
 * </pre>
 *
 * @author bclement
 */
public class GridLightningResource
        extends AbstractGridResource<GridLightningResourceData> {

    public static final String DENSITY_PARAM = "Lightning Density";

    private static final Unit<Time> BIN_OFFSET_UNIT = SI.SECOND;

    private static final Unit<Time> TIME_PARAM_UNIT = Units.MINUTE;

    private static final String TIME_PARAM_LABEL = "min";

    protected final Map<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>> cacheObjectMap = new ConcurrentHashMap<>();

    /**
     * This serves the same purpose as {@link AbstractGridResource#pdoMap} but
     * this only contains lightning records and also takes into account
     * {@link RepeatingBinOffset}. Since pdoMap is not publicly accessible it is
     * necessary to have this.
     */
    private final Map<DataTime, List<BinLightningRecord>> recordMap = new ConcurrentHashMap<>();

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected GridLightningResource(GridLightningResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria rval = new ParamLevelMatchCriteria();
        GridLightningResourceData resourceData = getResourceData();

        DisplayType type = resourceData.getDisplayType();
        RepeatingBinOffset offset = resourceData.getRepeatingBinOffset();
        String param = getDisplayParameterName(type, offset);
        /* cave gets angry if there is ever more than one parameter here */
        rval.setParameterName(Arrays.asList(param));

        int resolution = resourceData.getKmResolution();
        SingleLevel level = new SingleLevel(LevelType.DEFAULT);
        level.setValue(resolution);
        rval.setLevel(level);
        return rval;
    }

    private static String getDisplayParameterName(DisplayType type,
            RepeatingBinOffset offset) {
        String rval;
        if (!type.equals(DisplayType.UNDEFINED)) {
            StringBuilder sb = new StringBuilder();
            sb.append(type.label).append(' ');
            sb.append(DENSITY_PARAM).append(' ');
            UnitConverter converter = BIN_OFFSET_UNIT
                    .getConverterTo(TIME_PARAM_UNIT);
            double interval = converter.convert(offset.getInterval());
            int timeLabel = (int) Math.round(interval);
            sb.append(Integer.toString(timeLabel)).append(TIME_PARAM_LABEL);
            rval = sb.toString();
        } else {
            /*
             * if there isn't a display type defined, use the default param name
             */
            rval = DENSITY_PARAM;
        }

        return rval;
    }

    @Override
    protected ColorMapParameters createColorMapParameters(GeneralGridData data)
            throws VizException {
        /*
         * TODO investigate if the colormap parameter factory actually needs
         * those special cases or not
         */

        /*
         * colormap parameter factory doesn't allow for data scale mins that are
         * under 1 when you have a large max, extract the intended min from the
         * style preferences and restore it in the colormap parameters
         */
        float minRange = Float.NaN;
        ColorMapParameters rval;
        if (stylePreferences != null
                && stylePreferences instanceof ImagePreferences) {
            ImagePreferences imgPrefs = (ImagePreferences) stylePreferences;
            DataScale dataScale = imgPrefs.getDataScale();
            if (dataScale != null) {
                Double minValue = dataScale.getMinValue();
                if (minValue != null) {
                    minRange = minValue.floatValue();
                }
            }
            try {
                /*
                 * avoid calling super since it calls DataUtilities.getMinMax()
                 * which is slow for sparse data and not used for lightning
                 */
                rval = ColorMapParameterFactory.build(imgPrefs,
                        AbstractUnit.ONE);
            } catch (StyleException e) {
                throw new VizException("Problem building lightning colormap"
                        + " parameters from image preferences", e);
            }
        } else {
            /* call super as a fallback */
            rval = super.createColorMapParameters(data);
        }
        rval.setNoDataValue(0);
        if (!Double.isNaN(minRange)) {
            rval.setColorMapMin(minRange);
        }

        if (hasCapability(ColorMapCapability.class)) {
            ColorMapParameters params = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            if (params != null) {
                /*
                 * ColorMapParameters have already been made for this resource,
                 * retain setting from the current colormap parameters even if
                 * it's recreating renderables (such as during a reproject).
                 */
                if (params.getPersisted() != null) {
                    rval.applyPersistedParameters(params.getPersisted());
                }
                if (params.getColorMap() != null
                        || params.getColorMapName() != null) {
                    rval.setColorMapName(params.getColorMapName());
                    rval.setColorMap(params.getColorMap());
                }
            }
        }

        return rval;
    }

    @Override
    protected void disposeInternal() {
        cacheObjectMap.clear();
        super.disposeInternal();
    }

    @Override
    public void remove(DataTime dataTime) {
        recordMap.remove(dataTime);
        cacheObjectMap.remove(dataTime);
        super.remove(dataTime);
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        CoordinateReferenceSystem crs = descriptor.getCRS();
        GeneralGridGeometry gridGeom = descriptor.getGridGeometry();
        int kmResolution = resourceData.getKmResolution();
        /* convert to meters to match projection dimension units */
        int mResolution = kmResolution * 1000;
        int nx = getAxisDimension(0, mResolution);
        int ny = getAxisDimension(1, mResolution);
        GridEnvelope gridRange = new GridEnvelope2D(0, 0, nx, ny);
        GeneralGridGeometry imageGeometry = new GeneralGridGeometry(gridRange,
                gridGeom.getEnvelope());

        MathTransform latLonToGrid = getLonLatTransform(crs, imageGeometry);

        /*
         * use shorts to save space, a grid cell is unlikely to contain over 64K
         * strikes
         */
        SparseArray<short[]> data = new SparseShortArray(nx, ny);
        LightningFrame frame = getFrame(time);

        List<Iterator<double[]>> iterators = new ArrayList<>(4);
        if (resourceData.isHandlingPositiveStrikes()) {
            iterators.add(frame.getPosLatLonList().iterator());
        }
        if (resourceData.isHandlingNegativeStrikes()) {
            iterators.add(frame.getNegLatLonList().iterator());
        }
        if (resourceData.isHandlingCloudFlashes()) {
            iterators.add(frame.getCloudLatLonList().iterator());
        }
        if (resourceData.isHandlingPulses()) {
            iterators.add(frame.getPulseLatLonList().iterator());
        }

        for (Iterator<double[]> iter : iterators) {
            while (iter.hasNext()) {
                double[] lonLat = iter.next();
                DirectPosition2D src = new DirectPosition2D(lonLat[0],
                        lonLat[1]);
                DirectPosition2D dest = new DirectPosition2D();
                try {
                    latLonToGrid.transform(src, dest);
                } catch (Exception e) {
                    throw new VizException(e.getLocalizedMessage(), e);
                }
                int gridX = (int) Math.round(dest.x);
                int gridY = (int) Math.round(dest.y);
                /* ignore strikes that aren't on the map */
                if (gridX >= 0 && gridX < nx && gridY >= 0 && gridY < ny) {
                    data.add(gridX, gridY, 1);
                }
            }
        }
        GeneralGridData gridData = ScalarGridData
                .createScalarData(imageGeometry, data, AbstractUnit.ONE);
        return Arrays.asList(gridData);
    }

    /**
     * Get updated frame from cache
     *
     * @param time
     * @param pdos
     * @return
     */
    private LightningFrame getFrame(DataTime time) {
        LightningFrameRetriever retriever = LightningFrameRetriever
                .getInstance();
        CacheObject<LightningFrameMetadata, LightningFrame> co;
        synchronized (cacheObjectMap) {
            co = cacheObjectMap.get(time);
            if (co == null) {
                /*
                 * no local reference to cache object, create key and get cache
                 * object which may be new or from another resource
                 */
                LightningFrameMetadata key = new LightningFrameMetadata(
                        resourceData.getSource(), time,
                        resourceData.getRepeatingBinOffset());
                co = CacheObject.newCacheObject(key, retriever);
                cacheObjectMap.put(time, co);
            }
        }

        List<BinLightningRecord> pdos = recordMap.get(time);
        if (pdos == null) {
            pdos = Collections.emptyList();
        }
        return retriever.updateAndGet(pdos, co);
    }

    /**
     * Get grid dimension for ordinal axis using the provided resolution. Uses
     * the CRS and grid geometry of the descriptor.
     *
     * @param axis
     *            0 for x, 1 for y
     * @param mResolution
     *            in meters
     * @return
     */
    private int getAxisDimension(int axis, int mResolution) {
        CoordinateReferenceSystem crs = descriptor.getCRS();
        GeneralGridGeometry gridGeometry = descriptor.getGridGeometry();
        CoordinateSystemAxis csa = crs.getCoordinateSystem().getAxis(axis);
        Unit<Length> crsUnit = csa.getUnit().asType(Length.class);
        UnitConverter converter = crsUnit.getConverterTo(SI.METRE);
        Envelope env = gridGeometry.getEnvelope();
        return (int) Math
                .round(converter.convert(env.getSpan(axis)) / mResolution);
    }

    /**
     * Create a new transform from lon/lat to grid coordinates
     *
     * @param crs
     *            coordinate reference system of display
     * @param gridGeom
     *            target grid geometry
     * @return
     * @throws VizException
     */
    private MathTransform getLonLatTransform(CoordinateReferenceSystem crs,
            GeneralGridGeometry gridGeom) throws VizException {
        try {
            MathTransform latLonToCrs = MapUtil.getTransformFromLatLon(crs);
            MathTransform crsToGrid = gridGeom
                    .getGridToCRS(PixelInCell.CELL_CENTER).inverse();
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            return dmtf.createConcatenatedTransform(latLonToCrs, crsToGrid);
        } catch (Exception e) {
            throw new VizException(
                    "Problem converting from lon/lat to requested grid geometry",
                    e);
        }
    }

    /**
     * Add all plugin data objects to resource
     *
     * @param pdos
     */
    public void add(List<PluginDataObject> pdos) {
        for (PluginDataObject pdo : pdos) {
            addDataObject(pdo);
        }
    }

    @Override
    public String getName() {
        int res = resourceData.getKmResolution();
        return LightningResource.formatResourceName(resourceData) + "Lightning "
                + res + "km Grid ";
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        clearRequestedData();
        super.project(crs);
    }

    @Override
    protected void addDataObject(PluginDataObject pdo) {
        /*
         * This method is overridden because the method in super does not take
         * into account the repeating bin offset.
         */
        BinLightningRecord record = (BinLightningRecord) pdo;
        for (DataTime time : resourceData.getRepeatingBinOffset()
                .getNormalizedTimes(pdo.getDataTime().getValidPeriod())) {
            List<BinLightningRecord> pdos = this.recordMap.get(time);
            if (pdos == null) {
                pdos = new ArrayList<>();
                this.recordMap.put(time, pdos);
            }
            if (!pdos.contains(pdo)) {
                pdos.add(record);
                /* Must remove to clear all previously requested data. */
                super.remove(time);
                dataTimes.add(time);
            }
        }
    }

    @Override
    protected List<PluginDataObject> getPluginDataObjects(DataTime time) {
        return new ArrayList<>(recordMap.get(time));
    }

}
