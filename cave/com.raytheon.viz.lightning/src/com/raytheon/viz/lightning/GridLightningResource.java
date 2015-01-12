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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

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
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.grid.rsc.general.AbstractGridResource;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;
import com.raytheon.viz.lightning.cache.LightningFrame;
import com.raytheon.viz.lightning.cache.LightningFrameMetadata;
import com.raytheon.viz.lightning.cache.LightningFrameRetriever;

/**
 * Resource to render lightning point data as contours and images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 07, 2014 3333       bclement     Initial creation
 * Jul 22, 2014 3333       bclement     ignores strikes that aren't on map
 * Jul 28, 2014 3451       bclement     uses intended range min
 * Jul 29, 2014 3463       bclement     uses sparse data source
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridLightningResource extends
        AbstractGridResource<GridLightningResourceData> {

    public static final String DENSITY_PARAM = "lightning density";

    private final Map<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>> cacheObjectMap = new ConcurrentHashMap<>();

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected GridLightningResource(GridLightningResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#getMatchCriteria()
     */
    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria rval = new ParamLevelMatchCriteria();
        rval.setParameterName(Arrays.asList(DENSITY_PARAM));
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.general.AbstractGridResource#
     * createColorMapParameters
     * (com.raytheon.viz.grid.rsc.general.GeneralGridData)
     */
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
                rval = ColorMapParameterFactory.build(imgPrefs, Unit.ONE);
            } catch (StyleException e) {
                throw new VizException(
"Problem building lightning colormap"
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
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        cacheObjectMap.clear();
        super.disposeInternal();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#remove(com.raytheon
     * .uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        cacheObjectMap.remove(dataTime);
        super.remove(dataTime);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#getData(com.raytheon
     * .uf.common.time.DataTime, java.util.List)
     */
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
        LightningFrame frame = getFrame(time, pdos);

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
        GeneralGridData gridData = GeneralGridData.createScalarData(
                imageGeometry, data, Unit.ONE);
        return Arrays.asList(gridData);
    }

    /**
     * Get updated frame from cache
     * 
     * @param time
     * @param pdos
     * @return
     */
    private LightningFrame getFrame(DataTime time, List<PluginDataObject> pdos) {
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
                LightningFrameMetadata key = new LightningFrameMetadata(time,
                        resourceData.getBinOffset());
                co = CacheObject.newCacheObject(key, retriever);
                cacheObjectMap.put(time, co);
            }
        }

        return retriever.updateAndGet(ensurePdoType(pdos), co);
    }

    /**
     * @param pdos
     * @return list of all BinLightningRecords in pdos
     */
    private List<BinLightningRecord> ensurePdoType(List<PluginDataObject> pdos) {
        List<BinLightningRecord> rval = new ArrayList<>(pdos.size());
        for (PluginDataObject pdo : pdos) {
            if (pdo instanceof BinLightningRecord) {
                rval.add((BinLightningRecord) pdo);
            }
        }
        return rval;
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
        Unit<?> crsUnit = csa.getUnit();
        UnitConverter converter = crsUnit.getConverterTo(SI.METER);
        Envelope env = gridGeometry.getEnvelope();
        return (int) Math.round(converter.convert(env.getSpan(axis))
                / mResolution);
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
            GeneralGridGeometry gridGeom)
            throws VizException {
        try {
            MathTransform latLonToCrs = MapUtil.getTransformFromLatLon(crs);
            MathTransform crsToGrid = gridGeom.getGridToCRS(
                    PixelInCell.CELL_CENTER).inverse();
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            return dmtf.createConcatenatedTransform(
                    latLonToCrs, crsToGrid);
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        int res = resourceData.getKmResolution();
        return LightningResource.formatResourceName(resourceData)
                + "Lightning " + res + "km Grid ";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#project(org.opengis
     * .referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        clearRequestedData();
        super.project(crs);
    }

}
