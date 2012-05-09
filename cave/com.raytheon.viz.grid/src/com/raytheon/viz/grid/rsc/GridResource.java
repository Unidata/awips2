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
package com.raytheon.viz.grid.rsc;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.media.jai.Interpolation;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.CombinedGribRecord;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LatLonGridCoverage;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.AbstractInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleManager.StyleType;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineUtil;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.rsc.hdf5.MemoryBasedTileSet;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.grid.GridLevelTranslator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Grid Resource
 * 
 * Accepts grib data records from different time and levels. Data record array
 * should represent a single parameter.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Feb 28, 2007             chammack    Initial Creation.
 *    02/12/09                 njensen     Refactored to new rsc architecture
 *    04/03/2012   14774/14775 D. Friedman Fixed tiling and lockup problem
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GridResource extends
        AbstractVizResource<GridResourceData, MapDescriptor> implements
        IResourceDataChanged, IGridNameResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridResource.class);

    private static final String NO_DATA = "No Data";

    protected static final DecimalFormat sampleFormat = new DecimalFormat(
            "0.00");

    protected Map<DataTime, Map<Float, GridMemoryBasedTileSet>> tileSet;

    protected WKTReader reader;

    protected SingleLevel[] levels;

    protected float displayedLevel;

    protected String parameter;

    protected String parameterAbbrev;

    protected Map<Integer, GridMemoryBasedTileSet> baseTiles = new HashMap<Integer, GridMemoryBasedTileSet>();

    protected boolean ready = false;

    protected String units;

    protected Geometry baseTileCoverage;

    protected IGraphicsTarget target;

    protected SingleLevel level;

    protected String levelUnits;

    protected UnitConverter conversion;

    protected int numLevels;

    private UnitConverter levelConverter;

    protected String viewType;

    private final List<PluginDataObject> pdosToParse = new ArrayList<PluginDataObject>();

    private CombineOperation combineOperation;

    private GridCoverage gridCoverage;

    private StyleRule styleRule;

    /**
     * Extends the MemoryBasedTileSet class so that we can have direct access to
     * the loadedData
     */
    protected class GridMemoryBasedTileSet extends MemoryBasedTileSet {

        private boolean combined = false;

        public boolean isCombined() {
            return combined;
        }

        public void setCombined(boolean combined) {
            this.combined = combined;
        }

        public int[][] getDims() {
            return dims;
        }

        public String getGroup() {
            return group;
        }

        /**
         * @param group
         * @param dataset
         * @param sharedGeometryTileset
         * @param converter
         * @param record
         * @throws VizException
         */
        public GridMemoryBasedTileSet(String group, String dataset,
                AbstractTileSet sharedGeometryTileset, UnitConverter converter,
                GribRecord record) throws VizException {
            super(null, group, dataset, sharedGeometryTileset, converter,
                    record);
        }

        /**
         * @param dataURI
         * @param string
         * @param numLevels
         * @param i
         * @param gridGeometry2D
         * @param gridResource
         * @param conversion
         * @param cellCorner
         * @param record
         * @param viewType
         * @throws VizException
         */
        public GridMemoryBasedTileSet(String dataURI, String string,
                int numLevels, int i, GridGeometry2D gridGeometry2D,
                GridResource gridResource, UnitConverter conversion,
                PixelInCell cellCorner, GribRecord record, String viewType)
                throws VizException {
            super(null, dataURI, string, numLevels, i, gridGeometry2D,
                    gridResource, conversion, cellCorner, record, viewType);
        }

        @Override
        protected IDataRecord getDataRecord() throws StorageException {
            GribRecord gribRecord = (GribRecord) pdo;
            IDataRecord record = null;
            try {
                IDataRecord[] records = GridResourceData.getDataRecordsForTilt(
                        gribRecord, descriptor);
                if (records != null && records.length > 0) {
                    record = records[0];
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            if (record == null) {
                record = super.getDataRecord();
            }
            Unit<?> realDataUnit = gribRecord.getModelInfo()
                    .getParameterUnitObject();
            Unit<?> expectedDataUnit = getCapability(ColorMapCapability.class)
                    .getColorMapParameters().getDataUnit();
            if (!realDataUnit.equals(expectedDataUnit)
                    && realDataUnit.isCompatible(expectedDataUnit)) {
                if (record instanceof FloatDataRecord) {
                    UnitConverter converter = realDataUnit
                            .getConverterTo(expectedDataUnit);
                    record = record.clone();
                    float[] data = ((FloatDataRecord) record).getFloatData();
                    for (int i = 0; i < data.length; i++) {
                        if (data[i] > -9999) {
                            data[i] = (float) converter.convert(data[i]);
                        }
                    }
                }
            }
            GridGeometry2D realGridGeometry = gribRecord.getModelInfo()
                    .getLocation().getGridGeometry();
            GridGeometry2D expectedGridGeometry = this.gridGeometry[0];
            if (!realGridGeometry.equals(expectedGridGeometry)) {
                BilinearInterpolation interp = new BilinearInterpolation(
                        realGridGeometry, expectedGridGeometry, -9998,
                        Float.POSITIVE_INFINITY, -999999);
                interp.setMissingThreshold(1.0f);
                if (record instanceof FloatDataRecord) {
                    float[] data = ((FloatDataRecord) record).getFloatData();
                    record = record.clone();
                    interp.setData(data);
                    try {
                        data = interp.getReprojectedGrid();
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    record = record.clone();
                    record.setIntSizes(new int[] {
                            expectedGridGeometry.getGridRange2D().width,
                            expectedGridGeometry.getGridRange2D().height });
                    ((FloatDataRecord) record).setFloatData(data);
                }
            }

            // convert based on data mapping
            DataMappingPreferences dataMapping = getCapability(
                    ColorMapCapability.class).getColorMapParameters()
                    .getDataMapping();

            if (dataMapping != null
                    && expectedDataUnit.isCompatible(dataMapping
                            .getImageUnit(expectedDataUnit))) {
                UnitConverter conv = expectedDataUnit
                        .getConverterTo(dataMapping
                                .getImageUnit(expectedDataUnit));
                record = record.clone();
                float[] data = ((FloatDataRecord) record).getFloatData();
                for (int i = 0; i < data.length; i++) {
                    if (data[i] > -9999) {
                        data[i] = (float) conv.convert(data[i]);
                    }
                }
            }
            return record;
        }

        public float[] getLoadedData() {
            return (float[]) (loadedData != null ? loadedData[0] : null);
        }

        public void setLoadedData(float[] loadedData) {
            if (this.loadedData != null) {
                this.loadedData[0] = loadedData;
            }
        }

        public void setLoadedData(float[][] loadedData) {
            this.loadedData = loadedData;
        }

        public void preloadDataObject() {
            try {
                preloadDataObject(lastPaintedLevel);
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        public DataTime getDataTime() {
            return pdo.getDataTime();
        }

        public void getImageCoordinates(Coordinate coord,
                double[] envelopeCoordinates, double[] imageCoordinates)
                throws VizException {
            try {
                if (llToLocalProj != null) {
                    double[] in = new double[2];

                    in[0] = coord.x;
                    in[1] = coord.y;

                    llToLocalProj.transform(in, 0, envelopeCoordinates, 0, 1);

                } else {
                    envelopeCoordinates[0] = coord.x;
                    envelopeCoordinates[1] = coord.y;
                }
                this.inverseMathTransform[this.lastPaintedLevel].transform(
                        envelopeCoordinates, 0, imageCoordinates, 0, 1);
            } catch (TransformException e) {
                throw new VizException("Error interrogating ", e);
            }
        }

        public double getTileCoordinateValue(boolean getRaw,
                double[] envelopeCoordinates, double[] imageCoordinates)
                throws VizException {
            ImageTile[][] tiles = this.tileSet
                    .getTileGrid(this.lastPaintedLevel);

            if (tiles == null) {

                return Double.NaN;
            }

            try {
                if (!this.hasDataPreloaded(this.lastPaintedLevel)) {
                    this.preloadDataObject(this.lastPaintedLevel);
                }
            } catch (StorageException e1) {
                throw new VizException("Unable to load data to interrogate", e1);
            }

            for (int i = 0; i < tiles.length; i++) {
                for (int j = 0; j < tiles[0].length; j++) {
                    ImageTile tile = tiles[i][j];
                    if (tile != null
                            && tile.contains(envelopeCoordinates[0],
                                    envelopeCoordinates[1])) {
                        int coordX = (int) (imageCoordinates[0]);
                        int coordY = (int) (imageCoordinates[1]);

                        // Since createTile is asynchronous, wait to see
                        // if tile image is available
                        IImage image = imageMap.get(tile);
                        if (image == null && this.lastPaintedTarget != null) {
                            int retries = 20;

                            MultiKey key = new MultiKey(this.lastPaintedLevel,
                                    i, j);
                            if (jobMap.containsKey(key)) {
                                while (image == null && retries > 0) {
                                    image = imageMap.get(tile);
                                    try {
                                        Thread.sleep(10);
                                    } catch (InterruptedException e) {
                                        // ignore
                                    }
                                    retries--;
                                }
                            } else {
                                image = createTile(this.lastPaintedTarget,
                                        this.lastPaintedLevel, i, j);
                                imageMap.put(tile, image);
                            }

                        }

                        if (image != null) {
                            if (getRaw
                                    || (rsc.getCapability(
                                            ColorMapCapability.class)
                                            .getColorMapParameters()
                                            .getDataToDisplayConverter() == null)) {
                                if (image instanceof IColormappedImage) {
                                    return ((IColormappedImage) image)
                                            .getValue(coordX % this.tileSize,
                                                    coordY % this.tileSize);
                                } else {
                                    return 0;
                                }
                            } else {
                                if (image instanceof IColormappedImage) {
                                    double value = ((IColormappedImage) image)
                                            .getValue(coordX % this.tileSize,
                                                    coordY % this.tileSize);
                                    if (value > -9999) {
                                        return rsc
                                                .getCapability(
                                                        ColorMapCapability.class)
                                                .getColorMapParameters()
                                                .getDataToDisplayConverter()
                                                .convert(value);
                                    }
                                }
                            }
                        }

                    }
                }
            }

            return Double.NaN;
        }

        public boolean hasDataPreloaded() {
            return super.hasDataPreloaded(lastPaintedLevel);
        }

        public PluginDataObject getPluginDataObject() {
            return pdo;
        }

        public GridGeometry2D getGridGeometry() {
            return gridGeometry[0];
        }

        public double[] getPixelWidth() {
            return pixelWidth;
        }

        public void setDataRecord(FloatDataRecord rec, int level) {
            if (loadedData == null) {
                loadedData = new Object[levels];
                dims = new int[levels][];
            }

            if (rec != null) {
                loadedData[level] = rec.getDataObject();
                long[] d = rec.getSizes();
                dims[level] = new int[] { (int) d[0], (int) d[1] };

            }
            isLoaded[level] = true;
        }
    }

    private class DataRetrievalJob extends Job {
        private ConcurrentLinkedQueue<GridMemoryBasedTileSet> tilesToRetrieve = new ConcurrentLinkedQueue<GridMemoryBasedTileSet>();

        public DataRetrievalJob() {
            super("Retrieving Gridded Data");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (!tilesToRetrieve.isEmpty()) {
                GridMemoryBasedTileSet tile = tilesToRetrieve.poll();
                if (tile != null) {
                    tile.preloadDataObject();
                }
            }

            return Status.OK_STATUS;
        }

        public void retrieve(GridMemoryBasedTileSet mbts) {
            tilesToRetrieve.add(mbts);
            if (this.getState() != Job.RUNNING) {
                this.schedule();
            }
        }
    }

    private DataRetrievalJob dataRetriever = new DataRetrievalJob();

    /**
     * Constructor
     */
    public GridResource(GridResourceData data, LoadProperties props) {
        super(data, props);
        data.addChangeListener(this);
        numLevels = 1;
        tileSet = new HashMap<DataTime, Map<Float, GridMemoryBasedTileSet>>();
        reader = new WKTReader();
        dataTimes = new ArrayList<DataTime>();

        ICombinedResourceData combinedResourceData = null;

        try {
            combinedResourceData = getResourceData();
        } catch (ClassCastException e) {
            // do nothing
        }

        if (combinedResourceData != null) {
            this.combineOperation = combinedResourceData.getCombineOperation();
        }

        GribRecord[] records = resourceData.getRecords();
        GribRecord emptyRecord = new GribRecord();
        for (int i = 0; i < records.length; i++) {
            if (emptyRecord.equals(records[i])) {
                // Don't add empty records
                continue;
            }
            pdosToParse.add(records[i]);
        }

        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
            if (resourceData.secondaryResourceData != null
                    && resourceData.secondaryResourceData.getNameGenerator() == null) {
                resourceData.secondaryResourceData
                        .setNameGenerator(new GridNameGenerator());
            }
        }
        // Add in imaging Capability now
        if (!this.hasCapability(ImagingCapability.class)) {
            this.getCapability(ImagingCapability.class).setInterpolationState(
                    true);
            this.getCapability(ImagingCapability.class).setBrightness(0.5f);
        }
    }

    protected GridMemoryBasedTileSet addRecord(GribRecord record)
            throws VizException {

        Geometry curGeom;
        GridMemoryBasedTileSet mbts = null;

        ISpatialObject spatialObj = record.getSpatialObject();
        curGeom = spatialObj.getGeometry();

        Set<SingleLevel> lvlSet = new HashSet<SingleLevel>();
        if (levels != null) {
            for (SingleLevel f : levels) {
                lvlSet.add(f);
            }
        }

        if (baseTileCoverage == null) {
            baseTileCoverage = curGeom;

            parameter = record.getModelInfo().getParameterName();
            parameterAbbrev = record.getModelInfo().getParameterAbbreviation();
            units = record.getModelInfo().getParameterUnit();

            levelUnits = record.getModelInfo().getLevelUnit();

            gridCoverage = record.getModelInfo().getLocation();

            Unit<?> levelUnits = Unit.valueOf(this.levelUnits);
            levelConverter = UnitConverter.IDENTITY;
            if (levelUnits.isCompatible(SI.MILLI(NonSI.BAR))) {
                this.levelUnits = "MB";
                levelConverter = levelUnits.getConverterTo(SI.MILLI(NonSI.BAR));
            }

            level = GridLevelTranslator.constructMatching(record);
            if (level == null) {
                throw new VizException("Unhandled layer type: "
                        + record.getModelInfo().getLevelName());
            }

            this.getCapability(ColorMapCapability.class).setColorMapParameters(
                    initColorMapParameters(record));
        }

        mbts = createTileSet(record, lvlSet, levelConverter);
        mbts.setMapDescriptor(descriptor);
        Set<DataTime> dateSet = tileSet.keySet();
        dataTimes.clear();
        Iterator<DataTime> dateIterator = dateSet.iterator();
        while (dateIterator.hasNext()) {
            dataTimes.add(dateIterator.next());
        }

        Collections.sort(dataTimes);

        levels = new SingleLevel[lvlSet.size()];
        Iterator<SingleLevel> lvlIterator = lvlSet.iterator();
        for (int i = 0; i < levels.length; i++) {
            levels[i] = lvlIterator.next();
        }

        Arrays.sort(levels);

        return mbts;

    }

    private GridMemoryBasedTileSet createTileSet(GribRecord record,
            Set<SingleLevel> lvlSet, UnitConverter levelConverter)
            throws VizException {
        DataTime dataTime = record.getDataTime();
        Map<Float, GridMemoryBasedTileSet> tilemap = tileSet.get(dataTime);
        if (tilemap == null) {
            tilemap = new HashMap<Float, GridMemoryBasedTileSet>();
            tileSet.put(dataTime, tilemap);
        }

        float convertedLevel = (float) levelConverter.convert(record
                .getModelInfo().getLevelOneValue());

        SingleLevel l = GridLevelTranslator.constructMatching(record);
        GridMemoryBasedTileSet mts = null;
        if (tilemap.containsKey(convertedLevel)) {
            mts = tilemap.remove(convertedLevel);
            mts.dispose();
        }
        Integer baseTilesKey = record.getModelInfo().getLocation().getId();
        GridMemoryBasedTileSet commonTile = baseTiles.get(baseTilesKey);
        mts = createTile(record, commonTile);

        if (commonTile == null) {
            baseTiles.put(baseTilesKey, mts);
        }

        lvlSet.add(l);
        if (levelUnits.equals("MB")) {
            mts.setElevation(Controller.ptozsa(convertedLevel));
            l.setType(Level.LevelType.PRESSURE);
        } else {
            mts.setElevation(convertedLevel);
        }
        tilemap.put(convertedLevel, mts);

        return mts;
    }

    public GridMemoryBasedTileSet createTile(GribRecord record,
            GridMemoryBasedTileSet commonTile) throws VizException {

        if (commonTile != null) {
            return new GridMemoryBasedTileSet(record.getDataURI(), "Data",
                    commonTile, conversion, record);
        }

        boolean reproject = false;
        GridGeometry2D gridGeometry2D = record.getModelInfo().getLocation()
                .getGridGeometry();
        GridCoverage location = record.getModelInfo().getLocation();
        if (location != null && location.getSpacingUnit().equals("degree")) {
            double dx = location.getDx();
            Integer nx = location.getNx();
            if (dx * nx >= 360) {
                reproject = true;
            }
        }
        if (reproject == true) {
            try {
                gridGeometry2D = GridGeometry2D.wrap(MapUtil.reprojectGeometry(
                        gridGeometry2D, descriptor.getGridGeometry()
                                .getEnvelope()));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to reproject image:" + e.getLocalizedMessage(),
                        e);
            }
        }
        GridMemoryBasedTileSet mbts = new GridMemoryBasedTileSet(
                record.getDataURI(), "Data", numLevels, 512, gridGeometry2D,
                this, conversion, PixelInCell.CELL_CORNER, record, viewType);
        return mbts;
    }

    @Override
    public LegendParameters getLegendParameters() {
        LegendParameters legendParams = new LegendParameters();
        legendParams.model = resourceData.getModelInfo();
        legendParams.unit = units;
        legendParams.type = "Img";

        legendParams.dataTime = descriptor.getFramesInfo().getTimeForResource(
                this);

        return legendParams;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {

        for (Map.Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> set : tileSet
                .entrySet()) {
            for (Map.Entry<Float, GridMemoryBasedTileSet> tile : set.getValue()
                    .entrySet()) {
                tile.getValue().dispose();
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.target = target;

        synchronized (pdosToParse) {
            if (pdosToParse.size() > 0) {
                for (PluginDataObject pdo : pdosToParse) {
                    createTile(pdo);
                }
                pdosToParse.clear();
            }
        }

        boolean combineResources = combineOperation != CombineOperation.NONE;

        viewType = target.getViewType();
        Map<DataTime, Map<Float, GridMemoryBasedTileSet>> combinedSet = new HashMap<DataTime, Map<Float, GridMemoryBasedTileSet>>();
        for (Map.Entry<Integer, GridMemoryBasedTileSet> baseTileEntry : baseTiles
                .entrySet()) {
            GridMemoryBasedTileSet baseTile = baseTileEntry.getValue();
            baseTile.setMapDescriptor(descriptor);
            baseTile.init(target);
            if (combineResources && !baseTile.isCombined()) {
                Entry<Float, GridMemoryBasedTileSet> tileSetRef = null;
                // Need to find the reference in order to update the object when
                // it comes back as a new combined object
                for (Map.Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> set : tileSet
                        .entrySet()) {
                    for (Map.Entry<Float, GridMemoryBasedTileSet> tile : set
                            .getValue().entrySet()) {
                        if (tile.getValue().getDataTime()
                                .equals(baseTile.getDataTime())) {
                            tileSetRef = tile;
                            break;
                        }
                    }
                    if (tileSetRef != null) {
                        break;
                    }
                }

                GridMemoryBasedTileSet combinedResourceData = combineResourceData(baseTile);
                if (tileSetRef != null) {
                    tileSetRef.setValue(combinedResourceData);
                }

                baseTileEntry.setValue(combinedResourceData);
            }
        }
        DataTime[] primaryDataTimes = descriptor.getTimeMatchingMap().get(this);
        for (int i = 0; i < primaryDataTimes.length; i++) {
            Map<Float, GridMemoryBasedTileSet> map = tileSet
                    .get(primaryDataTimes[i]);
            if (map != null) {
                for (Map.Entry<Float, GridMemoryBasedTileSet> tile : map
                        .entrySet()) {
                    if (baseTiles.values().contains(tile.getValue())) {
                        if (combineResources) {
                            Map<Float, GridMemoryBasedTileSet> map2 = new HashMap<Float, GridResource.GridMemoryBasedTileSet>();
                            map2.put(tile.getKey(), tile.getValue());
                            combinedSet.put(primaryDataTimes[i], map2);
                        }
                        continue;
                    }

                    tile.getValue().init(target);

                    if (!tile.getValue().isCombined()) {
                        GridMemoryBasedTileSet combinedResourceData = combineResourceData(tile
                                .getValue());
                        Map<Float, GridMemoryBasedTileSet> map2 = new HashMap<Float, GridResource.GridMemoryBasedTileSet>();
                        map2.put(tile.getKey(), combinedResourceData);
                        combinedSet.put(primaryDataTimes[i], map2);
                    }
                }
            }
        }
        if (!combinedSet.isEmpty()) {
            tileSet = combinedSet;
            List<DataTime> newDataTimes = new ArrayList<DataTime>();

            for (Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> entry : combinedSet
                    .entrySet()) {
                newDataTimes.add(entry.getKey());
            }
            Collections.sort(newDataTimes, dataTime);
            dataTimes = newDataTimes;
        }
    }

    /**
     * DataTime Comparator
     */
    private static Comparator<DataTime> dataTime = new Comparator<DataTime>() {
        @Override
        public int compare(DataTime arg0, DataTime arg1) {
            return arg0.compareTo(arg1);
        }
    };

    /**
     * Combine the given tiles sets
     * 
     * @param gridMemoryBasedTileSet
     * @param gridMemoryBasedTileSet2
     * @param dataTime
     */
    private GridMemoryBasedTileSet combineResourceData(
            GridMemoryBasedTileSet sourceTileSet) {

        GridCoverage sourceCoverage = gridCoverage;
        PluginDataObject sourcePdo = sourceTileSet.getPluginDataObject();
        if (!(sourcePdo instanceof CombinedGribRecord)) {
            return sourceTileSet;
        }
        GribModel modelInfo = ((CombinedGribRecord) sourcePdo)
                .getSecondaryGribRecord().getModelInfo();
        GridCoverage targetCoverage = modelInfo.getLocation();
        Unit<?> targetUnits = modelInfo.getParameterUnitObject();

        try {
            IDataRecord[] sourceData = DataCubeContainer
                    .getDataRecord(((CombinedGribRecord) sourcePdo)
                            .getPrimaryGribRecord());
            IDataRecord[] targetData = DataCubeContainer
                    .getDataRecord(((CombinedGribRecord) sourcePdo)
                            .getSecondaryGribRecord());
            FloatDataRecord sourceDataRecord = (FloatDataRecord) sourceData[0];
            FloatDataRecord targetDataRecord = (FloatDataRecord) targetData[0];
            GridGeometry2D newTarget = targetCoverage.getGridGeometry();
            float primaryMin = getDataMin(sourceDataRecord);
            float primaryMax = getDataMax(sourceDataRecord);
            float secondaryMin = getDataMin(targetDataRecord);
            float secondaryMax = getDataMax(targetDataRecord);

            if (sourceCoverage.equals(targetCoverage)) {
                diffData(sourceDataRecord, targetDataRecord, primaryMin,
                        primaryMax, secondaryMin, secondaryMax, targetUnits);
                sourceTileSet = new GridMemoryBasedTileSet(
                        sourcePdo.getDataURI(), "Data", numLevels, 512,
                        newTarget, this, conversion, PixelInCell.CELL_CORNER,
                        ((GribRecord) sourcePdo), viewType);
                sourceTileSet.setMapDescriptor(descriptor);
                sourceTileSet.init(target);
                sourceTileSet.setDataRecord(sourceDataRecord, 0);
                sourceTileSet.setCombined(true);
                this.updateColorMap(sourceTileSet);

                return sourceTileSet;
            }
            double sourcePixeWidth = getPixelWidth(
                    sourceCoverage.getGridGeometry(), descriptor);
            double targetPixeWidth = getPixelWidth(
                    targetCoverage.getGridGeometry(), descriptor);
            if (sourcePixeWidth >= targetPixeWidth) {
                double sizeMultiplier = sourcePixeWidth / targetPixeWidth;
                sizeMultiplier = sizeMultiplier > 4 ? 1 : 4;
                long[] newSizes = {
                        (long) (targetDataRecord.getSizes()[0] * sizeMultiplier),
                        (long) (targetDataRecord.getSizes()[1] * sizeMultiplier) };
                newTarget = MapUtil.createFineIntersectingGeometry(
                        sourceCoverage.getGridGeometry().getEnvelope(),
                        targetCoverage.getGridGeometry().getEnvelope(),
                        newSizes);
            } else {
                double sizeMultiplier = targetPixeWidth / sourcePixeWidth;
                sizeMultiplier = sizeMultiplier > 4 ? 1 : 4;
                long[] newSizes = {
                        (long) (sourceDataRecord.getSizes()[0] * sizeMultiplier),
                        (long) (sourceDataRecord.getSizes()[1] * sizeMultiplier) };
                newTarget = MapUtil.createFineIntersectingGeometry(
                        targetCoverage.getGridGeometry().getEnvelope(),
                        sourceCoverage.getGridGeometry().getEnvelope(),
                        newSizes);
            }

            targetDataRecord = remapGrid(targetDataRecord, newTarget,
                    targetCoverage.getGridGeometry());
            sourceDataRecord = remapGrid(sourceDataRecord, newTarget,
                    sourceCoverage.getGridGeometry());

            diffData(sourceDataRecord, targetDataRecord, primaryMin,
                    primaryMax, secondaryMin, secondaryMax, targetUnits);

            sourceTileSet = new GridMemoryBasedTileSet(sourcePdo.getDataURI(),
                    "Data", numLevels, 512, newTarget, this, conversion,
                    PixelInCell.CELL_CORNER, ((GribRecord) sourcePdo), viewType);
            sourceTileSet.setMapDescriptor(descriptor);
            sourceTileSet.init(target);
            sourceTileSet.setDataRecord(sourceDataRecord, 0);
            sourceTileSet.setCombined(true);
            this.updateColorMap(sourceTileSet);

            return sourceTileSet;

        } catch (VizException e) {
            throw new RuntimeException(
                    "Unable to remap secondary resource grid data from "
                            + sourceCoverage.getName() + " to"
                            + targetCoverage.getName(), e);
        } catch (Exception e) {
            throw new RuntimeException(
                    "Unable to remap secondary resource grid data from "
                            + sourceCoverage.getName() + " to"
                            + targetCoverage.getName(), e);
        }
    }

    public double getPixelWidth(GridGeometry2D gridGeometry,
            IMapDescriptor descriptor) throws VizException {
        double pixelWidth = 0;
        try {
            double xCenter = gridGeometry.getGridRange().getSpan(0) / 2.0;
            double yCenter = gridGeometry.getGridRange().getSpan(1) / 2.0;

            double[] input = new double[] { xCenter, yCenter, xCenter + 1,
                    yCenter };
            double[] output = new double[input.length];

            MathTransform mathTransform = gridGeometry
                    .getGridToCRS(PixelInCell.CELL_CORNER);
            // convert the point s to lat/lon
            mathTransform.transform(input, 0, output, 0, input.length / 2);
            MathTransform localProjToLL = CRS.findMathTransform(
                    gridGeometry.getCoordinateReferenceSystem(),
                    DefaultGeographicCRS.WGS84);
            localProjToLL.transform(output, 0, output, 0, 2);

            double[] p1 = descriptor.worldToPixel(new double[] { output[0],
                    output[1] });
            double[] p2 = descriptor.worldToPixel(new double[] { output[2],
                    output[3] });

            // compute the number of map pixels per tile pixel
            pixelWidth = Math.abs(p2[0] - p1[0]);
        } catch (org.opengis.referencing.operation.NoninvertibleTransformException e) {
            throw new VizException(e);

        } catch (TransformException e) {
            throw new VizException(e);

        } catch (InvalidGridGeometryException e) {
            throw new VizException(e);

        } catch (FactoryException e) {
            throw new VizException(e);
        }
        return pixelWidth;
    }

    public float getDataMin(FloatDataRecord dataRecord) {
        float dataMin = Float.POSITIVE_INFINITY;
        float[] floatData = dataRecord.getFloatData();
        for (int i = 0; i < floatData.length; i++) {
            if (!Float.isNaN(floatData[i]) && floatData[i] != -999999) {
                dataMin = Math.min(dataMin, floatData[i]);
            }
        }
        return dataMin;
    }

    public float getDataMax(FloatDataRecord dataRecord) {
        float dataMax = Float.NEGATIVE_INFINITY;

        float[] floatData = dataRecord.getFloatData();
        for (int i = 0; i < floatData.length; i++) {
            if (!Float.isNaN(floatData[i]) && floatData[i] != -999999) {
                dataMax = Math.max(dataMax, floatData[i]);
            }
        }

        return dataMax;

    }

    private FloatDataRecord remapGrid(FloatDataRecord targetDataRecord,
            GridGeometry2D newTarget, GeneralGridGeometry gridGeometry)
            throws VizException {
        try {
            GridGeometry2D remappedImageGeometry = newTarget;
            AbstractInterpolation interp = new NearestNeighborInterpolation(
                    gridGeometry, remappedImageGeometry, -9998,
                    Float.POSITIVE_INFINITY, -999999);
            float[] data = targetDataRecord.getFloatData();
            interp.setData(data);
            data = interp.getReprojectedGrid();
            FloatDataRecord remapGrid = (FloatDataRecord) targetDataRecord
                    .clone();
            remapGrid.setIntSizes(new int[] {
                    remappedImageGeometry.getGridRange2D().width,
                    remappedImageGeometry.getGridRange2D().height });
            remapGrid.setFloatData(data);
            return remapGrid;
        } catch (FactoryException e) {
            throw new VizException(e);
        } catch (TransformException e) {
            throw new VizException(e);
        }
    }

    /**
     * Subtracts the second parameter from the first.
     * 
     * @param floatDataRecord1
     * @param floatDataRecord2
     * @param primaryMax
     * @param primaryMin
     * @param secondaryMax
     * @param secondaryMin
     * @param targetUnits
     */
    private void diffData(FloatDataRecord floatDataRecord1,
            FloatDataRecord floatDataRecord2, float primaryMin,
            float primaryMax, float secondaryMin, float secondaryMax,
            Unit<?> targetUnits) {
        if (this.styleRule != null) {
            Unit<?> primaryUnit = resourceData.getModelInfo()
                    .getParameterUnitObject();

            UnitConverter primaryUnitConverter = primaryUnit
                    .getConverterTo(this.styleRule.getPreferences()
                            .getDisplayUnits());
            UnitConverter secondaryUnitConverter = targetUnits
                    .getConverterTo(this.styleRule.getPreferences()
                            .getDisplayUnits());

            for (int i = 0; i < floatDataRecord2.getFloatData().length; i++) {

                float n1 = (float) primaryUnitConverter
                        .convert(floatDataRecord1.getFloatData()[i] < primaryMin
                                || floatDataRecord1.getFloatData()[i] > primaryMax ? Float.NaN
                                : floatDataRecord1.getFloatData()[i]);
                // convert secondary data to primary
                float n2 = (float) secondaryUnitConverter
                        .convert(floatDataRecord2.getFloatData()[i] < secondaryMin
                                || floatDataRecord2.getFloatData()[i] > secondaryMax ? Float.NaN
                                : floatDataRecord2.getFloatData()[i]);
                if (combineOperation == CombineOperation.DIFFERENCE) {
                    floatDataRecord1.getFloatData()[i] = (float) primaryUnitConverter
                            .inverse().convert(n1 - n2);
                }
            }
        } else {
            for (int i = 0; i < floatDataRecord2.getFloatData().length; i++) {

                float n1 = floatDataRecord1.getFloatData()[i] == -999999 ? Float.NaN
                        : floatDataRecord1.getFloatData()[i];
                float n2 = floatDataRecord2.getFloatData()[i] == -999999 ? Float.NaN
                        : floatDataRecord2.getFloatData()[i];
                floatDataRecord1.getFloatData()[i] = (n1 - n2);
                if (Float
                        .compare(floatDataRecord1.getFloatData()[i], Float.NaN) == 0) {
                    floatDataRecord1.getFloatData()[i] = -999999;
                }
            }
        }
    }

    /**
     * Dynamically scale the color map based on the data in the given tileset
     * 
     * @param gridMemoryBasedTileSet
     */
    private void updateColorMap(GridMemoryBasedTileSet gridMemoryBasedTileSet) {

        ColorMapParameters colorParameters = this.getCapability(
                ColorMapCapability.class).getColorMapParameters();

        // set the min and max range from the tile data. increase the range
        // approximately 5% larger

        colorParameters.setDataMin(Math.min(
                gridMemoryBasedTileSet.getDataMin(),
                colorParameters.getDataMin()) * 1.022f);
        colorParameters.setDataMax(Math.max(
                gridMemoryBasedTileSet.getDataMax(),
                gridMemoryBasedTileSet.getDataMax()) * 1.022f);
        colorParameters.setColorMapMin(colorParameters.getDataMin());
        colorParameters.setColorMapMax(colorParameters.getDataMax());

        float[] colorBarIntervals = new float[5];

        float increment = (colorParameters.getDataMax() - colorParameters
                .getDataMin()) / 4;

        for (int i = 0; i < colorBarIntervals.length; i++) {
            if (conversion != null) {
                colorBarIntervals[i] = (float) conversion
                        .convert((colorParameters.getDataMin() * 0.975)
                                + (increment * i));
            } else {
                colorBarIntervals[i] = (float) (colorParameters.getDataMin() * 0.975)
                        + (increment * i);
            }
        }

        colorParameters.setColorBarIntervals(colorBarIntervals);

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.target = target;

        DataTime time = descriptor.getFramesInfo().getTimeForResource(this);
        Map<Float, GridMemoryBasedTileSet> tileGroup = tileSet.get(time);
        if (tileGroup == null) {
            return;
        }
        float actualLevel = displayedLevel;
        GridMemoryBasedTileSet tile = tileGroup.get(displayedLevel);
        if (tile == null) {
            // TODO cleanup
            actualLevel = tileGroup.keySet().iterator().next();
            tile = tileGroup.get(actualLevel);
        }

        if (tile == null) {
            return;
        }

        if (CombineOperation.DIFFERENCE.equals(combineOperation)
                && !tile.isCombined()) {
            tile = combineResourceData(tile);
            tileGroup.put(actualLevel, tile);
            if (tile.isCombined()) {
                tile.paint(target, paintProps);
            }
        } else {
            tile.paint(target, paintProps);
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        this.descriptor = descriptor;
        for (Map.Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> set : tileSet
                .entrySet()) {
            for (Map.Entry<Float, GridMemoryBasedTileSet> tile : set.getValue()
                    .entrySet()) {
                tile.getValue().setMapDescriptor(this.descriptor);
            }
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        boolean reproject = false;
        if (gridCoverage != null
                && gridCoverage.getSpacingUnit().equals("degree")) {
            double dx = gridCoverage.getDx();
            Integer nx = gridCoverage.getNx();
            if (dx * nx >= 360) {
                reproject = true;
            }
        }
        if (reproject) {
            // If we are reprojecting to screen space, clear all tiles
            for (Map.Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> set : tileSet
                    .entrySet()) {
                for (Map.Entry<Float, GridMemoryBasedTileSet> tile : set
                        .getValue().entrySet()) {
                    tile.getValue().dispose();
                    pdosToParse.add(tile.getValue().getPluginDataObject());
                }
            }
            tileSet.clear();
            baseTiles.clear();
            if (pdosToParse.size() > 0) {
                for (PluginDataObject pdo : pdosToParse) {
                    createTile(pdo);
                }
                pdosToParse.clear();
            }
        } else {
            for (GridMemoryBasedTileSet tile : baseTiles.values()) {
                tile.reproject();
            }
            for (Map.Entry<DataTime, Map<Float, GridMemoryBasedTileSet>> set : tileSet
                    .entrySet()) {
                for (Map.Entry<Float, GridMemoryBasedTileSet> tile : set
                        .getValue().entrySet()) {
                    tile.getValue().reproject();
                }
            }
        }

    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {

        Map<Float, GridMemoryBasedTileSet> map = tileSet.get(descriptor
                .getFramesInfo().getTimeForResource(this));
        if (map == null) {
            return NO_DATA;
        }
        float actualLevel = displayedLevel;
        GridMemoryBasedTileSet tile = map.get(displayedLevel);
        if (tile == null) {
            // TODO cleanup
            actualLevel = map.keySet().iterator().next();
            tile = map.get(actualLevel);
        }

        if (tile == null) {
            return NO_DATA;
        }

        if (!tile.hasDataPreloaded()) {
            tile.preloadDataObject();
        }
        if (!tile.hasDataPreloaded()) {
            return NO_DATA;
        }

        Coordinate latLon;
        try {
            latLon = coord.asLatLon();
        } catch (Exception e) {
            throw new VizException("Error transforming coordinate to lat/lon",
                    e);
        }

        GridCoverage gc = this.getResourceData().getModelInfo().getLocation();
        if (gc instanceof LatLonGridCoverage) {
            LatLonGridCoverage llgc = (LatLonGridCoverage) gc;
            Envelope2D envelope = llgc.getGridGeometry().getEnvelope2D();
            latLon = (Coordinate) latLon.clone();
            try {
                // Try really hard to get this within the gridRange
                Coordinate projectedLatLon = JTS.transform(latLon,
                        new Coordinate(), MapUtil
                                .getTransformFromLatLon(envelope
                                        .getCoordinateReferenceSystem()));
                if (projectedLatLon.x < envelope.x) {
                    latLon.x += 360.0;
                } else if (projectedLatLon.x > envelope.x + envelope.width) {
                    latLon.x -= 360.0;
                }
            } catch (Exception e) {
                throw new VizException(e);
            }

        }

        double tileCoordinateValue = Double.NaN;
        if (getCapability(ImagingCapability.class).isInterpolationState()) {
            AbstractInterpolation interp = new BilinearInterpolation(
                    tile.getLoadedData(), tile.getGridGeometry(),
                    descriptor.getGridGeometry(), -9998f,
                    Float.POSITIVE_INFINITY, Float.NaN);
            try {
                Coordinate pixel = coord.asPixel(descriptor.getGridGeometry());
                tileCoordinateValue = interp.getReprojectedGridCell(
                        (int) pixel.x, (int) pixel.y);
                UnitConverter converter = getCapability(
                        ColorMapCapability.class).getColorMapParameters()
                        .getDataToDisplayConverter();
                if (converter != null) {
                    tileCoordinateValue = converter
                            .convert(tileCoordinateValue);
                }
            } catch (Exception e) {
                throw new VizException(e);
            }
        } else {
            double[] envelopeCoordinates = new double[2];
            double[] valueCoordinate = new double[2];
            tile.getImageCoordinates(latLon, envelopeCoordinates,
                    valueCoordinate);

            tileCoordinateValue = tile.getTileCoordinateValue(false,
                    envelopeCoordinates, valueCoordinate);

        }
        if (Double.isNaN(tileCoordinateValue)) {
            return NO_DATA;
        }

        // check if data mapping preferences exist
        DataMappingPreferences dataMapping = getCapability(
                ColorMapCapability.class).getColorMapParameters()
                .getDataMapping();
        if (dataMapping != null) {
            // if the pixel value matches the data mapping entry use that
            // label instead
            String label = dataMapping
                    .getLabelValueForDataValue(tileCoordinateValue);
            if (label != null) {
                return label;
            }
        }

        return sampleFormat.format(tileCoordinateValue) + units;
    }

    public float interpolateBiCubic(Coordinate latLon,
            GridMemoryBasedTileSet tile) throws VizException {

        double[] envelopeCoordinates = new double[2];
        double[] imageCoordinates = new double[2];
        tile.getImageCoordinates(latLon, envelopeCoordinates, imageCoordinates);

        double y = imageCoordinates[1] - 1;
        float[][] gridVals = new float[4][4];
        float missing = 1.0f;
        for (int i = 0; i < 4; i++) {
            double x = imageCoordinates[0] - 1;
            for (int j = 0; j < 4; j++) {
                if (y < 0 || y > tile.getDims()[0][1] - 1 || x < 0
                        || x > tile.getDims()[0][0] - 1) {
                    gridVals[i][j] = -999999f;
                    missing -= .125;
                } else {
                    double[] coordinate = new double[] { x, y };
                    gridVals[i][j] = (float) tile.getTileCoordinateValue(false,
                            envelopeCoordinates, coordinate);
                }
                x += 1;
            }
            y += 1;
        }
        Interpolation interpolation = Interpolation
                .getInstance(Interpolation.INTERP_BICUBIC);
        float xfrac = (float) (imageCoordinates[0] - Math
                .floor(imageCoordinates[0]));
        float yfrac = (float) (imageCoordinates[1] - Math
                .floor(imageCoordinates[1]));
        float val = interpolation.interpolate(gridVals, xfrac, yfrac);
        if (missing > 0.25 && !Double.isNaN(val)) {
            val = val / missing;
            return val;
        }
        return Float.NaN;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#getVerticalLevels
     * ()
     */
    public SingleLevel[] getVerticalLevels() {
        return levels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#getVerticalLevelType
     * ()
     */
    public Level.LevelType getVerticalLevelType() {
        return level.getType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#setVerticalLevel
     * (float)
     */
    public void setVerticalLevel(SingleLevel level) {
        displayedLevel = (float) level.getValue();
    }

    @Override
    public void remove(DataTime dataTime) {
        Map<Float, GridMemoryBasedTileSet> ts = tileSet.remove(dataTime);
        if (ts == null) {
            return;
        }

        for (Map.Entry<Float, GridMemoryBasedTileSet> tile : ts.entrySet()) {
            tile.getValue().dispose();
        }
        Set<DataTime> dateSet = tileSet.keySet();
        dataTimes.clear();
        Iterator<DataTime> dateIterator = dateSet.iterator();
        while (dateIterator.hasNext()) {
            dataTimes.add(dateIterator.next());
        }

        Collections.sort(dataTimes);

    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;

            for (PluginDataObject pdo : pdos) {

                if (pdo != null) {
                    if (CombineOperation.DIFFERENCE.equals(combineOperation)
                            && !(pdo instanceof CombinedGribRecord)) {
                        // Do nothing, timematcher will take care of it.
                    } else {
                        if (target != null) {
                            createTile(pdo);
                        } else {
                            pdosToParse.add(pdo);
                        }
                    }
                }
            }
        }

        issueRefresh();
    }

    private void createTile(PluginDataObject pdo) {
        GridMemoryBasedTileSet mbts;
        try {
            mbts = addRecord((GribRecord) pdo);
            if (mbts != null) {
                mbts.setMapDescriptor(descriptor);
                mbts.init(target);
                dataRetriever.retrieve(mbts);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating grid resource", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {

        if (combineOperation != null
                && combineOperation != CombineOperation.NONE) {
            return getCombinedName();
        } else {
            return super.getName();
        }

    }

    private String getCombinedName() {
        GridNameGenerator secondaryGenerator = (GridNameGenerator) resourceData.secondaryResourceData
                .getNameGenerator();
        if (secondaryGenerator == null) {
            secondaryGenerator = new GridNameGenerator();
        }
        String secondaryName;
        try {
            LegendParameters legendParams = new LegendParameters();
            GribRecord secondaryGribRecord = ((CombinedGribRecord) resourceData.records[0])
                    .getSecondaryGribRecord();
            GribModel modelInfo = secondaryGribRecord.getModelInfo();
            modelInfo.getParameterAbbreviation();
            String secondaryUnits = modelInfo.getParameterUnit();
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            match.setLevel(GridLevelTranslator
                    .constructMatching(secondaryGribRecord));
            match.setParameterName(Arrays.asList(modelInfo
                    .getParameterAbbreviation()));
            match.setCreatingEntityNames(Arrays.asList(modelInfo.getModelName()));
            StyleRule secondaryStyleRule = StyleManager.getInstance()
                    .getStyleRule(StyleType.IMAGERY, match);
            if (secondaryStyleRule != null
                    && secondaryStyleRule.getPreferences()
                            .getDisplayUnitLabel() != null) {
                secondaryUnits = secondaryStyleRule.getPreferences()
                        .getDisplayUnitLabel();
            }
            legendParams.model = modelInfo;
            legendParams.unit = secondaryUnits;
            legendParams.dataTime = descriptor.getFramesInfo()
                    .getTimeForResource(this);
            legendParams.type = "Img";
            secondaryName = secondaryGenerator.getName(legendParams,
                    resourceData.secondaryResourceData);
        } catch (Exception e) {
            return null;
        }

        return CombineUtil.getName(super.getName(), secondaryName,
                combineOperation);
    }

    /**
     * Initialize the colormap parameters given the record, units and model
     */
    protected ColorMapParameters initColorMapParameters(GribRecord record)
            throws VizException {
        String model = "" + record.getModelInfo().getModelName();
        Unit<?> gridUnits = record.getModelInfo().getParameterUnitObject();

        ColorMapParameters parameters = null;
        PersistedParameters persisted = null;
        IColorMap mapToUse = null;
        String colorMapName = null;
        if (hasCapability(ColorMapCapability.class)) {
            parameters = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            if (parameters != null) {
                mapToUse = parameters.getColorMap();
                colorMapName = parameters.getColorMapName();
                persisted = parameters.getPersisted();
            }
        }
        if (record instanceof CombinedGribRecord) {
            parameters = ColorMapParameterFactory.build(
                    ((CombinedGribRecord) record).getPrimaryGribRecord(),
                    parameterAbbrev, gridUnits, level, model);
        } else {
            parameters = ColorMapParameterFactory.build(record,
                    parameterAbbrev, gridUnits, level, model);
        }

        // These three ifs attempt to bring data within range of 16bit
        // float,
        // if 32 bit floats are passed to the graphics cards this can be
        // removed and will improve performance for cases where this hits,
        // which should be very few(only a few derived parameters)
        Unit<?> newUnits = null;
        if (Math.abs(parameters.getDataMax()) > 65500) {
            // 65500 is the maximum size for 16 bit floats
            // newUnits = gridUnits.times(Math.abs(parameters.getDataMax()))
            // .divide(65500);

            // just convert all the way
            newUnits = parameters.getDisplayUnit();
        }
        if (Math.abs(parameters.getDataMin()) != 0.0
                && Math.abs(parameters.getDataMin()) < 0.000061) {
            // 0.000061 is very close to the minimum size for normal 16 bit
            // floats. The mimumum for subnormal numbers is near
            // 0.0000000596 which allows a larger range but causes much more
            // loss of precision
            // newUnits = gridUnits.times(Math.abs(parameters.getDataMin()))
            // .divide(0.000061);

            // just convert all the way
            newUnits = parameters.getDisplayUnit();
        }

        if (newUnits != null) {
            UnitConverter converter = gridUnits.getConverterTo(newUnits);
            parameters.setDataMin((float) converter.convert(parameters
                    .getDataMin()));
            parameters.setDataMax((float) converter.convert(parameters
                    .getDataMax()));
            parameters.setColorMapMin((float) converter.convert(parameters
                    .getColorMapMin()));
            parameters.setColorMapMax((float) converter.convert(parameters
                    .getColorMapMax()));
            parameters.setDataUnit(newUnits);
            parameters.setImageUnit(newUnits);
        }

        if (mapToUse == null) {
            // No IColorMap specified
            if (colorMapName == null) {
                // No name specified, check current name from
                // ColorMapParametersFactory
                colorMapName = parameters.getColorMapName();
                if (colorMapName == null) {
                    // No name, default
                    colorMapName = "Grid/gridded data";
                }
            }
            // Load the color map
            mapToUse = ColorMapLoader.loadColorMap(colorMapName);
        }
        // Set colormap and name
        parameters.setColorMap(mapToUse);
        parameters.setColorMapName(colorMapName);

        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(level);
        match.setParameterName(Arrays.asList(parameterAbbrev));
        match.setCreatingEntityNames(Arrays.asList(model));
        styleRule = StyleManager.getInstance().getStyleRule(StyleType.IMAGERY,
                match);
        // Pull the parameters out and use them
        if (parameters.getDisplayUnit() != null) {

            if (gridUnits.isCompatible(parameters.getDisplayUnit())) {

                conversion = gridUnits.getConverterTo(parameters
                        .getDisplayUnit());
            } else {
                conversion = UnitConverter.IDENTITY;
                String message = "Data unit: " + gridUnits
                        + " cannot be converted to desired unit: "
                        + parameters.getDisplayUnit()
                        + " Data displayed will be displayed with unit: "
                        + gridUnits;
                statusHandler.handle(Priority.VERBOSE, message);
            }

            if (styleRule != null
                    && styleRule.getPreferences().getDisplayUnitLabel() != null) {
                units = styleRule.getPreferences().getDisplayUnitLabel();
            }
        }
        if (this.styleRule != null
                && this.styleRule.getPreferences() instanceof ImagePreferences) {
            boolean interpolationState = ((ImagePreferences) this.styleRule
                    .getPreferences()).isInterpolate();
            this.getCapability(ImagingCapability.class).setInterpolationState(
                    interpolationState);
        }

        if (persisted != null) {
            // Restore any persisted data
            parameters.applyPersistedParameters(persisted);
        }
        return parameters;
    }

}
