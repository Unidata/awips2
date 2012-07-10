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
package com.raytheon.viz.satellite.rsc;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.opengis.coverage.grid.GridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.dataplugin.satellite.units.counts.DerivedWVPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.BlendedTPWPixel;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMeshCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.rsc.hdf5.FileBasedTileSet;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.style.image.SamplePreferences;
import com.raytheon.viz.satellite.SatelliteConstants;

/**
 * Provides satellite raster rendering support
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Mar 1, 2007              chammack    Initial Creation.
 *  02/17/2009               njensen     Refactored to new rsc architecture.
 *  03/02/2009		2032	 jsanchez	 Added check for displayedDate if no data.
 *  03/25/2009      2086     jsanchez    Mapped correct converter to parameter type.
 *                                        Updated the call to ColormapParametersFactory.build
 *  03/30/2009      2169     jsanchez    Updated numLevels handling.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SatResource extends
        AbstractVizResource<SatResourceData, MapDescriptor> implements
        IResourceDataChanged, IMeshCallback {

    public static String RAW_VALUE = "rawValue";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatResource.class);

    protected Map<DataTime, FileBasedTileSet> tileSet;

    private Map<DataTime, SatelliteRecord> recordMap = new HashMap<DataTime, SatelliteRecord>();

    protected DataTime displayedDate;

    protected FileBasedTileSet baseTile;

    protected String legend;

    protected IGraphicsTarget target;

    protected boolean hasBeenInited;

    protected int numLevels;

    protected String viewType;

    protected FileBasedTileSet currentTile;

    protected GridGeometry recordGeometry;

    protected SamplePreferences sampleRange;

    /**
     * Constructor
     * 
     * @throws VizException
     */
    public SatResource(SatResourceData data, LoadProperties props) {
        super(data, props);
        data.addChangeListener(this);
        this.tileSet = new HashMap<DataTime, FileBasedTileSet>();
        this.dataTimes = new ArrayList<DataTime>();
        this.legend = null;
        SatelliteRecord[] records = data.getRecords();
        Arrays.sort(records, new SatelliteRecordComparator());

        for (SatelliteRecord record : records) {
            try {
                addRecord(record);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error adding satellite record", e);
            }
        }

        /*
         * This handles if there is no data for East & West CONUS simultaneously
         */
        if (dataTimes.size() > 1) {
            displayedDate = dataTimes.get(dataTimes.size() - 1);
        }
    }

    private void initializeFirstFrame(SatelliteRecord record)
            throws VizException {
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);

        SatelliteUnits.register();
        Unit<?> unit = null;
        String physicalElement = null;
        DerivedParameterRequest request = (DerivedParameterRequest) record
                .getMessageData();
        if (request == null) {
            physicalElement = record.getPhysicalElement();
        } else {
            physicalElement = request.getParameterAbbreviation();
        }
        if (record.getUnits() != null && request == null) {
            try {
                unit = UnitFormat.getUCUMInstance().parseSingleUnit(
                        record.getUnits(), new ParsePosition(0));
            } catch (ParseException e) {
                throw new VizException("Unable parse units ", e);
            }
        } else if (request != null) {
            if (physicalElement.equals("satDivWVIR")) {
                unit = new DerivedWVPixel();
            } else {
                unit = new GenericPixel();
            }
        }

        String creatingEntity = null;
        if (physicalElement.equals(SatelliteConstants.PRECIP)) {
            creatingEntity = record.getCreatingEntity();
            if (creatingEntity.equals(SatelliteConstants.DMSP)
                    || creatingEntity.equals(SatelliteConstants.POES)) {
                unit = new PolarPrecipWaterPixel();
            } else if (creatingEntity.equals(SatelliteConstants.MISC)) {
                unit = new BlendedTPWPixel();
            }
        }

        ColorMapParameters colorMapParameters = null;

        IColorMap colorMap = null;
        String cmName = null;
        if (hasCapability(ColorMapCapability.class)) {
            colorMapParameters = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            if (colorMapParameters != null) {
                colorMap = colorMapParameters.getColorMap();
                cmName = colorMapParameters.getColorMapName();
            }
        }
        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(Arrays.asList(physicalElement));
        match.setLevels(Arrays.asList((Level) level));
        match.setCreatingEntityNames(Arrays.asList(creatingEntity));
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, match);
        if (sr != null && sr.getPreferences() instanceof ImagePreferences) {
            sampleRange = ((ImagePreferences) sr.getPreferences())
                    .getSamplePrefs();
            String lg = ((ImagePreferences) sr.getPreferences())
        	.getLegend();
        	// test, so legend is not over written with empty string
        	if (lg != null && !lg.trim().isEmpty()) { 
        		legend = lg;
        	}
        }

        colorMapParameters = ColorMapParameterFactory.build(null,
                record.getDataURI() + "/Data", physicalElement, unit, level,
                creatingEntity);
        if (unit == null) {
            colorMapParameters.setColorMapMin(0.0f);
            colorMapParameters.setColorMapMax(255.0f);
        }
        if (unit instanceof GenericPixel) {
            // Derived parameter data will be signed
            colorMapParameters.setDataMin(-128.0f);
            colorMapParameters.setDataMax(127.0f);
        } else if (unit instanceof BlendedTPWPixel) {
            colorMapParameters.setDataMin(0.0f);
            colorMapParameters.setDataMax(252.0f);

            colorMapParameters.setColorMapMin(0.0f);
            colorMapParameters.setColorMapMax(252.0f);
        } else {
            colorMapParameters.setDataMin(0.0f);
            colorMapParameters.setDataMax(255.0f);
        }

        if (colorMap != null) {
            colorMapParameters.setColorMap(colorMap);
        }
        if (cmName != null) {
            colorMapParameters.setColorMapName(cmName);
        }

        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);

        numLevels = 1;
        int newSzX = record.getSpatialObject().getNx();
        int newSzY = record.getSpatialObject().getNy();
        while ((newSzX > 512 && newSzY > 512)) {
            newSzX /= 2;
            newSzY /= 2;
            numLevels++;
        }

    }

    @Override
    public String getName() {
        if (this.legend != null) {
            return this.legend + " ";
        }
        return "";
    }

    @Override
    protected void disposeInternal() {
        if (baseTile != null)
            baseTile.dispose();
        for (AbstractTileSet tile : this.tileSet.values())
            if (tile != baseTile)
                tile.dispose();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        synchronized (this) {
            this.viewType = target.getViewType();
            this.hasBeenInited = true;
            this.target = target;

            if (this.baseTile != null) {
                this.baseTile.init(target);
            }
            for (AbstractTileSet tile : this.tileSet.values()) {
                if (tile != baseTile) {
                    tile.init(target);
                }
            }
        }

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (params.getColorMap() == null) {
            String colorMapName = params.getColorMapName();
            if (colorMapName == null)
                colorMapName = "Sat/VIS/ZA (Vis Default)";

            params.setColorMap(target.buildColorMap(colorMapName));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.target = target;
        this.displayedDate = paintProps.getDataTime();
        if (this.displayedDate == null)
            return;

        currentTile = this.tileSet.get(this.displayedDate);
        if (currentTile != null) {
            currentTile.paint(target, paintProps);
        }
        // System.out.println("Time to paint: "
        // + (System.currentTimeMillis() - t0));
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        if (this.baseTile != null) {
            this.baseTile.setMapDescriptor(descriptor);
        }
        for (AbstractTileSet tile : this.tileSet.values())
            tile.setMapDescriptor(descriptor);

        this.descriptor = descriptor;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (this.baseTile != null)
            this.baseTile.reproject();

        for (AbstractTileSet tile : tileSet.values())
            tile.reproject();
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        Map<String, Object> dataMap = new HashMap<String, Object>();
        SatelliteRecord record = recordMap.get(displayedDate);
        if (record != null) {
            dataMap.put(ISpatialObject.class.toString(),
                    record.getSpatialObject());
            AbstractTileSet tile = tileSet.get(displayedDate);
            if (tile != null) {
                try {
                    Double raw = tile.interrogate(coord.asLatLon(), true);
                    if (raw != 0 && raw.isNaN() == false) {
                        UnitConverter dataToDisplay = getCapability(
                                ColorMapCapability.class)
                                .getColorMapParameters()
                                .getDataToDisplayConverter();
                        if (dataToDisplay != null) {
                            raw = dataToDisplay.convert(raw);
                        }
                        dataMap.put(RAW_VALUE, raw);
                    }
                } catch (Exception e) {
                    throw new VizException("Error interrogating raw data", e);
                }
            }
        }
        return dataMap;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> dataMap = interrogate(coord);
        Double value = (Double) dataMap.get(RAW_VALUE);
        if (value == null) {
            return "NO DATA";
        }
        ColorMapParameters cmp = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        // check if data mapping preferences exist
        DataMappingPreferences dataMapping = cmp.getDataMapping();
        if (dataMapping != null) {
            // convert to pixel value for checking labels
            double pixelValue = cmp.getDisplayToDataConverter().convert(
                    value.doubleValue());
            // if the pixel value matches the data mapping entry use that
            // label instead
            String label = dataMapping.getLabelValueForDataValue(pixelValue);
            if (label != null) {
                return label;
            }
        }

        Unit<?> unit = cmp.getDisplayUnit();
        // Had to use 'bit' as the display unit because
        // counts was not an acceptable unit.
        String unitString = unit == null ? ""
                : unit.toString().equals("bit") ? "counts" : unit.toString();
        double f1 = Double.NEGATIVE_INFINITY;
        double f2 = Double.POSITIVE_INFINITY;
        if (sampleRange != null) {
            f1 = sampleRange.getMaxValue();
            f2 = sampleRange.getMinValue();
        }
        if (value > f1 && value > f2) {
            return String.format(">%.1f%s", Math.max(f1, f2), unitString);
        }
        if (value < f1 && value < f2) {
            return String.format("<%.1f%s", Math.min(f1, f2), unitString);
        }
        return String.format("%.1f%s", value, unitString);
    }

    public class SatelliteRecordComparator implements
            Comparator<SatelliteRecord> {

        public int compare(SatelliteRecord o1, SatelliteRecord o2) {
            return o1.getDataTime().getRefTime()
                    .compareTo(o2.getDataTime().getRefTime());
        }
    }

    public void addRecord(PluginDataObject record) throws VizException {
        synchronized (this) {
            FileBasedTileSet tile;
            DataTime recordTime = null;
            if (resourceData.getBinOffset() != null || resourceData.equals(0)) {
                BinOffset binOffset = resourceData.getBinOffset();
                DataTime recTime = record.getDataTime();
                DataTime normTime = binOffset.getNormalizedTime(recTime);
                if (recordMap.containsKey(normTime)) {
                    // a normalized time was found...
                    // if this record's time is closer than the existing
                    // record's
                    // then replace the existing record with the new record
                    SatelliteRecord satRec = recordMap.get(normTime);
                    DataTime existingTime = satRec.getDataTime();
                    long existingTimeMillis = existingTime
                            .getRefTimeAsCalendar().getTimeInMillis();
                    long recTimeMillies = recTime.getRefTimeAsCalendar()
                            .getTimeInMillis();
                    long normTimeMillies = normTime.getRefTimeAsCalendar()
                            .getTimeInMillis();
                    if (Math.abs(normTimeMillies - existingTimeMillis) > Math
                            .abs(normTimeMillies - recTimeMillies)) {
                        // System.out.println("For " + normTime +
                        // "\n\treplaced "
                        // + existingTime + "\n\twith     " + recTime);
                        recordMap.remove(normTime);
                        FileBasedTileSet oldTile = tileSet.remove(normTime);
                        if (oldTile != null) {
                            oldTile.dispose();
                        }
                    }
                }
                recordTime = normTime;
            } else {
                recordTime = record.getDataTime();
            }
            recordMap.put(recordTime, (SatelliteRecord) record);
            if (baseTile == null) {
                initializeFirstFrame((SatelliteRecord) record);
            }

            if (baseTile == null) {
                tile = baseTile = new SatFileBasedTileSet(record, "Data",
                        numLevels, 256,
                        MapUtil.getGridGeometry(((SatelliteRecord) record)
                                .getSpatialObject()), this,
                        PixelInCell.CELL_CORNER, viewType);
            } else {
                tile = new SatFileBasedTileSet(record, "Data", baseTile);
            }
            tile.addMeshCallback(this);
            tile.setMapDescriptor(this.descriptor);
            if (hasBeenInited)
                tile.init(target);

            if (this.legend == null) {
                this.legend = getLegend(record);
            }
            FileBasedTileSet oldTile = tileSet.put(recordTime, tile);
            if (oldTile != null) {
                oldTile.dispose();
            }
            dataTimes.add(recordTime);

            Collections.sort(this.dataTimes);
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        if (!this.dataTimes.contains(dataTime))
            return;

        this.dataTimes.remove(dataTime);

        FileBasedTileSet tile = tileSet.remove(dataTime);
        if (tile != baseTile && tile != null)
            tile.dispose();

    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    this.addRecord(pdo);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating satellite resource", e);
                }
            }
        }
        issueRefresh();
    }

    private String getLegend(PluginDataObject record) {

        String productName = null;
        DerivedParameterRequest request = (DerivedParameterRequest) record
                .getMessageData();
        if (request == null) {
            productName = ((SatelliteRecord) record).getPhysicalElement();
        } else {
            productName = request.getParameterAbbreviation();
        }
        return SatelliteConstants.getLegend(productName,
                ((SatelliteRecord) record).getCreatingEntity());
    }

    @Override
    public void meshCalculated(ImageTile tile) {
        issueRefresh();
    }

}
