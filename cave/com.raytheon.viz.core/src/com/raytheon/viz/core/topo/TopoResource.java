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

package com.raytheon.viz.core.topo;

import java.io.File;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleManager.StyleType;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;
import com.raytheon.viz.core.style.image.DataScale;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.style.image.SamplePreferences;

/**
 * Provides an SRTM hdf5-backed topographic map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007             chammack    Initial Creation.
 * Apr 03, 2013     1562    mschenke    Fix for custom colormaps
 * Apr 24, 2013     1638    mschenke    Made topo configurable for source data
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TopoResource extends
        AbstractVizResource<TopoResourceData, IMapDescriptor> {

    private IResourceDataChanged changeListener = new IResourceDataChanged() {
        @Override
        public void resourceChanged(ChangeType type, Object object) {
            issueRefresh();
        }
    };

    protected File dataFile;

    protected TileSetRenderable topoTileSet;

    protected TopoResource(TopoResourceData topoData,
            LoadProperties loadProperties, File dataFile) throws VizException {
        super(topoData, loadProperties);
        this.dataFile = dataFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (topoTileSet != null) {
            topoTileSet.dispose();
            topoTileSet = null;
        }
        resourceData.removeChangeListener(changeListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        resourceData.addChangeListener(changeListener);

        // TODO: create topo style rules for topo and bathymetric topo
        ParamLevelMatchCriteria criteria = new ParamLevelMatchCriteria();
        criteria.setParameterName(Arrays.asList(resourceData.getTopoFile()));
        StyleRule styleRule = StyleManager.getInstance().getStyleRule(
                StyleType.IMAGERY, criteria);

        // Default colormap
        String colorMapName = "topo";

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        PersistedParameters persisted = null;
        if (params == null) {
            params = new ColorMapParameters();
        } else {
            persisted = params.getPersisted();
        }

        // Set data unit, specify in resource data? Look up in data record?
        params.setDataUnit(SI.METER);
        params.setDisplayUnit(NonSI.FOOT);
        params.setColorMapMin(-19);
        params.setColorMapMax(5000);
        params.setDataMin(Short.MIN_VALUE);
        params.setDataMax(Short.MAX_VALUE);
        params.setFormatString("0");

        if (styleRule != null) {
            // TODO: This basic logic should be extracted somewhere,
            // ColorMapParametersFactory has become overkill of any basic kind
            // of colormapping based on style rules and is extremely grib
            // specific
            ImagePreferences prefs = (ImagePreferences) styleRule
                    .getPreferences();
            Unit<?> prefDisplayUnit = prefs.getDisplayUnits();
            if (prefDisplayUnit != null) {
                params.setDisplayUnit(prefDisplayUnit);
            }

            DataScale scale = prefs.getDataScale();
            if (scale != null) {
                Double minVal = scale.getMinValue();
                Double maxVal = scale.getMaxValue();
                if (minVal != null) {
                    params.setColorMapMin((float) params
                            .getDisplayToDataConverter().convert(minVal));
                }
                if (maxVal != null) {
                    params.setColorMapMax((float) params
                            .getDisplayToDataConverter().convert(maxVal));
                }
            }

            String defaultCmap = prefs.getDefaultColormap();
            if (defaultCmap != null) {
                colorMapName = defaultCmap;
            }

            SamplePreferences samplePrefs = prefs.getSamplePrefs();
            if (samplePrefs != null && samplePrefs.getFormatString() != null) {
                params.setFormatString(samplePrefs.getFormatString());
            }

            LabelingPreferences labelPrefs = prefs.getColorbarLabeling();
            if (labelPrefs != null && labelPrefs.getValues() != null) {
                params.setColorBarIntervals(labelPrefs.getValues());
            }
        }

        if (params.getColorMap() == null) {
            if (params.getColorMapName() != null) {
                // Use one specified in params over style rules
                colorMapName = params.getColorMapName();
            }
            params.setColorMap(ColorMapLoader.loadColorMap(colorMapName));
        }

        if (persisted != null) {
            params.applyPersistedParameters(persisted);
        }

        getCapability(ColorMapCapability.class).setColorMapParameters(params);

        topoTileSet = new TileSetRenderable(
                getCapability(ImagingCapability.class), getTopoGeometry(),
                getTopoTileImageCreator(), getNumberOfTopoLevels(), 512);
        topoTileSet.project(descriptor.getGridGeometry());
    }

    protected TileImageCreator getTopoTileImageCreator() {
        return new TopoTileImageCreator(this, dataFile);
    }

    private int getNumberOfTopoLevels() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(dataFile);
        try {
            return ds.getDatasets("/interpolated").length + 1;
        } catch (Exception e) {
            throw new VizException("Error getting interpolation levels", e);
        }
    }

    private GridGeometry2D getTopoGeometry() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(dataFile);

        Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                1 });

        try {
            IDataRecord record = ds.retrieve("/", "full", request);
            Map<String, Object> attributes = record.getDataAttributes();
            int width = (Integer) attributes.get("Width");
            int height = (Integer) attributes.get("Height");
            double ulLat = (Double) attributes.get("ulLat");
            double ulLon = (Double) attributes.get("ulLon");
            double lrLat = (Double) attributes.get("lrLat");
            double lrLon = (Double) attributes.get("lrLon");
            String crsString = (String) attributes.get("CRS");

            // Construct CRS for topo data
            CoordinateReferenceSystem crs = CRSCache.getInstance()
                    .getCoordinateReferenceSystem(crsString);
            // Grid range
            GridEnvelope gridRange = new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { width, height });

            // Convert ulLat/ulLon to crs space
            MathTransform mt = CRS.findMathTransform(
                    DefaultGeographicCRS.WGS84, crs);
            double[] in = new double[] { ulLon, ulLat, lrLon, lrLat };
            double[] out = new double[in.length];

            mt.transform(in, 0, out, 0, 2);

            GeneralEnvelope gridEnvelope = new GeneralEnvelope(2);
            gridEnvelope.setCoordinateReferenceSystem(crs);
            gridEnvelope.setRange(0, Math.min(out[0], out[2]),
                    Math.max(out[0], out[2]));
            gridEnvelope.setRange(1, Math.min(out[1], out[3]),
                    Math.max(out[1], out[3]));

            return new GridGeometry2D(gridRange, gridEnvelope);
        } catch (Exception e) {
            throw new VizException("Error getting grid geometry", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (topoTileSet != null) {
            topoTileSet.paint(target, paintProps);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (topoTileSet != null) {
            topoTileSet.project(descriptor.getGridGeometry());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        double height;
        try {
            // height = TopoQuery.getInstance().getHeight(coord.asLatLon());
            height = topoTileSet.interrogate(coord.asLatLon());
        } catch (Exception e) {
            throw new VizException("Error transforming", e);
        }
        if (!Double.isNaN(height)) {
            ColorMapParameters parameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            UnitConverter cvt = parameters.getDataToDisplayConverter();

            DecimalFormat df = new DecimalFormat("0.00");
            return String.format(
                    "%s %s ",
                    df.format(cvt.convert(height)),
                    UnitFormat.getUCUMInstance().format(
                            parameters.getDisplayUnit()));
        }
        return "NO DATA";
    }

}
