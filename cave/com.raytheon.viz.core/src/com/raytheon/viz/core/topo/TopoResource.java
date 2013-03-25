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
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;

/**
 * Provides an SRTM hdf5-backed topographic map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TopoResource extends
        AbstractVizResource<TopoResourceData, IMapDescriptor> {

    private static final File DATA_FILE = new File("topo"
            + IPathManager.SEPARATOR + "srtm30.hdf");

    private IResourceDataChanged changeListener = new IResourceDataChanged() {
        @Override
        public void resourceChanged(ChangeType type, Object object) {
            issueRefresh();
        }
    };

    private TileSetRenderable topoTileSet;

    protected TopoResource(TopoResourceData topoData,
            LoadProperties loadProperties) throws VizException {
        super(topoData, loadProperties);
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

        Unit<Length> dataUnit = SI.METER;

        // TODO: create topo style rules for topo and bathymetric topo

        ColorMapParameters parameters = ColorMapParameterFactory.build(
                (Object) null, "topo", dataUnit, null);
        parameters.setDataMin(Short.MIN_VALUE);
        parameters.setDataMax(Short.MAX_VALUE);

        parameters.setColorMapMin(-19);
        parameters.setColorMapMax(5000);

        String colorMapName = parameters.getColorMapName();
        if (colorMapName == null) {
            colorMapName = "topo";
        }

        ColorMapParameters existing = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (existing != null) {
            PersistedParameters params = existing.getPersisted();
            if (params != null) {
                parameters.setColorMapMin(params.getColorMapMin());
                parameters.setColorMapMax(params.getColorMapMax());
            }

            if (existing.getColorMap() != null) {
                parameters.setColorMap(existing.getColorMap());
            } else if (existing.getColorMapName() != null) {
                colorMapName = existing.getColorMapName();
            }
        }

        if (parameters.getColorMap() == null) {
            parameters.setColorMap(ColorMapLoader.loadColorMap(colorMapName));
        }

        if (parameters.getDisplayUnit() == null) {
            parameters.setDisplayUnit(NonSI.FOOT);
        }
        parameters.setFormatString("0");

        getCapability(ColorMapCapability.class).setColorMapParameters(
                parameters);

        topoTileSet = new TileSetRenderable(
                getCapability(ImagingCapability.class), getTopoGeometry(),
                new TopoTileImageCreator(this, DATA_FILE),
                getNumberOfTopoLevels(), 512);
        topoTileSet.project(descriptor.getGridGeometry());
    }

    private int getNumberOfTopoLevels() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(DATA_FILE);
        try {
            return ds.getDatasets("/interpolated").length + 1;
        } catch (Exception e) {
            throw new VizException("Error getting interpolation levels", e);
        }
    }

    private GridGeometry2D getTopoGeometry() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(DATA_FILE);

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

            // construct the grid geometry that covers the topo grid
            CoordinateReferenceSystem crs = CRSCache.getInstance()
                    .getCoordinateReferenceSystem(crsString);
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(crs);
            ge.setRange(0, ulLon, lrLon);
            ge.setRange(1, lrLat, ulLat);

            GeneralGridEnvelope gr = new GeneralGridEnvelope(
                    new int[] { 1, 1 }, new int[] { width, height }, false);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, true });
            MathTransform mt = mapper.createTransform();

            GridGeometry2D gridGeom = new GridGeometry2D(
                    PixelInCell.CELL_CORNER, mt, ge, null);

            return gridGeom;
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
