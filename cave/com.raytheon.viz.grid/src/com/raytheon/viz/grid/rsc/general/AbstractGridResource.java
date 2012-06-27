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
package com.raytheon.viz.grid.rsc.general;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.Measure;
import javax.measure.quantity.Angle;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatBufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.style.AbstractStylePreferences;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleManager.StyleType;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.contours.ContourRenderable;
import com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedStreamlineDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedVectorDisplay;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.core.style.arrow.ArrowPreferences;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.grid.rsc.GriddedIconDisplay;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A single resource that can be easily extended to draw contours, images, wind
 * barbs, arrows, streamlines, or icons for any data that can be represented as
 * a grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractGridResource<T extends AbstractResourceData>
        extends AbstractVizResource<T, IMapDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGridResource.class);

    private final GridDataRequestJob requestJob;

    private Map<DataTime, List<PluginDataObject>> pdos = new HashMap<DataTime, List<PluginDataObject>>();

    private Map<DataTime, IRenderable> renderables = new ConcurrentHashMap<DataTime, IRenderable>();

    /**
     * This is a local cache of data that is used when sampling or reprojected.
     */
    private Map<DataTime, GeneralGridData> data = new ConcurrentHashMap<DataTime, GeneralGridData>();

    /**
     * StylePreferences from the styleManager appropriate for the display type
     * provided.
     */
    protected AbstractStylePreferences stylePreferences = null;

    /**
     * The format used by the default inspect method
     */
    protected DecimalFormat sampleFormat = new DecimalFormat("0.00");

    /**
     * The interpolation used when sampling to sample between data points.
     */
    protected GridSampler sampleInterpolion;

    protected AbstractGridResource(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    if (object instanceof PluginDataObject) {
                        addDataObject((PluginDataObject) object);
                    } else if (object instanceof PluginDataObject[]) {
                        for (PluginDataObject pdo : (PluginDataObject[]) object) {
                            addDataObject((PluginDataObject) pdo);
                        }
                    } else if (object instanceof Object[]) {
                        for (Object obj : (Object[]) object) {
                            if (obj instanceof PluginDataObject) {
                                addDataObject((PluginDataObject) obj);
                            }
                        }
                    }
                } else if (type == ChangeType.CAPABILITY) {
                    if (object instanceof AbstractCapability) {
                        AbstractCapability capability = (AbstractCapability) object;
                        for (IRenderable renderable : renderables.values()) {
                            updataRenderableCapabilities(renderable, capability);
                        }
                    }
                }
            }
        });
        dataTimes = new ArrayList<DataTime>();
        requestJob = new GridDataRequestJob(this);
        // Capabilities need to be inited in construction for things like the
        // image combination tool.
        initCapabilities();
    }

    /**
     * Adds the pdo to the appropriate time and removes any renderable or data
     * cached for that time.
     * 
     * @param pdo
     */
    protected void addDataObject(PluginDataObject pdo) {
        DataTime time = pdo.getDataTime();
        if (this.resourceData instanceof AbstractRequestableResourceData) {
            AbstractRequestableResourceData resourceData = (AbstractRequestableResourceData) this.resourceData;
            if (resourceData.getBinOffset() != null) {
                time = resourceData.getBinOffset().getNormalizedTime(time);
            }
        }
        List<PluginDataObject> pdos = this.pdos.get(time);
        if (pdos == null) {
            pdos = new ArrayList<PluginDataObject>();
            this.pdos.put(time, pdos);
        }
        if (pdos.contains(pdo)) {
            pdos.remove(pdo);
        }
        pdos.add(pdo);
        if (renderables.containsKey(time)) {
            disposeRenderable(renderables.remove(time));
        }
        requestJob.remove(time);
        data.remove(time);
        if (!dataTimes.contains(dataTimes)) {
            dataTimes.add(time);
        }
    }

    /**
     * Should be called immediately after construction if this resource was
     * created from another GridResource to prevent requesting data again.
     * 
     * @param data
     */
    protected void setData(Map<DataTime, GeneralGridData> data) {
        if (this.data.isEmpty()) {
            this.data.putAll(data);
        }
    }

    /**
     * Update a renderable to reflect a changed capability.
     * 
     * @param renderable
     * @param capability
     */
    protected void updataRenderableCapabilities(IRenderable renderable,
            AbstractCapability capability) {
        if (renderable instanceof GriddedImageDisplay2) {
            if (capability instanceof ImagingCapability) {
                initSampling();
            }
        } else if (renderable instanceof AbstractGriddedDisplay<?>) {
            AbstractGriddedDisplay<?> gridDisplay = (AbstractGriddedDisplay<?>) renderable;
            if (capability instanceof ColorableCapability) {
                gridDisplay.setColor(getCapability(ColorableCapability.class)
                        .getColor());
            }
            if (capability instanceof MagnificationCapability) {
                gridDisplay.setMagnification(getCapability(
                        MagnificationCapability.class).getMagnification());
            }
            if (capability instanceof DensityCapability) {
                gridDisplay.setDensity(getCapability(DensityCapability.class)
                        .getDensity());
            }
            if (gridDisplay instanceof GriddedVectorDisplay) {
                GriddedVectorDisplay vectorDisplay = (GriddedVectorDisplay) gridDisplay;
                vectorDisplay.setLineStyle(getCapability(
                        OutlineCapability.class).getLineStyle());
                vectorDisplay.setLineWidth(getCapability(
                        OutlineCapability.class).getOutlineWidth());
            }
        } else if (renderable instanceof ContourRenderable) {
            ContourRenderable contourRenderable = (ContourRenderable) renderable;
            if (capability instanceof ColorableCapability) {
                contourRenderable.setColor(getCapability(
                        ColorableCapability.class).getColor());
            }
            if (capability instanceof MagnificationCapability) {
                contourRenderable.setMagnification(getCapability(
                        MagnificationCapability.class).getMagnification());
            }
            if (capability instanceof DensityCapability) {
                contourRenderable.setDensity(getCapability(
                        DensityCapability.class).getDensity());
            }
            if (capability instanceof OutlineCapability) {
                contourRenderable.setLineStyle(getCapability(
                        OutlineCapability.class).getLineStyle());
                contourRenderable.setOutlineWidth(getCapability(
                        OutlineCapability.class).getOutlineWidth());
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        initStylePreferences();
        initSampling();
    }

    /**
     * Ensure we have any capabilities that will be used for the renderables for
     * this display type.
     */
    protected void initCapabilities() {
        DisplayType displayType = getDisplayType();
        List<DisplayType> altDisplayTypes = new ArrayList<DisplayType>();
        switch (displayType) {
        case IMAGE:
            if (!hasCapability(ImagingCapability.class)) {
                this.getCapability(ImagingCapability.class).setBrightness(0.5f);
                this.getCapability(ImagingCapability.class)
                        .setInterpolationState(true);
            }
            altDisplayTypes.add(DisplayType.CONTOUR);
            break;
        case BARB:
        case ARROW:
        case STREAMLINE:
            altDisplayTypes.add(DisplayType.BARB);
            altDisplayTypes.add(DisplayType.ARROW);
            altDisplayTypes.add(DisplayType.STREAMLINE);
        case DUALARROW:
            altDisplayTypes.add(DisplayType.ARROW);
        case CONTOUR:
            altDisplayTypes.add(DisplayType.IMAGE);
            getCapability(ColorableCapability.class);
            getCapability(DensityCapability.class);
            getCapability(MagnificationCapability.class);
            getCapability(OutlineCapability.class);
            break;
        case ICON:
            getCapability(ColorableCapability.class);
            getCapability(DensityCapability.class);
            getCapability(MagnificationCapability.class);
            break;
        }
        this.getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(altDisplayTypes);
    }

    /**
     * Gets style preferences from the StyleManager
     * 
     * @throws VizStyleException
     */
    protected void initStylePreferences() throws VizStyleException {
        DisplayType displayType = getDisplayType();
        StyleRule styleRule = null;
        switch (displayType) {
        case IMAGE:
            styleRule = StyleManager.getInstance().getStyleRule(
                    StyleType.IMAGERY, getMatchCriteria());
            break;
        case CONTOUR:
        case STREAMLINE:
            styleRule = StyleManager.getInstance().getStyleRule(
                    StyleType.CONTOUR, getMatchCriteria());
            break;
        case BARB:
        case ARROW:
        case DUALARROW:
            styleRule = StyleManager.getInstance().getStyleRule(
                    StyleType.ARROW, getMatchCriteria());
            break;
        }
        if (styleRule != null) {
            stylePreferences = styleRule.getPreferences();
        }
    }

    /**
     * Create an interpolation and format to be used when sampling
     */
    protected void initSampling() {
        if (this.hasCapability(ImagingCapability.class)) {
            ImagingCapability imagingCap = this
                    .getCapability(ImagingCapability.class);
            if (imagingCap.isInterpolationState()) {
                sampleInterpolion = new GridSampler(new BilinearInterpolation());
            } else {
                sampleInterpolion = new GridSampler(
                        new NearestNeighborInterpolation());
            }
        } else {
            sampleInterpolion = new GridSampler(new BilinearInterpolation());
        }
        if (stylePreferences != null
                && stylePreferences instanceof ImagePreferences) {
            ImagePreferences prefs = (ImagePreferences) stylePreferences;
            if (prefs.getSamplePrefs() != null
                    && prefs.getSamplePrefs().getFormatString() != null) {
                try {
                    int numDecimalPlaces = Integer.parseInt(prefs
                            .getSamplePrefs().getFormatString());
                    char[] zeroes = new char[numDecimalPlaces];
                    Arrays.fill(zeroes, '0');
                    sampleFormat = new DecimalFormat("0."
                            + String.copyValueOf(zeroes));

                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.INFO,
                            "Invalid sample format in style rules, expected an integer but recieved "
                                    + prefs.getSamplePrefs().getFormatString(),
                            e);
                }
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            time = getTimeForResource();
        }
        if (time == null) {
            return;
        } else if (renderables.containsKey(time)) {
            renderables.get(time).paint(target, paintProps);
            return;
        }

        GeneralGridData data = requestData(time);
        if (data == null) {
            updatePaintStatus(PaintStatus.INCOMPLETE);
            return;
        }

        IRenderable renderable = createRenderable(target, data);

        if (renderable != null) {
            renderables.put(time, renderable);
            renderable.paint(target, paintProps);
        } else {
            updatePaintStatus(PaintStatus.INCOMPLETE);
        }
    }

    /**
     * Create a renderable for this data.
     * 
     * @param target
     * @param data
     * @return
     * @throws VizException
     */
    public IRenderable createRenderable(IGraphicsTarget target,
            GeneralGridData data) throws VizException {

        IRenderable renderable = null;

        GridGeometry2D gridGeometry = getGridGeometry();

        DisplayType displayType = getDisplayType();

        switch (displayType) {
        case IMAGE:
            if (renderables.isEmpty()) {
                ColorMapParameters params = createColorMapParameters(data);
                if (params.getColorMap() == null) {
                    if (params.getColorMapName() == null) {
                        params.setColorMapName("Grid/gridded data");
                    }
                    params.setColorMap(ColorMapLoader.loadColorMap(params
                            .getColorMapName()));
                }
                this.getCapability(ColorMapCapability.class)
                        .setColorMapParameters(params);
            }
            GriddedImageDisplay2 imageRenderable = new GriddedImageDisplay2(
                    data.getScalarData(), gridGeometry, this, "2D");
            imageRenderable.init(target);
            renderable = imageRenderable;
            break;
        case BARB:
        case ARROW:
        case DUALARROW:
            convertData(data);
            GriddedVectorDisplay vectorDisplay = new GriddedVectorDisplay(
                    data.getMagnitude(), data.getDirection(), descriptor,
                    gridGeometry, 64, displayType);
            vectorDisplay.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            vectorDisplay.setLineStyle(getCapability(OutlineCapability.class)
                    .getLineStyle());
            vectorDisplay.setLineWidth(getCapability(OutlineCapability.class)
                    .getOutlineWidth());
            vectorDisplay.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            vectorDisplay.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            if (stylePreferences != null
                    && stylePreferences instanceof ArrowPreferences) {
                vectorDisplay.setScale(((ArrowPreferences) stylePreferences)
                        .getScale());
            }
            renderable = vectorDisplay;
            break;
        case ICON:
            GriddedIconDisplay iconDisplay = new GriddedIconDisplay(data
                    .getScalarData().array(), descriptor, gridGeometry, 80);
            iconDisplay.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            iconDisplay.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            iconDisplay.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            renderable = iconDisplay;
            break;
        case CONTOUR:
        case STREAMLINE:
            convertData(data);
            GriddedContourDisplay contourRenderable = null;
            if (displayType == DisplayType.CONTOUR) {
                contourRenderable = new GriddedContourDisplay(descriptor,
                        gridGeometry, data.getScalarData());
            } else {
                contourRenderable = new GriddedStreamlineDisplay(descriptor,
                        gridGeometry, data.getUComponent(),
                        data.getVComponent());
            }
            contourRenderable.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            contourRenderable.setLineStyle(getCapability(
                    OutlineCapability.class).getLineStyle());
            contourRenderable.setOutlineWidth(getCapability(
                    OutlineCapability.class).getOutlineWidth());
            contourRenderable.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            contourRenderable.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            if (stylePreferences != null
                    && stylePreferences instanceof ContourPreferences) {
                contourRenderable
                        .setPreferences((ContourPreferences) stylePreferences);
            }
            renderable = contourRenderable;
            break;

        }
        return renderable;
    }

    private void convertData(GeneralGridData data) {
        if (stylePreferences != null) {
            data.convert(stylePreferences.getDisplayUnits());
        }
    }

    /**
     * Called the first time an image is drawn to initialize the color map
     * parameters
     * 
     * @param data
     * @return
     * @throws VizException
     */
    protected ColorMapParameters createColorMapParameters(GeneralGridData data)
            throws VizException {
        ParamLevelMatchCriteria criteria = getMatchCriteria();
        String parameter = null;
        Unit<?> parameterUnits = data.getDataUnit();
        SingleLevel level = null;
        String creatingEntity = null;
        if (criteria.getParameterNames() != null
                && !criteria.getParameterNames().isEmpty()) {
            parameter = criteria.getParameterNames().get(0);
        }
        if (criteria.getLevels() != null && !criteria.getLevels().isEmpty()) {
            Level styleLevel = criteria.getLevels().get(0);
            if (styleLevel instanceof SingleLevel) {
                level = (SingleLevel) styleLevel;
            }
        }
        if (criteria.getCreatingEntityNames() != null
                && !criteria.getCreatingEntityNames().isEmpty()) {
            creatingEntity = criteria.getCreatingEntityNames().get(0);
        }
        return ColorMapParameterFactory.build(data.getScalarData().array(),
                parameter, parameterUnits, level, creatingEntity);
    }

    /**
     * Get the match criteria used for looking up style rules and/or colormaps.
     * 
     * @return
     */
    public abstract ParamLevelMatchCriteria getMatchCriteria();

    /**
     * The gridGeometry to use for this data.
     * 
     * @return
     */
    public abstract GridGeometry2D getGridGeometry();

    /**
     * This method should return a data object for the given time and/or pdos.
     * 
     * @param pdos
     *            Any pdos that have been added for this time.
     * @return
     * @throws VizException
     */
    public abstract GeneralGridData getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException;

    protected GeneralGridData requestData(DataTime time) {
        synchronized (requestJob) {
            GeneralGridData data = this.data.get(time);
            if (data == null) {
                data = requestJob.requestData(time, pdos.get(time));
                if (data != null) {
                    this.data.put(time, data);
                }
            }
            return data;
        }
    }

    @Override
    protected void disposeInternal() {
        requestJob.stopAndClear();
        for (IRenderable renderable : renderables.values()) {
            disposeRenderable(renderable);
        }
        renderables.clear();
    }

    /**
     * Dispose of a renderable.
     * 
     * @param renderable
     */
    private void disposeRenderable(final IRenderable renderable) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (renderable instanceof GriddedImageDisplay2) {
                    ((GriddedImageDisplay2) renderable).dispose();
                } else if (renderable instanceof AbstractGriddedDisplay<?>) {
                    ((AbstractGriddedDisplay<?>) renderable).dispose();
                } else if (renderable instanceof ContourRenderable) {
                    ((ContourRenderable) renderable).dispose();
                } else {
                    System.err.println("Undisposed renderable of type: "
                            + renderable.getClass().getSimpleName());
                }
            }

        });

    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        Iterator<IRenderable> iter = renderables.values().iterator();
        while (iter.hasNext()) {
            IRenderable renderable = iter.next();
            if (!projectRenderable(renderable, crs)) {
                disposeRenderable(renderable);
                iter.remove();
            }
        }
        initSampling();
    }

    /**
     * Attempt to reproject the renderable, if the renderable cannot be
     * reprojected return false and it will be disposed and a new one made for
     * the new projection.
     * 
     * @param renderable
     * @param crs
     * @return
     * @throws VizException
     */
    protected boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException {
        if (renderable instanceof GriddedImageDisplay2) {
            ((GriddedImageDisplay2) renderable).reproject();
            return true;
        } else if (renderable instanceof AbstractGriddedDisplay<?>) {
            ((AbstractGriddedDisplay<?>) renderable).reproject();
            return true;
        }
        return false;
    }

    protected DataTime getTimeForResource() {
        return descriptor.getTimeForResource(this);
    }

    protected GeneralGridData getCurrentData() {
        DataTime time = getTimeForResource();
        if (time == null) {
            return null;
        }
        return requestData(time);
    }

    public Measure<Float, ?> inspectValue(ReferencedCoordinate coord)
            throws VizException {
        GeneralGridData data = getCurrentData();
        if (data == null) {
            return null;
        }
        sampleInterpolion.setSource(new FloatBufferWrapper(
                data.getScalarData(), getGridGeometry()));
        float value = Float.NaN;
        try {
            Coordinate xy = coord.asPixel(getGridGeometry());
            value = (float) sampleInterpolion.sample(xy.x, xy.y);
        } catch (FactoryException e) {
            throw new VizException(e);
        } catch (TransformException e) {
            throw new VizException(e);
        }

        sampleInterpolion.setSource(null);
        Unit<?> unit = data.getDataUnit();

        if (stylePreferences != null) {
            Unit<?> styleUnit = stylePreferences.getDisplayUnits();
            if (unit != null && !unit.equals(styleUnit)
                    && unit.isCompatible(styleUnit)) {
                value = (float) unit.getConverterTo(styleUnit).convert(value);
                unit = styleUnit;
            }
        }

        return Measure.valueOf(value, unit);
    }

    public Measure<Float, Angle> inspectDirection(ReferencedCoordinate coord)
            throws VizException {
        GeneralGridData data = getCurrentData();
        if (data == null) {
            return null;
        }
        if (!data.isVector()) {
            return null;
        }
        sampleInterpolion.setSource(new FloatBufferWrapper(data.getDirection(),
                getGridGeometry()));
        float value = Float.NaN;
        try {
            Coordinate xy = coord.asPixel(getGridGeometry());
            value = (float) sampleInterpolion.sample(xy.x, xy.y);
        } catch (FactoryException e) {
            throw new VizException(e);
        } catch (TransformException e) {
            throw new VizException(e);
        }

        sampleInterpolion.setSource(null);

        return Measure.valueOf(value, NonSI.DEGREE_ANGLE);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Measure<Float, ?> value = inspectValue(coord);
        if (value == null) {
            return null;
        }
        if (value.getValue().isNaN()) {
            return "NO DATA";
        }
        Measure<Float, Angle> dir = inspectDirection(coord);
        if (dir != null) {
            return String.format("%.0f\u00B0 ",
                    dir.floatValue(NonSI.DEGREE_ANGLE))
                    + formatInspect(value);
        } else {
            return formatInspect(value);
        }
    }

    /**
     * Format the value and return the sample string.
     * 
     * @param value
     * @param unit
     * @return
     */
    protected String formatInspect(Measure<Float, ?> value) {
        Unit<?> dataUnit = value.getUnit();

        if (dataUnit == null) {
            return sampleFormat.format(value);
        }
        Unit<?> styleUnit = null;
        if (stylePreferences != null) {
            styleUnit = stylePreferences.getDisplayUnits();
        }
        if (dataUnit.equals(styleUnit)) {
            return sampleFormat.format(value.getValue())
                    + stylePreferences.getDisplayUnitLabel();
        } else {
            return sampleFormat.format(value.getValue())
                    + UnitFormat.getUCUMInstance().format(value.getUnit());
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        pdos.remove(dataTime);
        data.remove(dataTime);
        requestJob.remove(dataTime);
        dataTimes.remove(dataTime);
        IRenderable renderable = renderables.remove(dataTime);
        if (renderable != null) {
            disposeRenderable(renderable);
        }
    }

    /**
     * Shorthand method to get the DisplayType from the capability.
     * 
     * @return
     */
    public DisplayType getDisplayType() {
        return getCapability(DisplayTypeCapability.class).getDisplayType();
    }

    @Override
    public ResourceOrder getResourceOrder() {
        ResourceOrder order = super.getResourceOrder();
        if (order.equals(ResourceOrder.UNKNOWN)) {
            switch (getDisplayType()) {
            case IMAGE:
                order = RenderingOrderFactory.getRenderingOrder("IMAGE_WORLD");
                break;
            default:
                order = RenderingOrderFactory.getRenderingOrder("CONTOUR");
            }
        }
        return order;
    }

    /**
     * Reset renderables and any other data caches, data will be rerequested
     * next time it is needed.
     * 
     */
    protected void clearRequestedData() {
        requestJob.stopAndClear();
        for (IRenderable renderable : renderables.values()) {
            disposeRenderable(renderable);
        }
        renderables.clear();
        data.clear();
    }

    protected List<PluginDataObject> getCurrentPluginDataObjects() {
        return pdos.get(getTimeForResource());
    }

    protected List<PluginDataObject> getPluginDataObjects(DataTime time) {
        return pdos.get(time);
    }

}
