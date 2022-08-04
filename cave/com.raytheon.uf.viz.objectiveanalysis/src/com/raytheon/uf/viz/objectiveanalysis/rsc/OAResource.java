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
package com.raytheon.uf.viz.objectiveanalysis.rsc;

import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.measure.UnitConverter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.grid.display.GriddedVectorDisplay;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedStreamlineDisplay;

/**
 * Objective Analysis Resource. Objective analysis interpolates data from
 * irregularly spaced locations to a fixed grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 05, 2009           randerso  Initial creation
 * Jan 08, 2010  4205     jelkins   add equals checking for OA resources
 * Aug 27, 2013  2287     randerso  Added new parameters to GriddedVectorDisplay
 *                                  constructor
 * Sep 23, 2013  2363     bsteffen  Add more vector configuration options.
 * Jun 30, 2014  3165     njensen   Use ColorMapLoader to get ColorMap
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * May 17, 2018  7294     njensen   Fixed inspect of contours, initInternal()
 * May 22, 2018  7311     njensen   Support streamlines and arrows for Wind
 * Nov 01, 2018  7314     bsteffen  Use same colormap parameters for all frames.
 * Nov 15, 2018  58492    edebebe   Enabled configurable 'Wind Barb' properties 
 * 
 * </pre>
 * 
 * @author randerso
 */
public class OAResource
        extends AbstractVizResource<OAResourceData, MapDescriptor>
        implements IResourceDataChanged {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OAResource.class);

    private static final int GRID_SIZE = 300;

    /* Unknown source, seems to provide acceptable density. */
    private static final double VECTOR_DENSITY_FACTOR = 1.875;

    private class OAUpateJob extends Job {

        public OAUpateJob() {
            super("Preparing OA Data");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            for (Request req : requestQueue) {
                try {
                    computeNewFrame(req.dataTime);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                }
                requestQueue.remove(req);

            }
            return Status.OK_STATUS;
        }
    }

    private class Request implements Comparable<Request> {
        public final DataTime dataTime;

        public Request(DataTime dataTime) {
            this.dataTime = dataTime;
            requestQueue.add(this);
            updateJob.schedule();
        }

        @Override
        public int compareTo(Request o) {
            return o.dataTime.compareTo(this.dataTime);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result
                    + ((dataTime == null) ? 0 : dataTime.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Request other = (Request) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (dataTime == null) {
                if (other.dataTime != null) {
                    return false;
                }
            } else if (!dataTime.equals(other.dataTime)) {
                return false;
            }
            return true;
        }

        private OAResource getOuterType() {
            return OAResource.this;
        }

    }

    private final OAUpateJob updateJob;

    private final Set<Request> requestQueue;

    private final Set<DataTime> pendingUpdates;

    private DataTime displayedDataTime;

    private final ConcurrentMap<DataTime, IRenderable> renderableMap;

    private DisplayType displayType;

    private String parameterUnitString;

    private OAGridTransformer transformer;

    //Parameters used to construct 'VectorGraphicsConfig'
    private static final String PLUGIN_NAME = "ObjectiveAnalysisPlugin";
    private static final String CLASS_NAME = "OAResource";

    private VectorGraphicsConfig config = new VectorGraphicsConfig(PLUGIN_NAME, CLASS_NAME);

    protected OAResource(OAResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);

        renderableMap = new ConcurrentHashMap<>();
        requestQueue = new ConcurrentSkipListSet<>();
        updateJob = new OAUpateJob();
        pendingUpdates = new ConcurrentSkipListSet<>();

        DisplayTypeCapability displayTypeCapability = getCapability(
                DisplayTypeCapability.class);
        this.displayType = displayTypeCapability.getDisplayType();
        if ("Wind".equals(resourceData.getParameterName())) {
            displayTypeCapability.setAlternativeDisplayTypes(
                    Arrays.asList(DisplayType.ARROW, DisplayType.STREAMLINE));
        } else {
            displayTypeCapability.setAlternativeDisplayTypes(
                    Arrays.asList(DisplayType.IMAGE));
        }
        if (this.displayType == DisplayType.IMAGE) {
            this.getCapability(ImagingCapability.class);
        }
    }

    @Override
    protected void disposeInternal() {
        resourceData.removeChangeListener(this);
        for (IRenderable renderable : renderableMap.values()) {
            disposeRenderable(renderable);
        }
        renderableMap.clear();
    }

    private void disposeRenderable(final IRenderable renderable) {
        if (renderable == null) {
            return;
        }
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (renderable instanceof GriddedImageDisplay) {
                    ((GriddedImageDisplay) renderable).dispose();
                } else if (renderable instanceof GriddedContourDisplay) {
                    /*
                     * GriddedStreamlineDisplay extends GriddedContourDisplay so
                     * streamlines will be safely disposed here
                     */
                    ((GriddedContourDisplay) renderable).dispose();
                } else if (renderable instanceof GriddedVectorDisplay) {
                    ((GriddedVectorDisplay) renderable).dispose();
                }
            }
        });
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        dataTimes.addAll(Arrays.asList(resourceData.getAvailableTimes()));
        resourceData.addChangeListener(this);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // get the time of the current frame
        displayedDataTime = paintProps.getDataTime();
        if (displayedDataTime == null) {
            return;
        }

        // get the renderable for the current frame
        IRenderable renderable = renderableMap.get(displayedDataTime);

        // if the renderable is null or has pending updates submit a request
        // for the new frame to be calculated
        if (renderable == null || pendingUpdates.contains(displayedDataTime)) {
            new Request(displayedDataTime);
            pendingUpdates.remove(displayedDataTime);
        }

        // if the renderable is not null display it
        if (renderable != null) {
            if (renderable instanceof GriddedImageDisplay) {
                GriddedImageDisplay image = (GriddedImageDisplay) renderable;
                ImagingCapability imagingCap = getCapability(
                        ImagingCapability.class);
                GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                        paintProps, imagingCap.getBrightness(),
                        imagingCap.getContrast(),
                        imagingCap.isInterpolationState());

                ColorMapParameters parameters = getCapability(
                        ColorMapCapability.class).getColorMapParameters();

                if (parameters.getColorMap() == null) {
                    try {
                        parameters.setColorMap(ColorMapLoader
                                .loadColorMap(parameters.getColorMapName()));
                    } catch (ColorMapException e) {
                        throw new VizException(e);
                    }
                }

                image.setColorMapParameters(parameters);
                image.paint(target, giProps);
            } else if (renderable instanceof GriddedContourDisplay) {
                GriddedContourDisplay contour = (GriddedContourDisplay) renderable;

                RGB color = getCapability(ColorableCapability.class).getColor();
                contour.setColor(color);

                OutlineCapability outlineCap = getCapability(
                        OutlineCapability.class);
                contour.setLineStyle(outlineCap.getLineStyle());
                contour.setOutlineWidth(outlineCap.getOutlineWidth());
                DensityCapability densityCap = getCapability(
                        DensityCapability.class);
                contour.setDensity(densityCap.getDensity());
                MagnificationCapability magCap = getCapability(
                        MagnificationCapability.class);
                if (magCap != null) {
                    contour.setMagnification(magCap.getMagnification());
                }
                contour.paint(target, paintProps);
            } else if (renderable instanceof GriddedVectorDisplay) {
                GriddedVectorDisplay vector = (GriddedVectorDisplay) renderable;
                RGB color = getCapability(ColorableCapability.class).getColor();
                vector.setColor(color);
                OutlineCapability outlineCap = getCapability(
                        OutlineCapability.class);
                vector.setLineStyle(outlineCap.getLineStyle());
                vector.setLineWidth(outlineCap.getOutlineWidth());
                DensityCapability densityCap = getCapability(
                        DensityCapability.class);
                vector.setDensity(densityCap.getDensity());
                MagnificationCapability magCap = getCapability(
                        MagnificationCapability.class);
                vector.setMagnification(magCap.getMagnification());
                vector.paint(target, paintProps);
            }
        }
    }

    private void computeNewFrame(DataTime dataTime) throws VizException {
        if (!this.dataTimes.contains(dataTime)) {
            return;
        }
        try {

            TimeRange tr = this.resourceData.getBinOffset()
                    .getTimeRange(dataTime);
            Map<String, RequestConstraint> constraints = new HashMap<>(
                    this.resourceData.getMetadataMap());

            DataTime start = new DataTime(tr.getStart());
            DataTime end = new DataTime(tr.getEnd());

            RequestConstraint dtConstraint = new RequestConstraint();
            String[] constraintList = { start.toString(), end.toString() };
            dtConstraint.setBetweenValueList(constraintList);
            dtConstraint.setConstraintType(ConstraintType.BETWEEN);
            constraints.put(PluginDataObject.DATATIME_ID, dtConstraint);

            String parameter = this.resourceData.getParameter();
            String levelKey = resourceData.getLevelKey();
            if (levelKey.equals(OAResourceData.ALL_TILTS)) {
                if (dataTime.getLevelValue() < 0) {
                    return;
                } else if (dataTime.getLevelValue() < 1) {
                    levelKey = String.format("%.1fdeg",
                            dataTime.getLevelValue());
                } else {
                    levelKey = String.format("%.2gdeg",
                            dataTime.getLevelValue());
                }
            }
            float[] grid = transformer.computeGrid(parameter, constraints,
                    levelKey);

            if (grid == null) {
                dataTimes.remove(dataTime);
                return;
            }

            /*
             * if grid's length is double nx * ny, it's a vector, i.e. u
             * followed by v
             */
            boolean isVector = (grid.length == transformer.nx * transformer.ny
                    * 2);
            float[] u = null;
            float[] v = null;
            if (isVector) {
                int length = grid.length / 2;
                u = new float[length];
                v = new float[length];
                System.arraycopy(grid, 0, u, 0, length);
                System.arraycopy(grid, length, v, 0, length);
                // free some memory since it's now copied
                grid = null;
            }

            disposeRenderable(renderableMap.get(dataTime));
            SingleLevel level = resourceData.getLevel();
            if (dataTime.isSpatial()) {
                level.setValue(dataTime.getLevelValue());
            }
            switch (displayType) {
            case IMAGE: {
                if (isVector) {
                    grid = calculateMagnitude(u, v);
                }
                FloatBuffer data = FloatBuffer.wrap(grid);

                ColorMapParameters cmapParams = null;
                IColorMap mapToUse = null;
                String colorMapName = null;
                if (hasCapability(ColorMapCapability.class)) {
                    cmapParams = getCapability(ColorMapCapability.class)
                            .getColorMapParameters();
                    if (cmapParams != null) {
                        mapToUse = cmapParams.getColorMap();
                        colorMapName = cmapParams.getColorMapName();
                    }
                }
                if (cmapParams == null) {
                    cmapParams = ColorMapParameterFactory.build(grid,
                            transformer.getParmDescription().getParameterName(),
                            transformer.getParmDescription().getUnitObject(),
                            level);

                }
                if (cmapParams.getDisplayUnit() == null) {
                    cmapParams.setDisplayUnit(cmapParams.getDataUnit());
                }
                this.parameterUnitString = cmapParams.getDisplayUnit()
                        .toString();

                if (cmapParams.getColorMapName() == null) {
                    cmapParams.setColorMapName("Grid/gridded data");
                }

                if (mapToUse != null) {
                    cmapParams.setColorMap(mapToUse);
                    cmapParams.setColorMapName(colorMapName);
                } else if (colorMapName != null) {
                    cmapParams.setColorMapName(colorMapName);
                }

                getCapability(ColorMapCapability.class)
                        .setColorMapParameters(cmapParams);

                GriddedImageDisplay image = new GriddedImageDisplay(data,
                        this.descriptor, transformer.getGridGeom());

                renderableMap.put(dataTime, image);
                break;
            }

            case CONTOUR: {
                if (isVector) {
                    grid = calculateMagnitude(u, v);
                }
                FloatBuffer data = FloatBuffer.wrap(grid);

                GriddedContourDisplay contour = new GriddedContourDisplay(
                        descriptor, transformer.getGridGeom(), data);

                ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
                match.setLevel(level);
                match.setParameterName(
                        new ArrayList<>(Arrays.asList(parameter)));
                StyleRule sr = StyleManager.getInstance()
                        .getStyleRule(StyleManager.StyleType.CONTOUR, match);
                if (sr != null) {
                    ContourPreferences prefs = (ContourPreferences) sr
                            .getPreferences();
                    this.parameterUnitString = prefs.getDisplayUnitLabel();
                    UnitConverter converter = transformer.getParmDescription()
                            .getUnitObject()
                            .getConverterToAny(prefs.getDisplayUnits());

                    for (int i = 0; i < grid.length; i++) {
                        if (grid[i] != GridUtil.GRID_FILL_VALUE) {
                            grid[i] = (float) converter.convert(grid[i]);
                        }
                    }

                    contour.setPreferences(prefs);
                }

                renderableMap.put(dataTime, contour);
                break;
            }

            case BARB:
            case ARROW: {
                if (!isVector) {
                    throw new IllegalStateException(
                            "Vector data required to render " + displayType);
                }

                float[][] magDir = calculateMagnitudeDirection(u, v);
                FloatBuffer mag = FloatBuffer.wrap(magDir[0]);
                FloatBuffer dir = FloatBuffer.wrap(magDir[1]);
                GriddedVectorDisplay vector = new GriddedVectorDisplay(mag, dir,
                        descriptor, transformer.getGridGeom(),
                        VECTOR_DENSITY_FACTOR, true, displayType,
                        config);

                renderableMap.put(dataTime, vector);
                break;
            }

            case STREAMLINE: {
                if (!isVector) {
                    throw new IllegalStateException(
                            "Vector data required to render " + displayType);
                }

                FloatBuffer uBuf = FloatBuffer.wrap(u);
                FloatBuffer vBuf = FloatBuffer.wrap(v);
                GriddedStreamlineDisplay streamline = new GriddedStreamlineDisplay(
                        descriptor, transformer.getGridGeom(), uBuf, vBuf);

                renderableMap.put(dataTime, streamline);
                break;
            }
            default: {
                statusHandler.handle(Priority.PROBLEM,
                        "OAResource cannot display data as "
                                + displayType.toString());
            }
            }

            issueRefresh();
        } catch (Exception e) {
            throw new VizException(
                    "Error retrieving points for objective analysis", e);
        }
    }

    /**
     * Calculates a grid of magnitudes of vectors.
     * 
     * @param u
     * @param v
     * @return
     */
    private float[] calculateMagnitude(float[] u, float[] v) {
        int length = u.length;
        float[] magnitude = new float[length];
        for (int i = 0; i < length; i++) {
            magnitude[i] = (float) Math.hypot(u[i], v[i]);
        }
        return magnitude;
    }

    /**
     * Calculates vectors where the first grid returned will be magnitude and
     * the second grid returned will be direction.
     * 
     * @param u
     * @param v
     * @return
     */
    private float[][] calculateMagnitudeDirection(float[] u, float[] v) {
        int length = u.length;
        float[] magnitude = calculateMagnitude(u, v);
        float[] direction = new float[length];
        for (int i = 0; i < length; i++) {
            direction[i] = (float) Math.toDegrees(Math.atan2(-u[i], -v[i]));
        }
        return new float[][] { magnitude, direction };
    }

    private String getParameterUnitString() {
        if (parameterUnitString == null) {
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            SingleLevel level = resourceData.getLevel();
            if (displayedDataTime != null && displayedDataTime.isSpatial()) {
                level.setValue(displayedDataTime.getLevelValue());
            }
            match.setLevel(level);
            match.setParameterName(new ArrayList<>(
                    Arrays.asList(this.resourceData.getParameter())));
            try {
                StyleRule sr = StyleManager.getInstance()
                        .getStyleRule(StyleManager.StyleType.CONTOUR, match);
                if (sr != null) {
                    ContourPreferences prefs = (ContourPreferences) sr
                            .getPreferences();
                    this.parameterUnitString = prefs.getDisplayUnitLabel();
                }
            } catch (StyleException e) {
                statusHandler.handle(Priority.VERBOSE, e.getLocalizedMessage(),
                        e);
            }
        }
        return parameterUnitString;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            if (displayedDataTime == null) {
                return null;
            }
            Coordinate c = coord.asGridCell(transformer.getGridGeom(),
                    PixelInCell.CELL_CORNER);
            int index = (int) (c.y * transformer.getNx() + c.x);

            IRenderable renderable = renderableMap.get(displayedDataTime);
            if (renderable instanceof GriddedImageDisplay) {
                GriddedImageDisplay image = (GriddedImageDisplay) renderable;
                double value = ((FloatBuffer) image.getData()).get(index);

                if (value == GridUtil.GRID_FILL_VALUE || Double.isNaN(value)) {
                    return "NO DATA";
                }

                ColorMapParameters parameters = getCapability(
                        ColorMapCapability.class).getColorMapParameters();
                value = parameters.getDataToDisplayConverter().convert(value);

                DecimalFormat format = new DecimalFormat(
                        parameters.getFormatString());

                return format.format(value)
                        + parameters.getDisplayUnit().toString();
            } else if (renderable instanceof GriddedContourDisplay) {
                double x = c.x - 0.5;
                double y = c.y - 0.5;
                int x2 = (int) Math.min(Math.ceil(x), transformer.getNx() - 1);
                int x1 = (int) Math.max(Math.floor(x), 0);
                int y2 = (int) Math.min(Math.ceil(y), transformer.getNy() - 1);
                int y1 = (int) Math.max(Math.floor(y), 0);
                GriddedContourDisplay contour = (GriddedContourDisplay) renderable;
                float[] sliceData = ((FloatBufferWrapper) ((GeographicDataSource) contour
                        .getData()[0]).getWrappedSource()).getArray();
                double val11 = sliceData[y1 * transformer.getNx() + x1];
                double val21 = sliceData[y1 * transformer.getNx() + x2];
                double val12 = sliceData[y2 * transformer.getNx() + x1];
                double val22 = sliceData[y2 * transformer.getNx() + x2];
                double val = 0.0;
                boolean data = false;
                if (val11 != GridUtil.GRID_FILL_VALUE && !Double.isNaN(val11)) {
                    val += (x2 - x) * (y2 - y) * val11
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val21 != GridUtil.GRID_FILL_VALUE && !Double.isNaN(val21)) {
                    val += (x - x1) * (y2 - y) * val21
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val12 != GridUtil.GRID_FILL_VALUE && !Double.isNaN(val12)) {
                    val += (x2 - x) * (y - y1) * val12
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val22 != GridUtil.GRID_FILL_VALUE && !Double.isNaN(val22)) {
                    val += (x - x1) * (y - y1) * val22
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (data) {
                    return ((int) (val * 100)) / 100.0 + parameterUnitString;
                }
            }

        } catch (Exception e) {
            throw new VizException("Error inspecting objective analysis data",
                    e);
        }

        return "NO DATA";
    }

    @Override
    public String getName() {
        displayedDataTime = descriptor.getTimeForResource(this);
        StringBuilder sb = new StringBuilder();
        sb.append(this.resourceData.getSource());
        double levelvalue = -1;
        if (displayedDataTime != null && displayedDataTime.isSpatial()) {
            levelvalue = displayedDataTime.getLevelValue();
        }
        if (levelvalue == -1) {
            levelvalue = this.resourceData.getLevel().getValue();
        }
        if (levelvalue != -1) {
            sb.append(" ").append(levelvalue);
        }
        sb.append(" ").append(this.resourceData.getLevel().getTypeString());

        if (resourceData.getParameterName() != null) {
            sb.append(" ").append(resourceData.getParameterName());
        } else {
            sb.append(" ").append(resourceData.getParameter());
        }

        if (displayType.equals(DisplayType.IMAGE)) {
            sb.append(" ").append("Img");
        }

        if (getParameterUnitString() != null) {
            sb.append(" (").append(parameterUnitString).append(")");
        }
        return sb.toString();
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);

        try {
            float smoothPts = 200 * 1000 * GRID_SIZE / descriptor.getMapWidth();
            if (resourceData.getLevelKey().endsWith("deg") || resourceData
                    .getLevelKey().equals(OAResourceData.ALL_TILTS)) {
                transformer = new OATiltGridTransformer(
                        this.descriptor.getGridGeometry(),
                        this.descriptor.getCRS(), GRID_SIZE, smoothPts);
            } else {
                transformer = new OAGridTransformer(
                        this.descriptor.getGridGeometry(),
                        this.descriptor.getCRS(), GRID_SIZE, smoothPts);
            }
            transformer.addLatLonConstraints(resourceData.getMetadataMap());
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }

        for (DataTime dataTime : renderableMap.keySet()) {
            IRenderable renderable = renderableMap.remove(dataTime);
            if (renderable instanceof GriddedImageDisplay) {
                ((GriddedImageDisplay) renderable).dispose();
            } else if (renderable instanceof GriddedContourDisplay) {
                ((GriddedContourDisplay) renderable).dispose();
            }
        }
        issueRefresh();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        setDescriptor(descriptor);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            if (!(object instanceof AlertMessage[])) {
                return;
            }
            AlertMessage[] messages = (AlertMessage[]) object;
            for (AlertMessage message : messages) {
                try {
                    DataTime dataTime = this.resourceData.getBinOffset()
                            .getNormalizedTime((DataTime) message.decodedAlert
                                    .get(PluginDataObject.DATATIME_ID));
                    pendingUpdates.add(dataTime);

                    dataTimes.add(dataTime);
                } catch (Exception e) {
                    statusHandler.error("Error processing data update", e);
                }
            }
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        disposeRenderable(renderableMap.remove(dataTime));
    }

    @Override
    public ResourceOrder getResourceOrder() {
        if (displayType.equals(DisplayType.IMAGE)) {
            return RenderingOrderFactory.getRenderingOrder("IMAGE_WORLD");
        } else {
            return RenderingOrderFactory.getRenderingOrder("CONTOUR");
        }
    }
}
