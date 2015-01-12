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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.measure.converter.UnitConverter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
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
import com.raytheon.uf.viz.core.map.MapDescriptor;
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
import com.raytheon.viz.core.contours.rsc.displays.GriddedVectorDisplay;
import com.raytheon.viz.core.contours.util.VectorGraphicsConfig;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Objective Analysis Resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 05, 2009           randerso    Initial creation
 * Jan 08, 2010  4205     jelkins     add equals checking for OA resources
 * Aug 27, 2013  2287     randerso    Added new parameters to
 *                                    GriddedVectorDisplay constructor
 * Sep 23, 2013  2363     bsteffen    Add more vector configuration options.
 * Jun 30, 2014  3165     njensen     Use ColorMapLoader to get ColorMap
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class OAResource extends
        AbstractVizResource<OAResourceData, MapDescriptor> implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OAResource.class);

    private static final int GRID_SIZE = 300;

    /* Unknown source, seems to be provide acceptable density. */
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
        DataTime dataTime;

        public Request(DataTime dataTime) {
            this.dataTime = dataTime;
            requestQueue.add(this);
            updateJob.schedule();
        }

        @Override
        public int compareTo(Request o) {
            return o.dataTime.compareTo(this.dataTime);
        }
    }

    private final OAUpateJob updateJob;

    private final Set<Request> requestQueue;

    private final Set<DataTime> pendingUpdates;

    private DataTime displayedDataTime;

    private final Map<DataTime, IRenderable> renderableMap;

    private DisplayType displayType;

    private String parameterUnitString;

    private OAGridTransformer transformer; // robot in disguise

    protected OAResource(OAResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        renderableMap = new ConcurrentHashMap<DataTime, IRenderable>();
        requestQueue = new ConcurrentSkipListSet<Request>();
        updateJob = new OAUpateJob();
        pendingUpdates = new ConcurrentSkipListSet<DataTime>();
        dataTimes = new ArrayList<DataTime>(Arrays.asList(resourceData
                .getAvailableTimes()));

        this.displayType = getCapability(DisplayTypeCapability.class)
                .getDisplayType();
        getCapability(DisplayTypeCapability.class).setAlternativeDisplayTypes(
                Arrays.asList(DisplayType.IMAGE));
        resourceData.addChangeListener(this);
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
                    ((GriddedContourDisplay) renderable).dispose();
                } else if (renderable instanceof GriddedVectorDisplay) {
                    ((GriddedVectorDisplay) renderable).dispose();
                }
            }
        });
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
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
                ImagingCapability imagingCap = getCapability(ImagingCapability.class);
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

                OutlineCapability outlineCap = getCapability(OutlineCapability.class);
                contour.setLineStyle(outlineCap.getLineStyle());
                contour.setOutlineWidth(outlineCap.getOutlineWidth());
                DensityCapability densityCap = getCapability(DensityCapability.class);
                contour.setDensity(densityCap.getDensity());
                MagnificationCapability magCap = getCapability(MagnificationCapability.class);
                if (magCap != null) {
                    contour.setMagnification(magCap.getMagnification());
                }
                contour.paint(target, paintProps);
            } else if (renderable instanceof GriddedVectorDisplay) {
                GriddedVectorDisplay vector = (GriddedVectorDisplay) renderable;
                RGB color = getCapability(ColorableCapability.class).getColor();
                vector.setColor(color);
                OutlineCapability outlineCap = getCapability(OutlineCapability.class);
                vector.setLineStyle(outlineCap.getLineStyle());
                vector.setLineWidth(outlineCap.getOutlineWidth());
                DensityCapability densityCap = getCapability(DensityCapability.class);
                vector.setDensity(densityCap.getDensity());
                MagnificationCapability magCap = getCapability(MagnificationCapability.class);
                vector.setMagnification(magCap.getMagnification());
                vector.paint(target, paintProps);
            }
        }
    }

    /**
     * @param dataTime
     * @throws VizException
     */
    private void computeNewFrame(DataTime dataTime) throws VizException {
        if (!this.dataTimes.contains(dataTime)) {
            return;
        }
        try {
            System.out.println("OA new frame " + dataTime.getLegendString());
            long t0 = System.currentTimeMillis();

            TimeRange tr = this.resourceData.getBinOffset().getTimeRange(
                    dataTime);

            HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                    this.resourceData.getMetadataMap());

            DataTime start = new DataTime(tr.getStart());
            DataTime end = new DataTime(tr.getEnd());

            RequestConstraint dtConstraint = new RequestConstraint();
            String[] constraintList = { start.toString(), end.toString() };
            dtConstraint.setBetweenValueList(constraintList);
            dtConstraint.setConstraintType(ConstraintType.BETWEEN);
            constraints.put("dataTime", dtConstraint);

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

            disposeRenderable(renderableMap.get(dataTime));

            FloatBuffer data = FloatBuffer.wrap(grid);
            SingleLevel level = resourceData.getLevel();
            if (dataTime.isSpatial()) {
                level.setValue(dataTime.getLevelValue());
            }
            switch (displayType) {
            case IMAGE: {
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

                cmapParams = ColorMapParameterFactory.build(grid, transformer
                        .getParmDescription().getParameterName(), transformer
                        .getParmDescription().getUnitObject(), level);

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

                getCapability(ColorMapCapability.class).setColorMapParameters(
                        cmapParams);

                GriddedImageDisplay image = new GriddedImageDisplay(data,
                        this.descriptor, transformer.getGridGeom());

                renderableMap.put(dataTime, image);
                break;
            }

            case CONTOUR: {
                GriddedContourDisplay contour = new GriddedContourDisplay(
                        descriptor, transformer.getGridGeom(), data);

                ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
                match.setLevel(level);
                match.setParameterName(new ArrayList<String>(Arrays
                        .asList(parameter)));
                StyleRule sr = StyleManager.getInstance().getStyleRule(
                        StyleManager.StyleType.CONTOUR, match);
                if (sr != null) {
                    ContourPreferences prefs = (ContourPreferences) sr
                            .getPreferences();
                    this.parameterUnitString = prefs.getDisplayUnitLabel();
                    UnitConverter converter = transformer.getParmDescription()
                            .getUnitObject()
                            .getConverterTo(prefs.getDisplayUnits());

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
            case BARB: {
                // Split apart magnitude and direction
                FloatBuffer mag = data;
                data.position(transformer.getNx() * transformer.getNy());
                FloatBuffer dir = data.slice();
                GriddedVectorDisplay vector = new GriddedVectorDisplay(mag,
                        dir, descriptor, transformer.getGridGeom(),
                        VECTOR_DENSITY_FACTOR, true, displayType,
                        new VectorGraphicsConfig());

                renderableMap.put(dataTime, vector);
                break;
            }
            default: {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "OAResource cannot display data as "
                                + displayType.toString());
            }
            }

            System.out.println("OA new frame took "
                    + (System.currentTimeMillis() - t0) + "ms");

            issueRefresh();
        } catch (Exception e) {
            throw new VizException(
                    "Error retreiving points for objective analysis", e);
        }
    }

    private String getParameterUnitString() {
        if (parameterUnitString == null) {
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            SingleLevel level = resourceData.getLevel();
            if (displayedDataTime != null && displayedDataTime.isSpatial()) {
                level.setValue(displayedDataTime.getLevelValue());
            }
            match.setLevel(level);
            match.setParameterName(new ArrayList<String>(Arrays
                    .asList(this.resourceData.getParameter())));
            try {
                StyleRule sr = StyleManager.getInstance().getStyleRule(
                        StyleManager.StyleType.CONTOUR, match);
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
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

                if (value == GridUtil.GRID_FILL_VALUE) {
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
                FloatDataRecord fdr = (FloatDataRecord) contour.getData()[0];
                float[] sliceData = fdr.getFloatData();
                double val11 = sliceData[y1 * transformer.getNx() + x1];
                double val21 = sliceData[y1 * transformer.getNx() + x2];
                double val12 = sliceData[y2 * transformer.getNx() + x1];
                double val22 = sliceData[y2 * transformer.getNx() + x2];
                double val = 0.0;
                boolean data = false;
                if (val11 != GridUtil.GRID_FILL_VALUE) {
                    val += (x2 - x) * (y2 - y) * val11
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val21 != GridUtil.GRID_FILL_VALUE) {
                    val += (x - x1) * (y2 - y) * val21
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val12 != GridUtil.GRID_FILL_VALUE) {
                    val += (x2 - x) * (y - y1) * val12
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (val22 != GridUtil.GRID_FILL_VALUE) {
                    val += (x - x1) * (y - y1) * val22
                            / ((x2 - x1) * (y2 - y1));
                    data = true;
                }
                if (data) {
                    return ((int) (val * 100)) / 100.0 + parameterUnitString;
                }
            }

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return "NO DATA";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        displayedDataTime = descriptor.getTimeForResource(this);
        StringBuffer sb = new StringBuffer();
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        System.out.println("OA setDescriptor called");
        super.setDescriptor(descriptor);

        try {
            float smoothPts = 200 * 1000 * GRID_SIZE / descriptor.getMapWidth();
            if (resourceData.getLevelKey().endsWith("deg")
                    || resourceData.getLevelKey().equals(
                            OAResourceData.ALL_TILTS)) {
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        System.out.println("OA project called:");
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
                            .getNormalizedTime(
                                    (DataTime) message.decodedAlert
                                            .get("dataTime"));
                    pendingUpdates.add(dataTime);

                    if (!dataTimes.contains(dataTime)) {
                        dataTimes.add(dataTime);
                        Collections.sort(dataTimes);
                    }
                } catch (Exception e) {
                    // TODO: handle exception
                    e.printStackTrace();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        disposeRenderable(renderableMap.remove(dataTime));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#getRenderingOrderId()
     */
    @Override
    public ResourceOrder getResourceOrder() {
        if (displayType.equals(DisplayType.IMAGE)) {
            return RenderingOrderFactory.getRenderingOrder("IMAGE_WORLD");
        } else {
            return RenderingOrderFactory.getRenderingOrder("CONTOUR");
        }
    }

}
