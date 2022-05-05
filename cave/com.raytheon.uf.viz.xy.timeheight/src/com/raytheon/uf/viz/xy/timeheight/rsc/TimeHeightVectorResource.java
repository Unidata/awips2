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
package com.raytheon.uf.viz.xy.timeheight.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsRenderable;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor.TimeDirection;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import org.locationtech.jts.geom.Coordinate;

/**
 * Resource for displaying time height as vectors
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 04, 2007  625      njensen   Initial creation
 * Feb 20, 2009  1960     njensen   Refactored to new rsc architecture
 * Feb 14, 2011  8244     bkowal    enabled the magnification capability. Get
 *                                  graph in loadInterpolatedData after we
 *                                  ensure that it is not NULL.
 * Sep 23, 2013  2363     bsteffen  Add more vector configuration options.
 * Aug 12, 2016  5822     bsteffen  implement disposeRenderables
 * Feb 08, 2018  6825     njensen   Remove Load as Image capability to match A1
 * Feb 19, 2018  6666     bsteffen  Get data from adapter using loadPreparedData
 * Feb 28, 2018  7231     njensen   Use super's statusHandler
 * Nov 15, 2018  58492    edebebe   Enabled configurable 'Wind Barb' properties
 * 
 * </pre>
 * 
 * @author njensen
 */
public class TimeHeightVectorResource extends AbstractTimeHeightResource
        implements IResourceDataChanged {

    /* Unknown source, provides acceptable density. */
    private static final int VECTOR_SPACING = 60;

    private float[] vInterpolatedData;

    private Map<Coordinate, IImage> imageMap = new HashMap<>();

    //Parameters used to construct 'VectorGraphicsConfig'
    private static final String PLUGIN_NAME = "TimeHeight";
    private static final String CLASS_NAME = "TimeHeightVectorResource";

    private VectorGraphicsConfig config = new VectorGraphicsConfig(PLUGIN_NAME, CLASS_NAME);

    public TimeHeightVectorResource(TimeHeightResourceData data,
            LoadProperties props, AbstractVarHeightAdapter<?> adapter) {
        super(data, props, adapter);

        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.ARROW, match);
        } catch (StyleException e) {
            statusHandler.error("Error getting arrow style rules", e);
        }
        if (sr != null) {
            prefs = sr.getPreferences();
        }
        this.getResourceData().addChangeListener(this);
    }

    @Override
    protected void disposeRenderables() {
        for (IImage image : imageMap.values()) {
            image.dispose();
        }
        imageMap.clear();
    }

    @Override
    protected void loadInterpolatedData() {
        if (geometry == null || descriptor == null
                || descriptor.getGraph(this) == null
                || !descriptor.getGraph(this).isReady()) {
            // try again later
            loadDataJob.schedule(250);
            return;
        }

        IGraph graph = descriptor.getGraph(this);
        List<DataTime> dataTimes = new ArrayList<>(this.dataTimes);

        float[][] uColumns = new float[dataTimes
                .size()][(int) geometry.getGridRange2D().getHeight()];
        float[][] vColumns = new float[dataTimes
                .size()][(int) geometry.getGridRange2D().getHeight()];
        float[] times = new float[dataTimes.size()];

        TimeDirection direction = this.getDescriptor().getTimeDirection();

        for (int i = 0; i < dataTimes.size(); i++) {
            int dataTimesIndex = i;
            if (direction == TimeDirection.LEFT_TO_RIGHT) {
                dataTimesIndex = dataTimes.size() - 1 - i;
            }
            try {
                List<XYData> dataList = adapter.loadPreparedData(
                        dataTimes.get(dataTimesIndex), getUnit());
                List<XYData> uList = new ArrayList<>(dataList.size());
                List<XYData> vList = new ArrayList<>(dataList.size());
                for (XYData xyData : dataList) {
                    XYWindImageData windData = (XYWindImageData) xyData;
                    double dir = windData.getWindDir();
                    double spd = windData.getWindSpd();
                    dir = Math.toRadians(dir);
                    uList.add(new XYData(-spd * Math.sin(dir), xyData.getY()));
                    vList.add(new XYData(-spd * Math.cos(dir), xyData.getY()));
                }
                uColumns[i] = InterpUtils.makeColumn(uList,
                        (int) geometry.getGridRange2D().getHeight(), graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(),
                        Float.NaN);
                vColumns[i] = InterpUtils.makeColumn(vList,
                        (int) geometry.getGridRange2D().getHeight(), graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(),
                        Float.NaN);
                times[i] = dataTimes.get(dataTimesIndex).getValidTime()
                        .getTimeInMillis();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading Time Height Data for time "
                                + dataTimes.get(dataTimesIndex)
                                        .getLegendString()
                                + ": " + e.getLocalizedMessage(),
                        e);
            }
        }

        interpolatedData = InterpUtils.makeRows(uColumns, times,
                (int) geometry.getGridRange2D().getWidth(), graph,
                direction == TimeDirection.LEFT_TO_RIGHT, Float.NaN);
        vInterpolatedData = InterpUtils.makeRows(vColumns, times,
                (int) geometry.getGridRange2D().getWidth(), graph,
                direction == TimeDirection.LEFT_TO_RIGHT, Float.NaN);
        // reset any cached images/contours
        this.disposeRenderables();
        this.issueRefresh();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);

        if (interpolatedData == null || vInterpolatedData == null) {
            return;
        }

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        float[] uData = interpolatedData;
        float[] vData = vInterpolatedData;

        double density = getCapability(DensityCapability.class).getDensity();
        RGB color = getCapability(ColorableCapability.class).getColor();

        IExtent graphArea = descriptor.getGraph(this).getExtent();
        if (graphArea == null) {
            return;
        }
        double width = graphArea.getWidth()
                / geometry.getGridRange2D().getMaxX();
        double height = graphArea.getHeight()
                / geometry.getGridRange2D().getMaxY();

        PaintProperties imagePaintProperties = new PaintProperties(paintProps);
        imagePaintProperties.setAlpha(1.0f);
        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        int spacing = (int) ((geometry.getGridRange2D().getMaxX()
                * VECTOR_SPACING * ratio / Math.min(2.0, density))
                / paintProps.getCanvasBounds().width + 1);
        IExtent viewableArea = paintProps.getView().getExtent()
                .intersection(graphArea);

        config.setSizeScaler(magnification * ratio);
        VectorGraphicsRenderable vgr = new VectorGraphicsRenderable(
                this.descriptor, target, config);
        for (int i = spacing / 2; i < geometry.getGridRange2D()
                .getMaxX(); i += spacing) {
            for (int j = spacing / 2; j < geometry.getGridRange2D()
                    .getMaxY(); j += spacing) {
                double screenX = graphArea.getMinX() + (i + 0.5) * width;
                double screenY = graphArea.getMinY() + graphArea.getHeight()
                        - (j + 0.5) * height;
                if (!viewableArea.contains(new double[] { screenX, screenY })) {
                    continue;
                }
                int index = j * (int) geometry.getGridRange2D().getMaxX() + i;

                if (uData[index] <= -9999 || vData[index] <= -9999) {
                    continue;
                }
                float uudd = uData[index];
                float vvff = vData[index];

                double spd = Math.hypot(uudd, vvff);
                double dir = Math.atan2(-uudd, -vvff);
                Coordinate plotLoc = new Coordinate(screenX, screenY);

                if (getCapability(DisplayTypeCapability.class)
                        .getDisplayType() == DisplayType.ARROW) {
                    vgr.paintArrow(plotLoc, spd, dir);
                } else {
                    vgr.paintBarb(plotLoc, spd, dir);
                }
            }
        }
        vgr.setColor(color);
        vgr.setLineWidth(
                getCapability(OutlineCapability.class).getOutlineWidth());
        vgr.setLineStyle(getCapability(OutlineCapability.class).getLineStyle());
        vgr.paint(target);
        vgr.dispose();
    }

    @Override
    public void setDescriptor(TimeHeightDescriptor descriptor) {
        vInterpolatedData = null;
        super.setDescriptor(descriptor);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {
                getCapability(ColorableCapability.class)
                        .setColor(((ColorableCapability) object).getColor());
                imageMap.clear();
            }
        }
    }
}
