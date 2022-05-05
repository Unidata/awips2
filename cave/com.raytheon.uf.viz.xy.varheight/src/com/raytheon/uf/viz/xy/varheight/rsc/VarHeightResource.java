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
package com.raytheon.uf.viz.xy.varheight.rsc;

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.Unit;

import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.graph.GraphPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsRenderable;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.xy.graph.GraphProperties;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.hodo.HodographDescriptor;
import com.raytheon.uf.viz.xy.hodo.IHodographResource;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightDescriptor;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import com.raytheon.viz.core.map.GeoUtil;

import tec.uom.se.AbstractUnit;

/**
 * General var height resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ---------------------------------------
 * Nov 23, 2009           mschenke   Initial creation
 * Feb 10, 2011  8344     bkowal     enabled the magnification capability.
 * Sep 23, 2013  2363     bsteffen   Add more vector configuration options.
 * Dec 19, 2013  16795    dfriedman  Transform pixel coordinate in inspect
 * Jun 18, 2014  3242     njensen    Replaced deprecated calls
 * Aug 15, 2014  3535     njensen    Bigger inset map point
 * Nov 28, 2017  5863     bsteffen   Change dataTimes to a NavigableSet
 * Feb 06, 2018  6829     njensen    Draw wind barbs in a single, vertical column
 * Feb 07, 2018  6825     njensen    Implement complex sampling
 * Feb 19, 2018  6666     bsteffen   Get data from adapter using loadPreparedData
 * Nov 15, 2018  58492    edebebe    Enabled configurable 'Wind Barb' properties
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class VarHeightResource
        extends AbstractVizResource<VarHeightResourceData, VarHeightDescriptor>
        implements IInsetMapResource, IGraphableResource<Double, Double>,
        IHodographResource {

    protected static final DecimalFormat FORMATTER = new DecimalFormat("0.#");

    protected AbstractVarHeightAdapter<?> adapter;

    protected Map<DataTime, List<XYData>> xydata = new HashMap<>();

    protected GraphPreferences prefs;

    protected int displayedIndex = 0;

    protected VarHeightResource secondaryResource;

    protected CombineOperation combineOperation;

    protected DataTime currentTime = null;

    //Parameters used to construct 'VectorGraphicsConfig'
    private static final String PLUGIN_NAME = "VarHeightPlugin";
    private static final String CLASS_NAME = "VarHeightResource";

    private VectorGraphicsConfig config = new VectorGraphicsConfig(PLUGIN_NAME, CLASS_NAME);

    protected VarHeightResource(VarHeightResourceData resourceData,
            LoadProperties loadProperties, AbstractVarHeightAdapter<?> adapter)
            throws VizException {
        super(resourceData, loadProperties, false);

        this.adapter = adapter;
        ICombinedResourceData combinedResourceData = getResourceData();

        if (combinedResourceData != null) {
            this.secondaryResource = (VarHeightResource) combinedResourceData
                    .getSecondaryResource();
            this.combineOperation = combinedResourceData.getCombineOperation();
        }

        try {
            prefs = GraphPrefsFactory
                    .buildPreferences(resourceData.getParameter(), null);
        } catch (StyleException e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }

        resourceData.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    synchronized (VarHeightResource.this) {
                        PluginDataObject[] pdos = (PluginDataObject[]) object;
                        for (PluginDataObject pdo : pdos) {
                            addRecord(pdo);
                        }
                    }
                }
                issueRefresh();
            }

        });
    }

    @Override
    protected void disposeInternal() {
        if (secondaryResource != null) {
            secondaryResource.dispose();
        }
    }

    @Override
    public String getName() {

        if (getStatus() != ResourceStatus.INITIALIZED) {
            return null;
        }

        Boolean isPointData = false;
        String completeName = resourceData.getSource();

        String param = resourceData.getParameterName();
        Coordinate point = resourceData.getPoint();
        String pointLetter = resourceData.getPointLetter();

        String formattedPoint = GeoUtil.formatCoordinate(point);

        if (pointLetter != null) {
            completeName += " pt";
            completeName += pointLetter;
        }
        if (resourceData.getMetadataMap().get("location.stationId") != null) {
            String stnID = resourceData.getMetadataMap()
                    .get("location.stationId").getConstraintValue();
            if (stnID != null) {
                completeName += " " + stnID;
                isPointData = true;
            }
        }
        if (!isPointData) {
            completeName += " ";
            completeName += formattedPoint;
        }
        completeName += " ";
        completeName += param;
        completeName += " (";
        if (prefs.getDisplayUnitLabel() != null) {
            completeName += prefs.getDisplayUnitLabel();
        } else if (getUnits() != null) {
            completeName += getUnits();
        }
        completeName += ") ";

        if (secondaryResource != null) {
            return ICombinedResourceData.CombineUtil.getName(completeName,
                    secondaryResource.getName(), combineOperation);
        } else {
            return completeName;

        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (secondaryResource != null) {
            secondaryResource.setDescriptor(this.descriptor);
            secondaryResource.init(target);
        }
    }

    private void combineData(List<XYData> ourData, List<XYData> theirData) {
        for (int i = 0; i < ourData.size(); ++i) {
            XYData ours = ourData.get(i);
            XYData theirs = theirData.get(i);

            double n1 = ((Number) ours.getX()).doubleValue();
            double n2 = ((Number) theirs.getX()).doubleValue();
            double result = 0.0;
            switch (combineOperation) {
            case DIFFERENCE: {
                result = n1 - n2;
                break;
            }
            }
            ours.setX(new Double(result));
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        GraphProperties graphProps = (GraphProperties) paintProps;
        if (secondaryResource != null
                && !combineOperation.equals(CombineOperation.DIFFERENCE)) {
            secondaryResource.paint(target, paintProps);
        }

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        currentTime = graphProps.getDataTime();
        if (currentTime == null) {
            return;
        }

        // get the data
        List<XYData> data = xydata.get(graphProps.getDataTime());
        if (data == null) {
            data = adapter.loadPreparedData(currentTime,
                    prefs == null ? null : prefs.getDisplayUnits());
            if (secondaryResource != null
                    && combineOperation.equals(CombineOperation.DIFFERENCE)) {
                List<XYData> data2 = secondaryResource.xydata.get(currentTime);
                if (data2 == null) {
                    data2 = secondaryResource.adapter.loadData(currentTime);
                    secondaryResource.adapter.sortData(data2);
                    secondaryResource.adapter.convertData(data2,
                            secondaryResource.prefs == null ? null
                                    : secondaryResource.prefs
                                            .getDisplayUnits());
                    secondaryResource.xydata.put(currentTime, data2);
                }
                combineData(data, data2);

            }
            xydata.put(currentTime, data);
            descriptor.getGraph(this).setCurrentMagnification(magnification);
            descriptor.getGraph(this).reconstruct();
            // new data loaded, reconstruct graph
            // descriptor.setGraph(null);
        }

        // problem, graph is null, redraw
        // if (graphProps.getGraph() == null) {
        // target.setNeedsRefresh(true);
        // return;
        // }

        RGB color = getCapability(ColorableCapability.class).getColor();
        // IAxis domainAxis = graphProps.getGraph().getDomainAxis();
        // IAxis rangeAxis = graphProps.getGraph().getRangeAxis();

        // Rectangle graphArea = GraphUtil.getDrawingArea(graphProps.getGraph()
        // .getWorldExtent());
        // PixelExtent rectExtent = new PixelExtent(graphArea);

        // clip because zooming will draw the points outside the graph
        // don't clip images
        // target.setupClippingPlane(paintProps.getClippingPane());

        double previousScreenX = 0.0;
        double previousScreenY = 0.0;

        if (!descriptor.getGraph(this).isReady()) {
            return;
        }

        descriptor.getGraph(this).setCurrentMagnification(magnification);
        target.setupClippingPlane(descriptor.getGraph(this).getExtent());

        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        // build the renderable
        config.setSizeScaler(magnification * ratio);
        VectorGraphicsRenderable vgr = new VectorGraphicsRenderable(
                this.descriptor, target, config);
        for (int i = 0; i < data.size(); i++) {
            XYData d = data.get(i);
            double x = ((Number) d.getX()).doubleValue();

            double y = ((Number) d.getY()).doubleValue();
            double[] screenLoc = descriptor.getGraph(this).getGridLocation(x,
                    y);
            double screenX = screenLoc[0];
            double screenY = screenLoc[1];

            IExtent graphExtent = descriptor.getGraph(this).getExtent();

            if (d instanceof XYWindImageData) {
                XYWindImageData dd = (XYWindImageData) d;
                Coordinate plotLoc = new Coordinate(screenX, screenY);
                // check if plot is out of the graph
                boolean withinBounds = true;

                if (!graphExtent
                        .contains(new double[] { plotLoc.x, plotLoc.y })) {
                    withinBounds = false;
                }

                if (withinBounds) {
                    /*
                     * To match AWIPS 1, wind barbs are supposed to go straight
                     * up near the left edge and not follow the x-axis. 120
                     * looks good.
                     */
                    plotLoc.x = 120;
                    vgr.paintBarb(plotLoc, dd.getWindSpd(),
                            Math.toRadians(dd.getWindDir()));
                    vgr.setColor(color);
                    vgr.setLineWidth(getCapability(OutlineCapability.class)
                            .getOutlineWidth());
                    vgr.setLineStyle(getCapability(OutlineCapability.class)
                            .getLineStyle());
                }
                continue;
            }

            // Connects adjacent data points with a line
            if (previousScreenX != 0.0) {
                OutlineCapability lineCap = getCapability(
                        OutlineCapability.class);
                if (combineOperation != CombineOperation.NONE) {
                    DrawableLine line = new DrawableLine();
                    line.basics.color = color;
                    line.width = lineCap.getOutlineWidth();
                    line.lineStyle = lineCap.getLineStyle();
                    line.addPoint(screenX, screenY);
                    line.addPoint(previousScreenX, previousScreenY);
                    target.drawLine(line);
                }
            }

            previousScreenY = screenY;
            previousScreenX = screenX;
        }

        // clear clipping plane first so that barbs that start within the graph
        // are able to extend outside the graph
        target.clearClippingPlane();
        vgr.paint(target);
        vgr.dispose();
    }

    public String getUnits() {
        Unit<?> xPrefUnit = prefs == null ? null : prefs.getDisplayUnits();
        if (xPrefUnit != null && xPrefUnit != AbstractUnit.ONE) {
            return xPrefUnit.toString();
        }
        Unit<?> xDataUnit = adapter.getXUnit();
        if (xDataUnit != null && xDataUnit != AbstractUnit.ONE) {
            return xDataUnit.toString();
        }
        return "?";
    }

    @Override
    public Geometry getInsetMapLocation() {
        return IInsetMapResource.factory.createPoint(resourceData.getPoint());
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        // paint a point
        Coordinate point = resourceData.getPoint();
        double[] pixels = insetMapDescriptor
                .worldToPixel(new double[] { point.x, point.y });
        target.drawPoint(pixels[0], pixels[1], 0.0,
                getCapability(ColorableCapability.class).getColor(),
                PointStyle.STAR, 1.5f);
    }

    public void addRecord(PluginDataObject pdo) {
        adapter.addRecord(pdo);
        DataTime pdoTime = pdo.getDataTime().clone();
        pdoTime.setLevelValue(null);
        dataTimes.add(pdoTime);
    }

    @Override
    public void remove(DataTime dataTime) {
        dataTimes.remove(dataTime);
        adapter.remove(dataTime);
        issueRefresh();
    }

    @Override
    public void setDescriptor(VarHeightDescriptor descriptor) {
        adapter.setHeightScale(descriptor.getHeightScale());
        xydata.clear();
        super.setDescriptor(descriptor);
    }

    @Override
    public Object getGraphKey() {
        return (prefs != null ? prefs.getDisplayUnitLabel()
                : adapter.getParameterName());
    }

    @Override
    public IGraphLabel<Double>[] getXRangeData() {
        float minY = descriptor.getHeightScale().getMinVal();
        float maxY = descriptor.getHeightScale().getMaxVal();
        double minValue = Double.MAX_VALUE;
        double maxValue = Double.MIN_VALUE;
        for (List<XYData> list : xydata.values()) {
            for (XYData data : list) {
                double y = ((Number) data.getY()).doubleValue();
                if ((y < maxY && y < minY) || (y > maxY && y > minY)) {
                    continue;
                }
                double value = ((Number) data.getX()).doubleValue();
                if (minValue > value) {
                    minValue = value;
                }
                if (maxValue < value) {
                    maxValue = value;
                }
            }
        }
        return new DoubleLabel[] { new DoubleLabel(minValue),
                new DoubleLabel(maxValue) };
    }

    @Override
    public IGraphLabel<Double>[] getYRangeData() {
        HeightScale heightScale = descriptor.getHeightScale();
        double min = heightScale.getMinVal();
        double max = heightScale.getMaxVal();
        return new DoubleLabel[] { new DoubleLabel(min), new DoubleLabel(max) };
    }

    @Override
    public void redraw() {
        // TODO Auto-generated method stub

    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Coordinate object = coord.getObject();
        double[] worldCoord = descriptor
                .pixelToWorld(new double[] { object.x, object.y });
        Coordinate c = new Coordinate(worldCoord[0], worldCoord[1]);
        c = descriptor.getGraphCoordinate(this, c, true);

        if (c != null) {
            /*
             * Sampling var height is weird, we don't want the value at the
             * mouse cursor, we want the x value of the data that is in line
             * with the cursor.
             */
            List<XYData> dataList = xydata.get(currentTime);
            if (dataList == null || dataList.isEmpty()) {
                return null;
            }
            if (dataList.get(0) instanceof XYWindImageData) {
                return formatWindSample(c, dataList);
            }

            XYData[] bounds = getLowerUpperBounds(c.y, dataList);
            if (bounds == null) {
                return null;
            }
            XYData lowerBound = bounds[0];
            XYData upperBound = bounds[1];

            double x1 = ((Number) lowerBound.getX()).doubleValue();
            double x2 = ((Number) upperBound.getX()).doubleValue();
            double y1 = ((Number) lowerBound.getY()).doubleValue();
            double y2 = ((Number) upperBound.getY()).doubleValue();
            double x = linearInterpolation(c.y, x1, x2, y1, y2);

            StringBuilder sb = new StringBuilder();
            sb.append(FORMATTER.format((x)));

            sb.append("(");
            if (prefs != null && prefs.getDisplayUnits() != null) {
                sb.append(prefs.getDisplayUnitLabel());
            } else {
                sb.append(adapter.getXUnit().toString());
            }
            sb.append(")");

            /*
             * We need to put the height/y on the first visible, non-background
             * resource we find so height shows up in sampling once and only
             * once.
             */
            if (isFirstResource()) {
                sb.append("   ");
                sb.append(formatYHeightSample(c.y));
            }

            return sb.toString();
        }
        return null;
    }

    /**
     * Gets the XYData that is closest below the y coordinate, and the XYData
     * that is closest above the y coordinate.
     * 
     * @param coordY
     * @param dataList
     * @return the lower bound, the upper bound
     */
    public XYData[] getLowerUpperBounds(double coordY, List<XYData> dataList) {
        XYData lowerBound = null;
        XYData upperBound = null;
        for (int i = 0; i < dataList.size() - 1; i++) {
            XYData data = dataList.get(i);
            double y = ((Number) data.getY()).doubleValue();
            if (coordY > y) {
                lowerBound = dataList.get(i);
                upperBound = dataList.get(i + 1);
            }
        }

        if (lowerBound == null || upperBound == null) {
            return null;
        }
        return new XYData[] { lowerBound, upperBound };
    }

    /**
     * Perform linear interpolation between two points, solving for the x value
     * at the specified y value.
     * 
     * @param y
     * @param x1
     * @param x2
     * @param y1
     * @param y2
     * @return the value of x at y
     */
    protected double linearInterpolation(double y, double x1, double x2,
            double y1, double y2) {
        return x1 + (y - y1) * (x2 - x1) / (y2 - y1);
    }

    /**
     * Formats the y height for sampling
     * 
     * @param y
     * @return
     */
    protected String formatYHeightSample(double y) {
        StringBuilder sb = new StringBuilder();
        String heightUnit = descriptor.getHeightScale().getParameterUnit()
                .toString();
        if ("hPa".equals(heightUnit)) {
            sb.append((int) y);
            sb.append("mb");
        } else {
            // e.g. km above ground level
            DecimalFormat format = new DecimalFormat("0.00");
            sb.append(format.format(y));
            sb.append(heightUnit);
        }

        return sb.toString();
    }

    /**
     * Formats wind sampling
     * 
     * @param c
     * @param dataList
     * @return
     */
    protected String formatWindSample(Coordinate c, List<XYData> dataList) {
        XYData[] lowerUpper = getLowerUpperBounds(c.y, dataList);
        XYWindImageData lowerBound = (XYWindImageData) lowerUpper[0];
        XYWindImageData upperBound = (XYWindImageData) lowerUpper[1];
        double lowerDist = Math
                .abs(c.y - ((Number) lowerBound.getY()).doubleValue());
        double upperDist = Math
                .abs(c.y - ((Number) upperBound.getY()).doubleValue());
        /*
         * We're not going to interpolate wind, we're going to grab the closest
         * one. This is an arbitrary decision, I don't know what AWIPS 1 did.
         */
        XYWindImageData chosen = null;
        if (lowerDist < upperDist) {
            chosen = lowerBound;
        } else {
            chosen = upperBound;
        }
        StringBuilder sb = new StringBuilder();
        int windDir = (int) chosen.getWindDir();
        if (windDir < 0) {
            windDir += 360;
        }
        sb.append(windDir);
        sb.append("deg ");
        DecimalFormat format = new DecimalFormat("0.#");
        sb.append(format.format(chosen.getWindSpd()));
        sb.append(getUnits());

        // add height if first resource
        if (isFirstResource()) {
            sb.append("   ");
            sb.append(formatYHeightSample(c.y));
        }

        return sb.toString();
    }

    /**
     * Checks if this is the first visible, non-system, non-background resource.
     * Used by sampling so the height is shown in the sample text but only on
     * one resource's sample text.
     * 
     * @return
     */
    protected boolean isFirstResource() {
        for (ResourcePair pair : descriptor.getResourceList()) {
            AbstractVizResource<?, ?> rsc = pair.getResource();
            if (rsc != null && !rsc.getProperties().isSystemResource()
                    && rsc.getProperties().isVisible()
                    && !rsc.getProperties().isMapLayer()) {
                if (this == rsc) {
                    return true;
                }
                break;
            }
        }
        return false;
    }

    @Override
    public void paintHodograph(IGraphicsTarget target,
            PaintProperties paintProps, HodographDescriptor hodoDescriptor)
            throws VizException {
        DataTime time = descriptor.getTimeForResource(this);
        if (time == null) {
            return;
        }
        List<XYData> dataList = xydata.get(time);
        if (dataList == null || dataList.isEmpty()) {
            return;
        }
        DrawableLine line = new DrawableLine();
        line.basics.color = getCapability(ColorableCapability.class).getColor();
        line.width = 2.0f;
        for (XYData data : dataList) {
            if (data instanceof XYWindImageData) {
                XYWindImageData windData = (XYWindImageData) data;
                double dir = windData.getWindDir();
                double speed = windData.getWindSpd();
                double[] screen = hodoDescriptor
                        .polarToPixel(new double[] { speed, dir });
                line.addPoint(screen[0], screen[1]);
            }
        }
        if (!line.points.isEmpty()) {
            double[] screen = hodoDescriptor
                    .polarToPixel(new double[] { 0, 0 });
            line.addPoint(screen[0], screen[1]);

            target.drawLine(line);
        }
    }

    public boolean isWind() {
        return adapter.isWind();
    }
}
