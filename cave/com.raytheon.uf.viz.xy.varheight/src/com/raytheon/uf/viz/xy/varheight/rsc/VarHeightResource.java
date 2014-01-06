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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.graph.GraphProperties;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.hodo.HodographDescriptor;
import com.raytheon.uf.viz.xy.hodo.IHodographResource;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.map.rsc.PointRenderable;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightDescriptor;
import com.raytheon.viz.core.contours.util.VectorGraphicsRenderable;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.style.graph.GraphPreferences;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * General var height resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2009            mschenke     Initial creation
 * Feb 10, 2011 8344       bkowal       enabled the magnification capability.
 * Dec 19, 2013 DR 16795   D. Friedman  Transform pixel coordinate in inspect
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VarHeightResource extends
        AbstractVizResource<VarHeightResourceData, VarHeightDescriptor>
        implements IInsetMapResource, IGraphableResource<Double, Double>,
        IHodographResource {

    private static final double DEGREE_TO_RADIAN = Math.PI / 180.0;

    protected AbstractVarHeightAdapter<?> adapter;

    protected Map<DataTime, List<XYData>> xydata = new HashMap<DataTime, List<XYData>>();

    protected GraphPreferences prefs;

    protected int displayedIndex = 0;

    protected PointRenderable pointR = null;

    protected VarHeightResource secondaryResource;

    protected CombineOperation combineOperation;

    protected DataTime currentTime = null;

    private double imageSize = XYWindImageData.IMAGE_SIZE;

    protected VarHeightResource(VarHeightResourceData resourceData,
            LoadProperties loadProperties, AbstractVarHeightAdapter<?> adapter)
            throws VizException {
        super(resourceData, loadProperties);
        this.adapter = adapter;
        dataTimes = new ArrayList<DataTime>();
        ICombinedResourceData combinedResourceData = null;

        try {
            combinedResourceData = getResourceData();
        } catch (ClassCastException e) {
            // do nothing
        }

        if (combinedResourceData != null) {
            this.secondaryResource = (VarHeightResource) combinedResourceData
                    .getSecondaryResource();
            this.combineOperation = combinedResourceData.getCombineOperation();
        }

        prefs = GraphPrefsFactory.buildPreferences(resourceData.getParameter(),
                null);

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
            completeName += getUnits().toString();
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

    /**
     * 
     */
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

        DataTime currentTime = graphProps.getDataTime();
        if (currentTime == null) {
            return;
        }

        // get the data
        List<XYData> data = xydata.get(graphProps.getDataTime());
        if (data == null) {
            data = adapter.loadData(currentTime);
            adapter.sortData(data);
            adapter.convertData(data,
                    prefs == null ? null : prefs.getDisplayUnits());
            if (secondaryResource != null
                    && combineOperation.equals(CombineOperation.DIFFERENCE)) {
                List<XYData> data2 = secondaryResource.xydata.get(currentTime);
                if (data2 == null) {
                    data2 = secondaryResource.adapter.loadData(currentTime);
                    secondaryResource.adapter.sortData(data2);
                    secondaryResource.adapter
                            .convertData(
                                    data2,
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

        // build the renderable
        VectorGraphicsRenderable vgr = new VectorGraphicsRenderable(
                this.descriptor, target, this.imageSize, 1.0f);
        for (int i = 0; i < data.size(); i++) {
            XYData d = data.get(i);
            double x = ((Number) d.getX()).doubleValue();

            double y = ((Number) d.getY()).doubleValue();
            double[] screenLoc = descriptor.getGraph(this)
                    .getGridLocation(x, y);
            double screenX = screenLoc[0];
            double screenY = screenLoc[1];

            double ratio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;

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
                    double adjSize = (this.imageSize * magnification) * ratio;
                    vgr.paintBarb(plotLoc, adjSize, dd.getWindSpd(),
                            dd.getWindDir() * DEGREE_TO_RADIAN);
                    vgr.setColor(color);
                    vgr.setLineWidth(getCapability(OutlineCapability.class)
                            .getOutlineWidth());
                    vgr.setLineStyle(getCapability(OutlineCapability.class)
                            .getLineStyle());
                }
                continue;
            } else if (d instanceof XYImageData) {
                // screenX = domainAxis.valueToCoordinate(1);
                XYImageData imageData = (XYImageData) d;
                imageData.setColor(color);
                imageData.setTarget(target);
                PaintProperties imagePaintProperties = new PaintProperties(
                        paintProps);
                imagePaintProperties.setAlpha(1.0f);

                int[] dims = imageData.getDefaultSize();
                double adjDims[] = new double[2];
                adjDims[0] = dims[0] * 0.5 * ratio * magnification;
                adjDims[1] = dims[1] * 0.5 * ratio * magnification;

                Coordinate ul = new Coordinate(screenX - adjDims[0], screenY
                        - adjDims[1]);
                Coordinate ur = new Coordinate(screenX + adjDims[0], screenY
                        - adjDims[1]);
                Coordinate lr = new Coordinate(screenX + adjDims[0], screenY
                        + adjDims[1]);
                Coordinate ll = new Coordinate(screenX - adjDims[0], screenY
                        + adjDims[1]);
                PixelCoverage coverage = new PixelCoverage(ul, ur, lr, ll);

                target.drawRaster(imageData.getImage(), coverage,
                        imagePaintProperties);
                continue;
            }

            // Connects adjacent data points with a line
            if (previousScreenX != 0.0) {
                OutlineCapability lineCap = getCapability(OutlineCapability.class);
                if (combineOperation != CombineOperation.NONE) {
                    target.drawLine(screenX, screenY, 0.0, previousScreenX,
                            previousScreenY, 0.0, color,
                            lineCap.getOutlineWidth(), lineCap.getLineStyle());
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
        if (xPrefUnit != null && xPrefUnit != Unit.ONE) {
            return xPrefUnit.toString();
        }
        Unit<?> xDataUnit = adapter.getXUnit();
        if (xDataUnit != null && xDataUnit != Unit.ONE) {
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
        if (pointR == null) {
            pointR = new PointRenderable(resourceData.getPoint(),
                    getCapability(ColorableCapability.class).getColor(),
                    insetMapDescriptor);
        } else {
            pointR.setColor(getCapability(ColorableCapability.class).getColor());
        }
        pointR.paint(target, paintProps);
    }

    public void addRecord(PluginDataObject pdo) {
        adapter.addRecord(pdo);
        DataTime pdoTime = pdo.getDataTime().clone();
        pdoTime.setLevelValue(null);
        if (!dataTimes.contains(pdoTime)) {
            dataTimes.add(pdoTime);
            Collections.sort(dataTimes);
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        dataTimes.remove(dataTime);
        adapter.remove(dataTime);
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(VarHeightDescriptor descriptor) {
        adapter.setHeightScale(descriptor.getHeightScale());
        xydata.clear();
        super.setDescriptor(descriptor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.map.rsc.IGraphableResource#getGraphKey()
     */
    @Override
    public Object getGraphKey() {
        return (prefs != null ? prefs.getDisplayUnitLabel() : adapter
                .getParameterName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.map.rsc.IGraphableResource#getXRangeData()
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.map.rsc.IGraphableResource#getYRangeData()
     */
    @Override
    public IGraphLabel<Double>[] getYRangeData() {
        HeightScale heightScale = descriptor.getHeightScale();
        double min = heightScale.getMinVal();
        double max = heightScale.getMaxVal();
        return new DoubleLabel[] { new DoubleLabel(min), new DoubleLabel(max) };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.map.rsc.IGraphableResource#redraw()
     */
    @Override
    public void redraw() {
        // TODO Auto-generated method stub

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
        Coordinate object = coord.getObject();
        double[] worldCoord = descriptor.pixelToWorld(
                new double[] { object.x, object.y });
        Coordinate c = new Coordinate(worldCoord[0], worldCoord[1]);

        c = descriptor.getGraphCoordiante(this, c);
        if (c != null) {
            return c.x + ", " + c.y;
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.varheight.hodo.IHodographResource#paintHodograph
     * (com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.xy.varheight.hodo.HodographDescriptor)
     */
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
                double[] screen = hodoDescriptor.polarToPixel(new double[] {
                        speed, dir });
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

}
