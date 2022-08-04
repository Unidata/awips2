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

package com.raytheon.uf.viz.xy.timeseries.rsc;

import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.graph.GraphPreferences;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.labeling.DataTimeLabel;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

import tec.uom.se.format.SimpleUnitFormat;

/**
 * Represents a series of associated data, e.g. time and temperature, height and
 * pressure, distance and wind speed
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2007            njensen     Initial creation
 * Feb 10, 2011 8244       bkowal      enabled the magnification
 *                                     capability.
 * Feb 14, 2011 8244       bkowal      enabled magnification for wind barbs.
 * Dec 19, 2013 DR 16795   D. Friedman  Transform pixel coordinate in inspect
 * Jun 18, 2014 3242       njensen     Added ensembleId to legend
 * Aug 15, 2014 3535       njensen     Bigger inset map point
 * Nov 19, 2014 5056       jing        added getAdapter method, and
 *                                     changed getName to add level
 * Feb 08, 2018 6825       njensen     Enable sampling of all graphs regardless
 *                                     of cursor position
 * Oct 29, 2018 7529       bsteffen    Ensure descriptor is set on
 *                                     secondaryResource.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class TimeSeriesResource extends
        AbstractVizResource<TimeSeriesResourceData, TimeSeriesDescriptor>
        implements IGraphableResource<DataTime, Double>, IInsetMapResource {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesResource.class);

    private final AbstractTimeSeriesAdapter<?> adapter;

    private final SimpleDateFormat timeSampleFormat = new SimpleDateFormat(
            "HH:mm'Z' EEE");

    /** The graph preferences */
    protected GraphPreferences prefs;

    /** The data in xy form */
    protected volatile XYDataList data = new XYDataList();

    protected String units;

    /** Denotes whether shapes are plotted at each data point * */
    protected boolean shapesVisible = false;

    /** The graph to draw to */
    protected IGraph graph = null;

    protected TimeSeriesResource secondaryResource;

    protected CombineOperation combineOperation;

    protected Set<DataTime> dataTimes = new TreeSet<>();

    private Job dataRequestJob = new Job("Requesting Time Series Data") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            synchronized (adapter) {
                XYDataList oldData = data;
                try {
                    data = adapter.loadData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                sortData();

                dataTimes = new TreeSet<>();
                for (XYData d : data.getData()) {
                    dataTimes.add((DataTime) d.getX());
                }
                if (prefs != null && prefs.getDisplayUnits() != null) {
                    // convert to display Units
                    Unit<?> currentUnit = adapter.getDataUnit();
                    if (currentUnit.isCompatible(prefs.getDisplayUnits())) {
                        UnitConverter conv = UnitConv.getConverterToUnchecked(
                                currentUnit, prefs.getDisplayUnits());
                        ListIterator<XYData> it = data.getData().listIterator();
                        while (it.hasNext()) {
                            XYData d = it.next();
                            if (d instanceof XYWindImageData) {
                                XYWindImageData wind = (XYWindImageData) d;
                                double converted = conv
                                        .convert(wind.getWindSpd());
                                it.remove();
                                if (wind.getImage() != null) {
                                    wind.getImage().dispose();
                                }
                                it.add(new XYWindImageData(wind.getX(),
                                        wind.getY(), converted,
                                        wind.getWindDir()));
                            } else {
                                double converted = conv.convert(
                                        ((Number) d.getY()).doubleValue());
                                d.setY(converted);
                            }
                        }
                        units = prefs.getDisplayUnitLabel();
                    } else {
                        units = SimpleUnitFormat
                                .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                .format(adapter.getDataUnit());
                        statusHandler.handle(Priority.VERBOSE,
                                currentUnit + " is not compatible with "
                                        + prefs.getDisplayUnits()
                                        + ". No conversion will be preformed.");
                    }
                } else {
                    units = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(
                            adapter.getDataUnit());
                }
                if (graph != null) {
                    Double magnification = getCapability(
                            MagnificationCapability.class).getMagnification();
                    graph.setCurrentMagnification(magnification);
                    graph.reconstruct();
                }
                if (oldData != null) {
                    oldData.dispose();
                }
                try {
                    // This exists for the case when we have records, then we
                    // remove some times and then we want to add them back
                    // in before this job has removed them from the dataTimes
                    // set. The time matcher doesn't add them back in since they
                    // haven't been removed.This happens when swapping quickly,
                    // the frame count drops to 8 and if you swap it back fast
                    // enough the new times aren't added back in.
                    PluginDataObject[] newPDOS = resourceData
                            .getLatestPluginDataObjects(descriptor
                                    .getTimeMatchingMap().get(this),
                                    getDataTimes());
                    for (PluginDataObject pdo : newPDOS) {
                        addRecord(pdo);
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            issueRefresh();
            return Status.OK_STATUS;
        }

    };

    public TimeSeriesResource(TimeSeriesResourceData data,
            LoadProperties props, final AbstractTimeSeriesAdapter<?> adapter) {
        super(data, props);
        this.adapter = adapter;
        try {
            ICombinedResourceData combinedResourceData = getResourceData();
            this.secondaryResource = (TimeSeriesResource) combinedResourceData
                    .getSecondaryResource();
            this.combineOperation = combinedResourceData.getCombineOperation();
        } catch (ClassCastException e) {
            statusHandler.error("Failed to perform combination", e);
        }
        data.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] objects = (PluginDataObject[]) object;
                    for (PluginDataObject pdo : objects) {
                        addRecord(pdo);
                    }
                }
                issueRefresh();
            }

        });
    }

    public AbstractTimeSeriesAdapter<?> getAdapter() {
        return adapter;
    }

    @Override
    protected void disposeInternal() {
        if (secondaryResource != null) {
            secondaryResource.dispose();
        }
        if (data != null) {
            data.dispose();
        }
        this.data = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        if (data == null) {
            return;
        }

        if (secondaryResource != null) {
            secondaryResource.paint(target, paintProps);
        }

        if (combineOperation == CombineOperation.NONE) {
            return;
        }

        if (graph == null) {
            graph = descriptor.getGraph(this);
        }
        // Wait for graph to initialize before plotting to it, TODO: do better
        if (!graph.isReady()) {
            return;
        }

        graph.setCurrentMagnification(magnification);

        target.setupClippingPlane(graph.getExtent());
        double[] prevScreen = null;
        for (int i = 0; i < data.getData().size(); i++) {

            XYData point = data.getData().get(i);

            double[] screen = getScreenPosition(point.getX(), point.getY());

            RGB color = getCapability(ColorableCapability.class).getColor();
            // Draws shapes for each data point

            // draw wind Data
            if (point instanceof XYImageData) {
                // Draw all images in a striaight line. Move the line to be able
                // to accomodate multiple resources.
                List<AbstractVizResource<?, ?>> tsrs = descriptor
                        .getResourceList().getResourcesByType(
                                TimeSeriesResource.class);
                int index = tsrs.indexOf(this);
                IExtent extent = graph.getExtent();
                screen[1] = extent.getMinY() + (index + 1)
                        * (extent.getHeight() / (tsrs.size() + 1));
                XYImageData imageData = (XYImageData) point;
                imageData.setColor(color);
                imageData.setTarget(target);
                PaintProperties imagePaintProperties = new PaintProperties(
                        paintProps);
                imagePaintProperties.setAlpha(1.0f);
                double ratio = paintProps.getView().getExtent().getWidth()
                        / paintProps.getCanvasBounds().width;

                int[] dims = imageData.getDefaultSize();
                double adjDims[] = new double[2];
                adjDims[0] = (dims[0] * 0.5 * ratio) * magnification;
                adjDims[1] = (dims[1] * 0.5 * ratio) * magnification;

                Coordinate ul = new Coordinate(screen[0] - adjDims[0],
                        screen[1] - adjDims[1]);
                Coordinate ur = new Coordinate(screen[0] + adjDims[0],
                        screen[1] - adjDims[1]);
                Coordinate lr = new Coordinate(screen[0] + adjDims[0],
                        screen[1] + adjDims[1]);
                Coordinate ll = new Coordinate(screen[0] - adjDims[0],
                        screen[1] + adjDims[1]);
                PixelCoverage coverage = new PixelCoverage(ul, ur, lr, ll);

                target.drawRaster(imageData.getImage(), coverage,
                        imagePaintProperties);
                continue;
            }

            if (shapesVisible) {
                target.drawRect(new PixelExtent(screen[0] - 3, screen[0] + 3,
                        screen[1] - 3, screen[1] + 3), color, 1.0f, 1.0);
            }

            // Connects adjacent data points with a line
            if (prevScreen != null) {
                OutlineCapability lineCap = getCapability(OutlineCapability.class);
                DrawableLine line = new DrawableLine();
                line.addPoint(screen[0], screen[1]);
                line.addPoint(prevScreen[0], prevScreen[1]);
                line.basics.color = color;
                line.width = lineCap.getOutlineWidth();
                line.lineStyle = lineCap.getLineStyle();
                target.drawLine(line);
            }

            prevScreen = screen;
        }
        target.clearClippingPlane();
    }

    /**
     * @param i
     * @return
     */
    private double[] getScreenPosition(Object x, Object y) {
        double valY = ((Number) y).doubleValue();
        double valX = ((DataTime) x).getValidTime().getTimeInMillis();
        return graph.getGridLocation(valX, valY);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        if (secondaryResource != null) {
            secondaryResource.descriptor = this.descriptor;
            secondaryResource.init(target);
        }

        // Load the data from the records
        DisplayType dType = getCapability(DisplayTypeCapability.class)
                .getDisplayType();
        adapter.setColor(this.getCapability(ColorableCapability.class)
                .getColor());
        adapter.setDisplayType(dType);
        dataRequestJob.schedule();

        // Load the Graph Preferences
        if (prefs == null) {
            try {
                prefs = GraphPrefsFactory.buildPreferences(
                        resourceData.getYParameter().code, adapter.getLevel());
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
        }

        if (prefs != null && prefs.getDisplayUnits() != null) {
            units = prefs.getDisplayUnitLabel();
        }

        if (secondaryResource != null
                && combineOperation != CombineOperation.NONE) {
            combineData();
        }

    }

    /**
     * 
     */
    private void sortData() {
        Collections.sort(data.getData(), new Comparator<XYData>() {

            @Override
            public int compare(XYData xy1, XYData xy2) {
                DataTime t1 = (DataTime) xy1.getX();
                DataTime t2 = (DataTime) xy2.getX();
                return t1.compareTo(t2);
            }

        });

    }

    /**
     * 
     */

    private void combineData() {
        if (secondaryResource.prefs != null
                && secondaryResource.prefs.getDisplayUnits() != null
                && prefs != null && prefs.getDisplayUnits() != null) {
            UnitConverter secondUnitConverter = UnitConv
                    .getConverterToUnchecked(
                            secondaryResource.prefs.getDisplayUnits(),
                            prefs.getDisplayUnits());
            for (int i = 0; i < data.getData().size(); i++) {
                double n1 = ((Number) data.getData().get(i).getY())
                        .doubleValue();
                double n2 = secondUnitConverter
                        .convert(((Number) secondaryResource.data.getData()
                                .get(i).getY()).doubleValue());

                if (combineOperation == CombineOperation.DIFFERENCE) {
                    data.getData().get(i).setY(new Double(n1 - n2));
                }
            }
        }
    }

    @Override
    public String getName() {
        NumberFormat nf = NumberFormat.getInstance();
        nf.setMaximumFractionDigits(1);
        double x = resourceData.getCoordinate().x;
        double y = resourceData.getCoordinate().y;
        String lon = nf.format(Math.abs(x));
        String lat = nf.format(Math.abs(y));
        String stnID = "";
        String source = resourceData.getSource();

        if (resourceData.getMetadataMap().get("location.stationId") != null) {
            stnID = resourceData.getMetadataMap().get("location.stationId")
                    .getConstraintValue();
        }

        String levelKey = resourceData.getLevelKey();
        // String levelUnit = levelKey.replaceAll("[^a-zA-Z]", "");
        // boolean isHeight = levelUnit.equalsIgnoreCase("mb")
        // || levelUnit.equalsIgnoreCase("agl")
        // || levelUnit.contains("Agl");

        // Make legend for point data
        // add level in x legend only if levelKey is not empty
        StringBuilder sb = new StringBuilder(String.format(
                "%s %s pt%s %s%s %s%s", source, levelKey != null ? levelKey
                        : "", resourceData.getPointLetter(), lat, y >= 0 ? "N"
                        : "S", lon, x >= 0 ? "E" : "W"));

        if (stnID != null) {
            sb.append(" ").append(stnID);
        }

        // if (!isHeight) {
        // sb.append(" ").append(resourceData.getLevelKey());
        // }

        sb.append(String.format(" %s %s %s", adapter.getParameterName(),
                "TSer",
                units != null && !units.isEmpty() ? "("
                        + units + ")" : ""));

        if (adapter.getEnsembleId() != null) {
            sb.append(" Perturbation ");
            sb.append(adapter.getEnsembleId());
        }

        if (secondaryResource != null) {
            return ICombinedResourceData.CombineUtil.getName(sb.toString(),
                    secondaryResource.getName(), combineOperation);
        } else {
            return sb.toString();
        }
    }

    public String getUnits() {
        return units;
    }

    /**
     * @return the data
     */
    public XYDataList getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(XYDataList data) {
        this.data = data;
    }

    @Override
    public Object getGraphKey() {
        return (units != null ? units : adapter.getParameterName());
    }

    @Override
    public void redraw() {
        // Only used if wireframe shapes are constructed
    }

    public double getMinDataValue() {
        double min = Double.POSITIVE_INFINITY;
        if (data != null) {
            for (XYData d : data.getData()) {
                if (d.getY() instanceof Number) {
                    double y = ((Number) d.getY()).doubleValue();
                    min = Math.min(min, y);
                }
            }
        }
        return min;
    }

    public double getMaxDataValue() {
        double max = Double.NEGATIVE_INFINITY;
        if (data != null) {
            for (XYData d : data.getData()) {
                if (d.getY() instanceof Number) {
                    double y = ((Number) d.getY()).doubleValue();
                    max = Math.max(max, y);
                }
            }
        }
        return max;
    }

    @Override
    public IGraphLabel<DataTime>[] getXRangeData() {
        DataTimeLabel[] labels = new DataTimeLabel[dataTimes.size()];
        int i = 0;
        for (DataTime time : dataTimes) {
            labels[i] = new DataTimeLabel(time);
            i++;
        }
        return labels;
    }

    @Override
    public IGraphLabel<Double>[] getYRangeData() {
        double min = getMinDataValue();
        double max = getMaxDataValue();
        return new DoubleLabel[] { new DoubleLabel(min), new DoubleLabel(max) };
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        StringBuilder inspect = new StringBuilder();
        double[] worldCoord = descriptor.pixelToWorld(new double[] {
                coord.getObject().x, coord.getObject().y });
        /*
         * passing false here makes all graphs sample at the same time as the
         * extent is then ignored
         */
        Coordinate c = descriptor.getGraphCoordinate(this, new Coordinate(
                worldCoord[0], worldCoord[1]), false);
        if (c != null && data != null) {
            double[] vals = data.inspectXY(c);
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMaximumFractionDigits(2);
            inspect.append(nf.format(vals[1]));
            if (units != null && !units.isEmpty()) {
                inspect.append("(").append(units).append(")");
            }
            inspect.append("   ");

            /*
             * We need to put the time on the first non-background resource we
             * find so time shows up in sampling once and only once.
             */
            for (ResourcePair pair : descriptor.getResourceList()) {
                AbstractVizResource<?, ?> rsc = pair.getResource();
                if (rsc != null && !rsc.getProperties().isSystemResource()
                        && rsc.getProperties().isVisible()
                        && !rsc.getProperties().isMapLayer()) {
                    if (this == rsc) {
                        Calendar cal = Calendar
                                .getInstance(TimeZone.getTimeZone("GMT"));
                        cal.setTimeInMillis((long) c.x);
                        timeSampleFormat.setCalendar(cal);
                        inspect.append(timeSampleFormat.format(cal.getTime()));
                    }
                    break;
                }
            }

        }
        return inspect.length() == 0 ? null : inspect.toString();
    }

    @Override
    public Geometry getInsetMapLocation() {
        return IInsetMapResource.factory.createPoint(resourceData
                .getCoordinate());
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        // paint a point
        Coordinate point = resourceData.getCoordinate();
        double[] pixels = insetMapDescriptor.worldToPixel(new double[] {
                point.x, point.y });
        target.drawPoint(pixels[0], pixels[1], 0.0,
                getCapability(ColorableCapability.class).getColor(),
                PointStyle.STAR, 1.5f);
    }

    public String[] getTitles() {
        // The more graphs the less title to show...
        int graphs = descriptor.getGraphResource().visibleGraphCount();
        String name = "";
        if (graphs < 2) {
            // If only one graph, show the coordinate lat/lon
            Coordinate c = resourceData.getCoordinate();
            String ns = c.y >= 0 ? "N" : "S";
            String ew = c.x >= 0 ? "E" : "W";
            String latlon = String.format("%d%s%d%s",
                    Math.round(Math.abs(c.y)), ns, Math.round(Math.abs(c.x)),
                    ew);
            name += latlon + "  ";
        }
        if (graphs < 3) {
            name += resourceData.getSource() + "  ";
        }
        SingleLevel level = adapter.getLevel();
        if (!"SURFACE".equals(level.getTypeString())) {
            name += (int) level.getValue() + level.getTypeString() + "  ";
        }

        name += adapter.getParameterName();
        if (units != null && !units.isEmpty()) {
            name += "(" + units + ")";
        }

        if (secondaryResource != null) {
            name = ICombinedResourceData.CombineUtil.getName(name,
                    secondaryResource.getTitles()[1], combineOperation);
        }

        return new String[] { null, name };
    }

    public void addRecord(PluginDataObject record) {
        synchronized (adapter) {
            if (!adapter.hasRecord(record)) {
                adapter.addRecord(record);
                // 100 ms gives us time if we are adding multiple records.
                dataRequestJob.schedule(100);
            }
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        if (dataTimes.contains(dataTime)) {
            synchronized (adapter) {
                adapter.remove(dataTime);
            }
            // 100 ms gives us time if we are removing multiple records.
            dataRequestJob.schedule(100);
        }
    }

    @Override
    public DataTime[] getDataTimes() {
        return dataTimes.toArray(new DataTime[0]);
    }
    
    @Override
    public void setDescriptor(TimeSeriesDescriptor descriptor) {
        if (secondaryResource != null) {
            secondaryResource.setDescriptor(descriptor);
        }
        super.setDescriptor(descriptor);
    }
}
