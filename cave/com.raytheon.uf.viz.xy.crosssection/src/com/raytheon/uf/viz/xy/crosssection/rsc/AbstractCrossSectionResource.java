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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.LineString;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.AbstractStylePreferences;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionRotation;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.map.GeoUtil;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * 
 * Abstract resource for cross sections
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Dec 04, 2007           njensen   Initial creation
 * Feb 17, 2009           njensen   Refactored to new rsc architecture
 * Feb 27, 2012  14490    kshresth  Fixed cross sections not loading as images
 * Feb 19, 2014  2819     randerso  Removed unnecessary .clone() call
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Feb 18, 2018  7231     njensen   Made statusHandler protected
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 * 
 * </pre>
 * 
 * @author njensen
 */
public abstract class AbstractCrossSectionResource extends
        AbstractVizResource<CrossSectionResourceData, CrossSectionDescriptor>
        implements IInsetMapResource, IGraphableResource<Double, Double> {

    protected static final int GRID_SIZE = 100;

    protected static final DecimalFormat sampleFormat = new DecimalFormat(
            "0.00");

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    protected GridGeometry2D geometry = null;

    protected AbstractCrossSectionAdapter<?> adapter;

    protected Map<DataTime, List<float[]>> sliceMap = new HashMap<>(64);

    protected AbstractStylePreferences prefs;

    protected Map<LineString, IWireframeShape> lines = new HashMap<>();

    protected DataRetrievalJob dataRetrievalJob = new DataRetrievalJob(
            "Loading Cross Section Data");

    public AbstractCrossSectionResource(CrossSectionResourceData data,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(data, props, false);
        this.adapter = adapter;

        data.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdos = (PluginDataObject[]) object;
                    synchronized (AbstractCrossSectionResource.this) {
                        for (PluginDataObject pdo : pdos) {
                            addRecord(pdo);
                        }
                    }
                    // Schedule retrieval job as addRecord adds to times
                    dataRetrievalJob.schedule();
                }
                issueRefresh();
            }

        });
    }

    @Override
    protected void disposeInternal() {
        if (dataRetrievalJob != null) {
            dataRetrievalJob.times.clear();
            dataRetrievalJob.run = false;
        }
        for (IWireframeShape shape : lines.values()) {
            shape.dispose();
        }
        lines.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (prefs == null) {
            try {
                prefs = GraphPrefsFactory
                        .buildPreferences(resourceData.getParameter(), null);
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
        }
        int numTimes = dataTimes.size();
        if (numTimes > 0) {
            DataTime lastTime = dataTimes.last();
            sliceMap.put(lastTime, null);
            dataRetrievalJob.times.add(lastTime);
            for (DataTime time : dataTimes.headSet(lastTime)) {
                sliceMap.put(time, null);
                dataRetrievalJob.times.add(time);
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (!descriptor.getGraph(this).isReady()) {
            return;
        }
        if (geometry == null) {
            IExtent extent = descriptor.getGraph(this).getExtent().clone();
            // To be numerically accurate the grid geometry should be 1 grid
            // cell larger than the graph
            extent.scale(1.0 + (1.0 / GRID_SIZE));
            GeneralEnvelope env = new GeneralEnvelope(
                    new double[] { extent.getMinX(), extent.getMinY() },
                    new double[] { extent.getMaxX(), extent.getMaxY() });
            env.setCoordinateReferenceSystem(descriptor.getGridGeometry()
                    .getCoordinateReferenceSystem());
            GeneralGridEnvelope range = new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { GRID_SIZE, GRID_SIZE },
                    false);
            geometry = new GridGeometry2D(range, env);
            dataRetrievalJob.schedule();
        }
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        descriptor.getGraph(this).setCurrentMagnification(magnification);
    }

    @Override
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        adapter.setDescriptor(descriptor);
        this.descriptor = descriptor;
        Set<DataTime> times = new HashSet<>();
        for (DataTime time : dataTimes) {
            for (int i = 0; i < descriptor.getLines().size(); i++) {
                time = time.clone();
                time.setLevelValue((double) i);
                times.add(time);
                sliceMap.put(time, null);
                dataRetrievalJob.times.add(time);
            }
        }
        dataTimes.retainAll(times);
        dataTimes.addAll(times);
        dataRetrievalJob.schedule();
    }

    protected void loadSlice(DataTime time) throws VizException {
        List<float[]> floatData = sliceMap.get(time);
        if (floatData == null) {
            CrossSectionGraph graph = ((CrossSectionGraph) descriptor
                    .getGraph(this));
            floatData = adapter.loadData(time, graph, geometry);
            if (floatData == null) {
                sliceMap.remove(time);
                resourceData.blackListTime(time);
                descriptor.getTimeMatcher().redoTimeMatching(this);
                descriptor.getTimeMatcher().redoTimeMatching(descriptor);
                issueRefresh();
                return;
            }
            Coordinate[] lineData = GeoUtil.splitLine(GRID_SIZE,
                    descriptor.getLine(time).getCoordinates());
            int lineLengthInMeters = (int) graph
                    .getVirtualLocation(graph.getExtent().getMaxX(), 0)[0];
            floatData = CrossSectionRotation.rotateVector(
                    resourceData.getParameter(), Arrays.asList(lineData),
                    floatData, lineLengthInMeters, descriptor.getHeightScale(),
                    adapter.getDataCoordinateReferenceSystem());
            if (adapter.getUnit().isCompatible(getUnit())) {
                UnitConverter converter = UnitConv
                        .getConverterToUnchecked(adapter.getUnit(), getUnit());
                for (float[] floatArr : floatData) {
                    for (int i = 0; i < floatArr.length; i++) {
                        if (floatArr[i] > -9998) {
                            floatArr[i] = (float) converter
                                    .convert(floatArr[i]);
                        }
                    }
                }
            }
            double[] topoData = graph.getTopoData(descriptor.getLine(time),
                    GRID_SIZE);
            // filter below topo
            for (int i = 0; i < GRID_SIZE; i++) {
                double height = (GRID_SIZE
                        * (graph.getExtent().getMaxY() - topoData[i]))
                        / graph.getExtent().getHeight();
                for (int j = 0; j < height; j++) {
                    for (float[] floatArr : floatData) {
                        floatArr[(j * GRID_SIZE) + i] = -999_999;
                    }
                }
            }
            sliceMap.put(time, floatData);
        }
    }

    public SingleLevel[] getLevels() {
        HeightScale scale = descriptor.getHeightScale();
        float minVal = scale.getMinVal();
        float maxVal = scale.getMaxVal();
        if (minVal > maxVal) {
            float tmp = minVal;
            minVal = maxVal;
            maxVal = tmp;
        }
        int inc = scale.getIncrement();
        SingleLevel[] levels = new SingleLevel[(int) ((maxVal - minVal) / inc)
                + 1];
        int i = 0;
        for (float val = minVal; val <= maxVal; val += inc, ++i) {
            levels[i] = new SingleLevel(scale.getHeightType());
            levels[i].setValue(val);
        }
        return levels;
    }

    public void addRecord(PluginDataObject pdo) {

        // Refresh if any new records were added
        DataTime pdoTime = pdo.getDataTime().clone();
        pdoTime.setLevelValue(null);
        if (resourceData.getBinOffset() != null) {
            pdoTime = resourceData.getBinOffset().getNormalizedTime(pdoTime);
        }

        adapter.addRecord(pdo);
        if (descriptor != null) {
            for (int i = 0; i < descriptor.getLines().size(); i++) {
                pdoTime = pdoTime.clone();
                pdoTime.setLevelValue((double) i);
                if (!dataTimes.add(pdoTime)) {
                    // We are adding a record for a time we have, dispose of
                    // existing time data and add to retrieval job
                    disposeTimeData(pdoTime);
                    dataRetrievalJob.times.add(pdoTime);
                }
            }
        } else {
            dataTimes.add(pdoTime);
        }
    }

    @Override
    public final void remove(DataTime dataTime) {
        synchronized (this) {
            if (this.sliceMap != null) {
                sliceMap.remove(dataTime);
            }
            adapter.remove(dataTime);
            dataTimes.remove(dataTime);
            dataRetrievalJob.times.remove(dataTime);

            boolean someLeft = false;
            for (DataTime time : dataTimes) {
                if (dataTime.equals(time, true)) {
                    someLeft = true;
                    break;
                }
            }

            // if there are none left with the same data time while ignoring
            // level values then have the adapter remove all items which match
            // the data time while ignoring the level value
            if (!someLeft) {
                DataTime tmp = dataTime.clone();
                tmp.setLevelValue(null);
                adapter.remove(tmp);
            }
        }
        disposeTimeData(dataTime);
        issueRefresh();
    }

    /**
     * Dispose resource data for this time
     * 
     * @param dataTime
     */
    protected void disposeTimeData(DataTime dataTime) {
        sliceMap.remove(dataTime);
    }

    @Override
    public Geometry getInsetMapLocation() {
        List<LineString> dLines = descriptor.getLines();

        LineString[] lines = new LineString[dLines.size()];
        for (int i = 0; i < lines.length; ++i) {
            lines[i] = (LineString) dLines.get(i).clone();
        }
        return IInsetMapResource.factory.createMultiLineString(lines);
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        DataTime time = descriptor.getTimeForResource(this);
        if (time == null) {
            return;
        }
        LineString line = descriptor.getLine(time);
        IWireframeShape shape = lines.get(line);
        if (shape == null) {
            shape = target.createWireframeShape(false, insetMapDescriptor);
            JTSCompiler compiler = new JTSCompiler(null, shape,
                    insetMapDescriptor);
            compiler.handle(line);
            shape.compile();
            lines.put(line, shape);
        }
        target.drawWireframeShape(shape,
                getCapability(ColorableCapability.class).getColor(), 2.0f);
    }

    @Override
    public String getName() {
        StringBuilder completeName = new StringBuilder(
                resourceData.getSource());
        completeName.append(" ").append(descriptor.getLineID());

        // If this is point data, get the station ID

        String stnID = "";
        if (resourceData.getMetadataMap().get("location.stationId") != null) {

            stnID = resourceData.getMetadataMap().get("location.stationId")
                    .getConstraintValue();
            if (stnID == null) {
                stnID = "";
            }
        }
        if (!stnID.isEmpty()) {
            if (stnID.contains(",")) {
                // ID may be formatted point1,point2 to define a line
                String stn = stnID.replace(",", "-");
                // For display, need point1-point2
                completeName.append(" ").append(stn);
            } else {
                completeName.append(" ").append(stnID);
            }
        }
        String parameterName = resourceData.getParameterName();
        completeName.append(" ").append(parameterName);
        if (getCapability(DisplayTypeCapability.class)
                .getDisplayType() == DisplayType.IMAGE) {
            completeName.append(" Img");
        }
        completeName.append(" ( ");

        completeName.append(getUnitString());
        completeName.append(" ) ");

        return completeName.toString();
    }

    public String getUnitString() {
        String unitString = "?";
        if ((prefs != null) && (prefs.getDisplayUnitLabel() != null)) {
            unitString = prefs.getDisplayUnitLabel();
        } else if (adapter.getUnit() == AbstractUnit.ONE) {
            return "";
        } else {
            unitString = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(adapter.getUnit());
        }

        return unitString;
    }

    public Unit<?> getUnit() {
        Unit<?> xPrefUnit = prefs == null ? null : prefs.getDisplayUnits();
        if ((xPrefUnit != null) && (xPrefUnit != AbstractUnit.ONE)) {
            return xPrefUnit;
        }
        Unit<?> xDataUnit = adapter.getUnit();
        if ((xDataUnit != null) && (xDataUnit != AbstractUnit.ONE)) {
            return xDataUnit;
        }
        return AbstractUnit.ONE;
    }

    @Override
    public Object getGraphKey() {
        return AbstractCrossSectionResource.class;
    }

    @Override
    public IGraphLabel<Double>[] getXRangeData() {
        DataTime myTime = descriptor.getTimeForResource(this);
        if (myTime == null) {
            return new DoubleLabel[0];
        }
        Coordinate[] coords = descriptor.getLine(myTime).getCoordinates();
        double totalDistance = 0.0;

        for (int j = 0; j < (coords.length - 1); j++) {
            try {
                totalDistance += JTS.orthodromicDistance(coords[j],
                        coords[j + 1], MapUtil.getLatLonProjection());
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return new DoubleLabel[] { new DoubleLabel(0.0),
                new DoubleLabel(totalDistance) };
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

    private class DataRetrievalJob extends Job {
        protected ConcurrentLinkedQueue<DataTime> times = new ConcurrentLinkedQueue<>();

        protected boolean run = true;

        public DataRetrievalJob(String name) {
            super(name);
        }

        @Override
        public boolean shouldRun() {
            return geometry != null;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            DataTime time = null;

            while (run && ((time = times.poll()) != null)) {
                try {
                    AbstractCrossSectionResource.this.loadSlice(time);
                    AbstractCrossSectionResource.this.issueRefresh();
                } catch (VizException e) {
                    if (run) {
                        statusHandler.handle(Priority.ERROR,
                                "Error Loading Cross Section Data", e);
                    }
                }
            }
            return Status.OK_STATUS;
        }
    }
}
