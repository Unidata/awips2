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
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.RunnableFuture;

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
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.legend.IExtraLegendTextGeneratingResource;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameExtraRenderable;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameRenderable;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionRotation;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.map.GeoUtil;

import tech.units.indriya.AbstractUnit;
import tech.units.indriya.format.SimpleUnitFormat;

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
 * Oct 29, 2022  8959     mapeters  Update how data time levels are set
 * Nov 14, 2022  8973     mapeters  Prevent NPE when editor is closed
 * Nov 17, 2022  8978     mapeters  Don't cache inset map lines to prevent them
 *                                  from displaying in the wrong location
 * Feb 10, 2023  9010     mapeters  Load frames closest to current time first
 * Feb 22, 2023  9021     mapeters  Cache data as Futures
 * Dec 20, 2023  2036519  mapeters  Clear out more data on dispose
 * Apr 03, 2024  2037091  mapeters  Auto-update as new data comes in
 * May 22, 2024  2037092  mapeters  Put empty list in slice map for failed data load,
 *                                  schedule data retrieval on first PDO for frame,
 *                                  fix issue where data times' level type is wrong
 *                                  after changing baseline
 * May 29, 2024  2037244  mapeters  Implement redraw() so height scale changes work
 * Jun 20, 2024  2037565  mapeters  Implement IExtraLegendTextGeneratingResource, move
 *                                  some getName code to PointCSAdapter, fix duplicate
 *                                  records in the adapter after data updates
 * Aug 14, 2024  2037631  mapeters  Float data is now wrapped in a new class, prevent
 *                                  x axis values from not displaying at all or not
 *                                  updating when moving baseline
 *
 * </pre>
 *
 * @author njensen
 */
public abstract class AbstractCrossSectionResource extends
        AbstractVizResource<CrossSectionResourceData, CrossSectionDescriptor>
        implements IInsetMapResource, IGraphableResource<Double, Double>,
        IExtraLegendTextGeneratingResource {

    protected static final int GRID_SIZE = 100;

    protected static final DecimalFormat sampleFormat = new DecimalFormat(
            "0.00");

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    protected GridGeometry2D geometry = null;

    protected final AbstractCrossSectionAdapter<?> adapter;

    /**
     * Cache of requested data. Futures are used because they make it easier to
     * invalidate in-process data requests.
     */
    protected final Map<DataTime, Future<CrossSectionFrameData>> sliceMap = new HashMap<>(
            64);

    protected final Object lock = new Object();

    protected AbstractStylePreferences prefs;

    protected final DataRetrievalJob dataRetrievalJob = new DataRetrievalJob(
            "Loading Cross Section Data");

    public AbstractCrossSectionResource(CrossSectionResourceData data,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(data, props, false);
        this.adapter = adapter;

        data.addChangeListener((type, object) -> {
            if (type == ChangeType.DATA_UPDATE
                    && object instanceof PluginDataObject[]) {
                PluginDataObject[] pdos = (PluginDataObject[]) object;
                synchronized (lock) {
                    for (PluginDataObject pdo : pdos) {
                        addRecord(pdo);
                    }
                }
                // Schedule retrieval job as addRecord adds to times
                dataRetrievalJob.schedule();
            }
            issueRefresh();
        });
    }

    @Override
    protected void disposeInternal() {
        synchronized (lock) {
            dataRetrievalJob.times.clear();
            dataRetrievalJob.run = false;
            adapter.dispose();
            disposeFrames();
        }
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

        synchronized (lock) {
            for (DataTime time : dataTimes) {
                sliceMap.remove(time);
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
            /*
             * To be numerically accurate the grid geometry should be 1 grid
             * cell larger than the graph
             */
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
        synchronized (lock) {
            adapter.setDescriptor(descriptor);
            resourceData.setLineInfoFromDescriptor(descriptor);
            this.descriptor = descriptor;
            IGraph graph = descriptor.getGraph(this);
            if (graph != null) {
                // x axis values likely changed
                graph.reconstruct();
            }
            disposeFrames();
            Set<DataTime> times = new HashSet<>();
            for (DataTime time : dataTimes) {
                for (DataTime frameTime : resourceData
                        .getAffectedFrameTimes(time)) {
                    times.add(frameTime);
                    dataRetrievalJob.times.add(frameTime);
                }
            }
            dataTimes.retainAll(times);
            dataTimes.addAll(times);
            dataRetrievalJob.schedule();
        }
    }

    /**
     * Clear and dispose processed data/renderables for all frames, but leave
     * underlying data in the adapter.
     */
    protected void disposeFrames() {
        synchronized (lock) {
            sliceMap.clear();
        }
    }

    protected CrossSectionFrameData loadSlice(DataTime time)
            throws VizException {
        CrossSectionGraph graph = ((CrossSectionGraph) descriptor
                .getGraph(this));
        if (graph == null) {
            return null;
        }

        CrossSectionFrameData frameData = adapter.loadData(time, graph,
                geometry);
        List<float[]> floatData = null;
        CrossSectionFrameExtraRenderable frameInfo = null;
        if (frameData != null) {
            floatData = frameData.getData();
            frameInfo = frameData.getExtraRenderable();
        }
        if (floatData == null || floatData.isEmpty()) {
            /*
             * Return a non-null value to differentiate between data we've
             * attempted to load but couldn't (CrossSectionFrameData without
             * data), and data we haven't attempted to load yet (null).
             */
            return new CrossSectionFrameData(null, frameInfo);
        }

        Coordinate[] lineData = GeoUtil.splitLine(GRID_SIZE,
                descriptor.getLine(time).getCoordinates());
        int lineLengthInMeters = (int) graph
                .getVirtualLocation(graph.getExtent().getMaxX(), 0)[0];
        floatData = CrossSectionRotation.rotateVector(
                resourceData.getParameter(), Arrays.asList(lineData), floatData,
                lineLengthInMeters, descriptor.getHeightScale(),
                adapter.getDataCoordinateReferenceSystem());
        if (adapter.getUnit().isCompatible(getUnit())) {
            UnitConverter converter = UnitConv
                    .getConverterToUnchecked(adapter.getUnit(), getUnit());
            for (float[] floatArr : floatData) {
                for (int i = 0; i < floatArr.length; i++) {
                    if (floatArr[i] > -9998) {
                        floatArr[i] = (float) converter.convert(floatArr[i]);
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

        return new CrossSectionFrameData(floatData, frameInfo);
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
        if (getStatus() == ResourceStatus.DISPOSED) {
            return;
        }

        // Refresh if any new records were added
        DataTime pdoTime = pdo.getDataTime().clone();
        pdoTime.clearLevel();
        if (resourceData.getBinOffset() != null) {
            pdoTime = resourceData.getBinOffset().getNormalizedTime(pdoTime);
        }

        adapter.addRecord(pdo);
        for (DataTime frameTime : resourceData.getAffectedFrameTimes(pdoTime)) {
            if (!dataTimes.add(frameTime)) {
                /*
                 * We are adding a record for a time we have, dispose of
                 * existing time data before adding to retrieval job
                 */
                disposeFrame(frameTime, true);
            }
            dataRetrievalJob.times.add(frameTime);
        }
    }

    @Override
    public final void remove(DataTime dataTime) {
        super.remove(dataTime);
        remove(dataTime, false);
    }

    /**
     * Remove the given data time.
     *
     * @param dataTime
     *            the time to remove
     * @param onUpdate
     *            true if this is for the time's data to be recalculated on a
     *            data update, false otherwise
     */
    protected final void remove(DataTime dataTime, boolean onUpdate) {
        /*
         * This can be called with a frame time that has its level set to a line
         * type/index (e.g. from time matching) or with a level-less data time
         * that's for the underlying data (e.g. from DataCubeAlertMessageParser)
         */
        synchronized (lock) {
            for (DataTime frameTime : resourceData
                    .getAffectedFrameTimes(dataTime)) {
                disposeFrame(frameTime, onUpdate);
                dataTimes.remove(frameTime);
                dataRetrievalJob.times.remove(frameTime);
            }

            /*
             * If there are no frames left for the time, remove its data from
             * the adapter as well. This needs to be done even on updates, since
             * new records will be retrieved and added to the adapter, and we
             * don't want the old/duplicate records still around then.
             */
            boolean anyFramesLeftForTime = dataTimes.stream()
                    .anyMatch(time -> dataTime.equals(time, true));
            if (!anyFramesLeftForTime) {
                // Remove underlying data
                DataTime tmp = dataTime.clone();
                tmp.clearLevel();
                adapter.remove(tmp);
            }
        }

        issueRefresh();
    }

    /**
     * Dispose processed data/renderables for the given frame time, but leave
     * underlying data in the adapter.
     *
     * @param frameTime
     *            the frame time to dispose data for
     * @param onUpdate
     *            true if this is for the time's data to be recalculated on a
     *            data update, false otherwise
     */
    protected void disposeFrame(DataTime frameTime, boolean onUpdate) {
        synchronized (lock) {
            sliceMap.remove(frameTime);
        }
    }

    @Override
    public Geometry getInsetMapLocation() {
        List<LineString> dLines = descriptor.getLines();

        LineString[] lines = new LineString[dLines.size()];
        for (int i = 0; i < lines.length; ++i) {
            lines[i] = (LineString) dLines.get(i).copy();
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
        DrawableLine drawableLine = new DrawableLine();
        for (Coordinate coord : line.getCoordinates()) {
            double[] pixelCoord = insetMapDescriptor
                    .worldToPixel(new double[] { coord.x, coord.y });
            drawableLine.addPoint(pixelCoord[0], pixelCoord[1]);
        }
        drawableLine.width = 2.0f;
        drawableLine.basics.color = getCapability(ColorableCapability.class)
                .getColor();

        target.drawLine(drawableLine);
    }

    @Override
    public String getName() {
        StringBuilder completeName = new StringBuilder(
                resourceData.getSource());
        completeName.append(" ").append(descriptor.getLineID());

        String extraText = adapter.getExtraNameText();
        if (extraText != null && !extraText.isEmpty()) {
            completeName.append(" ").append(extraText);
        }

        String parameterName = resourceData.getParameterName();
        completeName.append(" ").append(parameterName);
        if (getCapability(DisplayTypeCapability.class)
                .getDisplayType() == DisplayType.IMAGE) {
            completeName.append(" Img");
        }
        completeName.append(" (");

        completeName.append(getUnitString());
        completeName.append(")");

        return completeName.toString();
    }

    @Override
    public String getExtraLegendText(DataTime time) {
        CrossSectionFrameRenderable frameRenderable = getFrameRenderable(time);
        if (frameRenderable != null) {
            return frameRenderable.getExtraLegendText();
        }
        return "";
    }

    public String getUnitString() {
        String unitString = "?";
        if ((prefs != null) && (prefs.getDisplayUnitLabel() != null)) {
            unitString = prefs.getDisplayUnitLabel();
        } else if (adapter.getUnit() == AbstractUnit.ONE) {
            return "";
        } else {
            unitString = SimpleUnitFormat
                    .getInstance(SimpleUnitFormat.Flavor.ASCII)
                    .format(adapter.getUnit());
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
        LineString currentLine = null;
        DataTime myTime = descriptor.getTimeForResource(this);
        if (myTime != null) {
            currentLine = descriptor.getLine(myTime);
        } else {
            /*
             * Sometimes a resource with data loads without the x axis distance
             * labels, and without the radar virtual volume line for resources
             * where that is applicable. That appears to be because the time is
             * null here, possibly due to a race condition with time matching.
             * This is a hack to still display the distance labels and radar
             * line in that case for most cross sections.
             */
            List<LineString> lines = descriptor.getLines();
            if (lines != null && lines.size() == 1) {
                currentLine = lines.get(0);
            }
        }
        if (currentLine == null) {
            return new DoubleLabel[0];
        }
        Coordinate[] coords = currentLine.getCoordinates();
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
        /*
         * Clear all processed frame data, but leave underlying data in adapter.
         * Then recalculate frame data so that it's re-scaled correctly.
         */
        synchronized (lock) {
            disposeFrames();
            dataRetrievalJob.times.addAll(dataTimes);
            dataRetrievalJob.schedule();
        }
        issueRefresh();
    }

    /**
     * Get the slice data for the given time. This does not wait for data
     * retrieval to complete.
     *
     * @param time
     *            the time to get slice data for
     * @return the slice data if immediately available, otherwise null
     */
    protected CrossSectionFrameData getSliceData(DataTime time) {
        return getSliceData(time, sliceMap);
    }

    /**
     * Get the slice data for the given time from the given slice map. This does
     * not wait for data retrieval to complete.
     *
     * @param time
     *            the time to get slice data for
     * @param sliceMap
     *            the slice map to lookup the time in
     * @return the slice data if immediately available, otherwise null
     */
    protected CrossSectionFrameData getSliceData(DataTime time,
            Map<DataTime, Future<CrossSectionFrameData>> sliceMap) {
        CrossSectionFrameData data = null;
        synchronized (lock) {
            Future<CrossSectionFrameData> future = sliceMap.get(time);
            if (future != null && future.isDone()) {
                try {
                    data = future.get();
                } catch (InterruptedException | ExecutionException e) {
                    statusHandler.error("Error getting slice data for " + time,
                            e);
                }
            }
        }
        return data;
    }

    /**
     * @param frameTime
     * @return the renderable data for the given frame time if immediately
     *         available, otherwise null
     */
    protected abstract CrossSectionFrameRenderable getFrameRenderable(
            DataTime frameTime);

    protected class DataRetrievalJob extends Job {

        protected final Queue<DataTime> times = new PriorityBlockingQueue<>(12,
                (t1, t2) -> {
                    /*
                     * Sorts times so that those closest to current time are
                     * requested first.
                     */
                    if (t1.hasFcst() == t2.hasFcst()) {
                        /*
                         * Sort forecast times in ascending order, observation
                         * times in descending order
                         */
                        return t1.hasFcst() ? t1.compareTo(t2)
                                : t2.compareTo(t1);
                    } else {
                        /*
                         * Put all forecast times after observation times in
                         * case they can be mixed here.
                         */
                        return t1.hasFcst() ? 1 : -1;
                    }
                });

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
            while (run) {
                DataTime time = times.poll();
                if (time == null) {
                    break;
                }
                try {
                    RunnableFuture<CrossSectionFrameData> dataFuture = new FutureTask<>(
                            () -> loadSlice(time));

                    synchronized (lock) {
                        sliceMap.put(time, dataFuture);
                    }

                    // Start data retrieval
                    dataFuture.run();
                    // Block until completion
                    dataFuture.get();

                    issueRefresh();
                } catch (Exception e) {
                    if (run) {
                        statusHandler.error("Error Loading Cross Section Data",
                                e);
                    }
                }
            }
            return Status.OK_STATUS;
        }
    }
}
