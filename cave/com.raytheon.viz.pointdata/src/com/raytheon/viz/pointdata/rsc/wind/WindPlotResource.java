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
package com.raytheon.viz.pointdata.rsc.wind;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataServerRequest;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.rsc.wind.WindPlotConfig.SampleFormat;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Resource for displaying wind barbs of point data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 13, 2015  4903     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class WindPlotResource extends
        AbstractVizResource<WindPlotResourceData, IMapDescriptor> {

    private JobPool primaryLoadPool;

    private Job secondaryLoadJob;

    private final Object framesLock = new Object();

    private Map<DataTime, DataFrame> frames;

    protected WindPlotResource(WindPlotResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        synchronized (framesLock) {
            dataTimes = new CopyOnWriteArrayList<>();
            frames = new HashMap<DataTime, DataFrame>();
            primaryLoadPool = new JobPool("Loading Wind Data", 4, false);
            secondaryLoadJob = new SecondaryUpdateJob();
            secondaryLoadJob.setSystem(true);
            secondaryLoadJob.schedule();
        }
    }

    @Override
    protected void disposeInternal() {
        secondaryLoadJob.cancel();
        secondaryLoadJob = null;
        primaryLoadPool.cancel();
        primaryLoadPool = null;
        synchronized (framesLock) {
            for (DataFrame frame : frames.values()) {
                frame.getRenderable().dispose();
            }
            this.frames = null;
        }

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        DataFrame frame;
        synchronized (framesLock) {
            frame = frames.get(time);
            if (frame == null) {
                frame = addFrame(time);
            }
        }
        primaryLoadPool.schedule(frame);
        frame.getRenderable().paint(target, paintProps);
    }

    protected DataFrame addFrame(DataTime dataTime) {
        synchronized (framesLock) {
            DataFrame frame = frames.get(dataTime);
            if (frame == null) {
                frame = new DataFrame(dataTime);
                int index = Collections.binarySearch(dataTimes, dataTime);
                if (index < 0) {
                    /* Keep the list sorted. */
                    dataTimes.add(-1 * index - 1, dataTime);
                }
                frames.put(dataTime, frame);
            }
            return frame;
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (framesLock) {
            frames.remove(dataTime);
            super.remove(dataTime);
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (resourceData.getConfig().getSample() == null) {
            return null;
        }
        DataTime time = descriptor.getTimeForResource(this);
        if (time == null) {
            return null;
        }
        DataFrame frame;
        synchronized (framesLock) {
            frame = frames.get(time);
        }
        if (frame == null) {
            return null;
        }
        try {
            String text = frame.getRenderable().getText(
                    coord.asPixel(descriptor.getGridGeometry()));
            if (text == null) {
                text = "NO DATA";
            }
            return text;
        } catch (TransformException | FactoryException e) {
            throw new VizException("Unable to transform point for inspect.", e);
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (framesLock) {
            for (DataFrame frame : frames.values()) {
                frame.reproject();
            }
        }
    }

    @Override
    public String getName() {
        String name = resourceData.getLegend();
        if (name == null) {
            name = "Wind";
        }
        return name;
    }

    protected void loadData(DataTime dataTime, WindPlotRenderable renderable,
            boolean initialLoad) {
        WindPlotConfig config = resourceData.getConfig();
        SampleFormat sample = config.getSample();
        Map<String, RequestConstraint> constraints = new HashMap<>(
                resourceData.getFullDataMetadataMap());
        constraints.put(PointDataServerRequest.REQUEST_MODE_KEY,
                new RequestConstraint(PointDataServerRequest.REQUEST_MODE_2D));
        constraints.put(PointDataServerRequest.REQUEST_PARAMETERS_KEY,
                new RequestConstraint(config.getUniqueParameters()));

        BinOffset binOffset = resourceData.getBinOffset();
        if (binOffset == null) {
            constraints.put(PluginDataObject.DATATIME_ID,
                    new RequestConstraint(dataTime.toString()));
        } else {
            TimeRange range = binOffset.getTimeRange(dataTime);
            constraints.put(PluginDataObject.REFTIME_ID, new RequestConstraint(
                    new DataTime(range.getStart()).toString(), new DataTime(
                            range.getEnd()).toString()));
        }
        PointDataServerRequest request = new PointDataServerRequest();
        request.setRcMap(constraints);
        try {
            PointDataContainer container = (PointDataContainer) ThriftClient
                    .sendRequest(request);
            if (container == null) {
                return;
            }
            for (int i = 0; i < container.getAllocatedSz(); i += 1) {
                PointDataView view = container.readRandom(i);
                double longitude = config.getLongitude().getNumericValue(view);
                double latitude = config.getLatitude().getNumericValue(view);
                double magnitude = config.getSpeed().getNumericValue(view);
                double direction = config.getDirection().getNumericValue(view);
                Coordinate lonLat = new Coordinate(longitude, latitude);
                if (sample == null) {
                    renderable.addBarb(lonLat, magnitude, direction,
                            !initialLoad);
                } else {
                    WindPlotParameter[] fields = sample.getFields();
                    Object[] args = new Object[fields.length];
                    for (int c = 0; c < args.length; c += 1) {
                        args[c] = fields[c].getValue(view);
                    }
                    String text = String.format(sample.getText(), args)
                            .toString();
                    renderable.addBarb(lonLat, magnitude, direction, text,
                            !initialLoad);
                }

            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving wind data.", e);
        }
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (type == ChangeType.DATA_UPDATE
                && updateObject instanceof PluginDataObject[]) {
            synchronized (framesLock) {
                BinOffset binOffset = resourceData.getBinOffset();
                for (PluginDataObject pdo : (PluginDataObject[]) updateObject) {
                    DataTime time = pdo.getDataTime();
                    if (binOffset != null) {
                        time = binOffset.getNormalizedTime(time);
                    }
                    DataFrame frame = frames.get(time);
                    if (frame != null) {
                        frame.update();
                    }
                }
                secondaryLoadJob.schedule();
            }
        }
    }


    private class DataFrame implements Runnable {

        private final DataTime time;

        private final WindPlotRenderable renderable;

        private boolean needsReproject = false;

        private boolean needsData = true;

        private boolean initialLoad = true;

        public DataFrame(DataTime time) {
            this.time = time;
            this.renderable = new WindPlotRenderable(WindPlotResource.this);
            renderable.setBaseDensity(resourceData.getBaseDensity());
            VectorGraphicsConfig config = renderable.getConfig();
            config.setBaseSize(20);
            config.setCalmCircleSizeRatio(0.3);
        }

        public WindPlotRenderable getRenderable() {
            return renderable;
        }

        public void update() {
            needsData = true;
        }

        public void reproject() {
            needsReproject = true;
        }

        @Override
        public void run() {
            asyncUpdate();
        }

        public synchronized boolean asyncUpdate() {
            boolean result = needsReproject | needsData;
            if (needsReproject) {
                needsReproject = false;
                renderable.reproject();
            }
            if (needsData) {
                needsData = false;
                loadData(time, renderable, initialLoad);
                initialLoad = false;
            }
            return result;
        }

    }

    private class SecondaryUpdateJob extends Job {

        public SecondaryUpdateJob() {
            super("Updating wind data.");
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            List<DataFrame> frameList;
            synchronized (framesLock) {
                if (frames == null) {
                    return Status.OK_STATUS;
                }
                frameList = new ArrayList<>(frames.values());
            }
            for (DataFrame frame : frameList) {
                if (frame.asyncUpdate()) {
                    /*
                     * Restart after every frame to make sure frames haven't
                     * been added or removed while it was updating.
                     */
                    schedule();
                    return Status.OK_STATUS;
                }
            }
            DataTime[] times = descriptor.getFramesInfo().getTimeMap()
                    .get(WindPlotResource.this);
            if (times == null) {
                return Status.OK_STATUS;
            }
            boolean added = false;
            for (DataTime time : times) {
                if (time == null) {
                    continue;
                }
                synchronized (framesLock) {
                    if (!frames.containsKey(time)) {
                        addFrame(time);
                        added = true;
                    }
                }
            }
            if (added) {
                /*
                 * Restart to ensure data is requested before optimization.
                 */
                schedule();
                return Status.OK_STATUS;
            }
            boolean finished = true;
            for (DataFrame frame : frameList) {
                if (!frame.getRenderable().optimizeDisclosure()) {
                    finished = false;
                }
            }
            if (!finished) {
                schedule();
            }
            return Status.OK_STATUS;
        }

    }

}
