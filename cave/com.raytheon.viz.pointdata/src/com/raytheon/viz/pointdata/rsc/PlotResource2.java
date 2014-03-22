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

package com.raytheon.viz.pointdata.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.Validate;
import org.eclipse.swt.graphics.RGB;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension.ISingleColorImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotAlertParser;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.raytheon.viz.pointdata.rsc.progdisc.AbstractProgDisclosure;
import com.raytheon.viz.pointdata.rsc.progdisc.AbstractProgDisclosure.IProgDiscListener;
import com.raytheon.viz.pointdata.rsc.progdisc.DynamicProgDisclosure;
import com.raytheon.viz.pointdata.rsc.progdisc.SpiProgDisclosure;
import com.raytheon.viz.pointdata.thread.GetDataTask;
import com.raytheon.viz.pointdata.thread.GetDataTask.Params;
import com.raytheon.viz.pointdata.thread.PlotThreadOverseer;
import com.raytheon.viz.pointdata.units.PlotUnits;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides a resource that will display plot data for a given reference time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2006            brockwoo    Initial creation.
 * Feb 17, 2009            njensen     Refactored to new rsc architecture.
 * Mar 17, 2009 2105       jsanchez    Plot goessounding/poessounding
 *                                     availability.
 * Mar 30, 2009 2169       jsanchez    Updated initNewFrame.
 * Apr 09, 2009 952        jsanchez    Plot acars.
 * Apr 13, 2009 2251       jsanchez    Plot profilers.
 * Apr 21, 2009            chammack    Refactor to common pointData model
 * Feb 01, 2013 1567       njensen     Refactor handling of updates
 * May 14, 2013 1869       bsteffen    Get plots working without dataURI
 * May 23, 2013 14996      snaples     Updated processUpdatedPlot to handle AWOS 
 *                                     stations updates properly
 * Jun 06, 2013 2072       bsteffen    Fix concurrency problems when init is
 *                                     called before time matching is done.
 * Jun 25, 2013 1869       bsteffen    Fix plot sampling.
 * Sep 04, 2013 16519      kshresth    Fix Metar Display Problem
 * Dec 02, 2013 2473       njensen     Prog Disclose paint frames at high priority
 * Mar 21, 2014 2868       njensen     Use PlotThreadOverseer for increased efficiency
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotResource2 extends
        AbstractVizResource<PlotResourceData, MapDescriptor> implements
        IResourceDataChanged, IPlotModelGeneratorCaller, IProgDiscListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotResource2.class);

    private static final double MAX_SAMPLE_DISANCE = 2;

    public static final String NO_DATA = "NO DATA";

    private AbstractProgDisclosure progressiveDisclosure;

    private PlotThreadOverseer generator;

    private double plotWidth;

    private final Map<DataTime, FrameInformation> frameMap;

    private DataTime displayedTime;

    private RGB imageColor = null;

    private JobPool frameRetrievalPool = new JobPool("Retrieving plot frame",
            8, true);

    private class FrameRetriever implements Runnable {

        private DataTime dataTime;

        @Override
        public void run() {
            try {
                initNewFrame(dataTime);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving frame information for time: "
                                + dataTime, e);
            }
        }

    }

    /**
     * A station represents all the data for a single location(station) for a
     * single frame
     * 
     */
    public static class Station {
        /*
         * Contains all PlotInfo objects for the same stationId with the same
         * normalized time(real time will be different)
         */
        public PlotInfo[] info;

        /*
         * The image to display for this plot
         */
        public PointImage plotImage;

        /*
         * Sampling text for this plot
         */
        public String rawMessage;

        /*
         * Information used be the progressive disclosure algorithm
         */
        public Object progDiscInfo;

        /*
         * Location of the plot in descriptor grid space.
         */
        public Coordinate pixelLocation;

    }

    public class FrameInformation {
        Map<String, Station> stationMap = new ConcurrentHashMap<String, Station>();

        List<Station> lastComputed = Collections.emptyList();
    }

    /**
     * Create a surface plot resource.
     * 
     * @param target
     *            The graphic target to draw to
     * @param refTime
     *            The reference time to request data against
     * @throws VizException
     */
    public PlotResource2(PlotResourceData data, LoadProperties props) {
        super(data, props);
        if (data.getAlertParser() == null) {
            data.setAlertParser(new PlotAlertParser());
        }
        this.dataTimes = new ArrayList<DataTime>();
        this.frameMap = new ConcurrentHashMap<DataTime, FrameInformation>();
        data.addChangeListener(this);
    }

    @Override
    public String getName() {
        return this.resourceData.getPlotSource();
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        DataTime curTime = paintProps.getDataTime();
        if (curTime == null) {
            return;
        }

        FrameInformation curFrame = frameMap.get(curTime);
        if (curFrame == null) {
            curFrame = startFrameInit(curTime);
        }

        if (progressiveDisclosure.updateProperties(paintProps)
                || !curTime.equals(displayedTime)) {
            displayedTime = paintProps.getDataTime();
            progressiveDisclosure.update(curFrame.stationMap.values(),
                    displayedTime, true);
            resourceData.getPlotInfoRetriever().updateActiveFrame(
                    paintProps.getDataTime(),
                    descriptor.pixelToWorld(paintProps.getView().getExtent(),
                            descriptor.getCRS()), descriptor.getCRS());
        }

        if (!progressiveDisclosure.isDone() || !generator.isDone()
                || frameRetrievalPool.isActive()) {
            issueRefresh();
        }

        List<Station> stationList = curFrame.lastComputed;

        if (stationList.isEmpty()) {
            return;
        }

        List<PointImage> images = new ArrayList<PointImage>(stationList.size());
        for (Station station : stationList) {
            if (station.plotImage == null) {
                continue;
            }
            // set image color so shader can draw in appropriate color
            ((ISingleColorImage) station.plotImage.getImage())
                    .setColor(imageColor);
            images.add(station.plotImage);
        }

        aTarget.getExtension(IPointImageExtension.class).drawPointImages(
                paintProps, images);
    }

    @Override
    protected void initInternal(IGraphicsTarget aTarget) throws VizException {
        Map<String, RequestConstraint> request = resourceData.getMetadataMap();
        RequestConstraint plugin = request.get("pluginName");
        PlotUnits.register();
        generator = new PlotThreadOverseer(aTarget, descriptor,
                this.resourceData.getPlotModelFile(),
                this.resourceData.getLevelKey(), plugin.getConstraintValue(),
                this.resourceData.getMetadataMap(), this);
        this.generator.setPlotModelColor(getCapability(
                ColorableCapability.class).getColor());
        this.imageColor = getCapability(ColorableCapability.class).getColor();
        this.generator.setPlotModelLineStyle(getCapability(
                OutlineCapability.class).getLineStyle());
        this.generator.setPlotModelLineWidth(getCapability(
                OutlineCapability.class).getOutlineWidth());
        this.generator.setPlotMissingData(resourceData.isPlotMissingData());
        this.generator.setLowerLimit(resourceData.getLowerLimit());
        this.generator.setUpperLimit(resourceData.getUpperLimit());
        this.plotWidth = generator.getOriginalPlotModelWidth();
        this.plotWidth *= getCapability(MagnificationCapability.class)
                .getMagnification();
        generator.setPlotModelSize(Math.round(plotWidth));

        // TODO this should be in a factory or something
        if (resourceData.getSpiFile() != null) {
            progressiveDisclosure = new SpiProgDisclosure(this, descriptor,
                    resourceData.getSpiFile());
        } else {
            progressiveDisclosure = new DynamicProgDisclosure(this, descriptor);
        }
        progressiveDisclosure.setMagnification(this.getCapability(
                MagnificationCapability.class).getMagnification());
        progressiveDisclosure.setDensity(this.getCapability(
                DensityCapability.class).getDensity());
        progressiveDisclosure.setPixelSizeHint(resourceData.getPixelSizeHint());
        progressiveDisclosure.setPlotWidth(plotWidth);

        DataTime[] dts = this.descriptor.getFramesInfo().getTimeMap().get(this);
        // if this is null then the time matcher has not yet had time to time
        // match this, in which case frames will have to load when they are
        // first painted.
        if (dts != null) {
            // init backwards
            for (int i = dts.length - 1; i > -1; i--) {
                DataTime time = dts[i];
                if (time != null) {
                    FrameInformation curFrame = frameMap.get(time);
                    if (curFrame == null) {
                        curFrame = startFrameInit(time);
                    }
                }
            }
        }
    }

    private synchronized FrameInformation startFrameInit(DataTime time) {
        FrameInformation frame = new FrameInformation();
        dataTimes.add(time);
        frameMap.put(time, frame);
        FrameRetriever retriever = new FrameRetriever();
        retriever.dataTime = time;
        frameRetrievalPool.schedule(retriever);
        return frame;
    }

    /**
     * Checks the plots to ensure they are displayable, ie a frame exists that
     * matches their time and they are within the descriptor's world extent. If
     * so, schedules them for disclosure. Also checks if a plot already exists
     * and this is an update, and if so, updates the plot.
     * 
     * @param stationsToParse
     *            stations to potentially process and display
     * @throws VizException
     */
    protected void updateRecords(PlotInfo[] stationsToParse)
            throws VizException {
        Validate.notNull(stationsToParse);
        Map<DataTime, List<PlotInfo>> plots = new HashMap<DataTime, List<PlotInfo>>();
        // Sort plots into normalized datatimes that should match frames
        for (PlotInfo info : stationsToParse) {
            DataTime normTime = getNormalizedTime(info.dataTime);
            if (frameMap.containsKey(normTime)) {
                List<PlotInfo> list = plots.get(normTime);
                if (list == null) {
                    list = new ArrayList<PlotInfo>();
                    plots.put(normTime, list);
                }
                list.add(info);

            }
        }

        GridEnvelope range = descriptor.getGridGeometry().getGridRange();
        PixelExtent worldExtent = new PixelExtent(range.getLow(0),
                range.getHigh(0), range.getLow(1), range.getHigh(1));

        for (Entry<DataTime, List<PlotInfo>> entry : plots.entrySet()) {
            DataTime time = entry.getKey();
            List<PlotInfo> info = entry.getValue();
            FrameInformation frameInfo = frameMap.get(time);

            // Sort this data in "backwards" so that the most recent observation
            // for a particular station display correctly
            if (info.size() > 1) {
                Collections.sort(info, new Comparator<PlotInfo>() {

                    @Override
                    public int compare(PlotInfo o1, PlotInfo o2) {
                        return o2.dataTime.compareTo(o1.dataTime);
                    }
                });
            }

            if (frameInfo != null) {
                Map<String, Station> stationMap = frameInfo.stationMap;
                for (PlotInfo plot : info) {
                    if (plot.stationId == null) {
                        plot.stationId = plot.latitude + "#" + plot.longitude;
                    }
                    synchronized (stationMap) {
                        if (stationMap.containsKey(plot.stationId)) {
                            processUpdatedPlot(stationMap.get(plot.stationId),
                                    plot);
                        } else {
                            double[] thisLocationPixel = descriptor
                                    .worldToPixel(new double[] {
                                            plot.longitude, plot.latitude });
                            if ((thisLocationPixel != null)
                                    && worldExtent.contains(
                                            thisLocationPixel[0],
                                            thisLocationPixel[1])) {
                                Station station = new Station();
                                station.info = new PlotInfo[] { plot };
                                station.pixelLocation = new Coordinate(
                                        thisLocationPixel[0],
                                        thisLocationPixel[1]);
                                stationMap.put(plot.stationId, station);
                            }
                        }
                    }
                }

                progressiveDisclosure.update(stationMap.values(), time);
            }
        }
        issueRefresh();
    }

    /**
     * Updates an existing station with a new plot.
     * 
     * @param existingStation
     *            the existing station
     * @param plot
     *            the newly received plot
     */
    protected void processUpdatedPlot(Station existingStation, PlotInfo plot) {
        if (existingStation.plotImage != null) {
            existingStation.plotImage.getImage().dispose();
            existingStation.plotImage = null;
            // DR14966
            existingStation.rawMessage = null;
            PlotInfo[] samplePlot = new PlotInfo[1];
            samplePlot[0] = new PlotInfo();
            samplePlot[0] = plot;
            List<PlotInfo[]> list = new ArrayList<PlotInfo[]>();
            list.add(samplePlot);
            Params params = Params.PLOT_AND_SAMPLE;
            GetDataTask task = new GetDataTask(list, params);
            generator.enqueueDataRetrieval(task);
            // End DR14996
        }
        boolean dup = false;
        for (PlotInfo element : existingStation.info) {
            String curUri = element.dataURI;
            String newUri = plot.dataURI;
            if ((curUri == null) || curUri.equals(newUri)) {
                dup = true;
                break;
            }
        }
        if (!dup) {
            // Added for DR14996
            existingStation.info = Arrays.copyOf(existingStation.info, 1);
            existingStation.info[0] = plot;
            Arrays.sort(existingStation.info, new Comparator<PlotInfo>() {
                @Override
                public int compare(PlotInfo o1, PlotInfo o2) {
                    return o1.dataTime.compareTo(o2.dataTime);
                }
            });
        }
    }

    private DataTime getNormalizedTime(DataTime time) {
        if (this.resourceData.getBinOffset() != null) {
            return this.resourceData.getBinOffset().getNormalizedTime(time);
        } else {
            return time;
        }
    }

    @Override
    protected void disposeInternal() {
        resourceData.getPlotInfoRetriever().cancel();
        progressiveDisclosure.shutdown();
        if (generator != null) {
            generator.shutdown();
        }
        clearImages();
    }

    private void initNewFrame(DataTime thisFrameTime) throws VizException {
        RequestConstraint time = new RequestConstraint();

        BinOffset tmpBinOffset = this.resourceData.getBinOffset();
        if (tmpBinOffset == null) {
            // do not set the bin offset in the resource data, it is probably
            // null for a reason. non-null binOffset can cause problems with
            // time matching for some products.
            tmpBinOffset = new BinOffset();
        }
        TimeRange tr = null;

        if (this.resourceData.isTopOfTheHour()) {
            long offset = tmpBinOffset.getNegOffset() * 1000l;
            long frameInMillis = thisFrameTime.getValidTime().getTimeInMillis();
            tr = new TimeRange(frameInMillis - offset, frameInMillis);
        } else if (this.resourceData.getDefaultPeriod() != null) {
            tr = this.resourceData.getDefaultPeriod().getTimeRange(
                    thisFrameTime);
        } else {
            tr = tmpBinOffset.getTimeRange(thisFrameTime);
        }
        if (tr.getDuration() > 0) {
            // handle binning
            DataTime start = new DataTime(tr.getStart());
            DataTime end = new DataTime(tr.getEnd());
            String[] constraintList = { start.toString(), end.toString() };
            time.setBetweenValueList(constraintList);
            time.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);

        } else {
            // handle exact times
            time.setConstraintValue(thisFrameTime.toString());
        }

        HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        metadataMap.put("dataTime", time);

        // results will be sent to resourceChanged(DATA_UPDATE) on current
        // thread
        resourceData.getPlotInfoRetriever().getStations(this, thisFrameTime,
                metadataMap);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String message = NO_DATA;
        if (displayedTime != null) {
            if (!this.frameMap.containsKey(displayedTime)) {
                return message;
            }
            Map<String, Station> stationMap = this.frameMap.get(displayedTime).stationMap;
            Coordinate latlon = null;
            try {
                latlon = coord.asLatLon();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transforming coordinate", e);
            }

            double[] ml = { latlon.x, latlon.y };
            double[] pml = descriptor.worldToPixel(ml);
            Coordinate mouseLocation = new Coordinate(pml[0], pml[1]);
            double screenToWorldRatio = progressiveDisclosure
                    .getScreenToWorldRatio();
            List<Station> availableStations = new ArrayList<Station>();
            for (Station station : stationMap.values()) {
                if (station != null) {
                    Coordinate pixelLocation = station.pixelLocation;
                    if (distanceBetween(pixelLocation, mouseLocation) <= (this
                            .getResourceData().getPixelSampleDistance() / screenToWorldRatio)) {

                        availableStations.add(station);
                    }

                }
            }

            Station inspectPlot = null;
            if (availableStations.size() == 1) {
                inspectPlot = availableStations.get(0);
            } else if (availableStations.size() > 1) {
                int index = findClosestPlot(latlon, availableStations);

                if (index != -1) {
                    inspectPlot = availableStations.get(index);
                }
            }

            if (inspectPlot != null) {
                message = inspectPlot.rawMessage;
                if (message == null) {
                    message = "Generating...";
                    List<PlotInfo[]> list = new ArrayList<PlotInfo[]>(1);
                    list.add(inspectPlot.info);
                    Params params = Params.PLOT_AND_SAMPLE;
                    if (inspectPlot.info[0].pdv != null) {
                        params = Params.SAMPLE_ONLY;
                    }
                    GetDataTask task = new GetDataTask(list, params);
                    generator.enqueueDataRetrieval(task);
                }
            }

        }
        return message;
    }

    private double distanceBetween(Coordinate c1, Coordinate c2) {

        double xComponent = (c1.x - c2.x) * (c1.x - c2.x);
        double yComponent = (c1.y - c2.y) * (c1.y - c2.y);
        double rval = Math.sqrt(xComponent + yComponent);
        return rval;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            try {
                updateRecords((PlotInfo[]) object);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error updating plot resource", e);
            }

        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorableCapability) {
                if (generator != null) {
                    generator.setPlotModelColor(((ColorableCapability) object)
                            .getColor());
                    this.imageColor = ((ColorableCapability) object).getColor();
                }
            } else if (object instanceof DensityCapability) {
                if (progressiveDisclosure != null) {
                    progressiveDisclosure.setDensity(getCapability(
                            DensityCapability.class).getDensity());
                    issueRefresh();
                }
            } else if (object instanceof MagnificationCapability) {
                if (generator != null) {
                    this.plotWidth = generator.getOriginalPlotModelWidth();
                    this.plotWidth *= getCapability(
                            MagnificationCapability.class).getMagnification();
                    generator.setPlotModelSize(Math.round(plotWidth));

                }
                if (progressiveDisclosure != null) {
                    progressiveDisclosure.setMagnification(getCapability(
                            MagnificationCapability.class).getMagnification());
                    progressiveDisclosure.setPlotWidth(plotWidth);
                }
                clearImages();
            } else if (object instanceof OutlineCapability) {
                if (generator != null) {
                    OutlineCapability cap = (OutlineCapability) object;
                    generator.setPlotModelLineStyle(cap.getLineStyle());
                    generator.setPlotModelLineWidth(cap.getOutlineWidth());
                    clearImages();
                }
            }
        }
        issueRefresh();
    }

    /**
     * Returns the index of the closest available plot in the same area as the
     * sampling position.
     * 
     * @param latlon
     *            The coordinate of the sampling position.
     * @param availableStations
     *            List of available plots in the same area as latlon.
     * @return The index of the plot closest to latlon.
     */
    private int findClosestPlot(Coordinate latlon,
            List<Station> availableStations) {
        double x1 = latlon.x;
        double y1 = latlon.y;
        double minDistance = MAX_SAMPLE_DISANCE;
        int minIndex = -1;

        for (int i = 0; i < availableStations.size(); i++) {
            PlotInfo info = availableStations.get(i).info[0];
            double x2 = info.longitude;
            double y2 = info.latitude;
            double d = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));

            if (d < minDistance) {
                minDistance = d;
                minIndex = i;
            }
        }

        return minIndex;
    }

    @Override
    public void clearImages() {
        for (Entry<DataTime, FrameInformation> entry : this.frameMap.entrySet()) {
            FrameInformation frameInfo = entry.getValue();
            for (Station station : frameInfo.stationMap.values()) {
                if (station.plotImage != null) {
                    station.plotImage.getImage().dispose();
                    station.plotImage = null;

                    if (station.info != null) {
                        for (PlotInfo info : station.info) {
                            info.plotQueued = false;
                        }
                    }
                }
            }
        }
        issueRefresh();
    }

    @Override
    public void modelGenerated(PlotInfo[] key, IImage image) {
        DataTime normTime = getNormalizedTime(key[0].dataTime);
        FrameInformation frameInfo = this.frameMap.get(normTime);
        if (frameInfo != null) {
            Station s = frameInfo.stationMap.get(key[0].stationId);
            if (s != null) {
                if (image != null) {
                    ISingleColorImage si = (ISingleColorImage) image;
                    s.plotImage = new PointImage(si, s.pixelLocation);
                    s.plotImage.setSiteId(s.info[0].stationId);
                    si.setColor(imageColor);
                } else {
                    frameInfo.stationMap.remove(key[0].stationId);
                    progressiveDisclosure.update(frameInfo.stationMap.values(),
                            normTime);
                }
                issueRefresh();
            } else if (image != null) {
                image.dispose();
            }
        } else if (image != null) {
            image.dispose();
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        FrameInformation frameInfo = this.frameMap.remove(dataTime);
        if (frameInfo != null) {
            for (Station s : frameInfo.stationMap.values()) {
                if ((s != null) && (s.plotImage != null)) {
                    s.plotImage.getImage().dispose();
                    s.plotImage = null;
                }
            }
            frameInfo.stationMap.clear();
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        clearImages();
        frameRetrievalPool.cancel();
        frameRetrievalPool = new JobPool("Retrieving plot frame", 8, true);
        frameMap.clear();
    }

    @Override
    public synchronized void disclosureComplete(DataTime time,
            List<Station> disclosed) {
        FrameInformation frame = frameMap.get(time);
        if (frame != null) {
            frame.lastComputed = disclosed;
            issueRefresh();
            Map<String, Station> stationMap = frame.stationMap;
            List<PlotInfo[]> toQueue = new ArrayList<PlotInfo[]>(200);
            for (Station station : disclosed) {
                if ((station.plotImage == null)
                        && stationMap.containsKey(station.info[0].stationId)) {
                    toQueue.add(station.info);
                }
            }
            if (toQueue.size() > 0) {
                GetDataTask task = new GetDataTask(toQueue, Params.PLOT_ONLY);
                generator.enqueueDataRetrieval(task);
            }
        } else {
            issueRefresh();
        }

    }

    @Override
    public void messageGenerated(PlotInfo[] key, String message) {
        // Key will be the same PlotInfo[] provided to the generator(which will
        // be all the PlotInfo[] from a single station) and message will be the
        // sample text.
        DataTime normTime = getNormalizedTime(key[0].dataTime);
        FrameInformation frameInfo = this.frameMap.get(normTime);
        if (frameInfo != null) {
            Station s = frameInfo.stationMap.get(key[0].stationId);
            if (s != null) {
                s.rawMessage = message;
                issueRefresh();
            }
        }
    }

}
