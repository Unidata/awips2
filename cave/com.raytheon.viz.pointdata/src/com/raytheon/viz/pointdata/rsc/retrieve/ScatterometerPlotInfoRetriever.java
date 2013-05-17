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
package com.raytheon.viz.pointdata.rsc.retrieve;

import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.pointdata.PlotInfo;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Retrieves available PlotInfo objects for ascat asynchronously and in fixed
 * size sets so they can be displayed during data retrieval.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 09, 2009            bsteffen    Initial creation
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ScatterometerPlotInfoRetriever extends PointDataPlotInfoRetriever {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScatterometerPlotInfoRetriever.class);

    private static final int MAX_RESULT_SIZE = 2000;

    private static final Envelope WORLD_ENVELOPE = new Envelope(-180, 180, -90,
            90);

    private static final int MIN_ENVELOPE_AREA = 100;

    private static class Request {

        private Request(DataTime time, Envelope envelope) {
            super();
            this.time = time;
            this.envelope = envelope;
        }

        private Request(Request request, Envelope envelope) {
            super();
            this.envelope = envelope;
            this.time = request.time;
            this.listener = request.listener;
            this.maxId = request.maxId;
            this.metadataMap = request.metadataMap;
        }

        IResourceDataChanged listener;

        HashMap<String, RequestConstraint> metadataMap;

        Integer maxId = Integer.MAX_VALUE;

        final DataTime time;

        final Envelope envelope;

    }

    private static class ScatterometerPlotInfo extends PlotInfo {

        public final int id;

        public ScatterometerPlotInfo(PlotInfo pi, int id) {
            super(pi.stationId, pi.latitude, pi.longitude, pi.dataTime,
                    pi.dataURI);
            this.id = id;
        }

    }

    @XmlTransient
    private Job job = new Job("Retrieving Scatterometer Locations") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (!screenQueue.isEmpty() && !monitor.isCanceled()) {
                Request request = null;
                synchronized (screenQueue) {
                    if (screenQueue.isEmpty()) {
                        request = backgroundQueue.getFirst();
                    } else {
                        request = screenQueue.getFirst();
                    }
                }
                request.metadataMap.put("id", new RequestConstraint(
                        request.maxId.toString(), ConstraintType.LESS_THAN));
                request.metadataMap.put("location.longitude",
                        new RequestConstraint(request.envelope.getMinX() + "--"
                                + request.envelope.getMaxX(),
                                ConstraintType.BETWEEN));
                request.metadataMap.put("location.latitude",
                        new RequestConstraint(request.envelope.getMinY() + "--"
                                + request.envelope.getMaxY(),
                                ConstraintType.BETWEEN));
                try {
                    List<PlotInfo> stations = getStations(request.metadataMap);
                    if (stations != null && !stations.isEmpty()) {
                        request.listener.resourceChanged(
                                ChangeType.DATA_UPDATE,
                                stations.toArray(new PlotInfo[0]));
                    }
                    if (stations != null && stations.size() == MAX_RESULT_SIZE) {
                        for (PlotInfo station : stations) {
                            int id = ((ScatterometerPlotInfo) station).id;
                            if (id < request.maxId) {
                                request.maxId = id;
                            }
                        }
                        synchronized (screenQueue) {
                            if (request.envelope.getArea() > MIN_ENVELOPE_AREA) {
                                Deque<Request> queue = null;
                                if (screenQueue.remove(request)) {
                                    queue = screenQueue;
                                } else {
                                    backgroundQueue.remove(request);
                                    queue = backgroundQueue;
                                }
                                Envelope e = request.envelope;
                                Coordinate c = e.centre();
                                Envelope subenvelope = new Envelope(c.x,
                                        e.getMinX(), c.y, e.getMinY());
                                queue.addLast(new Request(request, subenvelope));
                                subenvelope = new Envelope(c.x, e.getMinX(),
                                        c.y, e.getMaxY());
                                queue.addLast(new Request(request, subenvelope));
                                subenvelope = new Envelope(c.x, e.getMaxX(),
                                        c.y, e.getMinY());
                                queue.addLast(new Request(request, subenvelope));
                                subenvelope = new Envelope(c.x, e.getMaxX(),
                                        c.y, e.getMaxY());
                                queue.addLast(new Request(request, subenvelope));
                            }
                        }
                    } else {
                        synchronized (screenQueue) {
                            screenQueue.remove(request);
                            backgroundQueue.remove(request);
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);

                }
            }
            return Status.OK_STATUS;
        }

    };

    @XmlTransient
    private Deque<Request> screenQueue = new LinkedList<Request>();

    @XmlTransient
    private Deque<Request> backgroundQueue = new LinkedList<Request>();

    public ScatterometerPlotInfoRetriever() {
        needsDataUri = false;
        onlyRefTime = true;
    }

    @Override
    protected void addColumns(DbQuery dq) {
        dq.setMaxResults(MAX_RESULT_SIZE);
        dq.setOrderAscending(ResultOrder.DESC);
        dq.addOrderBy("id");
        super.addColumns(dq);
        dq.addColumn("id");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever#
     * getPlotInfo(java.lang.Object[])
     */
    @Override
    protected PlotInfo getPlotInfo(Object[] data) {
        PlotInfo info = super.getPlotInfo(Arrays.copyOf(data, data.length - 1));
        info = new ScatterometerPlotInfo(info, (Integer) data[data.length - 1]);
        return info;
    }

    public void getStations(IResourceDataChanged listener, DataTime time,
            HashMap<String, RequestConstraint> metadataMap) throws VizException {
        Request request = new Request(time, WORLD_ENVELOPE);
        request.listener = listener;
        request.metadataMap = metadataMap;
        synchronized (screenQueue) {
            screenQueue.addFirst(request);
        }
        // if (job.getState() == Job.NONE) {
        // job.setSystem(true);
        // }
        job.schedule();
    }

    public void updateActiveFrame(DataTime time, Envelope envelope,
            CoordinateReferenceSystem crs) {
        if (screenQueue.isEmpty() && backgroundQueue.isEmpty()) {
            return;
        }
        envelope = projectToLatLon(envelope, crs);
        synchronized (screenQueue) {
            backgroundQueue.addAll(screenQueue);
            screenQueue.clear();
            for (Request request : backgroundQueue) {
                if (request.time.equals(time)
                        && request.envelope.intersects(envelope)) {
                    screenQueue.add(request);
                }
            }
            backgroundQueue.removeAll(screenQueue);
        }
        if (!screenQueue.isEmpty()) {
            job.schedule();
        }
    }

    private Envelope projectToLatLon(Envelope envelope,
            CoordinateReferenceSystem crs) {
        try {
            ReferencedEnvelope nativeEnv = new ReferencedEnvelope(envelope, crs);
            GeneralEnvelope latLonEnv = new GeneralEnvelope(
                    nativeEnv.transform(MapUtil.LATLON_PROJECTION, false));
            latLonEnv.normalize(false);
            return new Envelope(latLonEnv.getMinimum(0),
                    latLonEnv.getMaximum(0), latLonEnv.getMinimum(1),
                    latLonEnv.getMaximum(1));
        } catch (Exception e) {
            return WORLD_ENVELOPE;
        }
    }

    @Override
    public void cancel() {
        job.cancel();
        synchronized (screenQueue) {
            screenQueue.clear();
            backgroundQueue.clear();
        }
    }

}
