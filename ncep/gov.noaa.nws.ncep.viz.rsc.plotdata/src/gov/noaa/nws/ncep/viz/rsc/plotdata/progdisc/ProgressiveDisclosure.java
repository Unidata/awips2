package gov.noaa.nws.ncep.viz.rsc.plotdata.progdisc;

import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.StaticPlotInfoPV;
import gov.noaa.nws.ncep.viz.rsc.plotdata.queue.QueueEntry;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.NcPlotResource2.Station;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.TimeLogger;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.Tracer;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import org.eclipse.swt.graphics.Rectangle;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.vividsolutions.jts.geom.Coordinate;

//import org.hibernate.mapping.Array;

public final class ProgressiveDisclosure {

    private final static double TOLERANCE = 0.0000000001;// 0.00000001;//0.000000000001;

    private ConcurrentLinkedQueue<QueueEntry> queueOfStationsToBeDisclosed;

    StaticPlotInfoPV spi;

    public boolean updateNextPaint;

    private VA_Advanced progDisc;

    private Task progDiscTask = null;

    private JobPool progDiscJobPool = null;

    public double minDist = Double.MAX_VALUE;

    private Rectangle canvasBounds = null;

    private IProgDiscListener progDiscListener;

    TimeLogger timeLogger = null;

    MathTransform WGS84toPROJCRS = null;

    public ProgressiveDisclosure(IProgDiscListener listener,
            StaticPlotInfoPV spiFile) {
        Tracer.print("> Entry");
        progDiscListener = listener;
        progDiscTask = new Task();
        updateNextPaint = false;
        spi = spiFile;
        for (StaticPlotInfoPV.SPIEntry entry : spi.getSpiList().values()) {
            if (entry.distance < minDist) {
                minDist = entry.distance;
            }
        }
        progDisc = new VA_Advanced();
        queueOfStationsToBeDisclosed = new ConcurrentLinkedQueue<QueueEntry>();
        progDiscJobPool = new JobPool("Running progressive disclosure...", 5,
                false);
        try {
            WGS84toPROJCRS = MapUtil
                    .getTransformFromLatLon(MapUtil.LATLON_PROJECTION);
        } catch (FactoryException e) {

            Tracer.print("ProgressiveDisclosure: Error setting up math transform");
            e.printStackTrace();
        }
        timeLogger = TimeLogger.getInstance();
        Tracer.print("< Exit");
    }

    public static interface IProgDiscListener {
        public void disclosureComplete(DataTime time,
                Collection<Station> disclosed);
    }

    private final class Task implements Runnable {
        IExtent extent;

        int canvasWidth = 0;

        NCMapDescriptor descriptor;

        double magnification = 1.0;

        double density = 1.0;

        double plotWidth = 90;

        double zoomLevel = 1.0;

        int pixelSizeHint = 90;

        static final float MAX_DENSITY = 3.0f;

        Station[] stations = new Station[0];

        Set<Station> setOfDisclosedStationsWithinScreenExtents = new HashSet<Station>();

        DataTime time = null;

        @Override
        protected Task clone() {
            Tracer.print("> Entry");
            Task task = new Task();
            task.descriptor = descriptor;
            task.magnification = magnification;
            task.density = density;
            task.extent = extent;
            task.zoomLevel = zoomLevel;
            task.canvasWidth = canvasWidth;
            task.pixelSizeHint = pixelSizeHint;
            task.plotWidth = plotWidth;
            task.stations = stations;
            task.setOfDisclosedStationsWithinScreenExtents = new HashSet<Station>();
            if (time != null) {
                task.time = time.clone();
            }
            Tracer.print("< Exit");

            return task;
        }

        @Override
        public void run() {
            Tracer.print("> Entry  START TASK " + Tracer.shortTimeString(time));
            if (canvasWidth == 0) {
                Tracer.print("canvasWidth for frame: " + shortTimeString(time)
                        + " - " + canvasWidth);
                final IDisplayPane activePane = NcDisplayMngr
                        .getActiveNatlCntrsEditor().getActiveDisplayPane();
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {
                        if (activePane != null) {
                            canvasBounds = activePane.getBounds();
                            canvasWidth = canvasBounds.width;
                            NCMapDescriptor mapDesc = (NCMapDescriptor) activePane
                                    .getDescriptor();
                            descriptor = mapDesc;

                            if (extent == null) {
                                extent = descriptor.getRenderableDisplay()
                                        .getView().getExtent().clone();
                            }
                        }
                    }
                });
                Tracer.print("updated canvasWidth for frame: "
                        + shortTimeString(time) + " - " + canvasWidth);
            }

            long t0 = System.nanoTime();
            // Tracer.print("The zoom level is " +
            // progDiscTask.zoomLevel);
            // if (extent != null) {
            // Tracer.print("minX= " + extent.getMinX()
            // +"\nminY="+extent.getMinY()
            // +"\nmaxX="+extent.getMaxX()
            // +"\nmaxY="+extent.getMaxY());
            // }
            descriptor = (NCMapDescriptor) NcDisplayMngr
                    .getActiveNatlCntrsEditor().getActiveDisplayPane()
                    .getDescriptor();
            int displayWidth = (int) (descriptor.getMapWidth() * zoomLevel);
            double kmPerPixel = (displayWidth / canvasWidth) / 1000.0;
            double displayHintSize = pixelSizeHint * magnification;
            double threshold = (displayHintSize * kmPerPixel) / density;
            double distFloor = (descriptor.getMapWidth() / 1000.0)
                    * pixelSizeHint / 32000.0;
            distFloor /= 3;
            boolean plotAll = (density > MAX_DENSITY || threshold < distFloor);

            synchronized (stations) {
                Tracer.print("P/D input list contains " + stations.length
                        + " stations");
                for (Station station : stations) {
                    double worldLoc[] = new double[] { station.info.longitude,
                            station.info.latitude };
                    double[] tempPixLoc = this.descriptor
                            .worldToPixel(worldLoc);
                    if (tempPixLoc == null)
                        continue;

                    station.pixelLocation = new Coordinate(tempPixLoc[0],
                            tempPixLoc[1]);

                    if (extent == null || !extent.contains(tempPixLoc))
                        continue;

                    if (station.distValue == null)
                        continue;

                    if (station.distValue.doubleValue() >= threshold || plotAll) {
                        setOfDisclosedStationsWithinScreenExtents.add(station);
                    }

                }
            }

            long t1 = System.nanoTime();
            timeLogger.append("Progressive Disclosure took " + (t1 - t0)
                    / 1000000 + " ms for " + time.toString());
            Tracer.print("Progressive Disclosure took " + (t1 - t0) / 1000000
                    + " ms for " + shortTimeString(time));
            Tracer.print("P/D output list contains "
                    + setOfDisclosedStationsWithinScreenExtents.size()
                    + " stations");
            progDiscListener.disclosureComplete(time,
                    setOfDisclosedStationsWithinScreenExtents);
            Tracer.print("< Exit   END TASK   " + Tracer.shortTimeString(time));

        }

    }

    public synchronized boolean checkAndUpdateProgDisclosureProperties() {
        Tracer.print("> Entry");
        boolean update = updateNextPaint;

        final IDisplayPane activePane = NcDisplayMngr
                .getActiveNatlCntrsEditor().getActiveDisplayPane();
        NCMapDescriptor mapDescriptor = (NCMapDescriptor) activePane
                .getDescriptor();
        IView view = mapDescriptor.getRenderableDisplay().getView();

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                Tracer.print("> Entry");
                if (activePane != null) {
                    canvasBounds = activePane.getBounds();

                }
            }
        });

        IExtent currViewExtents = view.getExtent();

        if (progDiscTask.extent == null) {
            progDiscTask.extent = new PixelExtent(currViewExtents.getMinX(),
                    currViewExtents.getMaxX(), currViewExtents.getMinY(),
                    currViewExtents.getMaxY());
            Tracer.print("New pix extents");
            update = true;

        }

        if (progDiscTask.descriptor == null) {
            progDiscTask.descriptor = (NCMapDescriptor) mapDescriptor;
            Tracer.print("New map descriptor");
            update = true;
        } else {
            if (progDiscTask.descriptor.equals(mapDescriptor)
                    && ((Math.abs(progDiscTask.extent.getMinX()
                            - currViewExtents.getMinX()) > TOLERANCE)
                            || (Math.abs(progDiscTask.extent.getMaxX()
                                    - currViewExtents.getMaxX()) > TOLERANCE)
                            || (Math.abs(progDiscTask.extent.getMinY()
                                    - currViewExtents.getMinY()) > TOLERANCE) || (Math
                            .abs(progDiscTask.extent.getMaxY()
                                    - currViewExtents.getMaxY()) > TOLERANCE))) {
                Tracer.print("Changed pix extents for the current view");

                progDiscTask.extent = currViewExtents.clone();
                update = true;
            } else if (!progDiscTask.descriptor.equals(mapDescriptor)) {

                progDiscTask.descriptor = (NCMapDescriptor) mapDescriptor;
                GeneralGridGeometry gridGeom = progDiscTask.descriptor
                        .getGridGeometry();
                PixelExtent tempExtent = new PixelExtent(0, gridGeom
                        .getGridRange().getHigh(0), 0, gridGeom.getGridRange()
                        .getHigh(1));
                Tracer.print("Changed the descriptor");

                if ((Math.abs(progDiscTask.extent.getMinX()
                        - tempExtent.getMinX()) > TOLERANCE)
                        || (Math.abs(progDiscTask.extent.getMaxX()
                                - tempExtent.getMaxX()) > TOLERANCE)
                        || (Math.abs(progDiscTask.extent.getMinY()
                                - tempExtent.getMinY()) > TOLERANCE)
                        || (Math.abs(progDiscTask.extent.getMaxY()
                                - tempExtent.getMaxY()) > TOLERANCE)) {

                    Tracer.print("Changed pix extents for the new grid geometry");
                    progDiscTask.extent = tempExtent.clone();
                    update = true;
                }
            }
        }

        if (Math.abs(progDiscTask.zoomLevel - activePane.getZoomLevel()) > 0.00000000000001) {
            progDiscTask.zoomLevel = activePane.getZoomLevel();
            progDiscTask.extent = currViewExtents.clone();
            Tracer.print("Changed zoom level");
            update = true;
        }

        if (progDiscTask.canvasWidth == 0
                || canvasBounds.width != progDiscTask.canvasWidth) {
            progDiscTask.canvasWidth = canvasBounds.width;
            Tracer.print("Changed canvas width");
            update = true;
        }

        // if(canvasBounds.height != progDiscTask.canvasWidth)

        updateNextPaint = false;
        Tracer.print("< Exit");
        return update;
    }

    public void queueListOfStationsToBeDisclosed(DataTime time,
            Collection<Station> listOfStationsToDisclose) {
        Tracer.print("> Entry");
        if (listOfStationsToDisclose == null
                || listOfStationsToDisclose.isEmpty() || time == null)
            return;

        QueueEntry newQueueEntry = new QueueEntry(time,
                listOfStationsToDisclose);
        timeLogger.append("Queueing stations (for PD) with reftime "
                + time.toString());
        Tracer.print("Queueing " + listOfStationsToDisclose.size()
                + " stations (for PD) with reftime " + shortTimeString(time));
        queueOfStationsToBeDisclosed.add(newQueueEntry);
        scheduleProgressiveDisclosure();
        Tracer.print("< Exit");
    }

    public void setDensity(double density) {
        Tracer.print("> Entry");
        if (density != progDiscTask.density) {
            progDiscTask.density = density;
            updateNextPaint = true;
        }
        Tracer.print("< Exit");
    }

    public void setPixelSizeHint(int pixelSizeHint) {
        Tracer.print("> Entry");
        if (pixelSizeHint != progDiscTask.pixelSizeHint) {
            progDiscTask.pixelSizeHint = pixelSizeHint;
            updateNextPaint = true;
        }
        Tracer.print("< Exit");
    }

    public void setPlotWidth(double plotWidth) {
        Tracer.print("> Entry");
        if (plotWidth != progDiscTask.plotWidth) {
            progDiscTask.plotWidth = plotWidth;
            updateNextPaint = true;
        }
        Tracer.print("< Exit");
    }

    public synchronized Collection<Station> calculateStaticProgDiscDistancesForStations(
            Collection<Station> collectionOfStations, int dynStations) {
        Tracer.print("> Entry");
        if (collectionOfStations == null || collectionOfStations.isEmpty()) {

            return null;
        }
        int size = collectionOfStations.size();
        Coordinate[] latLonArray = new Coordinate[size];
        Integer[] goodnessArray = new Integer[size];
        Double[] distArray = new Double[size];
        int index = 0;
        for (Station station : collectionOfStations) {
            latLonArray[index] = new Coordinate(station.info.longitude,
                    station.info.latitude);
            goodnessArray[index] = station.goodnessValue;
            distArray[index] = station.origDistValue;
            ++index;
        }

        progDisc.setVaJustGoodness(false);
        progDisc.setVaDistPass(dynStations * dynStations / size < 3000.0);
        if ((latLonArray != null && latLonArray.length > 0)
                && (goodnessArray != null && goodnessArray.length > 0)
                && (distArray != null && distArray.length > 0)) {
            progDisc.getVaAdvanced(latLonArray, goodnessArray, distArray);
            index = 0;

            for (Station station : collectionOfStations) {
                station.distValue = distArray[index];
                ++index;
            }
        }
        Tracer.print("< Exit");

        return collectionOfStations;
    }

    private void scheduleProgressiveDisclosure() {
        Tracer.print("> Entry");
        if (queueOfStationsToBeDisclosed.peek() == null)
            Tracer.print("No stations in queue for PD...");
        timeLogger.append("No stations in queue for PD...");
        Semaphore sm = new Semaphore(1);
        while (queueOfStationsToBeDisclosed.peek() != null) {
            sm.acquireUninterruptibly();
            QueueEntry currentEntry = queueOfStationsToBeDisclosed.poll();

            if (currentEntry == null)
                continue;
            synchronized (currentEntry) {

                Collection<Station> stnColl = currentEntry.getStations();
                synchronized (stnColl) {
                    try {
                        progDiscTask.stations = stnColl.toArray(new Station[0]);
                    } catch (Exception e) {
                        sm.release();
                    }
                }
            }
            sm.release();
            progDiscTask.time = new DataTime(currentEntry.getDataTime()
                    .getRefTime());

            Task task = progDiscTask.clone();
            Tracer.print("About to schedule progressive disclosure for frame "
                    + shortTimeString(currentEntry.getDataTime()));
            timeLogger
                    .append("About to schedule progressive disclosure for frame "
                            + currentEntry.getDataTime().toString());
            progDiscJobPool.schedule(task);
        }
        Tracer.print("< Exit");

    }

    public void setMapDescriptor(NCMapDescriptor theMapDescriptor) {
        progDiscTask.descriptor = theMapDescriptor;
    }

    private String shortTimeString(DataTime dt) {
        // temporary -- combine with
        // NcPlotResource2.FrameData.getShortFrameTime(), somehow
        return Tracer.shortTimeString(dt);
    }

}
