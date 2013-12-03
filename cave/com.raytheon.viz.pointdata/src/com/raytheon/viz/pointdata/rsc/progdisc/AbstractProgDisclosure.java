package com.raytheon.viz.pointdata.rsc.progdisc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.rsc.PlotResource2.Station;

/**
 * 
 * Abstract progressive disclosure job
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2011            bsteffen    Initial creation
 * Jul 01, 2011            njensen     Added queuing
 * Dec 02, 2013 2573       njensen     Smarter queuing
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractProgDisclosure extends Job {

    public static interface IProgDiscListener {
        public void disclosureComplete(DataTime time, List<Station> disclosed);
    }

    protected static class Task {

        IMapDescriptor descriptor;

        double magnification = 1.0;

        double density = 1.0;

        IExtent extent;

        float zoomLevel = 1.0f;

        int canvasWidth;

        int pixelSizeHint = 90;

        double plotWidth = 80;

        List<Station> stations;

        DataTime time = null;

        @Override
        protected Task clone() {
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
            if (time != null) {
                task.time = time.clone();
            }
            return task;
        }

    }

    protected static class QueueEntry {
        List<Station> stations;

        DataTime time;
    }

    private boolean updateNextPaint = false;

    private IProgDiscListener listener;

    private Task nextTask = new Task();

    private Deque<QueueEntry> queue = new LinkedList<QueueEntry>();

    public AbstractProgDisclosure(IProgDiscListener listener) {
        super("progressive disclosure");
        this.listener = listener;
        this.setSystem(true);
    }

    protected abstract List<Station> progDisc(Task task);

    public void setDescriptor(IMapDescriptor descriptor) {
        nextTask.descriptor = descriptor;
    }

    protected void update() {
        schedule();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        QueueEntry entry = null;
        synchronized (queue) {
            entry = queue.poll();
        }

        while (entry != null) {
            // Clone the current state so it does not change in the middle of
            // the
            // algorithm
            Task task = nextTask.clone();
            task.stations = entry.stations;
            task.time = entry.time;
            if (task != null && task.extent != null && task.time != null
                    && !task.stations.isEmpty()) {
                listener.disclosureComplete(task.time, progDisc(task));
            }
            synchronized (queue) {
                entry = queue.poll();
            }
        }

        return Status.OK_STATUS;
    }

    public void setMagnification(double magnification) {
        if (magnification != nextTask.magnification) {
            nextTask.magnification = magnification;
            updateNextPaint = true;
        }
    }

    public void setDensity(double density) {
        if (density != nextTask.density) {
            nextTask.density = density;
            updateNextPaint = true;
        }
    }

    public void setPixelSizeHint(int pixelSizeHint) {
        if (pixelSizeHint != nextTask.pixelSizeHint) {
            nextTask.pixelSizeHint = pixelSizeHint;
            updateNextPaint = true;
        }
    }

    public void setPlotWidth(double plotWidth) {
        if (plotWidth != nextTask.plotWidth) {
            nextTask.plotWidth = plotWidth;
            updateNextPaint = true;
        }
    }

    /**
     * Requests progressive disclosure to run for the specified time with the
     * available stations
     * 
     * @param stations
     * @param time
     */
    public void update(Collection<Station> stations, DataTime time) {
        update(stations, time, false);
    }

    /**
     * Requests progressive disclosure to run for the specified time with the
     * available stations. If highPriority is true, the next disclosure that is
     * run will be this one.
     * 
     * @param stations
     * @param time
     * @param highPriority
     */
    public void update(Collection<Station> stations, DataTime time,
            boolean highPriority) {
        QueueEntry entry = new QueueEntry();
        entry.stations = new ArrayList<Station>(stations);
        entry.time = time;
        synchronized (queue) {
            // time corresponds to a frame and there's no point in running
            // an older progressive disclosure task for the same time
            // if a newer one is requested, so remove the older one
            // if there is more than one for the same time
            Iterator<QueueEntry> itr = queue.iterator();
            while (itr.hasNext()) {
                QueueEntry alreadyQueued = itr.next();
                if (entry.time.equals(alreadyQueued.time)) {
                    itr.remove();
                    break;
                }
            }
            if (highPriority) {
                queue.addFirst(entry);
            } else {
                queue.addLast(entry);
            }
        }
        update();
    }

    public boolean updateProperties(PaintProperties paintProps) {
        boolean update = updateNextPaint;
        if (paintProps.getZoomLevel() != nextTask.zoomLevel) {
            nextTask.zoomLevel = paintProps.getZoomLevel();
            update = true;
        }
        if (!paintProps.getView().getExtent().equals(nextTask.extent)) {
            nextTask.extent = paintProps.getView().getExtent().clone();
            update = true;
        }
        if (paintProps.getCanvasBounds().width != nextTask.canvasWidth) {
            nextTask.canvasWidth = paintProps.getCanvasBounds().width;
            update = true;
        }
        updateNextPaint = false;
        return update;
    }

    public double getScreenToWorldRatio() {
        return nextTask.canvasWidth / nextTask.extent.getWidth();
    }

    public boolean isDone() {
        return getState() != Job.RUNNING && getState() != Job.WAITING;
    }

    public void shutdown() {
        this.cancel();
        nextTask = new Task();
    }

}
