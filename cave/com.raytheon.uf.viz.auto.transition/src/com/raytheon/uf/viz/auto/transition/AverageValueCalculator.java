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
package com.raytheon.uf.viz.auto.transition;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.measure.Measure;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Async calculation of the average value of a resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 10, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AverageValueCalculator extends Job {

    private static final int NUM_POINTS = 5;

    private final Object lock = new Object();

    private final Set<CalculatorListener> listeners = new CopyOnWriteArraySet<>();
    
    private Task nextTask;

    private Task lastTask;

    private double value = Double.NaN;

    public AverageValueCalculator() {
        super("Calculating Resource Average Value");
    }

    public boolean calculate(AbstractVizResource<?, ?> resource, DataTime time,
            IExtent extent) {
        Task newTask = new Task(resource, time, extent);
        synchronized (lock) {
            if (newTask.equals(lastTask)) {
                nextTask = null;
                return true;
            } else {
                nextTask = newTask;
                schedule();
                return false;
            }
        }
    }

    public double getValue() {
        return value;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        Task task = null;
        synchronized (lock) {
            task = nextTask;
        }
        if (task == null) {
            return Status.OK_STATUS;
        }
        double value = calculateAverage(task.resource, task.time, task.extent);
        synchronized (lock) {
            if (task.equals(nextTask)) {
                this.value = value;
                lastTask = task;
                nextTask = null;
            }
        }
        for (CalculatorListener listener : listeners) {
            listener.calculationComplete();
        }
        return Status.OK_STATUS;
    }

    public void addListener(CalculatorListener listener) {
        listeners.add(listener);
    }

    public void removeListener(CalculatorListener listener) {
        listeners.remove(listener);
    }

    /**
     * Calculate the average value of a resource at a particular time for the
     * specified area. Currently this just samples 25 points evenly spaced
     * throughout the visible area as proposed by Jordan Gerth.
     */
    public static double calculateAverage(AbstractVizResource<?, ?> resource,
            DataTime time, IExtent extent) {
        if (!(resource instanceof Interrogatable)) {
            return Double.NaN;
        }
        Interrogatable interrogatable = (Interrogatable) resource;
        IDescriptor descriptor = resource.getDescriptor();
        double sum = 0.0;
        int k = 0;
        for (int i = 1; i < NUM_POINTS; i++) {
            for (int j = 1; j < NUM_POINTS; j++) {
                double mx = extent.getMinX()
                        + (extent.getMaxX() - extent.getMinX()) / NUM_POINTS
                        * i;
                double my = extent.getMinY()
                        + (extent.getMaxY() - extent.getMinY()) / NUM_POINTS
                        * j;
                double[] p2w = descriptor.pixelToWorld(new double[] { mx, my });
                ReferencedCoordinate coord = new ReferencedCoordinate(
                        new Coordinate(p2w[0], p2w[1]));
                Measure<?, ?> value = Interrogator.interrogateSingle(
                        interrogatable, coord, time, Interrogator.VALUE);
                if (value != null && value.getValue() instanceof Number) {
                    double measuredValue = ((Number) value.getValue())
                            .doubleValue();
                    if (!Double.isNaN(measuredValue)) {
                        sum += measuredValue;
                        k++;
                    }
                }
            }
        }
        if (k > NUM_POINTS) {
            return sum / k;
        } else {
            return Double.NaN;
        }
    }

    private static class Task {

        public final AbstractVizResource<?, ?> resource;

        public final DataTime time;

        public final IExtent extent;

        public Task(AbstractVizResource<?, ?> resource, DataTime time,
                IExtent extent) {
            this.resource = resource;
            this.time = time;
            this.extent = extent;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((extent == null) ? 0 : extent.hashCode());
            result = prime * result
                    + ((resource == null) ? 0 : resource.hashCode());
            result = prime * result + ((time == null) ? 0 : time.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Task other = (Task) obj;
            if (extent == null) {
                if (other.extent != null)
                    return false;
            } else if (!extent.equals(other.extent))
                return false;
            if (resource == null) {
                if (other.resource != null)
                    return false;
            } else if (!resource.equals(other.resource))
                return false;
            if (time == null) {
                if (other.time != null)
                    return false;
            } else if (!time.equals(other.time))
                return false;
            return true;
        }

    }
    
    public static interface CalculatorListener{
        
        public void calculationComplete();
    }

}
