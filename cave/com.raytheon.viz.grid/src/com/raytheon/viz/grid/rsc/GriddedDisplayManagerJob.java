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
package com.raytheon.viz.grid.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.core.contours.Activator;
import com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay;
import com.raytheon.viz.grid.rsc.AbstractMapVectorResource.GriddedDisplayRequest;

/**
 * Uses the Eclipse Job Manager to manage a thread that is used to retrieve
 * gridded display information. A priority queue is used to keep track of the
 * date/time(s) that display information needs to be retrieved for. Once the job
 * is started it will continue until either: the priority queue is empty or it
 * receives a "Kill" signal because the user clicked on the clear button or
 * removed a layer with displays from the map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 09, 2010            rjpeter    Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GriddedDisplayManagerJob extends Job {

    /**
     * requestMap is an LRU, we will process the requests in the reverse so that
     * the MRU request is done next.
     */
    private LinkedHashMap<DataTime, GriddedDisplayRequest> requestMap = new LinkedHashMap<DataTime, GriddedDisplayRequest>(
            16, 0.75f, true);

    private Map<DataTime, AbstractGriddedDisplay<?>> responseMap = new HashMap<DataTime, AbstractGriddedDisplay<?>>();

    private boolean shutdownIndicator = false;

    public class RenderableDisposer implements Runnable {
        private AbstractGriddedDisplay<?> display;

        public RenderableDisposer(AbstractGriddedDisplay<?> display) {
            this.display = display;
        }

        @Override
        public void run() {
            this.display.dispose();
            this.display = null;
        }
    }

    public GriddedDisplayManagerJob() {
        super("Processing Gridded Displays");
    }

    public Collection<AbstractGriddedDisplay<?>> getResponseMapAsCollection() {
        synchronized (responseMap) {
            // Copy to avoid concurrent modification if caller iterates.
            return new ArrayList<AbstractGriddedDisplay<?>>(
                    this.responseMap.values());
        }
    }

    public void clearResponseMap() {
        Display display = PlatformUI.getWorkbench().getDisplay();
        synchronized (responseMap) {
            for (AbstractGriddedDisplay<?> griddedDisplay : this.responseMap
                    .values()) {
                display.asyncExec(new RenderableDisposer(griddedDisplay));
            }
            this.responseMap.clear();
        }
    }

    public void addRequest(DataTime dt, GriddedDisplayRequest request) {
        synchronized (requestMap) {
            this.requestMap.put(dt, request);
        }
        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }

    }

    public void removeDataTime(DataTime dt) {
        Display display = PlatformUI.getWorkbench().getDisplay();
        synchronized (requestMap) {
            this.requestMap.remove(dt);
        }

        synchronized (responseMap) {
            AbstractGriddedDisplay<?> griddedDisplay = this.responseMap
                    .remove(dt);
            if (griddedDisplay != null) {
                display.asyncExec(new RenderableDisposer(griddedDisplay));
                griddedDisplay = null;
            }
        }
    }

    public void makeHighestPriority(DataTime dt) {
        synchronized (requestMap) {
            requestMap.get(dt);
        }
    }

    public AbstractGriddedDisplay<?> request(DataTime dt) {
        AbstractGriddedDisplay<?> rval = null;
        synchronized (responseMap) {
            rval = responseMap.get(dt);
        }

        if (rval == null) {
            synchronized (requestMap) {
                /* The Request Has Not Been Serviced Yet */
                if (this.requestMap.containsKey(dt)) {
                    // Bump up the priority
                    requestMap.get(dt);
                }
            }
        }

        return rval;
    }

    public void reset() {
        this.shutdownIndicator = false;
    }

    public void shutdown() {
        this.shutdownIndicator = true;
    }

    public void disposeInternal() {
        this.clearResponseMap();
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (this.requestMap.size() > 0) {
            if (this.shutdownIndicator) {
                this.requestMap.clear();
                break;
            }

            DataTime dt = null;
            GriddedDisplayRequest req = null;

            Entry<DataTime, GriddedDisplayRequest> nextTask = null;
            synchronized (requestMap) {
                // Grab the first key(most recently accessed)
                Iterator<Entry<DataTime, GriddedDisplayRequest>> it = this.requestMap
                        .entrySet().iterator();
                while (it.hasNext()) {
                    nextTask = it.next();
                }
            }
            if (nextTask != null) {
                dt = nextTask.getKey();
                req = nextTask.getValue();
            }

            if (dt == null || req == null) {
                break;
            }
            try {
                AbstractGriddedDisplay<?> griddedDisplay = req.getData();
                synchronized (requestMap) {
                    GriddedDisplayRequest request = this.requestMap.get(dt);
                    synchronized (responseMap) {
                        AbstractGriddedDisplay<?> oldDisplay = responseMap
                                .remove(dt);
                        if (oldDisplay != null) {
                            // Make sure we dispose existing renderable
                            Display display = PlatformUI.getWorkbench()
                                    .getDisplay();
                            display.asyncExec(new RenderableDisposer(oldDisplay));
                        }
                        if (request != null) {
                            // If request is null then this was canceled
                            this.responseMap.put(dt, griddedDisplay);
                        }
                    }
                    if (request == req) {
                        this.requestMap.remove(dt);
                    } else {
                        // We will process this time again with the new request.
                    }
                }
            } catch (Throwable e) {
                return new Status(Status.ERROR, Activator.PLUGIN_ID,
                        "Error creating contours", e);
            }
        }

        return Status.OK_STATUS;
    }
}
