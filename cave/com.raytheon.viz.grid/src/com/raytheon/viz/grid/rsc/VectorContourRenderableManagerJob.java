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
import com.raytheon.viz.core.contours.ContourRenderable;
import com.raytheon.viz.grid.rsc.AbstractMapVectorResource.VectorContourRenderable;

/**
 * Uses the Eclipse Job Manager to manage a thread that is used to retrieve
 * contour information.A priority queue is used to keep track of the
 * date/time(s) that contour information needs to be retrieved for. Once the job
 * is started it will continue until either: the priority queue is empty or it
 * receives a "Kill" signal because the user clicked on the clear button or
 * removed a layer with contours from the map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2010            bkowal     Initial creation
 * Jul 9, 2010  #6851      bkowal     We let the UI-Thread dispose the renderables
 *                                    now.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class VectorContourRenderableManagerJob extends Job {

    /**
     * requestMap is an LRU, we will process the requests in the reverse so that
     * the MRU request is done next.
     */
    private LinkedHashMap<DataTime, VectorContourRenderable> requestMap = new LinkedHashMap<DataTime, VectorContourRenderable>(
            16, 0.75f, true);

    private Map<DataTime, VectorContourRenderable> responseMap = new HashMap<DataTime, VectorContourRenderable>();

    private boolean shutdownIndicator = false;

    public class RenderableDisposer implements Runnable {
        private ContourRenderable renderable;

        public RenderableDisposer(ContourRenderable renderable) {
            this.renderable = renderable;
        }

        @Override
        public void run() {
            this.renderable.dispose();
            this.renderable = null;
        }
    }

    public VectorContourRenderableManagerJob() {
        super("Processing Contours");
    }

    public Collection<VectorContourRenderable> getRequestMapAsCollection() {
        synchronized (requestMap) {
            // Copy to avoid concurrent modification if caller iterates.
            return new ArrayList<VectorContourRenderable>(
                    this.requestMap.values());
        }
    }

    public Collection<VectorContourRenderable> getResponseMapAsCollection() {
        synchronized (responseMap) {
            // Copy to avoid concurrent modification if caller iterates.
            return new ArrayList<VectorContourRenderable>(
                    this.responseMap.values());
        }
    }

    public void clearResponseMap() {
        Display display = PlatformUI.getWorkbench().getDisplay();
        synchronized (responseMap) {
            for (ContourRenderable renderable : this.responseMap.values()) {
                display.asyncExec(new RenderableDisposer(renderable));
            }
            this.responseMap.clear();
        }
    }

    public void addRequest(DataTime dt, VectorContourRenderable cr) {
        synchronized (requestMap) {
            this.requestMap.put(dt, cr);
        }
        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }

    }

    public void removeDataTime(DataTime dt) {
        Display display = PlatformUI.getWorkbench().getDisplay();
        synchronized (requestMap) {
            VectorContourRenderable renderable = this.requestMap.remove(dt);
            if (renderable != null) {
                display.asyncExec(new RenderableDisposer(renderable));
            }
        }

        synchronized (responseMap) {
            ContourRenderable renderable = this.responseMap.remove(dt);
            if (renderable != null) {
                display.asyncExec(new RenderableDisposer(renderable));
                renderable = null;
            }
        }
    }

    public void makeHighestPriority(DataTime dt) {
        synchronized (requestMap) {
            requestMap.get(dt);
        }
    }

    public ContourRenderable request(DataTime dt) {
        synchronized (responseMap) {
            if (this.responseMap.containsKey(dt)) {
                return this.responseMap.get(dt);
            }
        }

        synchronized (requestMap) {
            /* The Request Has Not Been Serviced Yet */
            if (this.requestMap.containsKey(dt)) {
                // Bump up the priority
                requestMap.get(dt);
            }
        }

        return null;
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
            VectorContourRenderable cr = null;

            Entry<DataTime, VectorContourRenderable> nextTask = null;
            synchronized (requestMap) {
                // Grab the last key(most recently accessed)
                Iterator<Entry<DataTime, VectorContourRenderable>> it = this.requestMap
                        .entrySet().iterator();
                while (it.hasNext()) {
                    nextTask = it.next();
                }
            }
            if (nextTask != null) {
                dt = nextTask.getKey();
                cr = nextTask.getValue();
            }

            if (dt == null || cr == null) {
                break;
            }
            try {
                cr.getData();
                synchronized (requestMap) {
                    ContourRenderable request = this.requestMap.get(dt);
                    synchronized (responseMap) {
                        ContourRenderable oldCR = responseMap.remove(dt);
                        if (oldCR != null) {
                            // Make sure we dispose existing renderable
                            Display display = PlatformUI.getWorkbench()
                                    .getDisplay();
                            display.asyncExec(new RenderableDisposer(oldCR));
                        }
                        if (request != null) {
                            // If request is null then this was canceled
                            this.responseMap.put(dt, cr);
                        }
                    }
                    if (request == cr) {
                        this.requestMap.remove(dt);
                    } else {
                        // We will process this time again with the new cr.
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
