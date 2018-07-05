package gov.noaa.gsd.viz.ensemble.control;

import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

/**
 * (This class only interacts with CAVE if the Ensemble Tool is editable.)
 * 
 * 
 * This class handles the 'registering' of incoming resources.
 * 
 * 
 * >>> Use Case <<<
 * 
 * When the ensemble tool is "editable" and the user loads product resources,
 * this class maintains, and allows access, to those resources.
 * 
 * Also, when the ensemble tool is "editable" there is guaranteed to be a tool
 * layer (<code>EnsembleToolLayer</code>) that offers a proxy/delegator
 * interface to those resources.
 * 
 * This class is fed these resources through the renderable display customizer
 * <code>EnsembleToolDisplayCustomizer</code> but only if they are deemed
 * "compatible" resources; the ensemble tool layer controls whether the resource
 * is compatible via the <code>EnsembleToolLayer.isResourceCompatible </code>
 * method.
 * 
 * 
 * @author jing
 * @author polster
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2014      5056       jing     Initial creation
 * Nov 2015      12977     polster   Make ingest poll asynchronously
 * Dec 2016      19325      jing     Add image capability for ensemble grid display
 * Mar 01,2017   19443    polster    Fix Time Series load problem; class cast exception
 * Mar 17 2017   19325    polster    Refactored for IResourceGroup; renamed for its lesser role.
 * 
 *          </pre>
 */

public class EnsembleResourceIngester {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleResourceIngester.class);

    /*
     * The single instance of this resource manager.
     */
    private static EnsembleResourceIngester SINGLETON = null;

    /**
     * Get the resource manager singleton.
     * 
     * @return
     */
    public static EnsembleResourceIngester getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new EnsembleResourceIngester();
        }
        return SINGLETON;
    }

    private ArrayBlockingQueue<AbstractVizResource<?, ?>> incomingSpooler = null;

    protected PollForIncomingResources registerIncomingResources = null;

    /**
     * The listeners to any resource, GUI... change event Register to any bus or
     * event list for to catch interesting events
     */
    private EnsembleResourceIngester() {

        incomingSpooler = new ArrayBlockingQueue<>(500);

    }

    public void startSpooler() {
        registerIncomingResources = new PollForIncomingResources(
                "Ensemble Resource Spooler");
        registerIncomingResources.setSystem(true);
        registerIncomingResources.setPriority(Job.SHORT);
        registerIncomingResources.schedule();
    }

    /*
     * Viz resources register with the ensemble tool via this thread-safe FIFO
     * queue.
     */
    protected void addResourceForRegistration(AbstractVizResource<?, ?> rsc) {
        incomingSpooler.add(rsc);
        startSpooler();
    }

    /**
     * Register a loaded model product in ensemble status
     * 
     * @param rsc
     *            - the loaded product resource
     * @param guiUpdate
     *            - if need update GUI
     */
    public synchronized void register(AbstractVizResource<?, ?> rsc) {

        if (rsc == null) {
            return;
        }
        if (!EnsembleTool.isCompatibleResource(rsc)) {
            return;
        }

        EnsembleToolLayer toolLayer = EnsembleTool.getToolLayer(rsc);
        if (toolLayer == null) {
            return;
        }

        toolLayer.getResourceList().add(rsc);
        rsc.getDescriptor().getResourceList().removeRsc(rsc);

    }

    /**
     * Completely delete all owned resources
     */
    public void dispose() {

        if (incomingSpooler != null) {
            incomingSpooler.clear();
            incomingSpooler = null;
        }

        SINGLETON = null;
    }

    /**
     * This job sleeps for a poll-period and is only active when there are
     * resources being registered.
     * 
     * It runs for as long as the Ensemble Tool is open (i.e. not disposed). It
     * wakes when new resources need to be registered.
     * 
     */
    private class PollForIncomingResources extends Job {

        private IStatus status = Status.OK_STATUS;

        private AbstractVizResource<?, ?> nextRsc = null;

        public PollForIncomingResources(String name) {
            super(name);
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            if (!monitor.isCanceled()) {
                nextRsc = incomingSpooler.poll();
                EnsembleResourceIngester.getInstance().register(nextRsc);
            }
            if (monitor.isCanceled()) {
                status = Status.CANCEL_STATUS;
            }

            return status;

        }
    }
}
