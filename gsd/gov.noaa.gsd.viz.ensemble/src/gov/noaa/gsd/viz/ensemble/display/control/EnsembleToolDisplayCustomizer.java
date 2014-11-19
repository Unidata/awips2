package gov.noaa.gsd.viz.ensemble.display.control;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInitListener;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.grid.rsc.general.GridResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.IRenderableDisplayCustomizer;

/**
 * Notice ensemble resource manager a resource has been changed. TODO This
 * description needs to be improved once we do an improvement DR.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- -----------    --------------------------
 * Dec 9, 2014    5056    epolster jing      Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */
public class EnsembleToolDisplayCustomizer implements
        IRenderableDisplayCustomizer {

    /**
     * The ensemble marked resource list holds the loaded and generated
     * resources and flags, is used by ensemble display, GUI and calculation.
     */
    static final private List<ResourcePair> batchedPairs = new CopyOnWriteArrayList<ResourcePair>();

    /** List of listeners we have for renderable displays */
    private final List<EnsembleToolRscLoadListener> listeners = new ArrayList<EnsembleToolRscLoadListener>();

    @Override
    public void customizeDisplay(IRenderableDisplay display) {

        boolean add = true;
        for (EnsembleToolRscLoadListener listener : listeners) {
            if (display == listener.getDisplay()) {
                add = false;
                break;
            }
        }

        if (add) {
            listeners.add(new EnsembleToolRscLoadListener(display));
        }
    }

    @Override
    public void uncustomizeDisplay(IRenderableDisplay display) {
        EnsembleToolRscLoadListener toRemove = null;
        for (EnsembleToolRscLoadListener listener : listeners) {
            if (listener.getDisplay() == display) {
                toRemove = listener;
                break;
            }
        }
        if (toRemove != null) {
            toRemove.dispose();
            listeners.remove(toRemove);
        }
    }

    private static class EnsembleToolRscLoadListener implements AddListener,
            RemoveListener, IInitListener {

        // for batching purposes, keep track of the amount of time spent between
        // requests
        private static long LAST_TIME = 0;

        // maximum wait period for batching
        private final int ALLOW_FOR_BUNCHED_REQUESTS_PERIOD = 1400;

        // thread which acts upon batched requests
        private Thread forceReset = null;

        // the display we are listening to
        private final IRenderableDisplay display;

        private boolean resourcesAdded = false;

        public EnsembleToolRscLoadListener(IRenderableDisplay display) {
            this.display = display;
            IDescriptor descriptor = display.getDescriptor();
            ResourceList list = descriptor.getResourceList();
            if (hasCompatibleResource(list)) {
                addResources(descriptor);
            }
            list.addPostAddListener(this);
            list.addPostRemoveListener(this);
        }

        public void dispose() {
            ResourceList list = display.getDescriptor().getResourceList();
            list.removePostAddListener(this);
            list.removePostRemoveListener(this);
        }

        /**
         * Notice a resource has been removed. (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
         *      (com.raytheon.uf.viz.core.drawables.ResourcePair) TODO: This
         *      approach will be improved in an upcoming DR
         */
        @Override
        public synchronized void notifyRemove(ResourcePair rp)
                throws VizException {
            /**
             * Pass,if the resource is not interested by ensemble tool
             * 
             */
            if (!isCompatibleResource(rp)) {
                return;
            }
            /**
             * Remove it from the resource manager and update GUI Should notice
             * GUI
             */
            if (getEditor() != null) {
                EnsembleResourceManager.getInstance().syncRegisteredResource(
                        getEditor());
            }

        }

        /**
         * Notice a resource has been added. (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com
         *      .raytheon .uf.viz.core.drawables.ResourcePair)
         */
        @Override
        public void notifyAdd(ResourcePair rp) throws VizException {

            /**
             * Return if the resource is not interested by ensemble tool
             * 
             */
            if (!isCompatibleResource(rp)) {
                return;
            }

            // add the resource pair to the staging list ...
            addButNoDuplicates(rp);

            // but wait for other resource pairs to catch up ...
            if (((LAST_TIME == 0) || ((System.currentTimeMillis() - LAST_TIME) > ALLOW_FOR_BUNCHED_REQUESTS_PERIOD))) {

                if (LAST_TIME == 0) {
                    // TODO: wait for the resource (e.g. unit conversion) to
                    // update. We probably need to look for a better solution.
                    try {
                        Thread.sleep(150);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }

                LAST_TIME = System.currentTimeMillis();

                /**
                 * Register the resource in to the resource manager
                 */
                if (forceReset != null) {
                    synchronized (forceReset) {
                        // restart the timer for another interval to pass
                        // before forcing a "final" refresh
                        if (forceReset.isAlive()) {
                            forceReset.interrupt();
                        }
                        forceReset = null;
                        forceReset = new Thread(new ForceRefreshIfNecessary(
                                ALLOW_FOR_BUNCHED_REQUESTS_PERIOD));
                        forceReset.start();
                    }
                } else {
                    forceReset = new Thread(new ForceRefreshIfNecessary(
                            ALLOW_FOR_BUNCHED_REQUESTS_PERIOD));
                    forceReset.start();
                }
            }
        }

        // add the resource pair if it is not already in the list.
        private void addButNoDuplicates(ResourcePair rp) {

            boolean found = false;
            for (ResourcePair orp : EnsembleToolDisplayCustomizer.batchedPairs) {
                if (orp.getResource().getName()
                        .compareTo(rp.getResource().getName()) == 0) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                EnsembleToolDisplayCustomizer.batchedPairs.add(rp);
            }
        }

        private class ForceRefreshIfNecessary implements Runnable {

            int waitForReset = 0;

            ForceRefreshIfNecessary(int waitTime) {
                waitForReset = waitTime;
            }

            @Override
            public void run() {

                try {
                    Thread.sleep(waitForReset);

                    addResourceToManager(EnsembleToolDisplayCustomizer.batchedPairs);

                } catch (InterruptedException e) {
                    /* ignore */
                }
            }
        }

        /**
         * Pass a resource into the ensemble resource manager.
         * 
         * @param resourcePairs
         */
        private void addResourceToManager(List<ResourcePair> resourcePairs) {

            Iterator<ResourcePair> pairsIter = resourcePairs.iterator();
            ResourcePair rp = null;
            while (pairsIter.hasNext()) {

                rp = pairsIter.next();
                if (pairsIter.hasNext()) {
                    EnsembleResourceManager.getInstance().registerResource(
                            rp.getResource(), getEditor(), false);

                } else {
                    EnsembleResourceManager.getInstance().registerResource(
                            rp.getResource(), getEditor(), true);
                }
            }
        }

        /**
         * Get the current Editor.
         * 
         * @return
         */
        private static AbstractEditor getEditor() {
            AbstractEditor editor = (AbstractEditor) EditorUtil
                    .getActiveEditor();
            return editor;
        }

        /**
         * 
         * @param list
         * @return
         */

        private boolean hasCompatibleResource(ResourceList list) {
            for (ResourcePair rp : list) {
                if (isCompatibleResource(rp)) {
                    return true;
                }
            }
            return false;
        }

        private boolean isCompatibleResource(ResourcePair rp) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource != null) {
                if (resource instanceof GridResource
                        || resource instanceof TimeSeriesResource) {
                    return true;
                }
            }
            return false;
        }

        private synchronized void addResources(IDescriptor descriptor) {
            if (!resourcesAdded) {
                ResourceList list = descriptor.getResourceList();
                list.instantiateResources(descriptor, true);
                resourcesAdded = true;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.rsc.IInitListener#inited(com.raytheon.uf
         * .viz.core.rsc.AbstractVizResource)
         */
        @Override
        public synchronized void inited(AbstractVizResource<?, ?> rsc) {
            if (rsc instanceof GridResource) {
                if (!resourcesAdded) {
                    addResources(rsc.getDescriptor());
                }
            }
        }

        /**
         * Get the IRenderableDisplay
         * 
         * @return the display
         */
        public IRenderableDisplay getDisplay() {
            return display;
        }
    }

}
