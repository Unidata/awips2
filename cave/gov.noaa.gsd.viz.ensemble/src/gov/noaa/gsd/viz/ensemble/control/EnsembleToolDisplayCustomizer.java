package gov.noaa.gsd.viz.ensemble.control;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.IInitListener;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.perspectives.IRenderableDisplayCustomizer;

import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

/**
 * The ensemble tool renderable display customizer classes contain the event
 * handlers that keeps track of when new displays are created or removed, and
 * listens also for when resources get added or removed from those displays.
 * 
 * TODO: We need verify whether we need only keep track of displays that are
 * created when the ensemble tool is "on". This is problematic for the very
 * first display because the ensemble tool would like to "know" of that display
 * but is not "on" by default. So how do we know that the new display is the
 * very first display created (i.e. initial "Map" editor display).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- -----------    --------------------------
 * Dec 9, 2014    5056     polster jing      Initial creation
 * Dec 1, 2017    41520    polster           Fixed approach to ingest registration
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */
public class EnsembleToolDisplayCustomizer
        implements IRenderableDisplayCustomizer {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleToolDisplayCustomizer.class);

    /* List of listeners we have for renderable displays */
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

        /*
         * If the ensemble tool is running, and if the active editor has an
         * ensemble tool layer in "editable on" state, then let the ensemble
         * tool know to prepare for a the new editor so it can pull in any
         * incoming resources into it.
         */
        if (EnsembleTool.isToolRunning()) {
            if (EnsembleTool.getInstance().isToolEditable()) {
                EnsembleTool.getInstance().prepareForNewEditor();
            }
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

    private static class EnsembleToolRscLoadListener
            implements AddListener, RemoveListener, IInitListener {

        // the display we are listening to
        private final IRenderableDisplay display;

        public EnsembleToolRscLoadListener(IRenderableDisplay display) {
            this.display = display;
            IDescriptor descriptor = display.getDescriptor();
            ResourceList list = descriptor.getResourceList();
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
        public void notifyRemove(ResourcePair rp) throws VizException {

            /**
             * The EnsembleResourceManager already listens for a dispose event
             * on all the resources it owns.
             */
        }

        /**
         * Notice a resource has been added. (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com
         *      .raytheon .uf.viz.core.drawables.ResourcePair)
         */
        @Override
        public void notifyAdd(ResourcePair rp) throws VizException {

            EnsembleToolLayer toolLayer = EnsembleTool
                    .getToolLayer(rp.getResource());
            if (toolLayer != null && toolLayer.isEditable()) {

                if ((rp.getResource()
                        .hasCapability(EditableCapability.class) == true)
                        && (!(rp.getResource() instanceof EnsembleToolLayer))) {
                    EnsembleTool.getInstance().setForeignEditableToolLoading();
                }

                AbstractVizResource<?, ?> rsc = rp.getResource();
                if ((rsc != null) && (toolLayer.isResourceCompatible(rp))) {

                    /*
                     * TODO: Is this going to catch every resource or can one
                     * slip through the cracks?
                     */
                    if (rsc.getStatus() == ResourceStatus.INITIALIZED) {
                        EnsembleResourceIngester.getInstance()
                                .addResourceForRegistration(rsc);
                    } else {
                        rsc.registerListener(this);
                    }

                }
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
            if (rsc != null) {
                EnsembleResourceIngester.getInstance()
                        .addResourceForRegistration(rsc);
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
