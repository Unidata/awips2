package com.raytheon.viz.ui;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * 
 * Loads a bundle to a container. Replaces contents of bundle on the container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class BundleLoader extends Job {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BundleLoader.class);

    public static enum BundleInfoType {
        FILE_LOCATION, XML
    }

    protected static class LoadItem {

        public final IDisplayPane loadTo;

        public final IRenderableDisplay loadFrom;

        public LoadItem(IDisplayPane loadTo, IRenderableDisplay loadFrom) {
            this.loadTo = loadTo;
            this.loadFrom = loadFrom;
        }

    }

    private class InstantiationTask implements Runnable {

        private LoadItem loadItem;

        private InstantiationTask(LoadItem loadItem) {
            this.loadItem = loadItem;
        }

        @Override
        public void run() {
            IDisplayPane loadTo = loadItem.loadTo;
            IRenderableDisplay loadFrom = loadItem.loadFrom;
            if (loadTo.getDescriptor() != loadFrom.getDescriptor()) {
                load(loadTo, loadFrom);
            }
            loadTo.getDescriptor().getResourceList()
                    .instantiateResources(loadTo.getDescriptor(), true);
        }

    }

    protected IDisplayPaneContainer container;

    private Bundle bundle;

    public BundleLoader(IDisplayPaneContainer container, Bundle bundle) {
        this("Bundle Loader", container, bundle);
    }

    protected BundleLoader(String name, IDisplayPaneContainer container,
            Bundle bundle) {
        super(name);
        this.container = container;
        this.bundle = bundle;
    }

    /**
     * Runs the loading synchronously.
     */
    public final void run() {
        run(new NullProgressMonitor());
    }

    @Override
    protected final IStatus run(IProgressMonitor monitor) {
        long t0 = System.currentTimeMillis();
        try {
            loadBundleToContainer(container, bundle);
            if (bundle.getLoopProperties() != null) {
                container.setLoopProperties(bundle.getLoopProperties());
            }

            /** refresh the editor */
            container.refresh();

            if (container instanceof IEditorPart) {
                /** update the history list */
                HistoryList.getInstance().refreshLatestBundle(
                        HistoryList.prepareHistoryEntry(container));
            }

            if (container instanceof IEditorPart) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        VizGlobalsManager.getCurrentInstance().updateUI(
                                container);
                    }
                });
            }
        } catch (VizException e) {
            return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                    "Error loading bundle", e);
        }
        long t2 = System.currentTimeMillis();
        System.out.println("Total bundle retrieval: " + (t2 - t0));
        return Status.OK_STATUS;
    }

    /**
     * Loads a {@link Bundle} onto an {@link IDisplayPaneContainer}
     * 
     * @param container
     * @param bundle
     * @throws VizException
     */
    private final void loadBundleToContainer(IDisplayPaneContainer container,
            Bundle bundle) throws VizException {
        LoadItem[] items = getLoadItems(container, bundle);
        int numItems = items.length;

        if (numItems > 0) {
            Thread[] threads = new Thread[numItems - 1];
            for (int i = 0; i < numItems; ++i) {
                Thread t = new Thread(new InstantiationTask(items[i]));
                if (i == 0) {
                    IRenderableDisplay loadFrom = items[i].loadFrom;
                    IDisplayPane loadTo = items[i].loadTo;
                    AbstractTimeMatcher srcTimeMatcher = loadFrom
                            .getDescriptor().getTimeMatcher();
                    if (srcTimeMatcher != null) {
                        loadTo.getDescriptor().getTimeMatcher()
                                .copyFrom(srcTimeMatcher);
                    }
                    loadTo.getDescriptor().getTimeMatcher().resetMultiload();
                    t.run();
                } else {
                    t.start();
                    threads[i - 1] = t;
                }
            }

            for (Thread t : threads) {
                try {
                    t.join();
                } catch (InterruptedException e) {
                    // Ignore
                }
            }
        }
    }

    /**
     * Gets the pairing of display->pane loading that should occur. Each item
     * will have {@link #load(IDisplayPane, IRenderableDisplay)} called on it
     * 
     * @param container
     * @param bundle
     * @return
     * @throws VizException
     */
    protected LoadItem[] getLoadItems(IDisplayPaneContainer container,
            Bundle bundle) throws VizException {
        IDisplayPane[] containerPanes = container.getDisplayPanes();
        AbstractRenderableDisplay[] bundleDisplays = bundle.getDisplays();

        if (containerPanes.length != bundleDisplays.length) {
            boolean success = ensureOneToOne(container, bundle);
            containerPanes = container.getDisplayPanes();
            if (success) {
                throw new VizException("Unable to load "
                        + bundleDisplays.length
                        + " displays onto container with "
                        + containerPanes.length + " panes");
            }
        }

        int numPanes = containerPanes.length;
        LoadItem[] items = new LoadItem[numPanes];

        List<AbstractRenderableDisplay> orderedDisplays = Arrays
                .asList(bundleDisplays);
        for (int i = 0; i < numPanes; ++i) {
            IDescriptor desc = bundleDisplays[i].getDescriptor();
            if (desc.getTimeMatcher() != null) {
                orderedDisplays = desc.getTimeMatcher().getDisplayLoadOrder(
                        orderedDisplays);
                for (AbstractRenderableDisplay d : orderedDisplays) {
                    d.getDescriptor().synchronizeTimeMatching(desc);
                }
                break;
            }
        }
        if (orderedDisplays.size() != numPanes) {
            throw new VizException(
                    "Error ordering bundle displays. Number of displays returned not same as passed in");
        }

        int j = 0;
        for (AbstractRenderableDisplay display : orderedDisplays) {
            for (int i = 0; i < numPanes; ++i) {
                if (display == bundleDisplays[i]) {
                    items[j] = new LoadItem(containerPanes[i],
                            bundleDisplays[i]);
                }
            }
            ++j;
        }

        return items;
    }

    /**
     * Ensures there is a one to one relationship for number of panes on
     * container to number of displays in bundle
     * 
     * @param container
     * @param bundle
     * @return true of mapping is 1-1, false otherwise
     */
    protected boolean ensureOneToOne(IDisplayPaneContainer container,
            Bundle bundle) {
        IDisplayPane[] containerPanes = container.getDisplayPanes();
        AbstractRenderableDisplay[] bundleDisplays = bundle.getDisplays();

        // Attempt to match 1-1 pane to display
        if (container instanceof IMultiPaneEditor) {
            final IMultiPaneEditor mpe = (IMultiPaneEditor) container;
            final int numPanes = containerPanes.length;
            final int numDisplays = bundleDisplays.length;
            final IDisplayPane[] cPanes = containerPanes;
            final AbstractRenderableDisplay[] bDisplays = bundleDisplays;
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    for (int i = numPanes; i < numDisplays; ++i) {
                        // This will hit if fewer panes than displays
                        mpe.addPane(bDisplays[i]);
                    }
                    for (int i = numDisplays; i < numPanes; ++i) {
                        // This will hit if fewer displays than panes
                        mpe.removePane(cPanes[i]);
                    }
                }
            });
        }
        containerPanes = container.getDisplayPanes();
        return containerPanes.length == bundleDisplays.length;
    }

    /**
     * Loads the renderable display onto the pane
     * 
     * @param loadTo
     * @param loadFrom
     */
    protected void load(final IDisplayPane loadTo,
            final IRenderableDisplay loadFrom) {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                loadTo.setRenderableDisplay(loadFrom);
            }
        });
    }

    /**
     * Gets a bundle object from bundle text, text type is specified by
     * {@link BundleInfoType} passed in
     * 
     * @param bundleText
     * @param variables
     * @param type
     * @return
     * @throws VizException
     */
    public static Bundle getBundle(String bundleText,
            Map<String, String> variables, BundleInfoType type)
            throws VizException {
        /** Make sure bundle text is not null */
        if (bundleText == null) {
            throw new IllegalArgumentException("Bundle text cannot be null");
        }

        Bundle b = null;
        /** Is the bundle location the bundle xml or a file with the xml? */
        if (type == BundleInfoType.FILE_LOCATION) {
            /** File with xml */
            b = Bundle.unmarshalBundle(PathManagerFactory.getPathManager()
                    .getStaticFile(bundleText), variables);
        } else {
            /** bundleLocation variable contains the xml */
            b = Bundle.unmarshalBundle(bundleText, variables);
        }

        return b;
    }

    /**
     * Schedules a {@link BundleLoader} to run to load the bundle on the
     * container
     * 
     * @param container
     * @param b
     */
    public static void loadTo(IDisplayPaneContainer container, Bundle b) {
        new BundleLoader(container, b).schedule();
    }
}
