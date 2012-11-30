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
package com.raytheon.viz.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009            chammack     Initial creation
 * Apr 7, 2009      2215  jsanchez     Updated the scaleFile.
 * June 25, 2010    1691  bkowal       The frame count for the created / 
 *                                     discovered editor will now be
 *                                     updated to match the frame
 *                                     count that was specified in the bundle.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class MenuLoader extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MenuLoader.class);

    private Bundle bundle;

    private AbstractEditor editor;

    private class InstantiationThread extends Thread {
        private IDisplayPane loadTo;

        private IRenderableDisplay loadFrom;

        public InstantiationThread(IDisplayPane loadTo,
                IRenderableDisplay loadFrom) {
            this.loadTo = loadTo;
            this.loadFrom = loadFrom;
        }

        @Override
        public void run() {
            IDescriptor existingDescriptor = loadTo.getDescriptor();

            /**
             * Update the frame count based on what has been listed in the
             * bundle if we don't have times already loaded
             */
            FramesInfo info = existingDescriptor.getFramesInfo();
            if (info.getFrameCount() == 0) {
                existingDescriptor.setNumberOfFrames(loadFrom.getDescriptor()
                        .getNumberOfFrames());
            }

            // Pull out the resources to load
            ResourceList rscs = loadFrom.getDescriptor().getResourceList();
            List<ResourcePair> resourcesToLoad = new ArrayList<ResourcePair>();

            for (ResourcePair rp : rscs) {
                if (rp.getProperties().isSystemResource() == false) {
                    resourcesToLoad.add(rp);
                }
            }

            rscs.clear();

            /**
             * For each resource pair in the bundle resources: Give a unique
             * color for the legend if one isn't set, add to pane's descriptor's
             * resource list
             */
            for (ResourcePair rp : resourcesToLoad) {
                AbstractResourceData ard = rp.getResourceData();

                try {
                    ard.configure(rp.getLoadProperties(), existingDescriptor);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

                boolean newRP = true;
                if (existingDescriptor.getResourceList().contains(rp)) {
                    newRP = false;
                }
                if (newRP
                        && (rp.getProperties().isSystemResource() == false && !rp
                                .getLoadProperties().getCapabilities()
                                .hasCapability(ColorableCapability.class))) {
                    rp.getLoadProperties()
                            .getCapabilities()
                            .getCapability(rp.getResourceData(),
                                    ColorableCapability.class)
                            .setColor(
                                    ColorUtil.getNewColor(
                                            editor.getDisplayPanes(),
                                            existingDescriptor, rp));
                }

                if (newRP) {
                    existingDescriptor.getResourceList().add(rp);
                }
            }

            existingDescriptor.getResourceList().instantiateResources(
                    existingDescriptor, true);
        }
    }

    public static enum BundleInfoType {
        FILE_LOCATION, XML
    }

    public MenuLoader(Bundle b, AbstractEditor editor) {
        super("Request EDEX Product");
        this.bundle = b;
        this.editor = editor;
    }

    public static void loadProduct(final String editorType,
            final String bundleLocation, final Map<String, String> variables)
            throws VizException {
        loadProduct(editorType, bundleLocation, variables,
                BundleInfoType.FILE_LOCATION);
    }

    public static void loadProduct(String editorType, String bundleLocation,
            Map<String, String> variables, BundleInfoType type)
            throws VizException {
        Bundle b = null;

        /** Make sure bundle location is not null */
        if (bundleLocation == null) {
            throw new VizException("bundleLocation was null");
        }

        /** Is the bundle location the bundle xml or a file with the xml? */
        if (type.equals(BundleInfoType.FILE_LOCATION)) {
            /** File with xml */
            b = Bundle.unmarshalBundle(PathManagerFactory.getPathManager()
                    .getStaticFile(bundleLocation), variables);
        } else {
            /** bundleLocation variable contains the xml */
            b = Bundle.unmarshalBundle(bundleLocation, null);
        }

        /** Load the editor from the bundle */
        AbstractEditor editor = null;
        editor = UiUtil.createOrOpenEditor(editorType, b.getDisplays());

        /** Error loading the editor */
        if (editor == null) {
            throw new VizException("unable to get editor: " + editorType);
        }

        /**
         * If the descriptor in the bundle did not get set as the descriptor in
         * the editor, we need to go through and add the resources in the
         * bundle's IDisplayPane's descriptor to the editor's IDisplayPane's
         * descriptor, which is done in the job
         */
        if (editor.getDisplayPanes()[0].getDescriptor() != b.getDisplays()[0]
                .getDescriptor()) {
            Job j = new MenuLoader(b, editor);
            j.schedule();
        } else {
            /**
             * The editor and bundle have the same descriptors meaning the
             * editor was opened from the bundle so we need to instantiate the
             * resources on them only (this needs to be done outside the job bc
             * paint will be called immediately after this and the resources may
             * not have been instantiated yet.
             * 
             * TODO: There may be a way to create a list of resources from the
             * bundle and in this statement just remove the resources from the
             * editor then re add them in the job. so this doesn't hang on
             * construction of the resource
             */
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                pane.getDescriptor().getResourceList()
                        .instantiateResources(pane.getDescriptor(), true);
            }
            HistoryList.getInstance().refreshLatestBundle();
        }

    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        long t0 = System.currentTimeMillis();
        try {
            /** extracted to method when doing a refactor, not really needed */
            loadExisting();
            /** refresh the editor */
            editor.refresh();
            /** update the history list */
            HistoryList.getInstance().refreshLatestBundle();
        } catch (VizException e) {
            return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
                    "Error loading bundle", e);
        }
        long t2 = System.currentTimeMillis();
        System.out.println("Total bundle retrieval: " + (t2 - t0));
        return Status.OK_STATUS;
    }

    private void loadExisting() throws VizException {
        IDisplayPane selected = null;
        if (editor instanceof IMultiPaneEditor) {
            selected = ((IMultiPaneEditor) editor)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
        }

        int bundleSize = bundle.getDisplays().length;
        int editorSize = editor.getDisplayPanes().length;

        List<IDisplayPane> loadToPanes = new ArrayList<IDisplayPane>();
        List<IRenderableDisplay> loadFromBundle = new ArrayList<IRenderableDisplay>();
        List<Thread> executionThreads = new ArrayList<Thread>();

        // Figure out what panes to load to
        if (selected != null && bundleSize == 1) {
            loadToPanes.add(selected);
            loadFromBundle.add(bundle.getDisplays()[0]);
        } else if (selected == null && bundleSize == 1) {
            // load to all panes the single bundle display
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                loadToPanes.add(pane);
                loadFromBundle.add(bundle.getDisplays()[0].cloneDisplay());
            }
        } else {
            int max = Math.max(bundleSize, editorSize);
            for (int i = 0; i < max; ++i) {
                if (editorSize > i) {
                    loadToPanes.add(editor.getDisplayPanes()[i]);
                } else {
                    loadToPanes.add(null);
                }

                if (bundleSize > i) {
                    loadFromBundle.add(bundle.getDisplays()[i]);
                } else {
                    loadFromBundle.add(null);
                }
            }
        }

        for (int i = 0; i < loadToPanes.size(); ++i) {
            IDisplayPane loadTo = loadToPanes.get(i);
            final IRenderableDisplay loadFrom = loadFromBundle.get(i);
            if (i == 0) {
                AbstractTimeMatcher srcTimeMatcher = loadFrom.getDescriptor()
                        .getTimeMatcher();
                if (srcTimeMatcher != null) {
                    loadTo.getDescriptor().getTimeMatcher()
                            .copyFrom(srcTimeMatcher);
                }
                loadTo.getDescriptor().getTimeMatcher().resetMultiload();
                new InstantiationThread(loadTo, loadFrom).run();
                continue;
            }

            if (loadFrom == null) {
                continue;
            } else if (loadTo == null) {
                if (editor instanceof IMultiPaneEditor) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            ((IMultiPaneEditor) (editor)).addPane(loadFrom);
                        }
                    });

                }
                continue;
            }

            Thread t = new InstantiationThread(loadTo, loadFrom);
            t.start();
            executionThreads.add(t);
        }

        for (Thread t : executionThreads) {
            try {
                t.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                VizGlobalsManager.getCurrentInstance().updateUI(editor);
            }
        });

    }
}
