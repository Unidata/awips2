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
package com.raytheon.uf.viz.d2d.core.sampling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.sampling.SamplingInputAdapter;
import com.raytheon.uf.viz.core.rsc.sampling.SamplingResource;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

/**
 * D2D Sampling resources, supports all pane sampling and long left click
 * sampling as well
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DSamplingResource extends SamplingResource implements
        ID2DSamplingResource {

    private class D2DMouseAdapter extends
            SamplingInputAdapter<D2DSamplingResource> {

        private static final String INSPECT_PREF = "com.raytheon.viz.ui.input.inspect";

        protected Job job;

        protected long timeUp;

        private MousePreferenceManager prefManager = MousePreferenceManager
                .getInstance();

        private boolean inspectForced = false;

        D2DMouseAdapter() {
            super(D2DSamplingResource.this);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            super.handleMouseDown(x, y, mouseButton);
            if (prefManager.handleClick(INSPECT_PREF, mouseButton)
                    && isSampling() == false) {
                inspectForced = true;
                setSampling(true);
                issueRefresh();
                return false;
            } else if (prefManager.handleLongClick(INSPECT_PREF, mouseButton)
                    && isSampling() == false) {
                timeUp = 0L;
                if (job == null) {
                    job = new Job("InspectAdapter") {

                        @Override
                        protected IStatus run(IProgressMonitor monitor) {
                            if (timeUp == 0L) {
                                inspectForced = true;
                                setSampling(true);
                                issueRefresh();
                            }
                            return Status.OK_STATUS;
                        }

                    };
                }
                if (job.getState() != Job.RUNNING) {
                    job.schedule(500);
                }
                return false;
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            super.handleMouseUp(x, y, mouseButton);
            if (prefManager.handleLongClick(INSPECT_PREF, mouseButton)) {
                timeUp = System.currentTimeMillis();
            }
            if (inspectForced) {
                inspectForced = false;
                setSampling(false);
                issueRefresh();
                return true;
            }
            return false;
        }

    }

    private boolean allPanelSampling = false;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public D2DSamplingResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected IInputHandler getSamplingInputHandler() {
        return new D2DMouseAdapter();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (isAllPanelSampling() == false) {
            super.paintInternal(target, paintProps);
            return;
        }

        if (sampleCoord == null || isSampling() == false) {
            return;
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container == null) {
            return;
        }

        ResourceList rList = new ResourceList();

        List<ResourceList> blendedLists = new ArrayList<ResourceList>();

        List<ResourcePair> invisibleList = new ArrayList<ResourcePair>();

        IDisplayPane[] panes = container.getDisplayPanes();
        if (panes.length == 4) {
            // Awips1 puts four panels in the wrong order.
            panes = new IDisplayPane[] { panes[0], panes[1], panes[3], panes[2] };
        }

        for (IDisplayPane pane : panes) {
            for (ResourcePair pair : pane.getDescriptor().getResourceList()) {
                if (pair.getResource() == null
                        || !pair.getProperties().isVisible()) {
                    continue;
                }
                if (!pair.getResource()
                        .hasCapability(BlendableCapability.class)) {
                    rList.add(pair);
                    continue;
                }
                ResourceList list = pair.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                ResourcePair rp = list.get(0);
                if (!rp.getProperties().isVisible()) {
                    invisibleList.add(rp);
                    rp.getProperties().setVisible(true);
                }
                rList.add(rp);
                blendedLists.add(list);
            }
        }
        for (int i = 1;; i++) {
            boolean done = true;
            for (ResourceList list : blendedLists) {
                if (list.size() <= i) {
                    continue;
                }
                ResourcePair rp = list.get(i);
                if (!rp.getProperties().isVisible()) {
                    invisibleList.add(rp);
                    rp.getProperties().setVisible(true);
                }
                rList.add(rp);
                done = false;
            }
            if (done) {
                break;
            }
        }

        // doHover goes in reverse list order
        Collections.reverse(rList);

        paintResult(target, paintProps, sampleCoord,
                doHover(sampleCoord, rList));
        for (ResourcePair pair : invisibleList) {
            pair.getProperties().setVisible(false);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.ID2DSamplingResource#
     * setAllPanelSampling(boolean)
     */
    @Override
    public void setAllPanelSampling(boolean allPanelSampling) {
        this.allPanelSampling = allPanelSampling;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.core.sampling.ID2DSamplingResource#isAllPanelSampling
     * ()
     */
    @Override
    public boolean isAllPanelSampling() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container instanceof IMultiPaneEditor) {
            // Only all panel sample if we have 1 displayed pane count
            return (allPanelSampling & (((IMultiPaneEditor) container)
                    .displayedPaneCount() == 1));
        }
        return allPanelSampling;
    }
}
