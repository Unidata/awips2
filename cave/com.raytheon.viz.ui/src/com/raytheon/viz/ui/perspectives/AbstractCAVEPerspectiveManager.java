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
package com.raytheon.viz.ui.perspectives;

import java.util.List;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.statusline.TimeDisplay;

/**
 * Abstract class for cave persepctives, solely for context activation and
 * refreshing, contributes the TimeDisplay to the status line as well
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractCAVEPerspectiveManager extends
        AbstractVizPerspectiveManager {

    /** Optional workbench part context activator for the perspective */
    protected AbstractWorkbenchPartContextActivator contextActivator;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#
     * activateContexts(com.raytheon.uf.viz.core.ContextManager)
     */
    @Override
    protected void activateContexts(ContextManager manager) {
        super.activateContexts(manager);
        if (contextActivator != null) {
            contextActivator.partActivated(page.getActivePartReference());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#deactivate
     * ()
     */
    @Override
    public void deactivate() {
        super.deactivate();
        if (contextActivator != null) {
            page.removePartListener(contextActivator);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#
     * deactivateContexts(com.raytheon.uf.viz.core.ContextManager)
     */
    @Override
    protected void deactivateContexts(ContextManager manager) {
        super.deactivateContexts(manager);
        if (contextActivator != null) {
            contextActivator.partDeactivated(page.getActivePartReference());
        }
    }

    @Override
    protected void activateInternal() {
        super.activateInternal();

        if (contextActivator != null) {
            page.addPartListener(contextActivator);
        }

        // repaint containers
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part instanceof IDisplayPaneContainer) {
                ((IDisplayPaneContainer) part).refresh();
            }
        }

        for (IViewReference ref : page.getViewReferences()) {
            IViewPart part = ref.getView(false);
            if (part instanceof IDisplayPaneContainer) {
                ((IDisplayPaneContainer) part).refresh();
            }
        }
    }

    @Override
    protected List<ContributionItem> getStatusLineItems() {
        List<ContributionItem> items = super.getStatusLineItems();
        items.add(new TimeDisplay());
        return items;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#
     * getPerspectiveInputHandlers(com.raytheon.viz.ui.editor.AbstractEditor)
     */
    public IInputHandler[] getPerspectiveInputHandlers(AbstractEditor editor) {
        return new IInputHandler[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#getTitle
     * (java.lang.String)
     */
    @Override
    protected String getTitle(String title) {
        return title + ":"
                + LocalizationManager.getContextName(LocalizationLevel.SITE)
                + " - " + getLabel();
    }

}
