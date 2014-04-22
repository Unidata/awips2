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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.statusline.TimeDisplay;

/**
 * Abstract class for cave perspectives, solely for context activation and
 * refreshing, contributes the TimeDisplay to the status line as well. Used for
 * managing perspectives that render to the display.
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
        AbstractVizPerspectiveManager implements IRenderableDisplayCustomizer {

    private static final String DISPLAY_CUSTOMIZER_EXT_ID = "com.raytheon.viz.ui.displayCustomizer";

    private static final String DISPLAY_CUSTOMIZER_EXT_CUSTOMIZER_ATTR_ID = "customizer";

    private static final String DISPLAY_CUSTOMIZER_EXT_PERSPECTIVE_ATTR_ID = "perspective";

    private IPartListener displayCustomizer;

    private IWorkbenchPage perspectivePage;

    /** Optional workbench part context activator for the perspective */
    protected AbstractWorkbenchPartContextActivator contextActivator;

    private Collection<IRenderableDisplayCustomizer> customizers;

    @Override
    public void activate() {
        if (displayCustomizer == null
                && perspectiveWindow.getActivePage() != null) {
            perspectivePage = perspectiveWindow.getActivePage();
            displayCustomizer = new CAVEPerspectiveDisplayCustomizer(this);
            perspectivePage.addPartListener(displayCustomizer);
            customizers = getPerspectiveDisplayCustomizers(perspectivePage
                    .getPerspective());
        }
        super.activate();
    }

    @Override
    public void close() {
        if (displayCustomizer != null) {
            perspectivePage.removePartListener(displayCustomizer);
            displayCustomizer = null;
        }
        super.close();
    }

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
            IWorkbenchPartReference partRef = page.getActivePartReference();
            if (partRef != null) {
                contextActivator.partDeactivated(partRef);
            }
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

    @Override
    public void customizeDisplay(IRenderableDisplay display) {
        if (customizers != null) {
            for (IRenderableDisplayCustomizer customizer : customizers) {
                customizer.customizeDisplay(display);
            }
        }
    }

    @Override
    public void uncustomizeDisplay(IRenderableDisplay display) {
        if (customizers != null) {
            for (IRenderableDisplayCustomizer customizer : customizers) {
                customizer.uncustomizeDisplay(display);
            }
        }
    }

    /**
     * Looks up the {@link IRenderableDisplayCustomizer}s for the perspective
     * descriptor passed in based on the displayCustomizer extension point
     * 
     * @param descriptor
     * @return
     */
    private static Collection<IRenderableDisplayCustomizer> getPerspectiveDisplayCustomizers(
            IPerspectiveDescriptor descriptor) {
        List<IRenderableDisplayCustomizer> customizers = new ArrayList<IRenderableDisplayCustomizer>();
        IExtensionPoint extPoint = Platform.getExtensionRegistry()
                .getExtensionPoint(DISPLAY_CUSTOMIZER_EXT_ID);
        if (extPoint != null) {
            String id = descriptor.getId();
            String name = descriptor.getLabel();
            for (IExtension ext : extPoint.getExtensions()) {
                for (IConfigurationElement element : ext
                        .getConfigurationElements()) {
                    try {
                        IRenderableDisplayCustomizer customizer = (IRenderableDisplayCustomizer) element
                                .createExecutableExtension(DISPLAY_CUSTOMIZER_EXT_CUSTOMIZER_ATTR_ID);
                        if (customizer != null) {
                            String perspectives = element
                                    .getAttribute(DISPLAY_CUSTOMIZER_EXT_PERSPECTIVE_ATTR_ID);
                            if (perspectives == null) {
                                customizers.add(customizer);
                            } else {
                                String[] parts = perspectives.split("[,]");
                                for (String part : parts) {
                                    part = part.trim();
                                    if (part.equalsIgnoreCase(id)
                                            || part.equalsIgnoreCase(name)) {
                                        customizers.add(customizer);
                                        break;
                                    }
                                }
                            }
                        }
                    } catch (CoreException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        return customizers;
    }

    private static class CAVEPerspectiveDisplayCustomizer implements
            IPartListener, IRenderableDisplayChangedListener {
        private AbstractCAVEPerspectiveManager perspectiveManager;

        public CAVEPerspectiveDisplayCustomizer(
                AbstractCAVEPerspectiveManager perspectiveManager) {
            this.perspectiveManager = perspectiveManager;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
         * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
         * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
         * com.raytheon.uf.viz
         * .core.IRenderableDisplayChangedListener.DisplayChangeType)
         */
        @Override
        public void renderableDisplayChanged(IDisplayPane pane,
                IRenderableDisplay renderableDisplay, DisplayChangeType type) {
            if (type == DisplayChangeType.ADD) {
                perspectiveManager.customizeDisplay(renderableDisplay);
            } else if (type == DisplayChangeType.REMOVE) {
                perspectiveManager.uncustomizeDisplay(renderableDisplay);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart
         * )
         */
        @Override
        public void partOpened(IWorkbenchPart part) {
            if (part instanceof IDisplayPaneContainer) {
                IDisplayPaneContainer container = (IDisplayPaneContainer) part;
                container.addRenderableDisplayChangedListener(this);
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    renderableDisplayChanged(pane, pane.getRenderableDisplay(),
                            DisplayChangeType.ADD);
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart
         * )
         */
        @Override
        public void partClosed(IWorkbenchPart part) {
            if (part instanceof IDisplayPaneContainer) {
                ((IDisplayPaneContainer) part)
                        .removeRenderableDisplayChangedListener(this);
            }
        }

        @Override
        public void partActivated(IWorkbenchPart part) {
            // Do nothing
        }

        @Override
        public void partBroughtToTop(IWorkbenchPart part) {
            // Do nothing
        }

        @Override
        public void partDeactivated(IWorkbenchPart part) {
            // Do nothing
        }

    }
}
