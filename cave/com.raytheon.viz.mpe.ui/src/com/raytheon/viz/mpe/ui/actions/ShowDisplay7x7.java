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
package com.raytheon.viz.mpe.ui.actions;

import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEGageResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 6, 2009             snaples     Initial creation
 * Mar 14, 2013  1457      mpduff      Don't throw error if nothing displayed.
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
public class ShowDisplay7x7 extends AbstractHandler implements IElementUpdater {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (MPEDisplayManager.getCurrent().getDisplayedFieldResource() != null) {
            IEditorPart activeEditor = (IEditorPart) EditorUtil
                    .getActiveEditorAs(IDisplayPaneContainer.class);
            if (activeEditor != null) {
                IDisplayPane first = ((IDisplayPaneContainer) activeEditor)
                        .getDisplayPanes()[0];
                List<MPEGageResource> rscs = first.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(MPEGageResource.class);
                for (MPEGageResource rsc : rscs) {
                    rsc.getProperties().setVisible(true);
                    EditableManager.makeEditable(rsc, true);
                    rsc.issueRefresh();
                }
            }
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void updateElement(UIElement element, Map parameters) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                List<AbstractVizResource<?, ?>> rscs = pane.getDescriptor()
                        .getResourceList()
                        .getResourcesByType(MPEGageResource.class);
                for (AbstractVizResource<?, ?> rsc : rscs) {
                    element.setChecked(rsc.getCapability(
                            EditableCapability.class).isEditable());
                    return;
                }
            }
        }
    }
}
