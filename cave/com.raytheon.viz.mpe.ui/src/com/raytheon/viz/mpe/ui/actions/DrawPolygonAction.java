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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.rsc.MPEPolygonResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Sets MPE into the Edit Polygon Mode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009            mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class DrawPolygonAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart activeEditor = (IEditorPart) EditorUtil
                .getActiveEditorAs(IDisplayPaneContainer.class);
        if (activeEditor != null) {
            IDisplayPane first = ((IDisplayPaneContainer) activeEditor)
                    .getDisplayPanes()[0];
            List<MPEPolygonResource> rscs = first.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(MPEPolygonResource.class);
            for (MPEPolygonResource rsc : rscs) {
                rsc.getProperties().setVisible(true);
                EditableManager.makeEditable(rsc, true);
                rsc.issueRefresh();
            }
        }
        return null;
    }

}
