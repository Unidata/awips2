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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.XmrgResource;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2008            randerso     Initial creation.
 * Jul 21, 2009            mpduff      Added code to update the Xmrg 
 *                                     data.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetDisplayField extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String f = event.getParameter("Field");

        IEditorPart editor = HandlerUtil.getActiveEditor(event);
        if (editor instanceof IDisplayPaneContainer) {
            return setDisplayField((IDisplayPaneContainer) editor, f);
        }
        return null;
    }

    public static Object setDisplayField(IDisplayPaneContainer editor, String f) {
        if (editor == null) {
            return null;
        }
        IDisplayPane pane = (editor).getDisplayPanes()[0];
        if (editor instanceof IMultiPaneEditor) {
            IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
            if (multiPane.getNumberofPanes() > 1) {
                pane = multiPane.getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                if (pane == null) {
                    pane = multiPane.getActiveDisplayPane();
                }
            } else {
                pane = (editor).getDisplayPanes()[0];
            }

        }

        MPEDisplayManager displayMgr = MPEDisplayManager.getInstance(pane);
        displayMgr.setAccum_interval(1);
        DisplayFieldData field = DisplayFieldData.valueOf(f);
        displayMgr.setDisplayFieldType(field);
        if (!displayMgr.getOtherDispType().equals(field)) {
            displayMgr.setOtherDispType(field);
            displayMgr.setAccum_interval(1);
        }

        // Update the screen
        XmrgResource xmrgRsc = (XmrgResource) displayMgr.getDisplayedResource();
        xmrgRsc.updateXmrg(false);
        return xmrgRsc;
    }
}
