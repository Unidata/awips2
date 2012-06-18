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
package com.raytheon.uf.viz.collaboration.ui.actions;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.ContributedEditorMenuAction;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Action to remove an editor from being shared
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UnshareEditorAction extends ContributedEditorMenuAction {

    public UnshareEditorAction() {
        super("Unshare");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        SharedEditorsManager manager = getActiveSharedEditorManager(editor);
        if (manager != null) {
            try {
                manager.removeEditor(editor);
            } catch (CollaborationException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    @Override
    public boolean shouldBeVisible() {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        return getActiveSharedEditorManager(editor) != null;
    }

    private SharedEditorsManager getActiveSharedEditorManager(
            AbstractEditor editor) {
        SharedEditorsManager manager = null;
        ISharedDisplaySession session = SharedEditorsManager
                .getSharedEditorSession(editor);
        if (session != null) {
            manager = SharedEditorsManager.getManager(session);
        }
        return manager;
    }
}
