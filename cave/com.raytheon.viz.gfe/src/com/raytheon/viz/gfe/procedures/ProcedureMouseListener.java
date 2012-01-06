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
/**
 * 
 */
package com.raytheon.viz.gfe.procedures;

import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.AbstractScriptCatalog;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.action.CopyAction;
import com.raytheon.viz.gfe.core.script.action.DeleteAction;
import com.raytheon.viz.gfe.core.script.action.ModifyAction;
import com.raytheon.viz.gfe.core.script.action.NewAction;
import com.raytheon.viz.gfe.core.script.action.RenameAction;
import com.raytheon.viz.gfe.core.script.action.SiteAction;
import com.raytheon.viz.gfe.core.script.action.ViewAction;
import com.raytheon.viz.gfe.procedures.util.ProcedureUtil;

/**
 * A mouse listener for the procedure list in DefineProceduresDialog.
 * 
 * @author wldougher
 * 
 */
public class ProcedureMouseListener extends MouseAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ProcedureMouseListener.class);

    private MenuManager menuMgr;

    /**
     * Constructor.
     */
    public ProcedureMouseListener() {
        super();
    }

    /**
     * Handle mouse button 3 presses by displaying a small menu for the
     * Procedure whose name is under the cursor. Actions which are always
     * available on the menu are Copy, Modify, and New (which doesn't care which
     * procedure the mouse is over). When the Procedure is at USER level, Delete
     * and Rename actions are also offered.
     * 
     * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
     */
    @Override
    public void mouseDown(MouseEvent e) {
        List list = (List) e.getSource();
        String selected = list.getItem(list.getSelectionIndex());

        if (e.button == 3) {
            if (menuMgr == null) {
                menuMgr = new MenuManager("#PopupMenu");
            } else {
                menuMgr.removeAll();
            }

            IScriptUtil util = new ProcedureUtil();
            AbstractScriptCatalog catalog = new ProcedureCatalog();

            menuMgr.add(new CopyAction(selected, util));
            menuMgr.add(new Separator());
            menuMgr.add(new ViewAction(selected, util));
            menuMgr.add(new ModifyAction(selected, util));
            menuMgr.add(new NewAction(util));

            LocalizationFile selectedFile = null;
            try {
                selectedFile = util.find(selected, LocalizationLevel.USER);
            } catch (GFEException e1) {
                statusHandler.handle(Priority.VERBOSE,
                        "Error finding procedure " + selected, e1);
            }
            LocalizationLevel level = LocalizationLevel.BASE;
            if (selectedFile != null) {
                level = selectedFile.getContext().getLocalizationLevel();
            }

            // The Rename and Delete options should only be available on
            // utilities that the user has created or copied, or to admins
            // TODO: add these if user has admin role
            if (LocalizationLevel.USER == level || false) {
                menuMgr.add(new Separator());
                menuMgr.add(new DeleteAction(selected, util));
                menuMgr.add(new RenameAction(selected, util));
            }

            if (true) { // TODO: only if user is a site admin
                if (LocalizationLevel.USER == level) {
                    menuMgr.add(new Separator());
                    menuMgr.add(new SiteAction(selected, util));
                }
            }

            Menu menu = menuMgr.createContextMenu(list);

            menu.setVisible(true);
            list.setMenu(menu);
        }
    }
}
