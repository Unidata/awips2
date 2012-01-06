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
package com.raytheon.viz.gfe.textproduct;

import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
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
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.action.CopyAction;
import com.raytheon.viz.gfe.core.script.action.ModifyAction;
import com.raytheon.viz.gfe.core.script.action.RenameAction;
import com.raytheon.viz.gfe.core.script.action.ViewAction;
import com.raytheon.viz.gfe.textproduct.action.NewAction;
import com.raytheon.viz.gfe.textproduct.action.TextProductDeleteAction;
import com.raytheon.viz.gfe.textproduct.action.TextProductSiteAction;
import com.raytheon.viz.gfe.textproduct.action.TextProductSiteOverrideAction;

/**
 * Listens to mouse events on the text products list in the
 * DefineTextProductsDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * Sept 19, 2008            askripsk    Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class ProductMouseListener implements MouseListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ProductMouseListener.class);

    private MenuManager menuMgr;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt
     * .events.MouseEvent)
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events
     * .MouseEvent)
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

            IScriptUtil util = new TextProductUtil();
            menuMgr.add(new CopyAction(selected, util));
            menuMgr.add(new Separator());
            menuMgr.add(new ViewAction(selected, util));
            menuMgr.add(new ModifyAction(selected, util));
            menuMgr.add(new NewAction());

            // Get the localization level of the selection
            LocalizationFile script = null;
            try {
                script = util.find(selected, null);
            } catch (GFEException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to find script " + selected, e1);
            }
            LocalizationLevel level = LocalizationLevel.BASE;
            if (script != null) {
                level = script.getContext().getLocalizationLevel();
            }

            // Some text product specific Action classes were created for use
            // here specifically for ticket #5074 that are largely copies of the
            // Action classes in the com.raytheon.viz.gfe.core.script.action
            // package with some specific changes for its unique storage
            // hierarchy. They are not well-designed but I was assured that the
            // design of the dialog this listener supports will be changing
            // soon.
            // TODO: REMOVE these classes and modify the ones within
            // com.raytheon.viz.gfe.core.script.action to support both text
            // products and the other python-based items that are currently
            // supported.

            // The Rename and Delete options should only be available on
            // utilities that the user has created or copied, or to admins
            // TODO: add these if user has admin role
            if (LocalizationLevel.USER == level || false) {
                menuMgr.add(new Separator());
                menuMgr.add(new TextProductDeleteAction(selected, util));
                menuMgr.add(new RenameAction(selected, util));
            }

            // TODO: skip if user is not site-admin

            if (true) { // TODO: check user site-admin rights
                if (LocalizationLevel.USER == level
                        || LocalizationLevel.SITE == level) {
                    menuMgr.add(new Separator());
                }
                boolean isOverride = selected.matches("(?i).+override");
                if (LocalizationLevel.USER == level) {
                    menuMgr.add(new TextProductSiteAction(selected, util));
                    if (!isOverride) {
                        menuMgr.add(new TextProductSiteOverrideAction(selected,
                                util));
                    }
                } else if (LocalizationLevel.SITE == level) {
                    if (isOverride) {
                        menuMgr.add(new TextProductSiteAction(selected, util));
                    }
                }
            }

            Menu menu = menuMgr.createContextMenu(list);

            menu.setVisible(true);
            list.setMenu(menu);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.
     * MouseEvent)
     */
    @Override
    public void mouseUp(MouseEvent arg0) {
    }

}
