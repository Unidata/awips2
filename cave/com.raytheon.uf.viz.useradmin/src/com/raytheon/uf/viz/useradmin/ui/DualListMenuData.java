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
package com.raytheon.uf.viz.useradmin.ui;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.viz.core.auth.IUserManager;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.viz.ui.widgets.duallist.IMenuData;

/**
 * Dual List menu data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 01, 2012            mpduff     Initial creation
 * Nov 06, 2012 1306       djohnson   Use API rather than implementation for role data.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DualListMenuData implements IMenuData {
    private String menuText = "menu";

    private boolean showMenu = true;

    private String application = null;

    private String selection = null;

    /**
     * @return the menuText
     */
    @Override
    public String getMenuText() {
        return menuText;
    }

    /**
     * @param menuText
     *            the menuText to set
     */
    public void setMenuText(String menuText) {
        this.menuText = menuText;
    }

    /**
     * @return the showMenu
     */
    @Override
    public boolean isShowMenu() {
        return showMenu;
    }

    /**
     * @param showMenu
     *            the showMenu to set
     */
    public void setShowMenu(boolean showMenu) {
        this.showMenu = showMenu;
    }

    /**
     * @return the application
     */
    public String getApplication() {
        return application;
    }

    /**
     * @param application
     *            the application to set
     */
    public void setApplication(String application) {
        this.application = application;
    }

    /**
     * @return the selection
     */
    public String getSelection() {
        return selection;
    }

    /**
     * @param selection
     *            the selection to set
     */
    public void setSelection(String selection) {
        this.selection = selection;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.widgets.IMenuData#setSelectedListSelection(java.lang
     * .String)
     */
    @Override
    public void setListSelection(String selection) {
        this.selection = selection;

    }

    /**
     * Show the right click menu for the list.
     * 
     * @param shell
     *            The shell
     * @param menuText
     *            The text for the menu item
     */
    @Override
    public void showListMenu(final Shell shell, String menuText) {
        Menu menu = new Menu(shell, SWT.POP_UP);
        MenuItem item1 = new MenuItem(menu, SWT.PUSH);
        item1.setText(menuText);
        item1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                StringBuilder messageText = new StringBuilder();
                boolean roleFlag = false;
                IUserManager man = UserController.getManager();
                for (IRole role : man.getRoles(application)) {
                    if (selection.equals(role.toString())) {
                        messageText.append("Role: ").append(selection);
                        messageText.append("\n\nDescription: ").append(
                                role.getDescription().trim());
                        List<IPermission> permissions = role.getPermissions();
                        if (!permissions.isEmpty()) {
                            messageText.append("\n\nPermissions: ");
                            for (IPermission perm : permissions) {
                                messageText.append("\n  ").append(
                                        perm.toString());
                            }
                        }
                        roleFlag = true;
                        break;
                    }
                }

                if (roleFlag == false) {
                    for (IPermission perm : man
                            .getPermissions(application)) {
                        if (perm.toString().equals(selection)) {
                            messageText.append("Permission: ")
                                    .append(selection);
                            messageText.append("\nDescription: ").append(
                                    perm.getDescription());
                            break;
                        }
                    }
                }
                if (messageText.length() == 0) {
                    messageText.append("No Description");
                }
                MessageBox messageDialog = new MessageBox(shell, SWT.ICON_INFORMATION);
                if (roleFlag) {
                    messageDialog.setText("Role Description");
                } else {
                    messageDialog.setText("Permission Description");
                }
                messageDialog.setMessage(messageText.toString());
                messageDialog.open();
            }
        });

        shell.setMenu(menu);
        menu.setVisible(true);
    }
}
