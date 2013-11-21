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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Change the status(Presence Mode) for the current user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ChangeStatusAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChangeStatusAction.class);

    private final Mode mode;

    public ChangeStatusAction() {
        super("Change Status", Action.AS_DROP_DOWN_MENU);
        mode = null;
        setMenuCreator(new MenuCreator());
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    public ChangeStatusAction(Mode mode) {
        super(CollaborationUtils.formatMode(mode), Action.AS_RADIO_BUTTON);
        this.mode = mode;
        String iconName = mode.toString().replaceAll("\\s+", "_").toLowerCase()
                + ".gif";
        setImageDescriptor(IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), iconName));
        Presence presence = CollaborationConnection.getConnection()
                .getPresence();
        if (mode.equals(presence.getMode())) {
            setChecked(true);
        }
    }

    @Override
    public void run() {
        if (!this.isChecked()) {
            return;
        }
        CollaborationConnection connection = CollaborationConnection
                .getConnection();

        Presence presence = connection.getPresence();
        Presence newPresence = new Presence(presence.getType(),
                presence.getStatus(), presence.getPriority(), mode);
        Tools.copyProperties(presence, newPresence);
        try {
            connection.getAccountManager().sendPresence(newPresence);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error sending presence", e);
        }

    }

    private static class MenuCreator implements IMenuCreator {

        private Menu menu;

        @Override
        public void dispose() {
            menu.dispose();
        }

        @Override
        public Menu getMenu(Control parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        private void fill() {
            for (int index = 0; index < CollaborationUtils.statusModes.length; ++index) {
                Action action = new ChangeStatusAction(
                        CollaborationUtils.statusModes[index]);
                IContributionItem contrib = new ActionContributionItem(action);
                contrib.fill(menu, -1);
            }
        }
    };

}
