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
package com.raytheon.uf.viz.collaboration.core.ui.action;

import static com.raytheon.uf.viz.collaboration.core.ui.CollaborationIconFactory.getIconImage;

import org.eclipse.jface.action.Action;

import com.raytheon.uf.viz.collaboration.core.ui.CollaborationGroupControl;

/**
 * Factory for constructing core collaboration ui functionality actions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationActionFactory {

    public static Action createAliasAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new AliasAction();
        action.setControl(control);
        action.setText("Alias...");
        // TODO: Find Alias image
        action.setImageDescriptor(getIconImage("alias.gif"));
        return action;
    }

    public static Action createChangeFontAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ChangeFontAction();
        action.setControl(control);
        action.setText("Change Font...");
        action.setImageDescriptor(getIconImage("font.gif"));
        return action;
    }

    public static Action createChangePasswordAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ChangePasswordAction();
        action.setControl(control);
        action.setText("Change Password...");
        return action;
    }

    public static Action createChooseRoleAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ChooseRoleAction();
        action.setControl(control);
        action.setText("Change Role");
        return action;
    }

    public static Action createChooseSiteAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ChooseSiteAction();
        action.setControl(control);
        action.setText("Change Site");
        return action;
    }

    public static Action createChooseStatusAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ChooseStatusAction();
        action.setControl(control);
        action.setText("Change Status");
        return action;
    }

    public static Action createCollapseAllAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new CollapseAllAction();
        action.setControl(control);
        action.setText("Collapse All...");
        action.setImageDescriptor(getIconImage("collapseall.gif"));
        return action;
    }

    public static Action createCreateGroupAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new CreateGroupAction();
        action.setControl(control);
        action.setText("Create Group...");
        action.setImageDescriptor(getIconImage("group.gif"));
        return action;
    }

    public static Action createCreateSessionAction(
            CollaborationGroupControl control) {
        AbstractCollaborationAction action = new CreateSessionAction();
        action.setControl(control);
        action.setText("Create Session...");
        action.setImageDescriptor(getIconImage("add_collaborate.gif"));
        return action;
    }

    public static Action createLoginAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new LogoutAction();
        action.setControl(control);
        action.setText("Login");
        action.setImageDescriptor(getIconImage("login.gif"));
        return action;
    }

    public static Action createLogoutAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new LogoutAction();
        action.setControl(control);
        action.setText("Logout");
        action.setImageDescriptor(getIconImage("logout.gif"));
        return action;
    }

    public static Action createP2PChatAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new P2PChatAction();
        action.setControl(control);
        action.setText("Chat...");
        action.setImageDescriptor(getIconImage("chats.gif"));
        return action;
    }

    public static Action createShowFeedAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ShowFeedAction();
        action.setControl(control);
        action.setText("Display Feed...");
        action.setImageDescriptor(getIconImage("feed.gif"));
        return action;
    }

    public static Action createViewLogAction(CollaborationGroupControl control) {
        AbstractCollaborationAction action = new ViewLogAction();
        action.setControl(control);
        action.setText("View Log...");
        action.setImageDescriptor(getIconImage("log.gif"));
        return action;
    }
}
