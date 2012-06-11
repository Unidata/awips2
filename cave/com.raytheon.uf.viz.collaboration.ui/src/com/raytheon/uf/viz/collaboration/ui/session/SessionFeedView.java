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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.presence.Presence;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Display;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Built for the session in which everyone joins
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionFeedView extends SessionView {

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionFeedView";

    private Action muteAction;

    private Action unMuteAction;

    private Action colorChangeAction;

    private Action autoJoinAction;

    private List<String> enabledUsers;

    /**
     * 
     */
    public SessionFeedView() {
        super();
        enabledUsers = readEnabledUsers();
    }

    private List<String> readEnabledUsers() {
        LocalizationFile file = getLocalizationFile();
        if (file.exists()) {
            SubscribeList list = JAXB.unmarshal(file.getFile(), SubscribeList.class);
            return list.getEnabledUsers();
        }
        return new ArrayList<String>();
    }

    private void persistEnabledUsers() {
        SubscribeList list = new SubscribeList();
        list.setEnabledUsers(enabledUsers);
        JAXB.marshal(list, getLocalizationFile().getFile());
    }

    @Subscribe
    public void refreshBlockList(SubscribeList list) {
        enabledUsers = list.getEnabledUsers();
    }

    private LocalizationFile getLocalizationFile() {
        LocalizationFile file = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        file = PathManagerFactory.getPathManager().getLocalizationFile(context,
                "collaboration" + File.separator + "subscribeList.xml");
        return file;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#createActions()
     */
    @Override
    protected void createActions() {
        super.createActions();
        muteAction = new Action("Ignore User") {
            @Override
            public void run() {
                IStructuredSelection selection = (IStructuredSelection) usersTable
                        .getSelection();
                IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
                UserId id = IDConverter.convertFrom(entry.getUser());
                if (enabledUsers.contains(id.getName())) {
                    ((Presence) entry.getPresence()).getProperties().put(
                            "UserStatus", "Ignored");
                    usersTable.refresh(entry);
                    enabledUsers.remove(id.getName());
                }
            }
        };

        unMuteAction = new Action("Subcribe User") {
            @Override
            public void run() {
                IStructuredSelection selection = (IStructuredSelection) usersTable
                        .getSelection();
                IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
                UserId id = IDConverter.convertFrom(entry.getUser());
                if (!enabledUsers.contains(id.getName())) {
                    ((Presence) entry.getPresence()).getProperties().put(
                            "UserStatus", "Subscribed");
                    usersTable.refresh(entry);
                    enabledUsers.add(id.getName());
                }
            }
        };

        colorChangeAction = new Action("Change Color...") {
            @Override
            public void run() {
                ColorDialog dlg = new ColorDialog(Display.getCurrent()
                        .getActiveShell());
                RGB rgb = dlg.open();
                IStructuredSelection selection = (IStructuredSelection) usersTable
                        .getSelection();
                IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
                manager.setColorForUser(
                        IDConverter.convertFrom(entry.getUser()), rgb);
                usersTable.refresh(entry);
            }
        };

        autoJoinAction = new Action("Autojoin", SWT.TOGGLE) {
            public void run() {
                Activator.getDefault().getPreferenceStore()
                        .setValue("autojoin", autoJoinAction.isChecked());
            };
        };

        autoJoinAction.setChecked(Activator.getDefault().getPreferenceStore()
                .getBoolean("autojoin"));
        Activator.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent event) {
                        autoJoinAction.setChecked(Activator.getDefault()
                                .getPreferenceStore().getBoolean("autojoin"));
                    }
                });
        MenuManager manager = (MenuManager) getViewSite().getActionBars()
                .getMenuManager();
        manager.add(autoJoinAction);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#fillContextMenu
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected void fillContextMenu(IMenuManager manager) {
        super.fillContextMenu(manager);
        IRosterEntry entry = (IRosterEntry) ((IStructuredSelection) usersTable
                .getSelection()).getFirstElement();
        UserId id = IDConverter.convertFrom(entry.getUser());
        if (!id.equals(CollaborationConnection.getConnection().getUser())) {
            if (enabledUsers.contains(id.getName())) {
                manager.add(muteAction);
            } else {
                manager.add(unMuteAction);
            }
        }
        manager.add(colorChangeAction);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#setParticipantValues
     * (com.raytheon.uf.viz.collaboration.ui.session.ParticipantsLabelProvider)
     */
    @Override
    protected void setParticipantValues(ParticipantsLabelProvider labelProvider) {
        super.setParticipantValues(labelProvider);
        labelProvider.setEnabledUsers(enabledUsers);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#handleMessage
     * (com.raytheon.uf.viz.collaboration.comm.identity.IMessage)
     */
    @Override
    public void handleMessage(IMessage message) {
        final IMessage msg = message;
        // so not to have delay, going to handle messages from yourself
        // separately
        if (message.getFrom().equals(
                CollaborationConnection.getConnection().getUser())) {
            return;
        }

        if (enabledUsers.contains(message.getFrom().getName())) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    appendMessage(msg);
                }
            });
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#participantArrived
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId)
     */
    @Override
    protected void participantArrived(UserId participant) {
        List<IRosterEntry> users = (List<IRosterEntry>) usersTable.getInput();
        Map<UserId, IRosterEntry> usersMap = session.getConnection()
                .getContactsManager().getUsersMap();
        IRosterEntry user = usersMap.get(participant);
        if (user != null) {
            for (IUser usr : usersMap.keySet()) {
                if (usr.getName().equals(participant.getName())) {
                    user = usersMap.get(usr);
                    users.add(user);
                    break;
                }
            }
        }
        usersTable.setInput(users);
        usersTable.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#participantDeparted
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId)
     */
    @Override
    protected void participantDeparted(UserId participant) {
        List<IRosterEntry> users = (List<IRosterEntry>) usersTable.getInput();
        for (int i = 0; i < users.size(); ++i) {
            UserId otherId = IDConverter.convertFrom(users.get(i).getUser());
            if (users.get(i) == null
                    || participant.getName().equals(otherId.getName())) {
                users.remove(i);
                usersTable.refresh();
                break;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.SessionView#dispose()
     */
    @Override
    public void dispose() {
        persistEnabledUsers();
        super.dispose();
    }
}
