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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.jivesoftware.smack.Roster;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterListener;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserNicknameChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.RosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;

/**
 * Manage contacts from local groups and roster on server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2012            bsteffen     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ContactsManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContactsManager.class);

    private final Job storeLocalGroupsJob = new Job("Storing Local Groups") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            LocalizationFile file = PathManagerFactory.getPathManager()
                    .getLocalizationFile(
                            context,
                            "collaboration" + File.separator
                                    + "localGroups.xml");
            LocalGroups obj;
            synchronized (localGroups) {
                obj = new LocalGroups(localGroups);
            }
            JAXB.marshal(obj, file.getFile());
            try {
                file.save();
            } catch (LocalizationOpFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            return org.eclipse.core.runtime.Status.OK_STATUS;
        }
    };

    private final CollaborationConnection connection;

    private final UserSearch search;

    private List<LocalGroup> localGroups;

    private Map<String, String> localAliases;

    private Set<LocalGroupListener> groupListeners = new HashSet<LocalGroupListener>();

    public ContactsManager(CollaborationConnection connection) {
        this.connection = connection;
        this.search = connection.createSearch();
        localAliases = UserIdWrapper.readAliasMap();
        initLocalGroups();
        final RosterManager rosterManager = connection.getRosterManager();
        final Roster roster = rosterManager.getRoster();
        // currently don't need to listen to roster since we only allow one
        // client
        rosterManager.addRosterListener(new RosterListener() {

            @Override
            public void entriesAdded(Collection<String> addresses) {
                // TODO handle roster additions from other clients
            }

            @Override
            public void entriesUpdated(Collection<String> addresses) {
                // TODO Auto-generated method stub

            }

            @Override
            public void entriesDeleted(Collection<String> addresses) {
                // TODO Auto-generated method stub

            }

            @Override
            public void presenceChanged(
                    org.jivesoftware.smack.packet.Presence presence) {
                // TODO Auto-generated method stub

            }
        });
    }

    private void initLocalGroups() {
        storeLocalGroupsJob.setSystem(true);
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(context,
                        "collaboration" + File.separator + "localGroups.xml");
        if (file.exists()) {
            this.localGroups = JAXB
                    .unmarshal(file.getFile(), LocalGroups.class).getGroups();
        }

        if (this.localGroups == null) {
            this.localGroups = new ArrayList<LocalGroup>();
        }
        for (LocalGroup group : localGroups) {
            group.setManager(this);
        }
    }

    public List<LocalGroup> getLocalGroups() {
        synchronized (this.localGroups) {
            return new ArrayList<LocalGroup>(this.localGroups);
        }
    }

    public void addToLocalGroup(String groupName, UserId user) {
        synchronized (this.localGroups) {
            LocalGroup group = createLocalGroup(groupName);
            String userId = user.getNormalizedId();
            List<String> userNames = group.getUserNames();
            if (!userNames.contains(userId)) {
                List<UserId> users = group.getUsers();
                group.getUserNames().add(userId);
                users.add(user);
            }
            RosterEntry entry = getRosterEntry(user);
            if (entry == null || entry.getGroups().isEmpty()) {
                // In order to get presence for a user they must be in the
                // roster, we can add them to the roster by either subscribing
                // to them using presence or adding them to the roster,
                // subscribing to the presence will not set the name correctly
                // so we use the roster add method.
                try {
                    RosterManager rosterManager = connection.getRosterManager();
                    Roster roster = rosterManager.getRoster();
                    roster.createEntry(userId, user.getAlias(), new String[0]);
                } catch (XMPPException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            for (LocalGroupListener listener : getSafeGroupListeners()) {
                listener.userAdded(group, user);
            }
        }
        storeLocalGroupsJob.schedule();
    }

    public void deleteFromLocalGroup(String groupName, UserId user) {
        synchronized (localGroups) {
            Iterator<LocalGroup> it = localGroups.iterator();
            while (it.hasNext()) {
                LocalGroup group = it.next();
                if (group.getName().equals(groupName)) {
                    group.getUsers().remove(user);
                    group.getUserNames().remove(user.getNormalizedId());
                    for (LocalGroupListener listener : getSafeGroupListeners()) {
                        listener.userDeleted(group, user);
                    }
                    break;
                }
            }
            if (getLocalGroups(user).isEmpty()) {
                // if the user is in no local groups and no roster groups remove
                // them from our roster.
                RosterEntry entry = getRosterEntry(user);
                if (entry != null && entry.getGroups().isEmpty()) {
                    Presence presence = new Presence(Type.unsubscribe);
                    presence.setTo(user.getNormalizedId());
                    try {
                        connection.getAccountManager().sendPresence(presence);
                    } catch (CollaborationException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
            storeLocalGroupsJob.schedule();
        }
    }

    public LocalGroup createLocalGroup(String groupName) {
        synchronized (localGroups) {
            for (LocalGroup group : this.localGroups) {
                if (groupName.equals(group.getName())) {
                    return group;
                }
            }
            LocalGroup group = new LocalGroup(groupName);
            group.setManager(this);
            localGroups.add(group);
            for (LocalGroupListener listener : getSafeGroupListeners()) {
                listener.groupCreated(group);
            }
            storeLocalGroupsJob.schedule();
            return group;
        }
    }

    public void deleteLocalGroup(String groupName) {
        synchronized (localGroups) {
            Iterator<LocalGroup> it = this.localGroups.iterator();
            while (it.hasNext()) {
                LocalGroup group = it.next();
                if (groupName.equals(group.getName())) {
                    List<UserId> users = new ArrayList<UserId>(
                            group.getUsers());
                    for (UserId user : users) {
                        deleteFromLocalGroup(groupName, user);
                    }
                    it.remove();
                    for (LocalGroupListener listener : getSafeGroupListeners()) {
                        listener.groupDeleted(group);
                    }
                }
            }
        }
        storeLocalGroupsJob.schedule();
    }

    public void renameLocalGroup(String oldName, String newName) {
        synchronized (localGroups) {

            for (LocalGroup group : localGroups) {
                if (oldName.equals(group.getName())) {
                    for (LocalGroupListener listener : getSafeGroupListeners()) {
                        listener.groupDeleted(group);
                    }
                    group.setName(newName);
                    for (LocalGroupListener listener : getSafeGroupListeners()) {
                        listener.groupCreated(group);
                    }
                }
            }
        }
        storeLocalGroupsJob.schedule();
    }

    public List<LocalGroup> getLocalGroups(UserId user) {
        List<LocalGroup> results = new ArrayList<LocalGroup>();
        synchronized (localGroups) {
            for (LocalGroup group : localGroups) {
                for (String userName : group.getUserNames()) {
                    if (user.getNormalizedId().equals(userName)) {
                        results.add(group);
                        break;
                    }
                }
            }
        }
        return results;
    }

    public void setNickname(UserId user, String nickname) {
        synchronized (localAliases) {

            localAliases.put(user.getNormalizedId(), nickname);
            try {
                UserIdWrapper.saveAliasMap(localAliases);
            } catch (LocalizationOpFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            connection.postEvent(new UserNicknameChangedEvent(user, nickname));
        }
    }

    /**
     * Attempt to find a good displayable name for a user, first it looks in the
     * local alias file, then it searches the roster to see if the server has
     * provided a name, finally if no name is found it will attempt to use the
     * username.
     * 
     * @param user
     * @return
     */
    public String getDisplayName(UserId user) {
        String alias = localAliases.get(user.getNormalizedId());
        if (alias == null) {
            // at this point try to get the user from roster;
            UserId rosterUser = null;
            if (user.getNormalizedId() != null) {
                rosterUser = getUser(user.getNormalizedId());
            }
            if (rosterUser != null) {
                user = rosterUser;
            }
            alias = user.getAlias();
            if (alias == null) {
                alias = user.getName();
            }
            if (alias.contains("@")) {
                alias = Tools.parseName(alias);
            }
        }
        return alias;
    }

    public UserId getUser(String userId) {
        RosterEntry entry = searchRoster(getRoster(), userId);
        if (entry == null) {
            return null;
        }
        return IDConverter.convertFrom(entry);
    }

    public RosterEntry getRosterEntry(UserId user) {
        return searchRoster(user);
    }

    public Presence getPresence(UserId user) {
        UserId self = connection.getUser();
        if (self.isSameUser(user)) {
            return getSelfPresence();
        }
        Roster roster = getRoster();
        return roster.getPresence(user.getNormalizedId());
    }

    public Presence getSelfPresence() {
        return connection.getPresence();
    }

    /**
     * Used by local groups to make sure all local group items are in the
     * roster.
     * 
     * @param name
     * @return
     */
    protected UserId findAndAddUser(String id) {
        UserId user = null;
        RosterEntry entry = searchRoster(getRoster(), id);
        if (entry != null) {
            user = IDConverter.convertFrom(entry);
        }
        if (user == null) {
            user = findUser(id);
            if (user != null) {
                try {
                    Roster roster = connection.getRosterManager().getRoster();
                    String alias = user.getAlias();
                    if (alias == null || alias.trim().isEmpty()) {
                        alias = user.getName();
                    }
                    roster.createEntry(user.getFQName(), alias, new String[0]);
                } catch (XMPPException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return user;

    }

    /**
     * @return
     */
    private Roster getRoster() {
        return connection.getRosterManager().getRoster();
    }

    private UserId findUser(String idString) {
        List<UserId> results;
        try {
            results = search.byId(idString);
        } catch (XMPPException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }
        for (UserId id : results) {
            String alias = localAliases.get(id.getNormalizedId());
            if (alias != null) {
                id.setAlias(alias);
            }
        }
        // since we are searching by ID, there should be 0 or 1 result
        return results.isEmpty() ? null : results.iterator().next();
    }

    private RosterEntry searchRoster(UserId user) {
        String userId = user.getNormalizedId();
        return searchRoster(connection.getRosterManager().getRoster(), userId);
    }

    private RosterEntry searchRoster(Roster roster, String userId) {
        return roster.getEntry(userId);
    }

    public void addLocalGroupListener(LocalGroupListener listener) {
        synchronized (groupListeners) {
            groupListeners.add(listener);
        }
    }

    public void removeLocalGroupListener(LocalGroupListener listener) {
        synchronized (groupListeners) {
            groupListeners.remove(listener);
        }
    }

    protected Set<LocalGroupListener> getSafeGroupListeners() {
        Set<LocalGroupListener> safeSet = new HashSet<LocalGroupListener>();
        synchronized (groupListeners) {
            safeSet.addAll(groupListeners);
        }
        return safeSet;
    }

    public static interface LocalGroupListener {

        public void groupCreated(LocalGroup group);

        public void groupDeleted(LocalGroup group);

        public void userAdded(LocalGroup group, UserId user);

        public void userDeleted(LocalGroup group, UserId user);

    }
}
