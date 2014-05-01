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
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.Presence;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.ecf.presence.roster.IRosterItem;
import org.eclipse.ecf.presence.roster.IRosterListener;
import org.eclipse.ecf.presence.search.ICriteria;
import org.eclipse.ecf.presence.search.ICriterion;
import org.eclipse.ecf.presence.search.IResult;
import org.eclipse.ecf.presence.search.ISearch;
import org.eclipse.ecf.presence.search.IUserSearchManager;
import org.eclipse.ecf.presence.search.UserSearchException;

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
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserNicknameChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2012            bsteffen     Initial creation
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

    private List<LocalGroup> localGroups;

    private Map<String, String> localAliases;

    private Set<LocalGroupListener> groupListeners = new HashSet<LocalGroupListener>();

    public ContactsManager(CollaborationConnection connection) {
        this.connection = connection;
        localAliases = UserIdWrapper.readAliasMap();
        applyLocalAliases(getRoster());
        initLocalGroups();
        connection.getRosterManager().addRosterListener(new IRosterListener() {

            @Override
            public void handleRosterUpdate(IRoster roster,
                    IRosterItem changedValue) {

            }

            @Override
            public void handleRosterEntryRemove(IRosterEntry entry) {

            }

            @Override
            public void handleRosterEntryAdd(IRosterEntry entry) {
                applyLocalAliases(entry);

            }
        });
    }

    private void applyLocalAliases(IRosterItem item) {
        if (item instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) item;
            IUser user = entry.getUser();
            String alias = localAliases.get(getUserId(user));
            if (alias != null && user instanceof User) {
                ((User) user).setNickname(alias);
            }
        } else if (item instanceof IRosterGroup) {
            Collection<?> entries = ((IRosterGroup) item).getEntries();
            synchronized (entries) {
                entries = new ArrayList<Object>(entries);
            }
            for (Object o : entries) {
                applyLocalAliases((IRosterItem) o);
            }
        } else if (item instanceof IRoster) {
            Collection<?> entries = ((IRoster) item).getItems();
            synchronized (entries) {
                entries = new ArrayList<Object>(entries);
            }
            for (Object o : entries) {
                applyLocalAliases((IRosterItem) o);
            }
        }
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

    public void addToLocalGroup(String groupName, IUser user) {
        synchronized (this.localGroups) {
            LocalGroup group = createLocalGroup(groupName);
            String userId = getUserId(user);
            List<String> userNames = group.getUserNames();
            if (!userNames.contains(userId)) {
                List<IUser> users = group.getUsers();
                group.getUserNames().add(userId);
                users.add(user);
            }
            IRosterEntry entry = getRosterEntry(user);
            if (entry == null || entry.getGroups().isEmpty()) {
                // In order to get presence for a user they must be in the
                // roster, we can add them to the roster by either subscribing
                // to them using presence or adding them to the roster,
                // subscribing to the presence will not set the name coreectly
                // so we use the roster add method.
                try {
                    // IPresence presence = new Presence(Type.SUBSCRIBE);
                    // connection.getRosterManager().getPresenceSender()
                    // .sendPresenceUpdate(user.getID(), presence);

                    connection.getRosterManager().getRosterSubscriptionSender()
                            .sendRosterAdd(userId, user.getName(), null);
                } catch (ECFException e) {
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

    public void deleteFromLocalGroup(String groupName, IUser user) {
        synchronized (localGroups) {
            Iterator<LocalGroup> it = localGroups.iterator();
            while (it.hasNext()) {
                LocalGroup group = it.next();
                if (group.getName().equals(groupName)) {
                    group.getUsers().remove(user);
                    group.getUserNames().remove(getUserId(user));
                    for (LocalGroupListener listener : getSafeGroupListeners()) {
                        listener.userDeleted(group, user);
                    }
                    break;
                }
            }
            if (getLocalGroups(user).isEmpty()) {
                // if the user is in no local groups and no roster groups remove
                // them from our roster.
                IRosterEntry entry = getRosterEntry(user);
                if (entry != null && entry.getGroups().isEmpty()) {
                    IPresence presence = new Presence(Type.UNSUBSCRIBE);
                    try {
                        connection.getRosterManager().getPresenceSender()
                                .sendPresenceUpdate(user.getID(), presence);
                    } catch (ECFException e) {
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
                    ArrayList<IUser> users = new ArrayList<IUser>(
                            group.getUsers());
                    for (IUser user : users) {
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

    public List<LocalGroup> getLocalGroups(IUser user) {
        List<LocalGroup> results = new ArrayList<LocalGroup>();
        synchronized (localGroups) {
            for (LocalGroup group : localGroups) {
                for (String userName : group.getUserNames()) {
                    if (getUserId(user).equals(userName)) {
                        results.add(group);
                        break;
                    }
                }
            }
        }
        return results;
    }

    public void setNickname(IUser user, String nickname) {
        synchronized (localAliases) {

            localAliases.put(getUserId(user), nickname);
            try {
                UserIdWrapper.saveAliasMap(localAliases);
            } catch (LocalizationOpFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            applyLocalAliases(getRoster());
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
    public String getDisplayName(IUser user) {
        String alias = localAliases.get(getUserId(user));
        if (alias == null) {
            // at this point try to get the user from roster;
            IUser rosterUser = null;
            if (user.getID() != null) {
                rosterUser = getUser(user.getID());
            }
            if (rosterUser != null) {
                user = rosterUser;
            }
            alias = user.getNickname();
            if (alias == null) {
                alias = user.getName();
                if (alias == null) {
                    alias = user.getID().getName();
                }
            }
            if (alias.contains("@")) {
                alias = Tools.parseName(alias);
            }
        }
        return alias;
    }

    public IUser getUser(ID id) {
        return getUser(getUserId(id));
    }

    public IUser getUser(String userId) {
        userId = normalizeId(userId);
        IRosterEntry entry = searchRoster(getRoster(), userId);
        if (entry == null) {
            return null;
        }
        return entry.getUser();
    }

    public IRosterEntry getRosterEntry(IUser user) {
        return searchRoster(user);
    }

    public IPresence getPresence(IUser user) {
        IRosterEntry entry = searchRoster(user);
        if (entry == null) {
            return new Presence(Type.UNKNOWN);
        }
        return entry.getPresence();
    }

    /**
     * Used by local groups to make sure all local group items are in the
     * roster.
     * 
     * @param name
     * @return
     */
    protected IUser findAndAddUser(String name) {
        name = normalizeId(name);
        IUser user = null;
        IRosterEntry entry = searchRoster(getRoster(), name);
        if (entry != null) {
            user = entry.getUser();
        }
        if (user == null) {
            user = findUser(name);
            if (user != null) {
                try {
                    connection
                            .getRosterManager()
                            .getRosterSubscriptionSender()
                            .sendRosterAdd(getUserId(user), user.getName(),
                                    null);
                } catch (ECFException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return user;

    }

    private IUser findUser(String name) {
        IUserSearchManager searchManager = connection
                .getPresenceContainerAdapter().getUserSearchManager();
        ICriterion criterion = searchManager.createRestriction().eq("Username",
                Tools.parseName(name));
        ICriteria criteria = searchManager.createCriteria();
        criteria.add(criterion);
        try {
            ISearch search = searchManager.search(criteria);
            for (Object result : search.getResultList().getResults()) {
                if (result instanceof IResult) {
                    IUser user = ((IResult) result).getUser();
                    String alias = localAliases.get(getUserId(user));
                    if (alias != null && user instanceof User) {
                        ((User) user).setNickname(alias);
                    }
                    return user;
                }
            }
        } catch (UserSearchException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return null;
    }

    private IRoster getRoster() {
        return connection.getRosterManager().getRoster();
    }

    /**
     * given a user return username@host.
     * 
     * @param user
     * @return
     */
    private String getUserId(IUser user) {
        if (user.getID() == null) {
            return normalizeId(user.toString());
        }
        return getUserId(user.getID());
    }

    /**
     * given a user ID return username@host.
     * 
     * @param id
     * @return
     */
    private String getUserId(ID id) {
        return normalizeId(id.getName());
    }

    private String normalizeId(String userId) {
        String name = Tools.parseName(userId);
        String hostname = Tools.parseHost(userId);
        hostname = IDConverter.normalizeHostname(hostname);
        return name + "@" + hostname;
    }

    private IRosterEntry searchRoster(IUser user) {
        String userId = getUserId(user);
        return searchRoster(getRoster(), userId);
    }

    private IRosterEntry searchRoster(IRosterItem item, String userName) {
        if (item instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) item;
            if (userName.equals(getUserId(entry.getUser()))) {
                return entry;
            }
        } else if (item instanceof IRosterGroup) {
            Collection<?> entries = ((IRosterGroup) item).getEntries();
            synchronized (entries) {
                entries = new ArrayList<Object>(entries);
            }
            for (Object o : entries) {
                IRosterEntry entry = searchRoster((IRosterItem) o, userName);
                if (entry != null) {
                    return entry;
                }
            }
        } else if (item instanceof IRoster) {
            Collection<?> entries = ((IRoster) item).getItems();
            synchronized (entries) {
                entries = new ArrayList<Object>(entries);
            }
            for (Object o : entries) {
                IRosterEntry entry = searchRoster((IRosterItem) o, userName);
                if (entry != null) {
                    return entry;
                }
            }
        } else {
            throw new IllegalStateException("Unexpected Roster entry: "
                    + item.getClass().getSimpleName());
        }
        return null;
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

        public void userAdded(LocalGroup group, IUser user);

        public void userDeleted(LocalGroup group, IUser user);

    }
}
