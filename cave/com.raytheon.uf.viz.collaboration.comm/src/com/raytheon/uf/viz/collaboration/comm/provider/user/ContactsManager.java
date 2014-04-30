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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.jivesoftware.smack.Roster;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;
import org.jivesoftware.smack.packet.RosterPacket.ItemStatus;
import org.jivesoftware.smack.packet.RosterPacket.ItemType;
import org.jivesoftware.smack.packet.XMPPError;
import org.jivesoftware.smackx.SharedGroupManager;

import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.collections.UpdatingSet;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;

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
 * Dec  6, 2013 2561       bclement     removed ECF
 * Dec 20, 2013 2563       bclement     roster items now removed from server,
 *                                      removed unneeded roster listener
 * Jan 24, 2014 2701       bclement     removed roster manager
 *                                      switched local groups to roster groups
 *                                      added shared groups
 * Jan 27, 2014 2700       bclement     fixed ungrouped entries being out of date
 *                                      added utility methods for subscription status
 * Jan 30, 2014 2698       bclement     removed unneeded nickname changed event
 * Jan 31, 2014 2700       bclement     added addToRoster, fixed add to group when in roster, but blocked
 * Feb  3, 2014 2699       bclement     fixed assumption that username search was exact
 * Apr 11, 2014 2903       bclement     moved roster listener from collaboration connection to here
 * Apr 16, 2014 2981       bclement     fixed NPE when cached shared group deleted on server
 * Apr 23, 2014 2822       bclement     moved roster listener to ContactsListener, 
 *                                      added getSharedDisplayEnabledResource()
 * Apr 24, 2014 3070       bclement     added checks for empty groups, added isContact(),
 *                                      added sendContactRequest()
 *                                      fixed contact request logic in addToGroup()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ContactsManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContactsManager.class);

    private final CollaborationConnection connection;

    private final XMPPConnection xmpp;

    private final UserSearch search;

    private Map<String, String> localAliases;

    private final ContactsListener contactsListener;

    /**
     * Cached view of shared groups list on openfire. Will only reach out to
     * server if it hasn't updated in an hour. This will disable itself if there
     * is a problem communicating with the server since the most likely case is
     * that the server doesn't support the operation.
     */
    private UpdatingSet<String> sharedGroups = new UpdatingSet<String>(
            TimeUtil.MILLIS_PER_HOUR) {
        @Override
        protected Set<String> update() {
            Set<String> rval;
            try {
                List<String> names = SharedGroupManager.getSharedGroups(xmpp);
                rval = new HashSet<String>(names);
            } catch (XMPPException e) {
                statusHandler.warn("Unable to get shared groups."
                        + " Feature may not exist on server",
                        e.getLocalizedMessage());
                disable();
                rval = null;
            }
            return rval;
        }
    };

    private Set<GroupListener> groupListeners = new HashSet<GroupListener>();

    /**
     * @param connection
     * @param xmpp
     */
    public ContactsManager(CollaborationConnection connection,
            XMPPConnection xmpp) {
        this.connection = connection;
        this.search = connection.createSearch();
        localAliases = UserIdWrapper.readAliasMap();
        this.xmpp = xmpp;
        Roster roster = xmpp.getRoster();
        this.contactsListener = new ContactsListener(this);
        roster.addRosterListener(this.contactsListener);
    }

    /**
     * Get groups that are managed by server. These are not modifiable from the
     * client.
     * 
     * @return
     */
    public Collection<SharedGroup> getSharedGroups() {
        Set<String> groups = sharedGroups.get();
        List<SharedGroup> rval = new ArrayList<SharedGroup>(groups.size());
        Roster roster = getRoster();
        for (String group : groups) {
            RosterGroup rg = roster.getGroup(group);
            /*
             * group will be null if it has been removed from server after
             * cached in shared groups.
             */
            if (rg != null && !rg.getEntries().isEmpty()) {
                rval.add(new SharedGroup(rg));
            }
        }
        return rval;
    }

    /**
     * Get groups that are managed by the client. This does not included shared
     * groups.
     * 
     * @return
     */
    public Collection<RosterGroup> getGroups() {
        Set<String> shared = sharedGroups.get();
        Collection<RosterGroup> groups = getRoster().getGroups();
        Collection<RosterGroup> rval;
        if (shared.isEmpty()) {
            rval = groups;
        } else {
            rval = new ArrayList<RosterGroup>(groups.size());
            for (RosterGroup group : groups) {
                if (!shared.contains(group.getName())
                        && !group.getEntries().isEmpty()) {
                    rval.add(group);
                }
            }
        }
        return rval;
    }

    /**
     * Add user to group. Creates group if it doesn't exist. Adds user to
     * contacts if not in roster.
     * 
     * @param groupName
     * @param user
     */
    public void addToGroup(String groupName, UserId user) {
        Roster roster = getRoster();
        RosterGroup group = roster.getGroup(groupName);
        if (group == null) {
            group = createGroup(groupName);
        }
        RosterEntry entry = getRosterEntry(user);
        if (entry != null && isBlocked(entry)) {
            /* entry is in roster, but we are blocked */
            try {
                sendContactRequest(user);
            } catch (CollaborationException e) {
                statusHandler.error("Problem subscribing to user", e);
            }
        }
        try {
            addToGroup(group, user);
            for (GroupListener listener : getSafeGroupListeners()) {
                listener.userAdded(group, user);
            }
        } catch (XMPPException e) {
            String msg = getGroupModInfo(e);
            statusHandler.error("Problem adding user to group: " + user
                    + " to " + group.getName() + ". " + msg, e);
        }
    }

    /**
     * Add user to group. Adds user to contacts if not in roster.
     * 
     * @param group
     * @param user
     * @throws XMPPException
     */
    private void addToGroup(RosterGroup group, UserId user)
            throws XMPPException {
        RosterEntry entry = getRosterEntry(user);
        if (entry == null) {
            // we dont have user as a contact at all
            // ensure that the user object is up-to-date
            user = update(user);
            String alias = user.getAlias();
            if (StringUtils.isBlank(alias)) {
                alias = user.getName();
            }
            getRoster().createEntry(user.getFQName(), alias,
                    new String[] { group.getName() });
        } else {
            // just need to update groups
            group.addEntry(entry);
        }
    }

    /**
     * Ensure that user is in roster.
     * 
     * @param user
     * @throws CollaborationException
     */
    public void addToRoster(UserId user) throws CollaborationException {
        RosterEntry entry = getRosterEntry(user);
        if (entry == null) {
            // we dont have user as a contact at all
            // ensure that the user object is up-to-date
            user = update(user);
            String alias = user.getAlias();
            if (StringUtils.isBlank(alias)) {
                alias = user.getName();
            }
            try {
                getRoster().createEntry(user.getFQName(), alias, new String[0]);
            } catch (XMPPException e) {
                throw new CollaborationException(
                        "Unable to add user to roster: " + user, e);
            }
        }
    }

    /**
     * Remove user from group.
     * 
     * @param groupName
     * @param user
     */
    public void deleteFromGroup(String groupName, UserId user) {
        RosterEntry entry = getRosterEntry(user);
        if (entry == null) {
            statusHandler.warn("Attempted to alter group for non-contact: "
                    + user);
            return;
        }
        RosterGroup group = getRoster().getGroup(groupName);
        if (group != null) {
            deleteFromGroup(group, entry);
        } else {
            statusHandler.warn("Attempted to modify non-existent group: "
                    + groupName);
        }
    }

    /**
     * Remove entry from group.
     * 
     * @param group
     * @param entry
     */
    private void deleteFromGroup(RosterGroup group, RosterEntry entry) {
        try {
            group.removeEntry(entry);
            for (GroupListener listener : getSafeGroupListeners()) {
                listener.userDeleted(group, IDConverter.convertFrom(entry));
            }
        } catch (XMPPException e) {
            String msg = getGroupModInfo(e);
            statusHandler.error(
                    "Problem removing entry from group: "
                            + IDConverter.convertFrom(entry) + " from "
                            + group.getName() + ". " + msg, e);
        }
    }

    /**
     * Attempt to get more information about group modification error. Returns
     * an empty string if no extra information is found.
     * 
     * @param e
     * @return
     */
    private String getGroupModInfo(XMPPException e) {
        XMPPError xmppError = e.getXMPPError();
        String rval = "";
        if (xmppError != null) {
            switch (xmppError.getCode()) {
            case 406:
                rval = "Group may not be modifiable. ";
                break;
            }
        }
        return rval;
    }

    /**
     * Remove entry from roster on server
     * 
     * @param entry
     * @throws CollaborationException
     */
    public void removeFromRoster(RosterEntry entry) {
        Roster roster = getRoster();
        try {
            roster.removeEntry(entry);
        } catch (XMPPException e) {
            statusHandler.error("Problem removing roster entry", e);
        }
    }

    /**
     * Create group. At least one entry must be placed into group for it to be
     * persisted on server.
     * 
     * @param groupName
     * @return
     */
    public RosterGroup createGroup(String groupName) {
        Roster roster = getRoster();
        RosterGroup rval = roster.getGroup(groupName);
        if (rval != null) {
            statusHandler.debug("Attempted to create existing group: "
                    + groupName);
            return rval;
        }
        rval = roster.createGroup(groupName);
        for (GroupListener listener : getSafeGroupListeners()) {
            listener.groupCreated(rval);
        }
        return rval;
    }

    /**
     * Remove all users from group.
     * 
     * @param groupName
     */
    public void deleteGroup(String groupName) {
        Roster roster = getRoster();
        RosterGroup group = roster.getGroup(groupName);
        if (group == null) {
            statusHandler.warn("Attempted to delete non-existent group: "
                    + groupName);
            return;
        }
        Collection<RosterEntry> entries = group.getEntries();
        for (RosterEntry entry : entries) {
            deleteFromGroup(group, entry);
        }
        for (GroupListener listener : getSafeGroupListeners()) {
            listener.groupDeleted(group);
        }
    }

    /**
     * Move all users from old group to new group. If new group already exists,
     * this will merge the two groups.
     * 
     * @param oldName
     * @param newName
     */
    public void renameGroup(String oldName, String newName) {
        Roster roster = getRoster();
        RosterGroup group = roster.getGroup(oldName);
        if (group == null) {
            statusHandler.warn("Attempted to rename non-existent group: "
                    + oldName);
            return;
        }
        boolean merger = roster.getGroup(newName) != null;
        group.setName(newName);
        for (GroupListener listener : getSafeGroupListeners()) {
            listener.groupDeleted(group);
        }
        if (!merger) {
            for (GroupListener listener : getSafeGroupListeners()) {
                listener.groupCreated(group);
            }
        }
    }

    /**
     * Get groups that the user is in.
     * 
     * @param user
     * @return
     */
    public Collection<RosterGroup> getGroups(UserId user) {
        RosterEntry entry = getRoster().getEntry(user.getNormalizedId());
        if (entry == null) {
            statusHandler.debug("Requested groups for user not in roster: "
                    + user);
            return Collections.emptyList();
        }
        return entry.getGroups();
    }

    /**
     * Update local alias for user. Does not persist to server.
     * 
     * @param user
     * @param nickname
     */
    public void setNickname(UserId user, String nickname) {
        synchronized (localAliases) {

            localAliases.put(user.getNormalizedId(), nickname);
            try {
                UserIdWrapper.saveAliasMap(localAliases);
            } catch (LocalizationOpFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
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

    /**
     * Get user info from roster. Does not include local alias information.
     * 
     * @param userId
     * @return
     */
    public UserId getUser(String userId) {
        RosterEntry entry = searchRoster(userId);
        if (entry == null) {
            return null;
        }
        return IDConverter.convertFrom(entry);
    }

    /**
     * Get entry from roster for user. Does not include local alias information.
     * 
     * @param user
     * @return
     */
    public RosterEntry getRosterEntry(UserId user) {
        return searchRoster(user.getNormalizedId());
    }

    /**
     * Get last known presence for contact.
     * 
     * @param user
     * @return
     */
    public Presence getPresence(UserId user) {
        UserId self = connection.getUser();
        if (self.isSameUser(user)) {
            return getSelfPresence();
        }
        Roster roster = getRoster();
        return roster.getPresence(user.getNormalizedId());
    }

    /**
     * Get presence for this account.
     * 
     * @return
     */
    public Presence getSelfPresence() {
        return connection.getPresence();
    }

    /**
     * Convenience method for accessing roster.
     * 
     * @return
     */
    private Roster getRoster() {
        return xmpp.getRoster();
    }

    /**
     * Perform an XMPP search for user. Includes any local alias information.
     * Only return non-null on an exact match.
     * 
     * @param username
     *            The part of the userid before the '@'
     * @return null if not found
     */
    private UserId findUser(String username) {
        try {
            UserId rval = search.byExactUsername(username);
            if (rval != null) {
                String alias = localAliases.get(rval.getNormalizedId());
                if (alias != null) {
                    rval.setAlias(alias);
                }
            }
            return rval;
        } catch (XMPPException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }
    }

    /**
     * Get entry from roster for user. Does not include local alias information.
     * 
     * @param userId
     * @return
     */
    private RosterEntry searchRoster(String userId) {
        return getRoster().getEntry(userId);
    }

    /**
     * Add listeners to get information on when groups are modified
     * 
     * @param listener
     */
    public void addGroupListener(GroupListener listener) {
        synchronized (groupListeners) {
            groupListeners.add(listener);
        }
    }

    /**
     * Remove listener
     * 
     * @param listener
     */
    public void removeGroupListener(GroupListener listener) {
        synchronized (groupListeners) {
            groupListeners.remove(listener);
        }
    }

    /**
     * Get a copy of the listeners set
     * 
     * @return
     */
    protected Set<GroupListener> getSafeGroupListeners() {
        Set<GroupListener> safeSet = new HashSet<GroupListener>();
        synchronized (groupListeners) {
            safeSet.addAll(groupListeners);
        }
        return safeSet;
    }

    /**
     * Get a list of roster entries that do not belong to any group
     * 
     * @return
     */
    public Collection<RosterEntry> getNonGroupedContacts() {
        Collection<RosterEntry> unfiled = getRoster().getUnfiledEntries();
        List<RosterEntry> rval = new ArrayList<RosterEntry>(unfiled.size());
        for (RosterEntry entry : unfiled) {
            if (isBlocked(entry)) {
                // check to see if we are really blocked
                entry = update(entry);
            }
            if (hasInteraction(entry)) {
                rval.add(entry);
            }
        }
        return rval;
    }

    /**
     * Get updated entry from roster. This is a work around for a smack
     * limitation where unfiled entry objects are not updated when the roster
     * entry objects are.
     * 
     * @param entry
     * @return
     */
    public RosterEntry update(RosterEntry entry) {
        RosterEntry rval = searchRoster(entry.getUser());
        return rval != null ? rval : entry;
    }

    /**
     * Get updated alias information for user. This will attempt to get local
     * alias information, if non available, it will return alias from user
     * search, if both fail, it will return the same object.
     * 
     * @param user
     * @return
     */
    public UserId update(UserId user) {
        UserId updated = findUser(user.getName());
        return updated != null ? updated : user;
    }

    /**
     * 
     * @param entry
     * @return true if we are blocked from seeing updates from user in entry
     */
    public static boolean isBlocked(RosterEntry entry) {
        ItemType type = entry.getType();
        return type != null
                && (type.equals(ItemType.none) || type.equals(ItemType.from));
    }

    /**
     * Test for interaction between this user and entry. Returns true if either
     * has a subscription to the other or there is a pending subscription
     * request.
     * 
     * @param entry
     * @return
     */
    public static boolean hasInteraction(RosterEntry entry) {
        ItemType type = entry.getType();
        boolean rval = true;
        if (type != null) {
            if (type.equals(ItemType.none)) {
                ItemStatus status = entry.getStatus();
                rval = status != null
                        && status.equals(ItemStatus.SUBSCRIPTION_PENDING);
            }
        }
        return rval;
    }

    /**
     * 
     * @param entry
     * @return true if we can see updates from user in entry
     */
    public static boolean isContact(RosterEntry entry) {
        ItemType type = entry.getType();
        return type != null
                && (type.equals(ItemType.both) || type.equals(ItemType.to));
    }

    /**
     * @see #isContact(RosterEntry)
     * @param id
     * @return true if we can see updates from user
     */
    public boolean isContact(UserId id) {
        RosterEntry entry = getRosterEntry(id);
        boolean rval = false;
        if (entry != null) {
            rval = isContact(entry);
        }
        return rval;
    }

    /**
     * @see ContactsListener#getSharedDisplayEnabledResource(UserId)
     * @param user
     * @return
     */
    public String getSharedDisplayEnabledResource(UserId user) {
        return contactsListener.getSharedDisplayEnabledResource(user);
    }

    /**
     * Send a contact request to user
     * 
     * @param user
     * @throws CollaborationException
     */
    public void sendContactRequest(UserId user) throws CollaborationException {
        IAccountManager manager = connection.getAccountManager();
        manager.sendPresence(user, new Presence(Type.subscribe));
    }

    /**
     * Listener interface for group update events
     */
    public static interface GroupListener {

        public void groupCreated(RosterGroup group);

        public void groupDeleted(RosterGroup group);

        public void userAdded(RosterGroup group, UserId user);

        public void userDeleted(RosterGroup group, UserId user);

    }
}
