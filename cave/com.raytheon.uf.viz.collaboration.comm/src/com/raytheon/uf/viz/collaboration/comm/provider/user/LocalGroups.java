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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * Group information not stored on chat server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "LocalGroups")
public class LocalGroups {

    @XmlElement(name = "group")
    private List<LocalGroup> groups;

    public LocalGroups() {
    }

    public LocalGroups(List<LocalGroup> groups) {
        this.groups = new ArrayList<LocalGroup>(groups);
    }

    public List<LocalGroup> getGroups() {
        return groups;
    }

    public void setGroups(List<LocalGroup> groups) {
        this.groups = groups;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class LocalGroup {

        @XmlAttribute
        private String name;

        @XmlElement(name = "user")
        private List<String> userNames;

        @XmlTransient
        private ContactsManager manager;

        @XmlTransient
        private List<UserId> users;

        public LocalGroup() {
        }

        public LocalGroup(String name) {
            this.name = name;
            this.userNames = new ArrayList<String>();
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public List<String> getUserNames() {
            if (userNames == null) {
                userNames = new ArrayList<String>();
            }
            return userNames;
        }

        public void setUserNames(List<String> userNames) {
            this.userNames = userNames;
        }

        public synchronized List<UserId> getUsers() {
            if (users == null) {
                users = new ArrayList<UserId>();
                for (String userName : userNames) {
                    UserId user = manager.findAndAddUser(userName);
                    if (user != null) {
                        users.add(user);
                    }
                }
            }
            return users;
        }

        public void setManager(ContactsManager manager) {
            this.manager = manager;
        }

    }

}
