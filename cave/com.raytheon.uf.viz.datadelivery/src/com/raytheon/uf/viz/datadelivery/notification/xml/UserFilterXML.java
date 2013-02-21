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
package com.raytheon.uf.viz.datadelivery.notification.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
public class UserFilterXML implements ISerializableObject {
    @XmlAttribute(name = "allUsers")
    protected boolean allUsers;
    
    @XmlAttribute(name = "selfInclude")
    protected boolean selfInclude;
    
    @XmlElements({ @XmlElement(name = "user", type = String.class) })
    protected ArrayList<String> userList = new ArrayList<String>();

    /**
     * The is all users flag.
     * 
     * @return
     *     true if all users selected
     */
    public boolean isAllUsers() {
        return allUsers;
    }

    /**
     * Set the all users flag.
     * 
     * @param allUsers
     *            all users flag
     */
    public void setAllUsers(boolean allUsers) {
        this.allUsers = allUsers;
    }

    /**
     * is Always include my notifications check box flag.
     * 
     * @return
     *     true if check box selected
     */
    public boolean isSelfInclude() {
        return selfInclude;
    }

    /**
     * Set the Always include my notifications check box.
     * 
     * @param selfInclude
     *             true if check box selected
     */
    public void setSelfInclude(boolean selfInclude) {
        this.selfInclude = selfInclude;
    }

    /**
     * Get the filter user list.
     * 
     * @return
     *      the user list
     */
    public ArrayList<String> getUserList() {
        return userList;
    }

    /**
     * Set the user list.
     * 
     * @param userList
     *          the user list
     */
    public void setUserList(ArrayList<String> userList) {
        this.userList = userList;
    }
    
    /**
     * Add a user to the list. 
     * 
     * @param user
     *          the user
     */
    public void addUser(String user) {
        if (!userList.contains(user)) {
            userList.add(user);
        }
    }
    
    /**
     * Clear the user list.
     */
    public void clearUsers() {
        
        userList.clear();
    }
}
