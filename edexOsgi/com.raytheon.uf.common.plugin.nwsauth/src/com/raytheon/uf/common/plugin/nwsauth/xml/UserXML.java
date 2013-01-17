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
package com.raytheon.uf.common.plugin.nwsauth.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.auth.user.IUserId;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * User element
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UserXML implements IUser, ISerializableObject {
    @DynamicSerializeElement
    @XmlAttribute(name = "userId")
    private String userId;

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "userPermission", type = String.class) })
    private List<String> permissionList = new ArrayList<String>();

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "userRole", type = String.class) })
    private List<String> roleList = new ArrayList<String>();

    public UserXML() {

    }

    /**
     * @param userId
     */
    public UserXML(String userId) {
        setUserId(userId);
    }

    /**
     * @return the userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * @param iduserId
     *            the userId to set
     */
    public void setUserId(String userId) {
        this.userId = userId;
    }

    /**
     * @return the permissionList
     */
    public List<String> getPermissionList() {
        return permissionList;
    }

    /**
     * @param permissionList
     *            the permissionList to set
     */
    public void setPermissionList(List<String> permissionList) {
        this.permissionList = permissionList;
    }

    /**
     * Add a permission
     * 
     * @param permission
     *            The permission to add
     */
    public void addPermission(String permission) {
        if (!this.permissionList.contains(permission)) {
            this.permissionList.add(permission);
        }
    }

    /**
     * @return the roleList
     */
    public List<String> getRoleList() {
        return roleList;
    }

    /**
     * @param roleList
     *            the roleList to set
     */
    public void setRoleList(List<String> roleList) {
        this.roleList = roleList;
    }

    public void addRole(String role) {
        if (!this.roleList.contains(role)) {
            this.roleList.add(role);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof UserXML) {
            UserXML that = (UserXML) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(this.getUserId(), that.getUserId());
            return builder.isEquals();

        }
        return super.equals(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.getUserId()).toHashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("userId:").append(this.getUserId());
        sb.append("\nroles:").append(this.getRoleList());
        sb.append("\npermissions:").append(this.getPermissionList());

        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.auth.user.IUser#uniqueId()
     */
    @Override
    public IUserId uniqueId() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.auth.user.IUser#authenticationData()
     */
    @Override
    public IAuthenticationData authenticationData() {
        // TODO Auto-generated method stub
        return null;
    }
}
