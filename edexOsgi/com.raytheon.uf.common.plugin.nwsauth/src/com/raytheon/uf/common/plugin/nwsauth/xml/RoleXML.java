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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Role element
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
public class RoleXML implements ISerializableObject {
    @DynamicSerializeElement
    @XmlAttribute(name = "roleId")
    private String roleId;

    @DynamicSerializeElement
    @XmlElement(name = "roleDescription", type = String.class)
    private String roleDescription = null;

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "rolePermission", type = String.class) })
    private List<String> permissionList = new ArrayList<String>();

    /**
     * @return the roleId
     */
    public String getRoleId() {
        return roleId;
    }

    /**
     * @param roleId
     *            the roleId to set
     */
    public void setRoleId(String roleId) {
        this.roleId = roleId;
    }

    /**
     * @return the description
     */
    public String getRoleDescription() {
        return roleDescription.trim();
    }

    /**
     * @param roleDescription
     *            the description to set
     */
    public void setRoleDescription(String roleDescription) {
        this.roleDescription = roleDescription.trim();
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
        this.permissionList.add(permission);
    }
}
