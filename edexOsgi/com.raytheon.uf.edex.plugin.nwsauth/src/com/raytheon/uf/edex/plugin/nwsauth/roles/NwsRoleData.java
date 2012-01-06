package com.raytheon.uf.edex.plugin.nwsauth.roles;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.core.EdexException;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class NwsRoleData implements ISerializableObject {

    private Map<String, NwsRole> userRolesMap;

    private List<NwsRole> allRoles = new ArrayList<NwsRole>();

    public NwsRole lookupRole(String roleId) {
        if (userRolesMap == null) {
            userRolesMap = new HashMap<String, NwsRole>();
            for (NwsRole role : allRoles) {
                userRolesMap.put(role.getRoleId().toLowerCase(), role);
            }

            for (NwsRole role : allRoles) {
                role.addRolesForUsers(userRolesMap);
            }
        }
        return userRolesMap.get(("" + roleId).toLowerCase());
    }

    public NwsRole[] getAllRolesCollection() {
        return this.allRoles.toArray(new NwsRole[this.allRoles.size()]);
    }

    @XmlElement(name = "role")
    public void setAllRolesCollection(NwsRole[] allRolesArray) {
        this.allRoles = new ArrayList<NwsRole>(Arrays.asList(allRolesArray));
    }

    public String toXML() throws EdexException {
        try {
            return SerializationUtil.marshalToXml(this);
        } catch (JAXBException e) {
            throw new EdexException("", e);
        }
    }

    public static NwsRoleData loadRoleData(File file) throws EdexException {
        try {
            return (NwsRoleData) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(file.getAbsolutePath());
        } catch (Exception e) {
            e.printStackTrace();
            throw new EdexException("", e);
        }
    }

    public void storeRoleData(File file) throws EdexException {
        try {
            if (file != null) {
                SerializationUtil.jaxbMarshalToXmlFile(this,
                        file.getAbsolutePath());
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new EdexException("", e);
        }
    }
}
