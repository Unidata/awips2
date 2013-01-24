package com.raytheon.uf.common.datadelivery.registry;

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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Connection XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2011    191      dhladky     Initial creation
 * Jun 28, 2012    819      djohnson    Remove proxy information.
 * Jul 24, 2012    955      djohnson    Add copy constructor.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "connection")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Connection implements ISerializableObject, Serializable {

    private static final long serialVersionUID = 8223819912383198409L;

    public Connection() {

    }

    /**
     * Copy constructor.
     * 
     * @param connection
     *            the connection to copy
     */
    public Connection(Connection connection) {
        setPassword(connection.getPassword());
        setUrl(connection.getUrl());
        setUserName(connection.getUserName());
    }

    @XmlElement(name = "userName")
    @DynamicSerializeElement
    private String userName;

    @XmlElement(name = "password")
    @DynamicSerializeElement
    private String password;

    @XmlElement(name = "url")
    @DynamicSerializeElement
    private String url;

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getUserName() {
        return userName;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getPassword() {
        return password;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

}
