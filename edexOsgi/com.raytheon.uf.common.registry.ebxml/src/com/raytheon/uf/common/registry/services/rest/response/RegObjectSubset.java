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

package com.raytheon.uf.common.registry.services.rest.response;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * A Helper class used to contain the RegistryObjectType Id, owner and
 * updateTime.
 * 
 * The particularity of this class is the fact that it is hashable based on the
 * String id.
 * 
 * This class will be used in the SynchronizationTask.java Custom computation to
 * allow comparison with other ids while also keeping track on the owner and
 * updateTime
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#   Engineer    Description
 * ------------- -------- --------- -----------------------
 * 11-13-2018    7238      skabasele   Initial creation
 *
 * </pre>
 *
 * @author skabasele
 */
@XmlRootElement(name = "RegObjectSubset")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = { "id", "owner", "updateTime" })
public class RegObjectSubset {

    @XmlElement
    private String id;

    @XmlElement
    private String owner;

    @XmlElement
    private Date updateTime;

    public RegObjectSubset() {
        this.id = new String();
        this.owner = new String();
        this.updateTime = new Date();
    }

    public RegObjectSubset(String id, String owner, Date updateTime) {
        this.id = id;
        this.owner = owner;
        this.updateTime = updateTime;

    }

    public void setId(String id) {
        this.id = id;
    }

    public String getId() {
        return this.id;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public String getOwner() {
        return owner;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    public Date getUpdateTime() {
        return this.updateTime;
    }

    @Override
    public int hashCode() {

        return this.id.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return this.id.equals(obj);
    }
}
