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

package oasis.names.tc.ebxml.regrep.xsd.rim.v4;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents information about the version of the object it is describing.
 * 
 * <p>
 * Java class for VersionInfoType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="VersionInfoType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attribute name="versionName" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="userVersionName" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/2/2013     1829       bphillip    Added hashcode and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Embeddable
@XmlRootElement(name = "VersionInfo")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class VersionInfoType implements Serializable,
        IPersistableDataObject<String>, Comparable<VersionInfoType> {

    private static final long serialVersionUID = -2869857115641981790L;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    protected String versionName = "1";

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    protected String userVersionName;

    public VersionInfoType() {

    }

    public VersionInfoType(String versionName) {
        this.versionName = versionName;
    }

    /**
     * Gets the value of the versionName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getVersionName() {
        return versionName;
    }

    /**
     * Sets the value of the versionName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setVersionName(String value) {
        this.versionName = value;
    }

    /**
     * Gets the value of the userVersionName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getUserVersionName() {
        return userVersionName;
    }

    /**
     * Sets the value of the userVersionName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setUserVersionName(String value) {
        this.userVersionName = value;
    }

    @Override
    public String toString() {
        return versionName + "_" + userVersionName;
    }

    @Override
    public String getIdentifier() {
        return versionName;
    }

    @Override
    public int compareTo(VersionInfoType obj) {

        if (this.versionName == null && obj.getVersionName() == null) {
            return 0;
        } else if (this.versionName == null) {
            return -1;
        } else if (obj.getVersionName() == null) {
            return 1;
        }

        String[] versionParts1 = this.versionName.split("\\.");
        String[] versionParts2 = obj.versionName.split("\\.");

        for (int i = 0; i < versionParts1.length; i++) {
            int part1 = Integer.parseInt(versionParts1[i]);
            if (i >= versionParts2.length) {
                return 1;
            }
            int part2 = Integer.parseInt(versionParts2[i]);
            if (part1 > part2) {
                return 1;
            } else if (part1 < part2) {
                return -1;
            } else if (i >= versionParts1.length - 1
                    && versionParts2.length > i) {
                return -1;
            }
        }
        return 0;
    }

    public boolean greaterThan(VersionInfoType obj) {
        return this.compareTo(obj) > 0 ? true : false;
    }

    public boolean greaterThanEquals(VersionInfoType obj) {
        return this.compareTo(obj) >= 0 ? true : false;
    }

    public boolean lessThan(VersionInfoType obj) {
        return this.compareTo(obj) < 0 ? true : false;
    }

    public boolean lessThanEquals(VersionInfoType obj) {
        return this.compareTo(obj) <= 0 ? true : false;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((userVersionName == null) ? 0 : userVersionName.hashCode());
        result = prime * result
                + ((versionName == null) ? 0 : versionName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        VersionInfoType other = (VersionInfoType) obj;
        if (userVersionName == null) {
            if (other.userVersionName != null)
                return false;
        } else if (!userVersionName.equals(other.userVersionName))
            return false;
        if (versionName == null) {
            if (other.versionName != null)
                return false;
        } else if (!versionName.equals(other.versionName))
            return false;
        return true;
    }

}
