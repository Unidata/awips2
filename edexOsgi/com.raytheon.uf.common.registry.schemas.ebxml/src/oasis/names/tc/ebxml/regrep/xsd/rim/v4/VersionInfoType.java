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
 * 
 */
@Embeddable
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class VersionInfoType implements Serializable,
        IPersistableDataObject<String> {

    private static final long serialVersionUID = -2869857115641981790L;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    protected String versionName = "1";

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    protected String userVersionName;

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

}
