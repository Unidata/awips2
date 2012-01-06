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
package com.raytheon.uf.common.dataplugin.warning;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * UGC Zones are part of Warning Records. This class will be utilized by the
 * Warning Decoder.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 12, 2008				bwoodle	Initial creation
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */
@Entity
@Table(name = "warning_ugczone")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UGCZone extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    @XmlAttribute
    @DynamicSerializeElement
    private Integer key;

    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String zone;

    @ManyToOne
    @JoinColumn(name = "parentWarning", nullable = false)
    private AbstractWarningRecord parentWarning;

    public UGCZone() {
    }

    public UGCZone(String zone) {
        this.zone = zone;
    }

    public UGCZone(String zone, AbstractWarningRecord warning) {
        this.zone = zone;
        parentWarning = warning;
    }

    public String toString() {
        return zone;
    }

    /**
     * @return the key
     */
    public Integer getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(Integer key) {
        this.key = key;
    }

    /**
     * @return the zone
     */
    public String getZone() {
        return zone;
    }

    /**
     * @param zone
     *            the zone to set
     */
    public void setZone(String zone) {
        this.zone = zone;
    }

    /**
     * @return the parentWarning
     */
    public AbstractWarningRecord getParentWarning() {
        return parentWarning;
    }

    /**
     * @param parentWarning
     *            the parentWarning to set
     */
    public void setParentWarning(AbstractWarningRecord parentWarning) {
        this.parentWarning = parentWarning;
    }
}
