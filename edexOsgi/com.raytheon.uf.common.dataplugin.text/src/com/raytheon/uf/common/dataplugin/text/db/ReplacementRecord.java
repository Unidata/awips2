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
package com.raytheon.uf.common.dataplugin.text.db;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data object for subscription record substitution attribute
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2014 2536       bclement    moved from autobldsrv to common.dataplugin.text
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = ReplacementRecord.SEQ_GEN_NAME, sequenceName = "replacementseq")
@Table(name = "replacements")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ReplacementRecord extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    public static final String SEQ_GEN_NAME = "REPLACEMENT_GENERATOR";

    @XmlAttribute
    @DynamicSerializeElement
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = SEQ_GEN_NAME)
    private long id;

    @ManyToOne
    @JoinColumn(name = "subscription", nullable = false)
    private SubscriptionRecord subscription;

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 50)
    private String key;

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 2048)
    private String value;

    /**
     * Constructor.
     */
    public ReplacementRecord() {
        super();
    }

    /**
     * Constructor. Creates a replacement record bound to the specified
     * subscription record.
     * 
     * @param parent
     *            the subscription record
     * @param key
     *            the replacement key
     * @param value
     *            the replacement value
     */
    public ReplacementRecord(SubscriptionRecord parent, String key, String value) {
        this.subscription = parent;
        this.key = key;
        this.value = value;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * @return the subscription
     */
    public SubscriptionRecord getSubscription() {
        return subscription;
    }

    /**
     * @param subscription
     *            the subscription to set
     */
    public void setSubscription(SubscriptionRecord subscription) {
        this.subscription = subscription;
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ReplacementRecord other = (ReplacementRecord) obj;
        if (this.key == null) {
            if (other.key != null) {
                return false;
            }
        } else if (!this.key.equals(other.key)) {
            return false;
        }
        if (this.value == null) {
            if (other.value != null) {
                return false;
            }
        } else if (!this.key.equals(other.key)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName()).append("[");
        sb.append("id=").append(this.id).append(",");
        sb.append("subscription=");
        try {
            sb.append(this.subscription.getId()).append(",");
        } catch (Exception e) {
            sb.append("N/A").append(",");
        }
        sb.append("key='").append(this.key).append("',");
        sb.append("value='").append(this.value).append("'");
        return sb.toString();
    }
}
