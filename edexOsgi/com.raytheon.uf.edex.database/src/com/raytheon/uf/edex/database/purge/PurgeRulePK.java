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

package com.raytheon.uf.edex.database.purge;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This class defines the primary key for data purge rules stored in the
 * database. This key consists of a sequence number, the pluginName to which
 * this rule applies and the key to which this rule applies.
 * <p>
 * The sequence number is a number obtained from the hibernate_sequence on the
 * database. This number has no significance other than to distinguish two rules
 * which may have the the same pluginName and same key values. The pluginName is
 * the plugin to which this purge rule applies. The key servers as a filter for
 * purging. The key value corresponds to the productKey field in the plugin data
 * object.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/15/11      #2469       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PurgeRulePK implements ISerializableObject, Serializable,
        Cloneable {

    /** The serial number */
    private static final long serialVersionUID = 3400703274820497880L;

    /**
     * A sequence number defined by the database for uniquely identifying this
     * rule
     */
    @XmlElement
    @DynamicSerializeElement
    private Long id;

    /** The plugin associated with this rule */
    @XmlElement
    @DynamicSerializeElement
    private String pluginName;

    /** The key used by this rule for purging */
    @XmlElement
    @DynamicSerializeElement
    private String key;

    /**
     * Creates a new PurgeRulePK object
     */
    public PurgeRulePK() {
    }

    /**
     * Creates a new plugin rule with the given plugin name and an empty key.
     * 
     * @param pluginName
     *            The plugin to which this purge rule applies
     */
    public PurgeRulePK(String pluginName) {
        this.pluginName = pluginName;
        this.key = "";
    }

    /**
     * Creates a new plugin rule with the given plugin name and key
     * 
     * @param pluginName
     *            The plugin to which this purge rule applies
     * @param key
     *            The key to use when applying this rule
     */
    public PurgeRulePK(String pluginName, String key) {
        this(pluginName);
        this.key = key;
    }

    /**
     * Creates a new plugin rule with the given plugin name key, and id value
     * 
     * @param pluginName
     *            The plugin to which this purge rule applies
     * @param key
     *            The key to use when applying this rule
     * @param id
     *            An identifying number
     */
    public PurgeRulePK(String pluginName, String key, long id) {
        this(pluginName, key);
        this.id = id;
    }

    public String toString() {
        return "Plugin:" + pluginName + " Key:" + key;
    }

    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(pluginName);
        builder.append(key);
        return builder.toHashCode();
    }

    public boolean equals(Object rval) {
        if (!(rval instanceof PurgeRulePK)) {
            return false;
        }
        PurgeRulePK rhs = (PurgeRulePK) rval;

        return this.pluginName.equals(rhs.getPluginName())
                && this.key.equals(rhs.getKey());

    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
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

    public Object clone() {
        PurgeRulePK rval = new PurgeRulePK(this.pluginName, this.key);
        rval.setId(this.id);
        return rval;
    }

    /**
     * @return the id
     */
    public Long getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(Long id) {
        this.id = id;
    }

}
