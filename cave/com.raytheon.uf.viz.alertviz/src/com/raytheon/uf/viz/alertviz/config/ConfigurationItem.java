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

package com.raytheon.uf.viz.alertviz.config;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Provides an abstract implementation for saving a priority to metadata mapping
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2008 1433       chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ConfigurationItem implements ISerializableObject {

    /**
     * Mapping of priorities to metadata
     */
    private Map<Priority, AlertMetadata> preferenceMapping = new HashMap<Priority, AlertMetadata>();

    public Map<Priority, AlertMetadata> getPreferenceMapping() {
        return preferenceMapping;
    }

    public void setPreferenceMapping(
            Map<Priority, AlertMetadata> preferenceMapping) {
        this.preferenceMapping = preferenceMapping;
    }

    @XmlElement(name = "metadata")
    public void setAlertMetadataCollection(AlertMetadata[] metadata) {
        preferenceMapping.clear();
        for (AlertMetadata am : metadata) {
            preferenceMapping.put(am.getPriority(), am);
        }
    }

    public AlertMetadata[] getAlertMetadataCollection() {
        return preferenceMapping.values().toArray(
                new AlertMetadata[preferenceMapping.size()]);
    }

    /**
     * Look up the alert metadata for a specific priority
     * 
     * Returns null if metadata not defined
     * 
     * @param priority
     * @return
     */
    public AlertMetadata lookup(Priority priority) {
        return this.preferenceMapping.get(priority);
    }

    public ConfigurationItem clone() {
        ConfigurationItem item = new ConfigurationItem();
        item.preferenceMapping = new HashMap<Priority, AlertMetadata>();
        for (AlertMetadata am : preferenceMapping.values()) {
            item.preferenceMapping.put(am.getPriority(), am.clone());
        }
        return item;
    }

}
