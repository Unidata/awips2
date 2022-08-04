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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Provides an abstract implementation for saving a priority to metadata mapping
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 Jan 2011  1978       cjeanbap    Initial creation
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ConfigurationMonitor implements ISerializableObject {

    private MonitorMetadata monitorMetadata;

    public ConfigurationMonitor() {
        this.monitorMetadata = new MonitorMetadata();
    }

    public ConfigurationMonitor(String imageFile, boolean isOmitted) {
        this.monitorMetadata = new MonitorMetadata(imageFile, isOmitted);
    }

    public ConfigurationMonitor(MonitorMetadata monitorMetadata) {
        this.monitorMetadata = monitorMetadata;
    }

    @XmlElement(name = "metadata")
    public void setMonitorMetadataCollection(MonitorMetadata[] metadata) {
        this.monitorMetadata = metadata[0];
    }

    public MonitorMetadata[] getMonitorMetadataCollection() {
        return new MonitorMetadata[] { monitorMetadata };
    }

    public ConfigurationMonitor clone() {
        ConfigurationMonitor monitor = new ConfigurationMonitor(
                monitorMetadata.clone());
        return monitor;
    }

    public MonitorMetadata getMonitorMetadata() {
        return monitorMetadata;
    }

    public void setMonitorMetadata(MonitorMetadata monitorMetadata) {
        this.monitorMetadata = monitorMetadata;
    }

    public boolean isMonitor() {
        return monitorMetadata == null ? false : monitorMetadata.hasImage();
    }
}
