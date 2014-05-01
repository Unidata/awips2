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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Provides source configuration
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2008 1433       chammack    Initial creation
 * Jan 21, 2011 1978       cjeanbap    Add monitor and MonitorItem.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class Source implements ISerializableObject {

    /**
     * User specified configuration
     */
    @XmlElement
    private ConfigurationItem configurationItem;

    /**
     * The description of the source
     */
    @XmlAttribute
    private String longName;

    /**
     * The name of the source
     */
    @XmlAttribute
    private String name;

    /**
     * Is the source locked (e.g. not deletable)?
     */
    @XmlAttribute
    private boolean locked;

    /**
     * User specified configuration
     */
    @XmlElement
    private ConfigurationMonitor configurationMonitor;

    public Source() {

    }

    public Source(String longName, String name) {
        this.longName = longName;
        this.name = name;
        locked = false;
    }

    public ConfigurationItem getConfigurationItem() {
        return configurationItem;
    }

    public void setConfigurationItem(ConfigurationItem configurationItem) {
        this.configurationItem = configurationItem;
    }

    public String getLongName() {
        return longName;
    }

    public void setLongName(String longName) {
        this.longName = longName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isLocked() {
        return locked;
    }

    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    public Source clone() {
        Source newSource = new Source();
        newSource.configurationItem = configurationItem.clone();
        newSource.locked = locked;
        newSource.longName = longName;
        newSource.name = name;
        if (configurationMonitor == null) {
            configurationMonitor = new ConfigurationMonitor();
        }
        newSource.configurationMonitor = configurationMonitor.clone();
        return newSource;
    }

    public boolean isMonitor() {
        return configurationMonitor == null ? false : configurationMonitor
                .isMonitor();
    }

    public ConfigurationMonitor getConfigurationMonitor() {
        return configurationMonitor;
    }

    public void setConfigurationMonitor(
            ConfigurationMonitor configurationMonitor) {
        this.configurationMonitor = configurationMonitor;
    }
}
