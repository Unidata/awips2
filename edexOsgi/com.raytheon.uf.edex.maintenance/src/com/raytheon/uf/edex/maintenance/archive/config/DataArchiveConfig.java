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
package com.raytheon.uf.edex.maintenance.archive.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Data archive configuration. Configuration should be pulled from common_static
 * localization. Configuration with a pluginName of default will all to all
 * plugins.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2012            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class DataArchiveConfig {
    @XmlElement
    private String pluginName;

    @XmlElement
    private Integer hoursToKeep;

    @XmlElement
    private Boolean archivingEnabled;

    @XmlElement
    private Boolean compressionEnabled;

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
     * @return the hoursToKeep
     */
    public Integer getHoursToKeep() {
        return hoursToKeep;
    }

    /**
     * @param hoursToKeep
     *            the hoursToKeep to set
     */
    public void setHoursToKeep(Integer hoursToKeep) {
        this.hoursToKeep = hoursToKeep;
    }

    /**
     * @return the archivingEnabled
     */
    public Boolean getArchivingEnabled() {
        return archivingEnabled;
    }

    /**
     * @param archivingEnabled
     *            the archivingEnabled to set
     */
    public void setArchivingEnabled(Boolean archivingEnabled) {
        this.archivingEnabled = archivingEnabled;
    }

    /**
     * @param compressionEnabled
     *            the compressionEnabled to set
     */
    public void setCompressionEnabled(Boolean compressionEnabled) {
        this.compressionEnabled = compressionEnabled;
    }

    /**
     * @return the compressionEnabled
     */
    public Boolean getCompressionEnabled() {
        return compressionEnabled;
    }

    public boolean isArchivingEnabledSet() {
        return archivingEnabled != null;
    }

    public boolean isHoursToKeepSet() {
        return hoursToKeep != null;
    }

    public boolean isCompressionEnabledSet() {
        return (compressionEnabled != null);
    }
}
