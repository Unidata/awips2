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
package com.raytheon.uf.common.dataplugin.warning.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang3.Validate;

/**
 * 
 * GeospatialConfiguration
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 21, 2007             chammack    Initial Creation.
 *    Aug 26, 2008 #1502       bclement    Added JAXB annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GeospatialConfiguration {

    @XmlElement
    private String pointSource;

    @XmlElement
    private String areaSource;

    @XmlElement
    private String parentAreaSource;

    @XmlElement
    private String timezoneField;

    @XmlElement
    private String timezoneSource;

    public String getPointSource() {
        return pointSource;
    }

    public void setPointSource(String pointSource) {
        this.pointSource = pointSource;
    }

    public String getAreaSource() {
        return areaSource;
    }

    public void setAreaSource(String areaSource) {
        this.areaSource = areaSource;
    }

    public String getParentAreaSource() {
        return parentAreaSource;
    }

    public void setParentAreaSource(String parentAreaSource) {
        this.parentAreaSource = parentAreaSource;
    }

    public String getTimezoneField() {
        return timezoneField;
    }

    public void setTimezoneField(String timezoneField) {
        this.timezoneField = timezoneField;
    }

    public String getTimezoneSource() {
        return timezoneSource;
    }

    public void setTimezoneSource(String timezoneSource) {
        this.timezoneSource = timezoneSource;
    }

    public void validate() {
        Validate.notNull(
                areaSource,
                "Area source must be provided for pointcast to operate. Check .xml if areaSource is set in geosptatialConfig.\n");
    }

}
