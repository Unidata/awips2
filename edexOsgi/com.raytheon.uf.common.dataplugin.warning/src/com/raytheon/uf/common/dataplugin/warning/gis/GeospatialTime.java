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
package com.raytheon.uf.common.dataplugin.warning.gis;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class GeospatialTime {
    @XmlElement
    private GeospatialMetadata metaData;

    @XmlAttribute
    private String fileName;

    @XmlAttribute
    private long areaSourceTime;

    @XmlAttribute
    private long timeZoneSourceTime;

    @XmlAttribute
    private long parentSourceTime;

    public GeospatialMetadata getMetaData() {
        return metaData;
    }

    public void setMetaData(GeospatialMetadata metaData) {
        this.metaData = metaData;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public long getAreaSourceTime() {
        return areaSourceTime;
    }

    public void setAreaSourceTime(long areaSourceTime) {
        this.areaSourceTime = areaSourceTime;
    }

    public long getTimeZoneSourceTime() {
        return timeZoneSourceTime;
    }

    public void setTimeZoneSourceTime(long timeZoneSourceTime) {
        this.timeZoneSourceTime = timeZoneSourceTime;
    }

    public long getParentSourceTime() {
        return parentSourceTime;
    }

    public void setParentSourceTime(long parentSourceTime) {
        this.parentSourceTime = parentSourceTime;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + (int) (areaSourceTime ^ (areaSourceTime >>> 32));
        result = prime * result
                + ((metaData == null) ? 0 : metaData.hashCode());
        result = prime * result
                + (int) (parentSourceTime ^ (parentSourceTime >>> 32));
        result = prime * result
                + (int) (timeZoneSourceTime ^ (timeZoneSourceTime >>> 32));
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
        GeospatialTime other = (GeospatialTime) obj;
        if (areaSourceTime != other.areaSourceTime)
            return false;
        if (metaData == null) {
            if (other.metaData != null)
                return false;
        } else if (!metaData.equals(other.metaData))
            return false;
        if (parentSourceTime != other.parentSourceTime)
            return false;
        if (timeZoneSourceTime != other.timeZoneSourceTime)
            return false;
        return true;
    }
}
