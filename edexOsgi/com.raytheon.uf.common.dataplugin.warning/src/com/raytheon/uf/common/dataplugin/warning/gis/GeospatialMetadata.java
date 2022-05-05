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

import java.util.List;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Geospatial data for a warngen configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2011            rjpeter     Initial creation
 * Dec  9, 2015 ASM #18209 D. Friedman Support cwaStretch.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeospatialMetadata {

    @XmlAttribute
    @DynamicSerializeElement
    private String areaSource;

    @XmlAttribute
    @DynamicSerializeElement
    private List<String> areaFields;

    @Transient
    private String fipsField;

    @XmlAttribute
    @DynamicSerializeElement
    private String parentAreaSource;

    @XmlAttribute
    @DynamicSerializeElement
    private String areaNotationField;

    @XmlAttribute
    @DynamicSerializeElement
    private String parentAreaField;

    @XmlAttribute
    @DynamicSerializeElement
    private String timeZoneSource;

    @XmlAttribute
    @DynamicSerializeElement
    private String timeZoneField;

    @XmlAttribute
    @DynamicSerializeElement
    private Double cwaStretch;

    public String getAreaSource() {
        return areaSource;
    }

    public void setAreaSource(String areaSource) {
        this.areaSource = areaSource;
    }

    public List<String> getAreaFields() {
        return areaFields;
    }

    public void setAreaFields(List<String> areaFields) {
        this.areaFields = areaFields;
    }

    public String getParentAreaSource() {
        return parentAreaSource;
    }

    public void setParentAreaSource(String parentAreaSource) {
        this.parentAreaSource = parentAreaSource;
    }

    public String getAreaNotationField() {
        return areaNotationField;
    }

    public void setAreaNotationField(String areaNotationField) {
        this.areaNotationField = areaNotationField;
    }

    public String getParentAreaField() {
        return parentAreaField;
    }

    public void setParentAreaField(String parentAreaField) {
        this.parentAreaField = parentAreaField;
    }

    public String getTimeZoneSource() {
        return timeZoneSource;
    }

    public void setTimeZoneSource(String timeZoneSource) {
        this.timeZoneSource = timeZoneSource;
    }

    public String getTimeZoneField() {
        return timeZoneField;
    }

    public void setTimeZoneField(String timeZoneField) {
        this.timeZoneField = timeZoneField;
    }

    public String getFipsField() {
        return fipsField;
    }

    public void setFipsField(String fipsField) {
        this.fipsField = fipsField;
    }

    public Double getCwaStretch() {
        return cwaStretch;
    }

    public void setCwaStretch(Double cwaStretch) {
        this.cwaStretch = cwaStretch;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((areaFields == null) ? 0 : areaFields.hashCode());
        result = prime
                * result
                + ((areaNotationField == null) ? 0 : areaNotationField
                        .hashCode());
        result = prime * result
                + ((areaSource == null) ? 0 : areaSource.hashCode());
        result = prime * result
                + ((parentAreaField == null) ? 0 : parentAreaField.hashCode());
        result = prime
                * result
                + ((parentAreaSource == null) ? 0 : parentAreaSource.hashCode());
        result = prime * result
                + ((timeZoneField == null) ? 0 : timeZoneField.hashCode());
        result = prime * result
                + ((timeZoneSource == null) ? 0 : timeZoneSource.hashCode());
        result = prime * result
                + ((cwaStretch == null) ? 0 : cwaStretch.hashCode());
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
        GeospatialMetadata other = (GeospatialMetadata) obj;
        if (areaFields == null) {
            if (other.areaFields != null)
                return false;
        } else if (!areaFields.equals(other.areaFields))
            return false;
        if (areaNotationField == null) {
            if (other.areaNotationField != null)
                return false;
        } else if (!areaNotationField.equals(other.areaNotationField))
            return false;
        if (areaSource == null) {
            if (other.areaSource != null)
                return false;
        } else if (!areaSource.equals(other.areaSource))
            return false;
        if (parentAreaField == null) {
            if (other.parentAreaField != null)
                return false;
        } else if (!parentAreaField.equals(other.parentAreaField))
            return false;
        if (parentAreaSource == null) {
            if (other.parentAreaSource != null)
                return false;
        } else if (!parentAreaSource.equals(other.parentAreaSource))
            return false;
        if (timeZoneField == null) {
            if (other.timeZoneField != null)
                return false;
        } else if (!timeZoneField.equals(other.timeZoneField))
            return false;
        if (timeZoneSource == null) {
            if (other.timeZoneSource != null)
                return false;
        } else if (!timeZoneSource.equals(other.timeZoneSource))
            return false;
        if (cwaStretch == null) {
            if (other.cwaStretch != null)
                return false;
        } else if (!cwaStretch.equals(other.cwaStretch))
            return false;
        return true;
    }
}
