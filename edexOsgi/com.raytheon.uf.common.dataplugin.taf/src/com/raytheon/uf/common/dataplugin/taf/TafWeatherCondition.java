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

package com.raytheon.uf.common.dataplugin.taf;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a weather condition item contained in a taf message
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 6/21/2007    180        Phillippe   Initial creation    
 * 20081106     1515       jkorman     Changed length of &quot;other&quot; attribute.
 * Nov 01, 2013 2361       njensen     Remove XML annotations
 * May 15, 2014 3002       bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "taf_weather_conditions")
@DynamicSerialize
public class TafWeatherCondition extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    /** The taf record this weather condition object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ChangeGroup parentID;

    /** The intensity proximity notation * */
    @DynamicSerializeElement
    @Column(length = 2)
    private String intensityProximity = "";

    /** The descriptor notation * */
    @DynamicSerializeElement
    @Column(length = 2)
    private String descriptor = "";

    /** The precipitation notation * */
    @DynamicSerializeElement
    @Column(length = 2)
    private String precipitation = "";

    /** The obscuration notation * */
    @DynamicSerializeElement
    @Column(length = 2)
    private String obscuration = "";

    /** The other notation * */
    @DynamicSerializeElement
    @Column(length = 3)
    private String other = "";

    @Transient
    private Integer sequenceId;

    /**
     * No-Arg Constructor.
     */
    public TafWeatherCondition() {
        this.intensityProximity = "";
        this.descriptor = "";
        this.precipitation = "";
        this.obscuration = "";
        this.other = "";
    }

    /**
     * Constructor.
     * 
     * @param intensityProximity
     * @param descriptor
     * @param precipitation
     * @param obscuration
     * @param other
     */
    public TafWeatherCondition(ChangeGroup parentID, String intensityProximity,
            String descriptor, String precipitation, String obscuration,
            String other, int sequence) {

        this.parentID = parentID;
        sequenceId = sequence;
        if (intensityProximity == null) {
            intensityProximity = "";
        }
        if (descriptor == null) {
            descriptor = "";
        }
        if (precipitation == null) {
            precipitation = "";
        }
        if (obscuration == null) {
            obscuration = "";
        }
        if (other == null) {
            other = "";
        }
        setIntensityProximity(intensityProximity);
        setDescriptor(descriptor);
        setPrecipitation(precipitation);
        setObscuration(obscuration);
        setOther(other);
    }

    /**
     * @return the descriptor
     */
    public String getDescriptor() {
        return descriptor;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(String descriptor) {
        if (descriptor != null) {
            this.descriptor = descriptor;
        }
    }

    /**
     * @return the intensityProximity
     */
    public String getIntensityProximity() {
        return intensityProximity;
    }

    /**
     * @param intensityProximity
     *            the intensityProximity to set
     */
    public void setIntensityProximity(String intensityProximity) {
        if (intensityProximity != null) {
            this.intensityProximity = intensityProximity;
        }
    }

    /**
     * @return the obscuration
     */
    public String getObscuration() {
        return obscuration;
    }

    /**
     * @param obscuration
     *            the obscuration to set
     */
    public void setObscuration(String obscuration) {
        if (obscuration != null) {
            this.obscuration = obscuration;
        }
    }

    /**
     * @return the other
     */
    public String getOther() {
        return other;
    }

    /**
     * @param other
     *            the other to set
     */
    public void setOther(String other) {
        if (other != null) {
            this.other = other;
        }
    }

    /**
     * @return the parentID
     */
    public ChangeGroup getParentID() {
        return parentID;
    }

    /**
     * @param parentID
     *            the parentID to set
     */
    public void setParentID(ChangeGroup parentID) {
        this.parentID = parentID;

    }

    /**
     * @return the precipitation
     */
    public String getPrecipitation() {
        return precipitation;
    }

    /**
     * @param precipitation
     *            the precipitation to set
     */
    public void setPrecipitation(String precipitation) {
        if (precipitation != null) {
            this.precipitation = precipitation;
        }
    }

    /**
     * @return the sequenceId
     */
    public Integer getSequenceId() {
        return sequenceId;
    }

    public StringBuilder toString(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        // buffer.append(parentID);
        // buffer.append(":");
        buffer.append((sequenceId != null) ? sequenceId : "--");
        buffer.append(":");
        buffer.append((intensityProximity != null) ? intensityProximity : "_");

        buffer.append((intensityProximity != null) ? intensityProximity : "_");
        buffer.append(":");
        buffer.append((descriptor != null) ? descriptor : "__");
        buffer.append(":");
        buffer.append((precipitation != null) ? precipitation : "__");
        buffer.append(":");
        buffer.append((obscuration != null) ? obscuration : "__");
        buffer.append(":");
        buffer.append((other != null) ? other : "__");
        return buffer;
    }

    @Override
    public String toString() {
        return toString(null).toString();
    }

    /**
     * @param sequenceId
     *            the sequenceId to set
     */
    public void setSequenceId(Integer sequenceId) {
        this.sequenceId = sequenceId;
    }

    @Override
    public boolean equals(Object obj) {

        if (obj instanceof TafWeatherCondition) {
            TafWeatherCondition cond = (TafWeatherCondition) obj;

            if (!(this.precipitation == null ? cond.getPrecipitation() == null
                    : this.precipitation.equals(cond.getPrecipitation()))) {
                return false;
            }

            if (!(this.obscuration == null ? cond.getObscuration() == null
                    : this.obscuration.equals(cond.getObscuration()))) {
                return false;
            }

            if (parentID != cond.getParentID()) {
                return false;
            }

            if (!(this.intensityProximity == null ? cond
                    .getIntensityProximity() == null : this.intensityProximity
                    .equals(cond.getIntensityProximity()))) {
                return false;
            }

            if (!(this.descriptor == null ? cond.getDescriptor() == null
                    : this.descriptor.equals(cond.getDescriptor()))) {
                return false;
            }

            if (!(this.other == null ? cond.other == null : this.other
                    .equals(cond.getOther()))) {
                return false;
            }

            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(precipitation).append(
                obscuration).append(parentID).append(intensityProximity)
                .append(descriptor).append(other).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

}
