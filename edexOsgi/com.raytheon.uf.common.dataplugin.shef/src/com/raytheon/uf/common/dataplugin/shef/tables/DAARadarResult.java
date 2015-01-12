package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table DAARadarResult
 * 
 * </pre>
 * 
 * @author OHD
 * @version 1.1
 */

@Entity
@Table(name = "daaradarresult")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DAARadarResult extends PersistableDataObject implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @XmlElement
    @DynamicSerializeElement
    private DAARadarResultId id;

    @XmlElement
    @DynamicSerializeElement
    private Short numGages;

    @XmlElement
    @DynamicSerializeElement
    private String radAvail;

    @XmlElement
    @DynamicSerializeElement
    private Double rwBiasValUsed;

    @XmlElement
    @DynamicSerializeElement
    private Double memSpanUsed;

    @XmlElement
    @DynamicSerializeElement
    private String editBias;

    @XmlElement
    @DynamicSerializeElement
    private String ignoreRadar;

    public DAARadarResult() {
    }

    public DAARadarResult(DAARadarResultId id) {
        this.id = id;
    }

    public DAARadarResult(DAARadarResultId id, Short numGages, String radAvail,
            Double rwBiasValUsed, Double memSpanUsed, String editBias,
            String ignoreRadar) {
        this.id = id;
        this.numGages = numGages;
        this.radAvail = radAvail;
        this.rwBiasValUsed = rwBiasValUsed;
        this.memSpanUsed = memSpanUsed;
        this.editBias = editBias;
        this.ignoreRadar = ignoreRadar;
    }

    @EmbeddedId
    @AttributeOverrides( {
            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3)),
            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29)) })
    public DAARadarResultId getId() {
        return this.id;
    }

    public void setId(DAARadarResultId id) {
        this.id = id;
    }

    @Column(name = "num_gages")
    public Short getNumGages() {
        return this.numGages;
    }

    public void setNumGages(Short numGages) {
        this.numGages = numGages;
    }

    @Column(name = "rad_avail", length = 1)
    public String getRadAvail() {
        return this.radAvail;
    }

    public void setRadAvail(String radAvail) {
        this.radAvail = radAvail;
    }

    @Column(name = "rw_bias_val_used", precision = 17, scale = 17)
    public Double getRwBiasValUsed() {
        return this.rwBiasValUsed;
    }

    public void setRwBiasValUsed(Double rwBiasValUsed) {
        this.rwBiasValUsed = rwBiasValUsed;
    }

    @Column(name = "mem_span_used", precision = 17, scale = 17)
    public Double getMemSpanUsed() {
        return this.memSpanUsed;
    }

    public void setMemSpanUsed(Double memSpanUsed) {
        this.memSpanUsed = memSpanUsed;
    }

    @Column(name = "edit_bias", length = 1)
    public String getEditBias() {
        return this.editBias;
    }

    public void setEditBias(String editBias) {
        this.editBias = editBias;
    }

    @Column(name = "ignore_radar", length = 1)
    public String getIgnoreRadar() {
        return this.ignoreRadar;
    }

    public void setIgnoreRadar(String ignoreRadar) {
        this.ignoreRadar = ignoreRadar;
    }

}
