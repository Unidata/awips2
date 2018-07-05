package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * POJO representative of a dprradar record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table DPRRadar
 * 09/22/2016   5631       bkowal      Remove ISerializableObject.
 * 
 * </pre>
 * 
 * @author OHD
 */
@NamedQueries({
        @NamedQuery(name = DPRRadar.SELECT_BY_RAD_ID_BETWEEN_OBS_TIME, query = DPRRadar.SELECT_BY_RAD_ID_BETWEEN_OBS_TIME_HQL) })
@Entity
@Table(name = "dprradar")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DPRRadar extends PersistableDataObject<DPRRadarId>
        implements Serializable, IGriddedRadarRecord {

    private static final long serialVersionUID = 1L;

    public static final String SELECT_BY_RAD_ID_BETWEEN_OBS_TIME = "selectDPRRadarByRadIdBetweenObsTime";

    protected static final String SELECT_BY_RAD_ID_BETWEEN_OBS_TIME_HQL = "FROM DPRRadar r WHERE r.id.radid = :radid AND r.id.obstime >= :startObsTime AND r.id.obstime <= :endObsTime";

    @XmlElement
    @DynamicSerializeElement
    private DPRRadarId id;

    @XmlElement
    @DynamicSerializeElement
    private Short volcovpat;

    @XmlElement
    @DynamicSerializeElement
    private Short opermode;

    @XmlElement
    @DynamicSerializeElement
    private Float maxval;

    @XmlElement
    @DynamicSerializeElement
    private Float scale;

    @XmlElement
    @DynamicSerializeElement
    private Float setoff;

    @XmlElement
    @DynamicSerializeElement
    private Integer jEndDate;

    @XmlElement
    @DynamicSerializeElement
    private Integer jEndTime;

    @XmlElement
    @DynamicSerializeElement
    private Short meanFieldBias;

    @XmlElement
    @DynamicSerializeElement
    private Short precipDetectedFlag;

    @XmlElement
    @DynamicSerializeElement
    private String gridFilename;

    public DPRRadar() {
    }

    public DPRRadar(DPRRadarId id) {
        this.id = id;
    }

    public DPRRadar(DPRRadarId id, Short volcovpat, Short opermode,
            Float maxval, Float scale, Float setoff, Integer jEndDate,
            Integer jEndTime, Short meanFieldBias, Short precipDetectedFlag,
            String gridFilename) {
        this.id = id;
        this.volcovpat = volcovpat;
        this.opermode = opermode;
        this.maxval = maxval;
        this.scale = scale;
        this.setoff = setoff;
        this.jEndDate = jEndDate;
        this.jEndTime = jEndTime;
        this.meanFieldBias = meanFieldBias;
        this.precipDetectedFlag = precipDetectedFlag;
        this.gridFilename = gridFilename;
    }

    @Transient
    @Override
    public Date getObsTime() {
        return id.getObstime();
    }

    @EmbeddedId
    @AttributeOverrides({
            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3) ),
            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29) ) })
    public DPRRadarId getId() {
        return this.id;
    }

    public void setId(DPRRadarId id) {
        this.id = id;
    }

    @Column(name = "volcovpat")
    public Short getVolcovpat() {
        return this.volcovpat;
    }

    public void setVolcovpat(Short volcovpat) {
        this.volcovpat = volcovpat;
    }

    @Column(name = "opermode")
    public Short getOpermode() {
        return this.opermode;
    }

    public void setOpermode(Short opermode) {
        this.opermode = opermode;
    }

    @Column(name = "maxval", precision = 8, scale = 8)
    public Float getMaxval() {
        return this.maxval;
    }

    public void setMaxval(Float maxval) {
        this.maxval = maxval;
    }

    @Column(name = "scale", precision = 8, scale = 8)
    public Float getScale() {
        return this.scale;
    }

    public void setScale(Float scale) {
        this.scale = scale;
    }

    @Column(name = "setoff", precision = 8, scale = 8)
    public Float getSetOff() {
        return this.setoff;
    }

    public void setSetOff(Float setoff) {
        this.setoff = setoff;
    }

    @Column(name = "j_end_date")
    public Integer getJEndDate() {
        return this.jEndDate;
    }

    public void setJEndDate(Integer jEndDate) {
        this.jEndDate = jEndDate;
    }

    @Column(name = "j_end_time")
    public Integer getJEndTime() {
        return this.jEndTime;
    }

    public void setJEndTime(Integer jEndTime) {
        this.jEndTime = jEndTime;
    }

    @Override
    @Column(name = "mean_field_bias")
    public Short getMeanFieldBias() {
        return this.meanFieldBias;
    }

    public void setMeanFieldBias(Short meanFieldBias) {
        this.meanFieldBias = meanFieldBias;
    }

    @Column(name = "precipdetectedflag")
    public Short getPrecipDetectedFlag() {
        return this.precipDetectedFlag;
    }

    public void setPrecipDetectedFlag(Short precipDetectedFlag) {
        this.precipDetectedFlag = precipDetectedFlag;
    }

    @Override
    @Column(name = "grid_filename", length = 20)
    public String getGridFilename() {
        return this.gridFilename;
    }

    public void setGridFilename(String gridFilename) {
        this.gridFilename = gridFilename;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("DPRRadar [");
        sb.append("id=").append(id.toString());
        sb.append(", volcovpat=").append(volcovpat);
        sb.append(", opermode=").append(opermode);
        sb.append(", maxval=").append(maxval);
        sb.append(", scale=").append(scale);
        sb.append(", setoff=").append(setoff);
        sb.append(", jEndDate=").append(jEndDate);
        sb.append(", jEndTime=").append(jEndTime);
        sb.append(", meanFieldBias=").append(meanFieldBias);
        sb.append(", precipDetectedFlag=").append(precipDetectedFlag);
        sb.append(", gridFilename=").append(gridFilename);
        sb.append("]");
        return sb.toString();
    }
}