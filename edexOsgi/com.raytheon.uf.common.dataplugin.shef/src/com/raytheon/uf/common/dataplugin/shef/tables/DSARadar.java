package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;

import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
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
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table DSARadar
 * 
 * </pre>
 * 
 * @author OHD
 * @version 1.1
 */

	@Entity
	@Table(name = "dsaradar")
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.NONE)
	@DynamicSerialize
	public class DSARadar extends PersistableDataObject implements Serializable, ISerializableObject 
	{

	    private static final long serialVersionUID = 1L;

	    @XmlElement
	    @DynamicSerializeElement
	    private DSARadarId id;

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
	    private Date beginTime;

	    @XmlElement
	    @DynamicSerializeElement
	    private Date endTime;
	    
	    @XmlElement
	    @DynamicSerializeElement
	    private Short jBeginDate;

	    @XmlElement
	    @DynamicSerializeElement
	    private Short jBeginTime;
	    
	    @XmlElement
	    @DynamicSerializeElement
	    private Short jEndDate;

	    @XmlElement
	    @DynamicSerializeElement
	    private Short jEndTime;

	    @XmlElement
	    @DynamicSerializeElement
	    private Short meanFieldBias;

	    @XmlElement
	    @DynamicSerializeElement
	    private Short nullProductFlag;
	    
	    @XmlElement
	    @DynamicSerializeElement
	    private String gridFilename;

	    public DSARadar() {
	    }

	    public DSARadar(DSARadarId id)
	    {
	        this.id = id;
	    }

	    public DSARadar(DSARadarId id,
	    		Short volcovpat, Short opermode,
	    		Float maxval, Float scale, Float setoff,
	    		Date beginTime, Date endTime,
	    		Short jBeginDate, Short jBeginTime,
	            Short jEndDate, Short jEndTime,
	            Short meanFieldBias, Short nullProductFlag,
	            String gridFilename)
	    
	    {
	        this.id = id;
	        this.volcovpat = volcovpat;
	        this.opermode = opermode;
	        this.maxval = maxval;
	        this.scale = scale;
	        this.setoff = setoff;
	        this.beginTime = beginTime;
	        this.endTime = endTime;
	        this.jBeginDate = jBeginDate;
	        this.jBeginTime = jBeginTime;
	        this.jEndDate = jEndDate;
	        this.jEndTime = jEndTime;
	        this.meanFieldBias = meanFieldBias;
	        this.nullProductFlag = nullProductFlag;
	        this.gridFilename = gridFilename;
	    }

	    @EmbeddedId
	    @AttributeOverrides( {
	            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3)),
	            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29)) })
	    public DSARadarId getId() {
	        return this.id;
	    }

	    public void setId(DSARadarId id) {
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
	    
	    @Temporal(TemporalType.TIMESTAMP)
	    @Column(name = "begin_time")
	    public Date getBeginTime() {
	        return this.beginTime;
	    }

	    public void setBeginTime(Date beginTime) {
	        this.beginTime = beginTime;
	    }
	    
	    @Temporal(TemporalType.TIMESTAMP)
	    @Column(name = "end_time")
	    public Date getEndTime() {
	        return this.endTime;
	    }

	    public void setEndTime(Date endTime) {
	        this.endTime = endTime;
	    }
	    
	    @Column(name = "j_beg_date")
	    public Short getJBeginDate() {
	        return this.jBeginDate;
	    }

	    public void setJBeginDate(Short jBeginDate) {
	        this.jBeginDate = jBeginDate;
	    }
	    
	    @Column(name = "j_beg_time")
	    public Short getJBeginTime() {
	        return this.jBeginTime;
	    }

	    public void setJBeginTime(Short jBeginTime) {
	        this.jBeginTime = jBeginTime;
	    }
	    
	    @Column(name = "j_end_date")
	    public Short getJEndDate() {
	        return this.jEndDate;
	    }

	    public void setJEndDate(Short jEndDate) {
	        this.jEndDate = jEndDate;
	    }

	    @Column(name = "j_end_time")
	    public Short getJEndTime() {
	        return this.jEndTime;
	    }

	    public void setJEndTime(Short jEndTime) {
	        this.jEndTime = jEndTime;
	    }

	    @Column(name = "mean_field_bias")
	    public Short getMeanFieldBias() {
	        return this.meanFieldBias;
	    }

	    public void setMeanFieldBias(Short meanFieldBias) {
	        this.meanFieldBias = meanFieldBias;
	    }
	    
	    @Column(name = "nullproductflag")
	    public Short getNullProductFlag() {
	        return this.nullProductFlag;
	    }

	    public void setNullProductFlag(Short nullProductFlag) {
	        this.nullProductFlag = nullProductFlag;
	    }

	    @Column(name = "grid_filename", length = 20)
	    public String getGridFilename() {
	        return this.gridFilename;
	    }

	    public void setGridFilename(String gridFilename) {
	        this.gridFilename = gridFilename;
	    }

	}
