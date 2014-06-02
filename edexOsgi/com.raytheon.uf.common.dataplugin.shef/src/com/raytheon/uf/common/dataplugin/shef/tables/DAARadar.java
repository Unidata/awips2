package com.raytheon.uf.common.dataplugin.shef.tables;

import javax.persistence.AttributeOverride;

import java.io.Serializable;
import java.util.Date;
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
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table DAARadar
 * 
 * </pre>
 * 
 * @author OHD
 * @version 1.1
 */

@Entity
@Table(name = "daaradar")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DAARadar extends PersistableDataObject implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @XmlElement
    @DynamicSerializeElement
    private DAARadarId id;
    
    @XmlElement
    @DynamicSerializeElement
    private Short minOff;

    @XmlElement
    @DynamicSerializeElement
    private Float maxValh;
    
    @XmlElement
    @DynamicSerializeElement
    private Float maxVald;

    @XmlElement
    @DynamicSerializeElement
    private Float s1BiasValue;

    @XmlElement
    @DynamicSerializeElement
    private Date productTime;

    @XmlElement
    @DynamicSerializeElement
    private Short nullProductFlag;
    
    @XmlElement
    @DynamicSerializeElement
    private Integer coverageDur;

    @XmlElement
    @DynamicSerializeElement
    private String gridFilename;

    public DAARadar() {
    }

    public DAARadar(DAARadarId id) {
        this.id = id;
    }

    public DAARadar(DAARadarId id, 
    		Short minOff, Float maxValh, 
    		Float maxVald, Float s1BiasValue,
    		Date productTime, Short nullProductFlag,
    		Integer coverageDur, String gridFilename)
    {

    	this.id = id;
    	this.minOff = minOff;
    	this.maxValh = maxValh;
    	this.maxVald = maxVald;
    	this.s1BiasValue = s1BiasValue;
    	this.productTime = productTime;
    	this.nullProductFlag = nullProductFlag;
    	this.coverageDur = coverageDur;
    	this.gridFilename = gridFilename;
    }
    
    /*
    public DAARadar(String id, String obsTime, 
    				Short minOff, Float maxValh, 
    				Float maxVald, Float s1BiasValue,
    				String productTime, Short nullProductFlag,
    				Integer coverageDur, String gridFilename)
    {
        //this.id = id;
       // this.obsTime = obsTime;
        this.minOff = minOff;
        this.maxValh = maxValh;
        this.maxVald = maxVald;
        this.s1BiasValue = s1BiasValue;
        this.productTime = productTime;
        this.nullProductFlag = nullProductFlag;
        this.coverageDur = coverageDur;
        this.gridFilename = gridFilename;
    }
*/
    @EmbeddedId
    @AttributeOverrides( {
            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3)),
            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29)) })
    public DAARadarId getId() {
        return this.id;
    }

    public void setId(DAARadarId id) {
        this.id = id;
    }

    @Column(name = "minoff")
    public Short getminOff() {
        return this.minOff;
    }

    public void setminOff(Short minOff) {
        this.minOff = minOff;
    }

    @Column(name = "maxvalh")
    public Float getmaxValh() {
        return this.maxValh;
    }

    public void setmaxValh(Float maxValh) {
        this.maxValh = maxValh;
    }

    @Column(name = "maxvald")
    public Float getmaxVald() {
        return this.maxVald;
    }

    public void setmaxVald(Float maxVald) {
        this.maxVald = maxVald;
    }

    @Column(name = "s1_bias_value")
    public Float gets1BiasValue() {
        return this.s1BiasValue;
    }

    public void sets1BiasValue(Float s1BiasValue) {
        this.s1BiasValue = s1BiasValue;
    }
    
    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "producttime")
    public Date getproductTime() {
        return this.productTime;
    }

    public void setproductTime(Date productTime) {
        this.productTime = productTime;
    }

    @Column(name = "null_product_flag")
    public Short getnullProductFlag() {
        return this.nullProductFlag;
    }

    public void setnullProductFlag(Short nullProductFlag) {
        this.nullProductFlag = nullProductFlag;
    }
    
    @Column(name = "coverage_dur")
    public Integer getcoverageDur() {
        return this.coverageDur;
    }

    public void setcoverageDur(Integer coverageDur) {
        this.coverageDur = coverageDur;
    }
    
    @Column(name = "grid_filename")
    public String getgridFilename() {
        return this.gridFilename;
    }

    public void setgridFilename(String gridFilename) {
        this.gridFilename = gridFilename;
    }
    
    public String toString()
    {
    	StringBuffer buffer = new StringBuffer();
    	
    	buffer.append(id.getRadid() + " " + id.getObstime() + " " + getminOff() + " " +
    			getmaxValh() + " " + getmaxVald() + " " + gets1BiasValue() +
    			" " + getproductTime() + " " + getnullProductFlag() +
    			" " + getcoverageDur() + " " + getgridFilename() ) ;
    
    	return buffer.toString();
    	
    }


}
