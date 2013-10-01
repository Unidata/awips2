package gov.noaa.nws.ncep.common.dataplugin.geomag;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for geomag k 1 min. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "geomagseq")
@Table(name = "geomag_k1min")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagK1min extends PersistableDataObject<Object> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String ID_GEN = "idgen";
	
	/** The id */
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = ID_GEN)
    private Integer id;
	
	/**
     * station code
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String stationCode;
	
    /**
     * time tag
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Date refTime;
	
    /**
     * insert time tag
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Date lastUpdate;
	
    /**
     * H data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
	private int kestIndex;
   
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kestReal;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kestGamma;
	
    /**
     * H data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
	private int hkIndex;
   
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float hkReal;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float hkGamma;
	
    /**
     * H data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
	private int dkIndex;
   
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float dkReal;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float dkGamma;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int hCount;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int dCount;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int aest;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float ks;
	
	
	
	public GeoMagK1min() {
		
	}
	
	public void generateId() {
        this.id = hashCode();
    }
	
	/**
     * @return the hHrAvg
     */
    public int getKestIndex() {
        return kestIndex;
    }

    public void setKestIndex(int kestIndex) {
        this.kestIndex = kestIndex;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getKestReal() {
        return kestReal;
    }

    public void setKestReal(float kestReal) {
        this.kestReal = kestReal;
    }
    
    /**
     * @return the hHrAvg
     */
    public float getKestGamma() {
        return kestGamma;
    }

    public void setKestGamma(float kestGamma) {
        this.kestGamma = kestGamma;
    }
    
	/**
     * @return the hHrAvg
     */
    public int getHkIndex() {
        return hkIndex;
    }

    public void setHKIndex(int hkIndex) {
        this.hkIndex = hkIndex;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getHKReal() {
        return hkReal;
    }

    public void setHKReal(float hkReal) {
        this.hkReal = hkReal;
    }
    
    /**
     * @return the hHrAvg
     */
    public float getHKGamma() {
        return hkGamma;
    }

    public void setHKGamma(float hkGamma) {
        this.hkGamma = hkGamma;
    }
    
	/**
     * @return the hHrAvg
     */
    public int getDKIndex() {
        return dkIndex;
    }

    public void setDKIndex(int dkIndex) {
        this.dkIndex = dkIndex;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getDKReal() {
        return dkReal;
    }

    public void setDKReal(float dkReal) {
        this.dkReal = dkReal;
    }
    
    /**
     * @return the hHrAvg
     */
    public float getDKGamma() {
        return dkGamma;
    }

    public void setDKGamma(float dkGamma) {
        this.dkGamma = dkGamma;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getHCount() {
        return hCount;
    }

    public void setHCount(int hCount) {
        this.hCount = hCount;
    }
    
    /**
     * @return the dHrAvg
     */
    public int getDCount() {
        return dCount;
    }

    public void setDCount(int dCount) {
        this.dCount = dCount;
    }
    
    /**
     * @return the dHrAvg
     */
    public int getAest() {
        return aest;
    }

    public void setAest(int aest) {
        this.aest = aest;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getKs() {
        return ks;
    }

    public void setKs(float ks) {
        this.ks = ks;
    }
    
    /**
     * @return The id
     */
    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

	/**
     * @return the timeTag
     */
    public Date getRefTime() {
        return refTime;
    }

    public void setRefTime(Date refTime) {
        this.refTime = refTime;
    }
    
    /**
     * @return the timeTag
     */
    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
    
    /**
     * @return the stationCode
     */
    public String getStationCode() {
        return stationCode;
    }

    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }
}
