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
 * Record implementation for geomag k 3 hr. 
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
@Table(name = "geomag_k3hr")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagK3hr extends PersistableDataObject<Object> {


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
     * insert time 
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Date lastUpdate;
	
    /**
     * k_index
     */
	@Column(length=16)
    @DynamicSerializeElement
	private int kIndex;
   
	/**
     * k_real
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kReal;
	
	/**
     * Gamma
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kGamma;
	
	/**
     * est k_index
     */
	@Column(length=16)
    @DynamicSerializeElement
	private int kestIndex;
   
	/**
     * est k_real
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kestReal;
	
	/**
     * est gamma
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kestGamma;
	
	/**
     * A Final Running
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int aFinalRunning;
	
	/**
     * A Running
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int aRunning;
	
	/**
     * forecaster manual editing
     */
	@Column(length=16)
    @DynamicSerializeElement
    private int isManual;
	
	
	
	public GeoMagK3hr() {
		
	}
	
	public void generateId() {
        this.id = hashCode();
    }
	
	/**
     * @return the hHrAvg
     */
    public int getKIndex() {
        return kIndex;
    }

    public void setKIndex(int kIndex) {
        this.kIndex = kIndex;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getKReal() {
        return kReal;
    }

    public void setKReal(float kReal) {
        this.kReal = kReal;
    }
    
    /**
     * @return the hHrAvg
     */
    public float getKGamma() {
        return kGamma;
    }

    public void setKGamma(float kGamma) {
        this.kGamma = kGamma;
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
     * @return the dHrAvg
     */
    public int getARunning() {
        return aRunning;
    }

    public void setARunning(int aRunning) {
        this.aRunning = aRunning;
    }
    
    /**
     * @return the dHrAvg
     */
    public int getAFinalRunning() {
        return aFinalRunning;
    }

    public void setAFinalRunning(int aFinalRunning) {
        this.aFinalRunning = aFinalRunning;
    }
    
    /**
     * @return the dHrAvg
     */
    public int getIsManual() {
        return isManual;
    }

    public void setIsManual(int isManual) {
        this.isManual = isManual;
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
