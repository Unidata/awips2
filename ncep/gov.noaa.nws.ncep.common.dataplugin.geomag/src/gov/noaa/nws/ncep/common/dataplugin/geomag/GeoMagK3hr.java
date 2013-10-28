package gov.noaa.nws.ncep.common.dataplugin.geomag;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

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
@Table(name = "geomag_k3hr")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagK3hr extends PersistableDataObject<Object> {


	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	
	/** The id */
    @Id
    @DynamicSerializeElement
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
	private int kIndex;
   
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kReal;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float kGamma;
	
	/**
     * D data Hour Average
     */
	@Column(length=16)
    @DynamicSerializeElement
    private float aFinalRunning;
	
	
	
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
     * @return the dHrAvg
     */
    public float getAFinalRunning() {
        return aFinalRunning;
    }

    public void setAFinalRunning(float aFinalRunning) {
        this.aFinalRunning = aFinalRunning;
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

