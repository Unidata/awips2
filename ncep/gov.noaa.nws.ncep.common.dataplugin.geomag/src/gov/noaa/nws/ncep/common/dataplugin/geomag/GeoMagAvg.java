package gov.noaa.nws.ncep.common.dataplugin.geomag;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
//import javax.persistence.UniqueConstraint;
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
 * Record implementation for geomag avg. 
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
@Table(name = "geomag_houravg")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagAvg extends PersistableDataObject<Object> {


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
    private Date avgTime;
	
    /**
     * insert time tag
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Date insertTime;
	
    /**
     * H data Hour Average
     */
	@Column(length=16)
	@XmlAttribute
    @DynamicSerializeElement
	private float hHrAvg;
   
	/**
     * D data Hour Average
     */
	@Column(length=16)
	@XmlAttribute
    @DynamicSerializeElement
    private float dHrAvg;
	
	
	public GeoMagAvg() {
		
	}
	
	public void generateId() {
        this.id = hashCode();
    }
	
	/**
     * @return the hHrAvg
     */
    public float getHHrAvg() {
        return hHrAvg;
    }

    public void setHHrAvg(float hHrAvg) {
        this.hHrAvg = hHrAvg;
    }
    
    /**
     * @return the dHrAvg
     */
    public float getDHrAvg() {
        return dHrAvg;
    }

    public void setDHrAvg(float dHrAvg) {
        this.dHrAvg = dHrAvg;
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
    public Date getAvgTime() {
        return avgTime;
    }

    public void setAvgTime(Date avgTime) {
        this.avgTime = avgTime;
    }
    
    /**
     * @return the timeTag
     */
    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
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
