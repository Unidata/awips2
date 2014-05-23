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
 * 03/03/2014   #1110      qzhou              modified get/set
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
    @Column(length = 16)
    @DynamicSerializeElement
    private int kestIndex;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float kestReal;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float kestGamma;

    /**
     * H data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private int hkIndex;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float hkReal;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float hkGamma;

    /**
     * H data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private int dkIndex;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float dkReal;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float dkGamma;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private int hCount;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private int dCount;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private int aest;

    /**
     * D data Hour Average
     */
    @Column(length = 16)
    @DynamicSerializeElement
    private float ks;

    public GeoMagK1min() {

    }

    public void generateId() {
        this.id = hashCode();
    }

    /**
     * @return the kestIndex
     */
    public int getKestIndex() {
        return kestIndex;
    }

    public void setKestIndex(int kestIndex) {
        this.kestIndex = kestIndex;
    }

    /**
     * @return the kestReal
     */
    public float getKestReal() {
        return kestReal;
    }

    public void setKestReal(float kestReal) {
        this.kestReal = kestReal;
    }

    /**
     * @return the kestGamma
     */
    public float getKestGamma() {
        return kestGamma;
    }

    public void setKestGamma(float kestGamma) {
        this.kestGamma = kestGamma;
    }

    /**
     * @return the hkReal
     */
    public float getHkReal() {
        return hkReal;
    }

    public void setHkReal(float hkReal) {
        this.hkReal = hkReal;
    }

    /**
     * @return the hkGamma
     */
    public float getHkGamma() {
        return hkGamma;
    }

    public void setHkGamma(float hkGamma) {
        this.hkGamma = hkGamma;
    }

    /**
     * @return the hkIndex
     */
    public int getHkIndex() {
        return hkIndex;
    }

    public void setHkIndex(int hkIndex) {
        this.hkIndex = hkIndex;
    }

    /**
     * @return the dkIndex
     */
    public int getDkIndex() {
        return dkIndex;
    }

    public void setDkIndex(int dkIndex) {
        this.dkIndex = dkIndex;
    }

    /**
     * @return the dkReal
     */
    public float getDkReal() {
        return dkReal;
    }

    public void setDkReal(float dkReal) {
        this.dkReal = dkReal;
    }

    /**
     * @return the dkGamma
     */
    public float getDkGamma() {
        return dkGamma;
    }

    public void setDkGamma(float dkGamma) {
        this.dkGamma = dkGamma;
    }

    /**
     * @return the hCount
     */
    public int gethCount() {
        return hCount;
    }

    public void sethCount(int hCount) {
        this.hCount = hCount;
    }

    /**
     * @return the dCount
     */
    public int getdCount() {
        return dCount;
    }

    public void setdCount(int dCount) {
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
