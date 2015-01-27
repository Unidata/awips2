/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
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
 * Record implementation for geomag station state. Tracks all the magnetometers
 * station state changes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 06/27/2014   R4078       sgurung            Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "geomagstatechangeseq")
@Table(name = "geomag_station_state_changes")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagStationStateChange extends PersistableDataObject<Object> {

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
     * issued action
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String issuedAction;

    /**
     * insert time tag
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Date insertTime;

    public String getStationCode() {
        return stationCode;
    }

    /**
     * 
     * @param stationCode
     */
    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }

    /**
     * 
     * @return issuedAction
     */
    public String getIssuedAction() {
        return issuedAction;
    }

    /**
     * 
     * @param issuedAction
     */
    public void setIssuedAction(String issuedAction) {
        this.issuedAction = issuedAction;
    }

    /**
     * 
     * @return insertTime
     */
    public Date getInsertTime() {
        return insertTime;
    }

    /**
     * 
     * @param insertTime
     */
    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

}
