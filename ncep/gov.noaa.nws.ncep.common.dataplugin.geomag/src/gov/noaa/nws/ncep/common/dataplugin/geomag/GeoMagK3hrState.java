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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
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
 * Record implementation for geomag_K3hr_state.
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
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "geomagk3hrstateseq")
@Table(name = "geomag_k3hr_state", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "k3hrId", "stateId" }) })
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GeoMagK3hrState extends PersistableDataObject<Object> {

    private static final long serialVersionUID = 1L;

    public static final String ID_GEN = "idgen";

    /** The id */
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = ID_GEN)
    private Integer id;

    /**
     * station state id
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer stateId;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer k3hrId;

    /**
     * 
     * @return id
     */
    public Integer getId() {
        return id;
    }

    /**
     * 
     * @param id
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * 
     * @return stateId
     */
    public Integer getStateId() {
        return stateId;
    }

    /**
     * 
     * @param stateId
     */
    public void setStateId(Integer stateId) {
        this.stateId = stateId;
    }

    /**
     * 
     * @return k3hrId
     */
    public Integer getK3hrId() {
        return k3hrId;
    }

    /**
     * 
     * @param k3hrStateId
     */
    public void setK3hrId(Integer k3hrId) {
        this.k3hrId = k3hrId;
    }

}
