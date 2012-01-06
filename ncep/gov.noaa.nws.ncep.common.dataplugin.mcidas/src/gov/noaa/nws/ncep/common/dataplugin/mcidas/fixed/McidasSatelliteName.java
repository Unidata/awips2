/**
 *  * This java class is to create McIDAS satellite name.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/2009		144			T. Lee		Created
 * 05/2010		144			L. Lin		Migration to TO11DR11.
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

@Entity
@Table(name = "mcidas_satellite_names")
public class McidasSatelliteName extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = -4678013903413236803L;

    /** Satellite ID */
    @Id
    private int satelliteId;

    /** Satellite name */
    @Column(length = 64)
    private String satelliteName;

    /**
     * Constructs an empty satellite name
     */
    public McidasSatelliteName() {

    }

    /**
     * Constructs a new satellite ID
     * 
     * @param satelliteId
     *            The satellite id number
     * @param satelliteName
     *            The satellite ID
     */
    public McidasSatelliteName(int satelliteId, String satelliteName) {
        this.satelliteId = satelliteId;
        this.satelliteName = satelliteName;
    }

    public int getSatelliteId() {
        return satelliteId;
    }

    public void setSatelliteId(int satelliteId) {
        this.satelliteId = satelliteId;
    }

    public String getSatelliteName() {
        return satelliteName;
    }

    public void setSatelliteName(String satelliteName) {
        this.satelliteName = satelliteName;
    }

    public String toString() {
        return satelliteName;
    }
}
