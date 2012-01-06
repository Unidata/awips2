/**
 * This java class is to create McIDAS area names.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 11/2009		144			T. Lee		Created
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
@Table(name = "mcidas_area_names")

public class McidasAreaName extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    /** The area file id number */
    @Id
    private int areaId;

    /** The area file name */
    @Column(length = 64)
    private String areaName;

    /**
     * Constructs and empty McidasAreaName
     */
    public McidasAreaName() {
    }
    
    /**
     * Constructs a new SatelliteName
     * 
     * @param areaId
     *            The area file ID number
     * @param areaName
     *            The area name
     */
    public McidasAreaName (int areaId, String areaName) {
        this.areaId = areaId;
        this.areaName = areaName;
    }

    public int getAreaId() {
        return areaId;
    }

    public void setAreaId(int areaId) {
        this.areaId = areaId;
    }

    public String getAreaName() {
        return areaName;
    }

    public void setAreaName(String areaName) {
        this.areaName = areaName;
    }

    public String toString() {
        return areaName;
    }
}
