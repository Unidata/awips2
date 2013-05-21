package gov.noaa.nws.ncep.common.dataplugin.geomag.fixed;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
/**
 * This java class is to create GEOMAG source preferences.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 04/2013		975			S. Gurung	Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

@Entity
@Table(name = "geomag_source_preferences")

public class GeoMagSourcePreference extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    /** The source id number */
    @Id
    private int preferenceId;

    /** The source name */
    @Column(length = 128)
    private String description;

    /** The preference order number */
    @Column(length = 128)
    private int preferenceOrder;
    
    /**
     * Constructs and empty GeoMagSourcePreference
     */
    public GeoMagSourcePreference() {
    }
    
    /**
     * Constructs a new GeoMagSourcePreference
     * 
     * @param preferenceId
     *            The area file ID number
     * @param description
     *            The area name
     */
    public GeoMagSourcePreference (int sourceId, String sourceName) {
        this.preferenceId = sourceId;
        this.description = sourceName;
    }

    public int getSourceId() {
        return preferenceId;
    }

    public void setSourceId(int sourceId) {
        this.preferenceId = sourceId;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String desc) {
        this.description = desc;
    }
    public int getPreferenceOrder() {
        return preferenceOrder;
    }

    public void setPreferenceOrder(int preferenceOrder) {
        this.preferenceOrder = preferenceOrder;
    } 

    public String toString() {
        return description;
    }
    
}
