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
 * This java class is to create GEOMAG sources.
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
@Table(name = "geomag_sources", uniqueConstraints=@javax.persistence.UniqueConstraint(columnNames="sourceName"))

public class GeoMagSource extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    /** The source id number */
    @Id
    private int sourceId;

    /** The source preference id number */
    @Column(length = 16)
    private int preferenceId;
     
    /** The source description */
    @Column(length = 128)
    private String sourceDesc;
    
    /** The source name */
    @Column(length = 64)
    private String sourceName;
    
    /**
     * Constructs and empty GeoMagSource
     */
    public GeoMagSource() {
    }
    
    /**
     * Constructs a new GeoMagSource
     * 
     * @param sourceId
     *            The source ID number
     * @param sourceName
     *            The source name
     * @param preferenceId
     *            The preference ID number
     * @param sourceDesc
     *            The source description
     */
    public GeoMagSource (int sourceId, int preferenceId, String sourceName, String sourceDesc) {
        this.sourceId = sourceId;
        this.preferenceId = preferenceId;
        this.sourceName = sourceName;
        this.sourceDesc = sourceDesc;
    }

    public int getSourceId() {
        return sourceId;
    }

    public void setSourceId(int sourceId) {
        this.sourceId = sourceId;
    }

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public int getPreferenceId() {
        return preferenceId;
    }

    public void setPreferenceId(int preferenceId) {
        this.preferenceId = preferenceId;
    }
    
    public String getSourceDesc() {
        return sourceDesc;
    }

    public void setSourceDesc(String sourceDesc) {
        this.sourceDesc = sourceDesc;
    }
    
    public String toString() {
        return sourceName;
    }
}
