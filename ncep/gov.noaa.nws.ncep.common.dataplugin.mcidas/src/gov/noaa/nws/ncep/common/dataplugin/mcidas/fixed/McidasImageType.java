/**
 * This java class is to create McIDAS image type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
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
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

@Entity
@Table(name = "mcidas_image_type")
public class McidasImageType extends PersistableDataObject implements 
		Serializable, ISerializableObject {

	private static final long serialVersionUID = 294073530675679025L;

	/** The identifier */
	@Id
	private int id;

	/** The image type */
	@Column (length = 32)
	private String imageType; 

    /** 
     * The image type number.  A image type number may represent different image type
     * for different satellite.
     */
    @Column (length = 16)
    private String imageTypeNumber;
    
    /**
     * The satellite ID
     */
    @Column (length=16)
    private String satelliteId;
    
    /**
     * Default constructor.
     */
    public McidasImageType() {
    }

    /**
     * Constructs a new McidasImageType
     * 
     * @param id
     *            The identifier id
     * @param imageType
     * 			  The image type
     * @param imageTypeNumber
     * 			  The image type number
     * @param satelliteId
     *            The satellite name
     */
    public McidasImageType(int id, String imageType, String imageTypeNumber, 
    		String satelliteId) {
    	this.id = id;
    	this.imageType = imageType;
    	this.satelliteId = satelliteId;
    	this.imageTypeNumber = imageTypeNumber;
    }

    public int getId() {
    	return id;
    }

    public void setId(int id) {
    	this.id = id;
    }

    public String getImageType() {
    	return imageType;
    }

    public void setImageType(String imageType) {
    	this.imageType = imageType;
    }

    public String getImageTypeNumber() {
    	return imageTypeNumber;
    }

    public void setImageTypeNumber(String imageTypeNumber) {
    	this.imageTypeNumber = imageTypeNumber;
    }

    public String getSatelliteId() {
    	return satelliteId;
    }

    public void setSatelliteId(String satelliteId) {
    	this.satelliteId = satelliteId;
    }
}
