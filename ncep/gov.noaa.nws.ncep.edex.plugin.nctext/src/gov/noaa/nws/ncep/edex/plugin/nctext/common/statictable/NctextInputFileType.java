/**
 * 
 * NctextInputFieType
 * 
 * This java class create nctext_inputfile_type table for NTEXT
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 02/22/2010		TBD		Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.common.statictable;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

@Entity
@Table(name = "nctext_inputfile_type")
public class NctextInputFileType  extends PersistableDataObject implements
		ISerializableObject, Serializable {

	private static final long serialVersionUID = 1L;
	
	
	
	@Id
	private int tblid;
	

	@Column(length=8)
    private String fileExt; /** use ingest text file extension as product type */

    
    @Column(length=8)
    private String fileType;

	/**
	 * 
	 */
	public NctextInputFileType() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param fileExt
	 * @param fileType
	 */
	public NctextInputFileType( String fileExt, String fileType) {
		//this.id = id;
		this.fileExt = fileExt;
		this.fileType = fileType;
	}



	public String getFileExt() {
		return fileExt;
	}

	public void setFileExt(String fileExt) {
		this.fileExt = fileExt;
	}

	public String getFileType() {
		return fileType;
	}

	public void setFileType(String fileType) {
		this.fileType = fileType;
	}
	

}


