/**
 * 
 * 
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 *
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.gpd.product;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Embeddable;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Embeddable
@Entity
@Table(name = "gpd_productinfo")
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name="GPD-ProdInfo-Def")
@DynamicSerialize
public class GenericPointDataProductInfo extends
		PersistableDataObject<String> {
    private static final long serialVersionUID = 1L;

    @Id
    @DynamicSerializeElement
    @XmlAttribute(name = "prodName", required = true)
    //@DataURI(position = 0)
    private String name;

    @DynamicSerializeElement
    @XmlAttribute
    private int maxNumberOfLevel= 64;
    
    @ManyToOne(optional = false)
    @PrimaryKeyJoinColumn
    @XmlElement(name="GPD-Level-Def")
    @DynamicSerializeElement
    //@DataURI(position = 1, embedded = true)
    private MasterLevel masterLevel;

    @ManyToMany
    @DynamicSerializeElement
	@XmlElement(name="GPD-Parameter-Def")
	private List<Parameter> parameterLst = new ArrayList<Parameter>();
    
    
	public GenericPointDataProductInfo() {
		super();
		//System.out.println("GenericPointDataProductInfo(1) entered");
	}

	public GenericPointDataProductInfo(String name, int maxNumberOfLevel
			) {
		super();
		this.name = name;
		this.maxNumberOfLevel = maxNumberOfLevel;
		MasterLevel ml = new MasterLevel();
		ml.setName("UNKNOWN");
		this.masterLevel = ml;
		//System.out.println("GenericPointDataProductInfo(4) entered");
	}

	public GenericPointDataProductInfo(String name, int maxNumberOfLevel,
			MasterLevel masterLevel) {
		super();
		this.name = name;
		this.maxNumberOfLevel = maxNumberOfLevel;
		this.masterLevel = masterLevel;
		//System.out.println("GenericPointDataProductInfo(2) entered");
	}



	public GenericPointDataProductInfo(String name, int maxNumberOfLevel,
			MasterLevel masterLevel, List<Parameter> parameterLst) {
		super();
		this.name = name;
		this.maxNumberOfLevel = maxNumberOfLevel;
		this.masterLevel = masterLevel;
		this.parameterLst = parameterLst;
		//System.out.println("GenericPointDataProductInfo(3) entered");
	}


	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}


	public MasterLevel getMasterLevel() {
		return masterLevel;
	}

	public void setMasterLevel(MasterLevel masterLevel) {
		this.masterLevel = masterLevel;
	}


	public List<Parameter> getParameterLst() {
		return parameterLst;
	}


	public void setParameterLst(List<Parameter> parameterLst) {
		this.parameterLst = parameterLst;
	}


	public int getMaxNumberOfLevel() {
		return maxNumberOfLevel;
	}


	public void setMaxNumberOfLevel(int maxNumberOfLevel) {
		this.maxNumberOfLevel = maxNumberOfLevel;
	}


	@Override
	public GenericPointDataProductInfo clone() throws CloneNotSupportedException {
		GenericPointDataProductInfo rpt = new GenericPointDataProductInfo(this.name,this.maxNumberOfLevel,this.masterLevel);
		rpt.getParameterLst().clear();
		for(Parameter parm:this.parameterLst){
			String abb = parm.getAbbreviation();
			String name = parm.getName();
			String units = parm.getUnitString();
			Parameter newParm = new Parameter();
			newParm.setAbbreviation(abb);
			newParm.setUnitString(units);
			newParm.setName(name);
			rpt.getParameterLst().add(newParm);
		}
		return rpt;
	}	
}
