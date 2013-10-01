/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SpcPhoneList
 * 
 * 19 August 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Implementation of SPC phone list.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/13		TTR794		B. Yin   	Initial Creation.
 * </pre>
 * 
 * @author B. Yin
 */

@XmlRootElement(name="spcPhoneNumbers")
@XmlAccessorType(XmlAccessType.NONE)
public class SpcPhoneList implements ISerializableObject{

	//phone table
    private static final String SPC_PHONE_TBL = "spcPhones.xml";
    
    //singleton
    private static SpcPhoneList INSTANCE = null; 
    
    //phone list
	@XmlElements({@XmlElement(name="phone")})
	private List<SpcPhone> spcPhones = null;

	/**
	 * Constructor
	 */
	private SpcPhoneList(){
		super();
		spcPhones = new ArrayList<SpcPhone>();
	}
	
    /**
     * Gets or creates the instance.
     * @return instance
     */
	public static SpcPhoneList getInstance(){
		if ( INSTANCE == null ){
			return load();
		}
		else {
			return INSTANCE;
		}
	}
	
	/**
	 * Loads the phone list from a table in localization.
	 * Returns an empty list if the table does not exist. 
	 * @return instance of this class
	 */
	private static SpcPhoneList load(){
		String phoneTbl = PgenStaticDataProvider.getProvider()
				.getFileAbsolutePath(
						PgenStaticDataProvider.getProvider()
						.getPgenLocalizationRoot()
						+ SPC_PHONE_TBL);
		SpcPhoneList phoneList = null;
		try {
			phoneList = SerializationUtil
					.jaxbUnmarshalFromXmlFile(SpcPhoneList.class, phoneTbl);
		} catch (SerializationException e) {
			// return empty list
			phoneList = new SpcPhoneList();
		}

		return phoneList;
	}
	
	/**
	 * Returns the phone list 
	 * @return
	 */
	public List<SpcPhone> getSpcPhones() {
		return spcPhones;
	}

	/**
	 * Sets the phone list.
	 * @param spcPhones
	 */
	public void setSpcPhones(List<SpcPhone> spcPhones) {
		this.spcPhones = spcPhones;
	}

}
