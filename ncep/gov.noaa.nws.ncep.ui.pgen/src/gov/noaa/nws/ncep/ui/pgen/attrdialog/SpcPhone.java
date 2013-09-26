/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SpcPhone
 * 
 * 19 August 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Implementation of SPC phone with a phone number and a passcode.
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

@XmlAccessorType(XmlAccessType.NONE)
public class SpcPhone implements ISerializableObject {
	
	@XmlElement
	private String phoneNumber;
	
	@XmlElement
	private String passcode;
	
	public SpcPhone(){
		super();
	}
	
	public String getPhoneNumber() {
		return phoneNumber;
	}

	public void setPhoneNumber(String phoneNumber) {
		this.phoneNumber = phoneNumber;
	}

	public String getPasscode() {
		return passcode;
	}

	public void setPasscode(String passcode) {
		this.passcode = passcode;
	}
}
