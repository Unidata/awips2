/*
 * CatMapInfo
 * 
 * Date created (20 November 2009)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.gempak.parameters.core.categorymap;
/**<pre>
*An object of the CatMapInfo class is to be created and used only by the class CATMAP 
* Final class CatMapInfo is parameterized with two parameters:
* A String - to store the label
* A Float - to store the value
*For more details about how the label and value are obtained
*see {@link gov.noaa.nws.ncep.gempak.parameters.core.categorymap.CatMap} 
* 
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 20-Nov-2009    194        Archana.S   Initial Creation
* 07-Jun-2010    194        Archana.S   Renamed the package as
*                                       gov.noaa.nws.ncep.gempak.parameters.categorymap 
* </pre>
* @author Archana.S
* @version 1
*/
@SuppressWarnings("hiding")
final class CatMapInfo<String,Float> {
	
	/**A string containing the label*/
	private String label;
	
	/**A string containing the floating point value*/
	private Float value;
	
	/***
	 *Stores the input string and floating point parameters
	 *
	 * @param label - the string stored as the label for the CatMapInfo object
	 * @param value - the floating point value stored as the value associated with the given label
	 */
	protected CatMapInfo(String label, Float value){
           this.setLabel(label);
           this.setValue(value);
	}
/**
 * 
 * @param label - the string to set
 */
	protected void setLabel(String label) {
		this.label = label;
	}
/**
 * 
 * @return the string label
 */
	protected String getLabel() {
		return label;
	}
/**
 * 
 * @param value - the floating point data to set
 */
	protected void setValue(Float value) {
		this.value = value;
	}
/**
 * 
 * @return the floating point value
 */
	protected Float getValue() {
		return value;
	}
	
}
