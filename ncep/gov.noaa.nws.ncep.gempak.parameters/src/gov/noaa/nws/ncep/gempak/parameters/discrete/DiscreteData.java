/*
 * Discrete
 * 
 * Date created (10-December-2009)
 * 
 * This code has been developed by the SIB for the AWIPS2 system
 */
package gov.noaa.nws.ncep.gempak.parameters.discrete;

/**<pre>
* 
* Parameterized class that can only comprise of <tt>Float</tt> objects.
* Created by the class <tt>Discrete</tt> after parsing the user-entered string
* of the form <tt>value1-value2=value3;...;valuen1-valuen2=valuen3</tt>
* Each DiscreteData object comprises of 3 Float objects, whose values are 
* obtained from each value1-value2=value3 pair.
* startValue corresponds to value1, endValue to value2 and discreteValue to value3.
*   
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 10-Dec-2009    205       Archana.S   Initial Creation
* 07-Jun-2010    205        Archana.S   Renamed the package as
*                                       gov.noaa.nws.ncep.gempak.parameters.discrete  
* 
* </pre>
* @author Archana.S
* @version 1
*/


final class DiscreteData<Float> {
	
	/**
	 * Floating point number denoting the start of the range for a discrete value
	 */
	protected Float startValue;
	
	/**
	 * Floating point number denoting the end of the range for a discrete value
	 */
	protected Float endValue;
	
	/**
	 * The discrete value for given range
	 */
	protected Float discreteValue;
	
	
	/**
	 * Overloaded constructor that accepts 3 Float objects value1, value2 and value3 to set 
	 * startValue,endValue and discreteValue respectively.
	 * @param value1 - Float object to set startValue
	 * @param value2 - Float object to set endValue
	 * @param value3 - Float object to set discreteValue
	 */
	protected DiscreteData(Float value1,Float value2,Float value3){
		setStartValue(value1);
		setEndValue(value2);
		setDiscreteValue(value3);
	}
	/**
	 * @return the startValue
	 */
	public Float getStartValue() {
		return startValue;
	}
	/**
	 * @param startValue the startValue to set
	 */
	public void setStartValue(Float startValue) {
		this.startValue = startValue;
	}
	/**
	 * @return the endValue
	 */
	public Float getEndValue() {
		return endValue;
	}
	/**
	 * @param endValue the endValue to set
	 */
	public void setEndValue(Float endValue) {
		this.endValue = endValue;
	}
	/**
	 * @return the discreteValue
	 */
	public Float getDiscreteValue() {
		return discreteValue;
	}
	/**
	 * @param discreteValue the discreteValue to set
	 */
	public void setDiscreteValue(Float discreteValue) {
		this.discreteValue = discreteValue;
	}


}
