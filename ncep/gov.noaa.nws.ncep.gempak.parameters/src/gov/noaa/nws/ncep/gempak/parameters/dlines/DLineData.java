/*
 * DLINES
 * 
 * Date created (29 December 2009)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.gempak.parameters.dlines;

/**<pre>
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 29-Dec-2009    211        Archana.S    Initial Creation
* 07-Jun-2010    211        Archana.S   Renamed the package as
*                                       gov.noaa.nws.ncep.gempak.parameters.dlines  
*                                       
* </pre>
* @author Archana.S
* @version 1
*/
@SuppressWarnings("hiding")
public final class DLineData<Boolean, Double> {

	/**<tt>Boolean</tt> object that decides if the contour values to the right of the
	 * current contour line are greater than it*/
	Boolean isRightOfContourLineGreater;
	
	/**<tt>Boolean</tt> object that decides if the contour values to the left of the
	 * current contour line are greater than it*/	
	Boolean isLeftOfContourLineGreater;
	
	/**The Double object to be added to grid points to the right of the contour
     * line, in cases where only like-valued contours (or a single contour) exist.*/
	Double epsilon;
	
	/**Overloaded constructor that initializes the private Boolean and Double data
	 **/
	public DLineData(Boolean rState, Boolean lState, Double epsilon){
		setRightOfContourLineGreater(rState);
		setLeftOfContourLineGreater(lState);
		setEpsilon(epsilon);
	}

	/**
	 * @returns the Boolean isRightOfContourLineGreater
	 */
	public Boolean isRightOfContourLineGreater() {
		return isRightOfContourLineGreater;
	}

	/**
	 * @return the Boolean object isLeftOfContourLineGreater
	 */
	public Boolean isLeftOfContourLineGreater() {
		return isLeftOfContourLineGreater;
	}

	/**
	 * Returns the Double object to be added to grid points to the right of the contour
     * line, in cases where only like-valued contours (or a single contour) exist
	 * @return epsilon
	 */
	public Double getEpsilon() {
		return epsilon;
	}

	/**
	 * @param rightDlineState the Boolean object to set
	 */
	private void setRightOfContourLineGreater(Boolean rightDlineState) {
		this.isRightOfContourLineGreater = rightDlineState;
	}

	/**
	 * @param leftDlineState the Boolean object to set
	 */
	private void setLeftOfContourLineGreater(Boolean leftDlineState) {
		this.isLeftOfContourLineGreater = leftDlineState;
	}

	/**
	 * @param epsilon the Double object to set
	 */
	private void setEpsilon(Double epsilon) {
		this.epsilon = epsilon;
	}
	
}
