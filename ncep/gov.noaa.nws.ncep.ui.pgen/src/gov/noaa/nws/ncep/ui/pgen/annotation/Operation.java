/*
 * Operation
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.annotation;

/**
 * Specifies a set of possible operations that can be performed on 
 * PGEN DrawableElements.
 * 
 * @author sgilbert
 *
 */
public enum Operation {

	CONNECT,
	COPY_MOVE,
	DELETE_PART,
	DELETE_POINT,
	ADD_POINT,
	EXTRAPOLATE,
	FLIP,
	INTERPOLATE,
	MODIFY,
	ROTATE,
	GFA_FROM
	
}
