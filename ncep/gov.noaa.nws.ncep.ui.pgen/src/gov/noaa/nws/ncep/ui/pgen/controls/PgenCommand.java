/*
 * PgenCommand
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import gov.noaa.nws.ncep.ui.pgen.PGenException;

/**
 * This Interface is used to implement PGEN Commands that change are used to
 * change any objects in the PgenResource.  These commands will be used to 
 * implement an Undo/Redo feature in the PGEN drawing tools.
 * 
 * The necessary steps to make the desired change are implemented in the execute() method, and
 * the necessary steps to undo that action should be implemented in the undo() method.
 * @author sgilbert
 *
 */
public abstract class PgenCommand {

	/**
	 * Executes the PGEN command
	 * @throws PGenException
	 */
	abstract public void execute() throws PGenException;
	
	/**
	 * Un-does the operation implemented in the execute() method
	 * @throws PGenException
	 */
	abstract public void undo() throws PGenException;
	
}
