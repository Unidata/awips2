/*
 * CommandStackListener
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

/**
 * The listener interface for receiving the PgenCommandManager's stack sizes,
 * when either changes.
 * @author sgilbert
 *
 */
public interface CommandStackListener {

	/**
	 * Invoked when the size or either the Undo or redo stack in the PgenCommandManager changes
	 * @param undoSize
	 * @param redoSize
	 */
	public void stacksUpdated( int undoSize, int redoSize);
	
}
