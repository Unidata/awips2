/*
 * PgenSession
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen;

import gov.noaa.nws.ncep.ui.pgen.controls.PgenCommandManager;
import gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

/**
 * This singleton is intended to couple a PGEN Palette with a PGgenResource, so that
 * a palette can be updated and used to modify a specific PgenResource
 * @author sgilbert
 *
 */
public class PgenSession {

	/*
	 * The singleton instance
	 */
	private static PgenSession instance = null;
	
	/*
	 * the current PGEN resource
	 */
	private PgenResource pgenResource = null;
	
	/*
	 * the current PGEN palette
	 */
	private PgenPaletteWindow palette = null;
	
	/*
	 * Hide default constructor
	 */
	private PgenSession() {
			
	}
	
	/**
	 * Static method to get THE PgenSession instance
	 * @return PgenSession reference
	 */
	public static synchronized PgenSession getInstance() {
		
		if ( instance == null ) instance = new PgenSession();
		return instance;
	}
	
	/**
	 * Sets a PgenResource for the current session
	 * @param rsc a Pgen Resource
	 */
	public void setResource(PgenResource rsc) {
		
		/*
		 * Remove the current PGEN Resource from the Session
		 */
		removeResource();
		
		// set new PGEN resource
		pgenResource = rsc;
		// add the palette's stack listener to new resource's command Manager
		if ( pgenResource != null ) pgenResource.getCommandMgr().addStackListener(palette);

	}
	
	/**
	 * Removes the current PGEN resource from the Session
	 */
	public void removeResource() {
		if ( pgenResource != null ) {
			//Remove the Palette's stack listener from the Resource's CommandManager
			pgenResource.getCommandMgr().removeStackListener(palette);
		}
		pgenResource = null;
		
		/*
		 * disable the palette's Undo and redo buttons.
		 */
		if ( palette != null ) palette.disableUndoRedo();
	}
	
	/**
	 * Gets an appropriate PGEN Resource.  Returns the current Pgen Resource registered with this
	 * PGEN Session if there is one.  If not, it will look for an existing resource in the current
	 * editor.  If one is not found, a new PgenResource will be created.
	 * @return the rsc
	 */
	public PgenResource getPgenResource() {
		
		if ( pgenResource == null ) {
//			PgenResource rsc = PgenUtil.findPgenResource(NmapUiUtils.getActiveNatlCntrsEditor());
			PgenResource rsc = PgenUtil.findPgenResource(PgenUtil.getActiveEditor());
			if ( rsc != null ) {
				pgenResource = rsc;
			}
			else {
				pgenResource = PgenUtil.createNewResource();
			}
		}
		
		return pgenResource;
	}
	
	/**
	 * Get the PGEN Resource currently registered with the session
	 * @return
	 */
	public PgenResource getCurrentResource() {
		return pgenResource;
	}

	/**
	 * Gets the Resource's Command Manager
	 * @return the commandMgr
	 */
	public PgenCommandManager getCommandManager() {
		return pgenResource.getCommandMgr();
	}

	/**
	 * Register the given palette with the Session
	 * @param pal
	 */
	public void setPalette(PgenPaletteWindow pal) {
		palette = pal;
		// Register this palette's stack listener with the CommandManager, if able
		if ( pgenResource != null ) pgenResource.getCommandMgr().addStackListener(palette);
	}
	
	/**
	 * Remove the current palette from this Session
	 */
	public void removePalette() {
		// Remove this palette's stack listener from the CommandManager, if able
		if ( pgenResource != null ) pgenResource.getCommandMgr().removeStackListener(palette);
		palette = null;
	}
	
	/**
	 *  Clear and disable undo/redos.
	 */	
	public void disableUndoRedo() {
		
		if ( pgenResource != null ) getCommandManager().clearStacks();
		
		if ( palette != null ) {
		    palette.disableUndoRedo(); 
		}
	
	}	
	
	/**
	 *	Return the palette window
	 */
	public PgenPaletteWindow getPgenPalette(){
		return palette;
	}
		
}
