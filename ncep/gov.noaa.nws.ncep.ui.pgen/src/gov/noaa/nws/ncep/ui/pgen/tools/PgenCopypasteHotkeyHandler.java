/*
 * PgenClipboard
 * 
 * Date created: 01/02/2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PgenClipboard;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

/**
 * This handler handles copy/paste/cut across PGEN layers/activities using hotkeys.
 *
 *  "CUT"  -  Ctrl+X only works under "Select" and "MultiSelect" Modes. It copies all
 *            selected elements to PgenClipboard, then delete originals from PgenResource.
 *  "COPY"  - Ctrl+C copies all selected elements to PgenClipboard.
 *  "PASTE" - Ctrl+V pastes all elements in PgenClipboard to the current active layer.
 *  "SELECTALL" - Ctrl+A only works under "Select" and "MultiSelect" Modes.  It selects 
 *            all elements in current active layer and sends to PgenResource. No attribute 
 *            dialog will be popped up. This could be followed up by Ctrl+C.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/2013		#967		Jun Wu  	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 *
 */
public class PgenCopypasteHotkeyHandler extends AbstractHandler {

	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
         String action = event.getParameter("action");
         if ( action == null || action.isEmpty()){
        	 return null;
         }
         
         PgenResource pgenRsc = PgenSession.getInstance().getPgenResource();
         if ( pgenRsc == null ) {
        	 return null;       	 
         }
                  
         Layer activeLyr = pgenRsc.getActiveLayer();
         if ( activeLyr == null ) {
             return null;       	 
         }
        
         boolean changed = false;
    	 String curAction = PgenSession.getInstance().getPgenPalette().getCurrentAction();
    	 
    	 /*
    	  *  "CUT"  -  Ctrl+X only works under "Select" and "MultiSelect" Modes. It copies all
    	  *            selected elements to PgenClipboard, then delete originals from PgenResource.
    	  *  "COPY"  - Ctrl+C copies all selected elements to PgenClipboard.
    	  *  "PASTE" - Ctrl+V pastes all elements in PgenClipboard to the current active layer.
    	  *  "SELECTALL" - Ctrl+A only works under "Select" and "MultiSelect" Modes.  It selects 
    	  *            all elements in current active layer and sends to PgenResource. No attribute 
    	  *            dialog will be popped up. This could be followed up by Ctrl+C.
    	  */
         if ( action.equalsIgnoreCase( "CUT") ) {
        	 if ( curAction != null && ( curAction.equalsIgnoreCase("Select") || 
        			                     curAction.equalsIgnoreCase("MultiSelect") ) ) {
        	     if ( pgenRsc.getAllSelected() != null && !pgenRsc.getAllSelected().isEmpty() ) {
                     PgenClipboard.getInstance().copy( pgenRsc.getAllSelected() );
        		     pgenRsc.deleteSelectedElements();
        		     changed = true;
        		     
        		     if ( curAction.equalsIgnoreCase("MultiSelect") ) {
            		     PgenUtil.setMultiSelectMode();      		    	       		    	 
        		     }
        		     else {
            		     PgenUtil.setSelectingMode();      		    	 
        		     }
        	     }
        	 }
         }
         else if ( action.equalsIgnoreCase( "COPY") ) {
       		 if ( pgenRsc.getAllSelected().size() > 0 ) {
        	     PgenClipboard.getInstance().copy( pgenRsc.getAllSelected() );  
       		 }
         }
         else if ( action.equalsIgnoreCase( "PASTE") ) {
        	 List<AbstractDrawableComponent> elms = PgenClipboard.getInstance().getElSelected();
        	 if ( elms != null && elms.size() > 0 ) {
           		 changed = true;
        		 pgenRsc.addElements( elms );
       	     }
         }
         else if ( action.equalsIgnoreCase( "SELECTALL") ) {
        	 if ( curAction != null && ( curAction.equalsIgnoreCase("Select") || 
                                         curAction.equalsIgnoreCase("MultiSelect") ) ) {
        		 if ( pgenRsc.getActiveLayer().getDrawables().size() > 0 ) {                     
        		     
        		     if ( curAction.equalsIgnoreCase("MultiSelect") ) {
            		     PgenUtil.setMultiSelectMode();      		    	       		    	 
        		     }
        		     else {
            		     PgenUtil.setSelectingMode();      		    	 
        		     }
       			 
        			 pgenRsc.setSelected( pgenRsc.getActiveLayer().getDrawables() );
//        			 PgenClipboard.getInstance().copy( pgenRsc.getActiveLayer().getDrawables() );  
        		 }
        	 }
         }
                 	  
         if ( changed ) PgenUtil.refresh();
          
		return null;
	}

}
