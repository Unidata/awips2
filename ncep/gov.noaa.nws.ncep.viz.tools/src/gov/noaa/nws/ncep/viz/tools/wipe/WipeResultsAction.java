/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * May 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.wipe;

import java.util.*;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

//import gov.noaa.nws.ncep.viz.overlays.resources.*;
import gov.noaa.nws.ncep.viz.resources.*;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * The class for unloading all but overlay data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/10		#265		G. Zhang   	Initial Creation.
 * 05/10        #265        G. Hull     test for AbstractNatlCntrsRequestableResourceData to
 *                                      remove PGEN dependency
 * 02/13        #972        G. Hull     setDisplayModified flag
 *
 * </pre>
 * 
 * @author	G. Zhang
 */
public class WipeResultsAction extends AbstractHandler {
	
	private Shell shell = null;
	private WipeDialog wDlg = null;
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
  	        
        // Pop up a Message Box for confirmation
         
        shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        wDlg = new WipeDialog(shell);
		Object userResponse = wDlg.open();    
		
		//if user choose OK, we proceed to delete data
		if( userResponse instanceof Boolean ){
			if( ((Boolean) userResponse).booleanValue() ){
				
				 
				// delete all, but overlays and the basic map; 
				// based on NmapUiUtil's findResource().
				 
				AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
				
				if( editor == null ) return null;

				IRenderableDisplay disp = editor.getActiveDisplayPane().getRenderableDisplay();
				if( disp == null ) return null;
				
				IDescriptor idtor= disp.getDescriptor();
				if( idtor == null) return null;				
				ResourceList rscList = idtor.getResourceList();
				
				List<ResourcePair> rmList = new ArrayList<ResourcePair>();				
				
				for( ResourcePair rp : rscList ) {
					
					if( rp != null && isRemovable( rp.getResource() ) ){										
						rmList.add(rp);									
					}
				}				
				
				rscList.removeAll(rmList);	//rscList.clear() removes everything.

				NcEditorUtil.setDisplayAvailable( editor, true );
				
				editor.refresh();

			}
		}else{
			return null;
		}
		
        //mapEditor.refresh();
					
        return null;
    }	
 
    /*
     * all but overlays and basic geo-political map remains
     */
    private boolean isRemovable(AbstractVizResource avr){
    	
    	if(avr == null)
    		return false;
    	
    	AbstractResourceData ard = avr.getResourceData();
    	if(ard == null)
    		return false;
    	
    	if( ard instanceof AbstractNatlCntrsRequestableResourceData  ) 
    		return true;
    	else
    		return false;    	
    }

}
