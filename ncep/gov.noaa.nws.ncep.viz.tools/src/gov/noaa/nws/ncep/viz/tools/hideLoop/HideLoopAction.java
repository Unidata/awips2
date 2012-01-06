package gov.noaa.nws.ncep.viz.tools.hideLoop;

import java.util.*;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import gov.noaa.nws.ncep.viz.resources.*;
import gov.noaa.nws.ncep.viz.tools.Activator;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;


/**
 * The class for unloading all but overlay data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		#314		Q. Zhou   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	Q. Zhou
 */
public class HideLoopAction extends AbstractHandler implements IElementUpdater  { //AbstractTool { 
	
	private ImageDescriptor loopShow = null;
	private ImageDescriptor loopHide = null;
	
	public HideLoopAction() {
		loopShow = AbstractUIPlugin.imageDescriptorFromPlugin(
        		Activator.PLUGIN_ID, "icons/show_loop.gif"); //Show btn displayed
		loopHide = AbstractUIPlugin.imageDescriptorFromPlugin(
        		Activator.PLUGIN_ID, "icons/hide_loop.gif"); //Hide btn displayed
	}
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	/*   	 
		 * Hide all, but overlays and the basic map; 
		 * based on NmapUiUtil's findResource().
    	 */
    	NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor(); //AbstractEditor
    	if( editor != null &&  editor instanceof NCMapEditor ) {		
			if (editor.getHideShow() == false) //data visible
				editor.setHideShow(true);      //reverse it
			else
				editor.setHideShow(false);
			
			NCMapDescriptor idtor = (NCMapDescriptor)editor.getDescriptor();
				
			if( idtor != null) {	
				ResourceList rscList = idtor.getResourceList();
				
				for( ResourcePair rp : rscList ) {
					
					if( rp != null && isRemovable( rp.getResource() ) ){
						if (editor.getHideShow() == true) {
							
							rp.getProperties().setVisible(false); 							
						}
					    else {
					
							rp.getProperties().setVisible(true);							
						}
					}
				}						
			}
		
			editor.refresh();	
					
		
			/*   	 
			 * calls updateElement()
			 */
			editor.refreshGUIElements();
		}
        return null;
    }	
 
    @Override
	public void updateElement(UIElement element, Map parameters) {
    	NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
    	if (editor != null) {
    		if (editor.getHideShow() == true) {			
    			element.setIcon( loopShow );
    		}
    		else {			
    			element.setIcon( loopHide );
    		}
		}
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

