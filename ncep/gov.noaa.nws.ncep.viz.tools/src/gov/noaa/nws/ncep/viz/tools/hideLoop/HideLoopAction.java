package gov.noaa.nws.ncep.viz.tools.hideLoop;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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
 * 09-Aug-2012  #839        Archana     Updated to toggle the
 *                                      colorbar when its corresponding resource
 *                                      is toggled on/off.  
 * 12/19/12     #960        G. Hull     refresh when Showing data to update the buttons image
 * 	                                    Also remove code to handle colorBar resources since this is now 
 *                                      done in propertiesChanged()    
 * 12/19/12     #960        G. Hull     don't hide the Pgen Resource.
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
							
    		editor.setHideShow( !editor.getHideShow() );      //reverse it
					
            for( ResourcePair resPair : editor.getActiveDisplayPane().getDescriptor().getResourceList() ) {
            
            	if( resPair != null && 
            		!resPair.getProperties().isSystemResource() && 
            		!resPair.getProperties().isMapLayer() &&
            		 resPair.getResource().getClass().getSimpleName().compareTo("PgenResource") != 0 ) {
            	
            		resPair.getProperties().setVisible( !editor.getHideShow() );
            		}
						}
            
			editor.refresh();	
					
			editor.refreshGUIElements(); // triggers updateElement()
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
}

