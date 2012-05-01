/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IMiddleClickCapableResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.ChooseColorAction;
import com.raytheon.viz.ui.cmenu.ColorEditDialogAction;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * This handler is responsible for picking up mouse clicks and key press events on resources in
 * the legend
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/03/2012              S. Gurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NCLegendHandler extends AbstractNCLegendInputHandler {
	
    /**
     * @param resource
     */
    protected NCLegendHandler(NCLegendResource resource) {
        super(resource);
    }
   
    private ResourcePair mouseDownRsc = null;

    private int currentRscIndex = 0;
	
	private boolean isShiftDown = false;	

	@Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        
    	if (mouseButton ==1) {
    		//IDisplayPaneContainer editor = getResourceContainer();
            //if (prefManager.handleClick(HIDE_RESOURCE_PREF, mouseButton)) {
    		NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor(); //AbstractEditor
    		if( editor != null &&  editor instanceof NCMapEditor ) {
    			IDisplayPane activePane = editor.getActiveDisplayPane();
    			IRenderableDisplay display = editor.getActiveDisplayPane()
                    .getRenderableDisplay();
    			mouseDownRsc = resource.checkLabelSpace(display.getDescriptor(),
                    activePane.getTarget(), x, y);
    		}
    	}
    	return false;
    }
    
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	if (mouseButton ==1) {
    		NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor(); //AbstractEditor
    		
    		if( editor != null &&  editor instanceof NCMapEditor ) {
    			IDisplayPane activePane = editor.getActiveDisplayPane();
    			IRenderableDisplay display = editor.getActiveDisplayPane()
                    .getRenderableDisplay();
    			ResourcePair rsc = resource.checkLabelSpace(display.getDescriptor(),
                    activePane.getTarget(), x, y);

    			if (rsc != null && rsc == mouseDownRsc) {
    				mouseDownRsc = null;
    				toggleVisibility(rsc);
    				editor.refresh();
    				
    				return true;
    			}
    		}     
    	}
    	return false;
  	
    }
    
    @Override
    public boolean handleDoubleClick(int x, int y, int mouseButton) {
        return false;
    }
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {       
    	return (mouseDownRsc != null);
    }
    public boolean handleMouseHover(int x, int y) {
    	 return false;
    }
    public  boolean handleMouseMove(int x, int y) {
    	 return false;
    }
    public  boolean handleMouseWheel(Event event, int x, int y) {
    	 return false;
    }
    public  boolean handleMouseExit(Event event) {
    	 return false;
    }
    public  boolean handleMouseEnter(Event event) {
    	 return false;
    }

    @Override
	public boolean handleKeyUp(int keyCode) {
    	
    	if ( keyCode == SWT.SHIFT ) {
			isShiftDown = false;
		}
    	
		NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor(); //AbstractEditor
		
		if ( (keyCode==SWT.ARROW_UP) || (keyCode==SWT.ARROW_DOWN) ) {

			if ( isShiftDown ) {
				/*
				 * Make all resources visible
				 */
				ResourceList rl = editor.getActiveDisplayPane().getDescriptor().getResourceList();
				for (int i=0; i < rl.size(); i++ ) {
					rl.get(i).getProperties().setVisible(true);
				}

			}
			else {
				ResourceList rl = editor.getActiveDisplayPane().getDescriptor().getResourceList();
				
				int incr = 1;
				if (keyCode==SWT.ARROW_DOWN) incr = -1;

				/*
				 * look for next non map/system layer resource
				 */
				int search = currentRscIndex;
				do {
					search += incr;
					if ( search < 0 ) search = rl.size() - 1;
					if ( search >= rl.size() ) search = 0;
					if ( ! rl.get(search).getProperties().isMapLayer() ) {
						currentRscIndex = search;
						break;
					}
				} while ( search != currentRscIndex );

				/*
				 * turn off all non map/system layer resources
				 */
				for (int i=0; i < rl.size(); i++ ) {
					if ( rl.get(i).getProperties().isMapLayer() || rl.get(i).getProperties().isSystemResource())
						rl.get(i).getProperties().setVisible(true);
					else
						rl.get(i).getProperties().setVisible(false);
				}

				//  re-enable selected resource.
				rl.get(currentRscIndex).getProperties().setVisible(true);

			}
			editor.refresh();

		}
		isShiftDown = false;
		return false;
	}
	
	@Override
	public boolean handleKeyDown(int keyCode) {  
		
		if ( keyCode == SWT.SHIFT ) {
			isShiftDown = true;
			return false;
		}
		return false;
	}
    
    private void toggleVisibility(ResourcePair rp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null) {
            if (rsc.hasCapability(BlendedCapability.class)) {
                ResourcePair parentRsc = rsc.getCapability(
                        BlendedCapability.class).getBlendableResource();
                ResourceList children = parentRsc.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                if (parentRsc.getProperties().isVisible() == false) {
                    parentRsc.getProperties().setVisible(true);
                    for (ResourcePair child : children) {
                        child.getProperties().setVisible(true);
                    }
                } else {
                    // topmost resource is visible, toggle us and other rsc
                    if (rp.getProperties().isVisible() == false) {
                        rp.getProperties().setVisible(true);
                        parentRsc
                                .getResource()
                                .getCapability(BlendableCapability.class)
                                .setAlphaStep(BlendableCapability.BLEND_MAX / 2);
                    } else {
                        parentRsc.getResource()
                                .getCapability(BlendableCapability.class)
                                .toggle(rp);
                    }
                }
                return;
            }
        }
        rp.getProperties().setVisible(!rp.getProperties().isVisible());
    }
}
