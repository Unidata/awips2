/*****************************************************************************************
 * COPYRIGHT (c), 2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.ui.display;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.legend.ILegendDecorator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.EnableDisableAction;
import com.raytheon.viz.ui.cmenu.MoveDownAction;
import com.raytheon.viz.ui.cmenu.MoveUpAction;
import com.raytheon.viz.ui.cmenu.RemoveResourceAction;

/**
 * Legend decorator for Natl Cntrs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2010     #259     Greg Hull    Initial Creation
 * 07/13/2001     #446     Q. Zhou      Added implements IInputHandler
 * 										Added mouse handlers, initInternal, toggleVisibility. See D2DLegendResource
 * 02/06/2011              S. Gurung    Separated/moved input handler code to class NCLegendHandler
 * 02/29/2011     651      Archana      Added the overridden method fillContextMenu()										
 * 07/27/2012	  695	   B. Yin		Added editable capability for resource legends
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class NCLegendResource extends
        AbstractLegendResource<GenericResourceData> implements ILegendDecorator {

	private IInputHandler legendHandler = new NCLegendHandler(this);
	 
	 /**
     * @param resourceData
     * @param loadProperties
     */
    public NCLegendResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {

        List<LegendData> labels = new ArrayList<LegendData>();
        ResourceList resourceList = descriptor.getResourceList();
        if (resourceList != null) {
            for (int i = 0; i < resourceList.size(); i++) {
                ResourcePair resourcePair = resourceList.get(i);
                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                // See if resource is visible
                boolean vis = resourcePair.getProperties().isVisible();
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (system) {
                    continue;
                } else {
                    LegendData legend = new LegendData();
                    if (rsc == null) {
                        continue;
                    } else if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                        continue;
                    } else {
                        legend.label = rsc.getName();
                        //if the resource is editable, add (Editable) to the legend string
                        if (rsc.hasCapability(EditableCapability.class)
                                && rsc.getCapability(EditableCapability.class)
                                        .isEditable()) {
                        	legend.label += " (Editable) ";
                        }
                        legend.resource = resourcePair;
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        // get the color from the NatlCntrs Resource.
                        legend.color = new RGB(250, 250, 250); // default to
                                                               // white

                        try {
                            // HACK ALERT : currently there is a cyclical
                            // dependency bug
                            // that prevents the display project from
                            // referencing the
                            // resources project.
                            // get the method from INatlCntrsResource to get the
                            // legend
                            // color
                            Method[] mthds = legend.resource.getResourceData()
                                    .getClass().getMethods();

                            for (Method m : mthds) {
                                // System.out.println( m.getName() );
                                if (m.getName().equals("getLegendColor")) {
                                    if (m.getReturnType() == RGB.class) {
                                        legend.color = (RGB) m
                                                .invoke(legend.resource
                                                        .getResourceData());
                                        break;
                                    }
                                }
                            }
                        } catch (Exception e) {
                            System.out.println(e.getMessage());
                        }
                    }
                    labels.add(legend);
                }

            }
        }

        LegendEntry[] entries = new LegendEntry[labels.size()];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].legendParts = new LegendData[] { labels.get(i) };
        }
        return entries;
    }
    
    protected void initInternal(IGraphicsTarget target) {
        try {
			super.initInternal(target);
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
        target.initializeFont(NCLegendResource.class.getName());
        IDisplayPaneContainer rc = getResourceContainer();
        if (rc == null) 
        	return;
       
        rc.registerMouseHandler(legendHandler,InputPriority.SYSTEM_RESOURCE);
    }
    
    @Override
	protected void disposeInternal() {
		super.disposeInternal();
 
		IDisplayPaneContainer rc = getResourceContainer();
        if (rc == null) 
        	return;
        
        rc.unregisterMouseHandler(legendHandler);
	}
    
   @Override
   protected ResourcePair checkLabelSpace(IDescriptor descriptor,
           IGraphicsTarget target, double x, double y) {
       // NOTE: Overridden so legend handlers can call
       return super.checkLabelSpace(descriptor, target, x, y);
   }

//   @Override
// protected void fillContextMenu(IMenuManager menuManager,
//		ResourcePair selectedResource) {
//
//   	MoveUpAction upAction = new MoveUpAction();
//	MoveDownAction downAction = new MoveDownAction();
//	EnableDisableAction enableDisableAction = new EnableDisableAction();
//	RemoveResourceAction rrAction = new RemoveResourceAction();
//	NCMapEditor container = NmapUiUtils.getActiveNatlCntrsEditor();
//
//    upAction.setContainer(container);
//	upAction.setSelectedRsc(selectedResource);
//	downAction.setContainer(container);
//	downAction.setSelectedRsc(selectedResource);
//	enableDisableAction.setContainer(container);
//	enableDisableAction.setSelectedRsc(selectedResource);
//	rrAction.setContainer(container);
//	rrAction.setSelectedRsc(selectedResource);
//	
//    AbstractVizResource<?,?> thisResource = selectedResource.getResource();
//    //Cyclic dependancy work-around..
//	Object editRscAttrsAction =  null;
//    Method[] arrayOfMethods = thisResource.getClass().getMethods();
//    for ( Method m : arrayOfMethods ){
//    	if ( m.getName().compareTo("resourceAttrsModified") == 0){
//    		try {
//				editRscAttrsAction =  Class.forName("gov.noaa.nws.ncep.viz.resources.attributes.EditResourceAttrsAction").newInstance();
//				( ( AbstractRightClickAction ) editRscAttrsAction ).setContainer(container);
//				( ( AbstractRightClickAction ) editRscAttrsAction ).setSelectedRsc(selectedResource);
//				break;
//			} catch (InstantiationException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			} catch (IllegalAccessException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			} catch (ClassNotFoundException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//    		
//    	}
//    }
//
//    if ( editRscAttrsAction != null )
//    	menuManager.add( ( IAction )editRscAttrsAction);
//    
//	menuManager.add(upAction);
//	menuManager.add(downAction);
//	menuManager.add(enableDisableAction);
//	menuManager.add(rrAction);
//	
// }

}