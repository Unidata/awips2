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

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.legend.ILegendDecorator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;

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
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NCLegendResource extends
        AbstractLegendResource<GenericResourceData> implements IInputHandler, ILegendDecorator {

	ResourcePair mouseDownRsc = null;
	
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
        
        IInputHandler handler = new NCLegendResource(resourceData, loadProperties);
        rc.registerMouseHandler(handler, InputPriority.PERSPECTIVE); //.RESOURCE
        
    }
    
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
    			mouseDownRsc = checkLabelSpace(display.getDescriptor(),
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
    			ResourcePair rsc = checkLabelSpace(display.getDescriptor(),
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
    public  boolean handleKeyDown(int keyCode) {
    	 return false;
    }
    public  boolean handleKeyUp(int keyCode) {
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