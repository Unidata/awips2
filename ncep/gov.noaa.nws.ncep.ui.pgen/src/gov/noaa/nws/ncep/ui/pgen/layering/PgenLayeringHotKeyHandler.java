/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.layering;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

/**
 * @author archana
 *
 */
public class PgenLayeringHotKeyHandler extends AbstractHandler {

	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
         String layerIndexStr = event.getParameter("layerIndex");
         if(layerIndexStr == null || layerIndexStr.isEmpty()){
        	 return null;
         }
         
         int layerIndex = Integer.parseInt(layerIndexStr);
//          PgenResource pgenResource = PgenUtil.findPgenResource(NmapUiUtils.getActiveNatlCntrsEditor());
          PgenResource pgenResource = PgenUtil.findPgenResource(PgenUtil.getActiveEditor());
          if(pgenResource != null && layerIndex > 0){
        	  Product activeProduct = pgenResource.getActiveProduct();
        	  int layerListSize = activeProduct.getLayers().size();
        	  
        	 if(layerListSize >= layerIndex){ 
        	  Layer layerToActivate = activeProduct.getLayer(layerIndex - 1);
        	  layerToActivate.setOnOff(true);
        	  pgenResource.setActiveLayer(layerToActivate);
        	
        	  for(int index=0;index < layerListSize; index ++){
        		  if(index == layerIndex-1){
        			  continue;
        		  }
        		  /*Switch off the other layers*/
        		  Layer currentLayer =  pgenResource.getActiveProduct().getLayer(index);
        		  currentLayer.setOnOff(false);
        	  }
        	  PgenUtil.refresh();
          }
          
	}
		return null;
	}

}
