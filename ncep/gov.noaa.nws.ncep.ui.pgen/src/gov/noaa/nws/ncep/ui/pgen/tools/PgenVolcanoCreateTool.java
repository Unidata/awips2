/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.VolcanoCreateDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.EditorUtil;

/**
 * The class for Volcano creation and Volcano create dialog opening
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

public class PgenVolcanoCreateTool extends AbstractPgenTool {	
 
    protected AttrDlg attrDlg;

    protected String pgenType = null;  
    protected String pgenCategory = null; 

    @Override
    protected void activateTool( ) {    	
    	
    	PlatformUI.getWorkbench().getActiveWorkbenchWindow().
    				getActivePage().activate(EditorUtil.getActiveEditor());

    	super.activateTool();
    	
    	/*
    	 * 2010-04-14: one Product contains ONLY
    	 * one Volcano; use a NEW Volcano Product
    	 * Center for another Volcano
    	 */
    	if( isUsedVolProd() ){ 
    		openConfirmBox("Please start with Volcano Activity to create a new volcano."); 
        	return;
    	}
    	
    	String param;
    	param = event.getParameter("name");
    	if ( param != null ) pgenType = param;
    	param = event.getParameter("className");
    	if ( param != null ) pgenCategory = param;
    	
    	if ( super.isDelObj() ){
   
    		int numEls = drawingLayer.selectObj( pgenType );
    		
    		if ( numEls > 0 ){
    			
    			MessageDialog confirmDlg = new MessageDialog( 
    					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    					"Confirm Delete", null, "Are you sure you want to delete all " + numEls +" selected element(s)?",
    					MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
    			
    			confirmDlg.open();

    			if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
    				drawingLayer.deleteSelectedElements();
    			}else {    				
    				drawingLayer.removeSelected();
    			}    			
    			editor.refresh();
    		}

    		return;
    	}    	
        
    	//NOT an attributes dialog, so create here without using AttrFactory
    	VolcanoCreateDlg attrDlg = 
    		VolcanoCreateDlg.getInstance(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
        
        if ( attrDlg != null ) {

        	attrDlg.setBlockOnOpen( false );
        	attrDlg.open();
       		
       	    attrDlg.setDefaultAttr();
        	        	
       	    attrDlg.setPgenCategory(pgenCategory);
       	    attrDlg.setPgenType(pgenType);
    		attrDlg.setDrawingLayer(drawingLayer);
    		attrDlg.setMapEditor( mapEditor );  
        	attrDlg.setBlockOnOpen( true );
        	
        	attrDlg.setCreateTool(this);

        }
           	
    }

   // @Override   
    public void deactivateTool() {

    	super.deactivateTool();
    	
        //if (editor != null)
            //editor.unregisterMouseHandler( this.mouseHandler );
        if ( attrDlg != null ){
        	attrDlg.close();
        	attrDlg = null;
        }
    }
    
    /**
     * check if the Product already has a Volcano
     * to ensure there is ONLY one Volcano in
     * a Product Center
     * @return boolean: true if a new Product Center needed
     */
    
    private boolean isUsedVolProd(){ 
    	
    	if( drawingLayer == null )
    		return true;    	 
    	
    	Product prod = drawingLayer.getActiveProduct();
    	
    	if( prod == null )
    		return true;
    	
    	//if the prod is NOT a Volcano prod then a NEW Volcano prod is needed
    	if( !"VOLCANO".equalsIgnoreCase( prod.getType() ) )
    		return true;
    	
    	/*
    	 * if prod in the map then a NEW prod is needed---initial logic.
    	 * 
    	 * changed to: 	if prod in VOL_PROD_MAP, but the Volcano is deleted
    	 * 				then this Product can be re-used
    	 */
    	if(VaaInfo.VOL_PROD_MAP.containsValue(prod)){
        	   		
    		List<Layer> lyrList = prod.getLayers();    		
    		
    		//NO fixed Layers of Volcano and Ash Clouds
    		if(lyrList == null ) 
    			return true;    		
   		
    		boolean isCloudExist = false;
    		
    		for(Layer lyr : lyrList){			
    			List<AbstractDrawableComponent> list = lyr.getDrawables();
    			
    			if( list != null ){    				
	    			for(AbstractDrawableComponent adc : list){
	    				//a Volcano exists
	    				if(adc != null && VaaInfo.PGEN_TYPE_VOLCANO.equals(adc.getPgenType()) ){
	    					return true; 					
	    				}    				
	    				
	    				if(adc != null && VaaInfo.PGEN_TYEP_CLOUD.equals(adc.getPgenType()) ){
	    					isCloudExist = true; 					
	    				}
	    			}  		
    			}
    		}		
    		
    		/*
    		 * if Volcano is deleted but Ash Clouds still remain
    		 * then user is informed so clouds can be deleted or 
    		 * reused. 
    		 */
    		if( isCloudExist ){
    			openConfirmBox( "Not Deleted Ash Clouds of the Deleted Volcano Should Be\n"+
    							"Deleted Or Will Belong to the Newly Created Volcano!\n");
    		}
    		//NO Volcano found, this Product can be re-used
    		return false;
    	}else{
    		return false;
    	}
    	
    	
    }
    
    /**
     * informing the user to use a new Product Center
     * @param msg TODO
     */
    
    private void openConfirmBox(String msg){
    	MessageDialog confirmDlg = new MessageDialog( 
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
				"Message", null, msg,
				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
		
		confirmDlg.open();
    }

	@Override
	public IInputHandler getMouseHandler() {
		return null;
	}


}
