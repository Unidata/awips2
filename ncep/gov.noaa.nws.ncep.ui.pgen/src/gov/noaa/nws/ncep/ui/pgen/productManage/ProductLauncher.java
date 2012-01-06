/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.LaunchPgenProduct
 * 
 * Sept 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productManage;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;
import gov.noaa.nws.ncep.ui.pgen.productTypes.PgenLayer;
import gov.noaa.nws.ncep.ui.pgen.productTypes.PgenSave;
import gov.noaa.nws.ncep.ui.pgen.productTypes.ProductType;
import gov.noaa.nws.ncep.ui.pgen.productTypes.ProductTypes;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

/**
 * 
 * Contribution items added to the PGEN product "Launch" drop-down menu which display 
 * the product types available for launching a product template. The product types are 
 * configured and saved in "productTypes.xml".
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/2010  	151    		J. Wu		Initial Creation
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 1
 */
public class ProductLauncher extends ContributionItem {
	
    protected ProductTypes prdTypes = null;
	
	public ProductLauncher() {
	}
 
	public ProductLauncher(String id) {
		super( id ) ;
	}
 
	/**
	 * Fill the menu with product types
	 */
	@Override
	public void fill(Menu menu, int index) {
        
    	// Load the product types in "productTypes.xml".
  	    prdTypes = ProductConfigureDialog.loadProductTypes();

		//Create the menu item with all types
        int ii = 0;
        for ( ProductType ptyp : prdTypes.getProductType() ) {
        
            MenuItem menuItem = new MenuItem( menu, SWT.PUSH, ii );
    		menuItem.setText( ptyp.getName() );
    		menuItem.addSelectionListener(new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				/*
    				 * Create an active product with an active layer and add to the
    				 * product List .
    				 */	
   			         quickLaunch( ((MenuItem)e.widget).getText() );
    			}
    		});
    		
    		ii++;
        }      
	}
	
	/*
	 * Quick launch a product template for a given product type
	 */
	private void quickLaunch( String prdtype ) {

		/*
		 * Create a product list with one product with layers defined in the 
		 * product type.  If no layers specified for the type, a default layer
		 * is created. The names of the product and the default layer are set 
		 * to the product type's name.
		 */
		List<Product> productList = new ArrayList<Product>();
		
  	    Product prd = new Product( prdtype, "Default", "Default",
		                           new ProductInfo(), new ProductTime(), 
		                           new ArrayList<Layer>() );
	    prd.setType(  prdtype );
	    PgenSave psave = getPgenSave( prdtype );
	    if ( psave != null ) {
	    	prd.setOutputFile( psave.getOutputFile() );
	    }
        
	    List<PgenLayer> players = getPgenLayers( prdtype );

	    if ( players != null ) {
	        for ( PgenLayer plyr : players ) {
		        Layer lyr= new Layer();
		        lyr.setName( plyr.getName() );
				lyr.setOnOff( plyr.isOnOff() );
				lyr.setMonoColor( plyr.isMonoColor());
				lyr.setFilled( plyr.isFilled() );
				lyr.setInputFile( plyr.getInputFile());
				lyr.setOutputFile( plyr.getOutputFile() );
				
				Color clr = new Color( plyr.getColor().getRed(),
	                                   plyr.getColor().getGreen(),
	                                   plyr.getColor().getBlue(),
	                                   plyr.getColor().getAlpha() );
			    lyr.setColor( clr );
	
		        prd.addLayer( lyr );	 	    	        	
	        }
	    }	    
	    else {	    	
	        Layer lyr= new Layer();
	        lyr.setName( prdtype );
	        prd.addLayer( lyr );	 	    
	    }
	      	 
	    productList.add( prd );
  	 
		/*
		 * Add this product list to the PgenResource to launch the product template
		 */
	    PgenSession.getInstance().getPgenResource().replaceProduct( productList );     

	}
	
	/**
     *  Retrieve the layers defined in a product type
     */    
    private List<PgenLayer> getPgenLayers( String prdtypName ) {
	    
    	List<PgenLayer> players = null;
 
        ProductType curPrdType = null;
    	for ( ProductType ptyp : prdTypes.getProductType() ) {
        	if ( prdtypName.equals( ptyp.getName() ) ) {
        		curPrdType = ptyp;
        		break;
        	}        	
        }
	    
    	if ( curPrdType != null ) {
	    	players = curPrdType.getPgenLayer();
	    	if ( players.size() <= 0 ) {
	    		players = null;
	    	}
	    }
	    	    	   	
    	return players;

    }
	
    /**
     *  Retrieve the PgenSave defined in a product type
     */    
    private PgenSave getPgenSave( String prdtypName ) {
	    
    	PgenSave pSave = null;
 
        ProductType curPrdType = null;
    	for ( ProductType ptyp : prdTypes.getProductType() ) {
        	if ( prdtypName.equals( ptyp.getName() ) ) {
        		curPrdType = ptyp;
        		break;
        	}        	
        }
	    
    	if ( curPrdType != null ) {
	    	pSave = curPrdType.getPgenSave();
	    }
	    	    	   	
    	return pSave;

    }

}
