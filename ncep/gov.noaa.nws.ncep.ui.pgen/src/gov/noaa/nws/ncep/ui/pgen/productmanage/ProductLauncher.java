/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.LaunchPgenProduct
 * 
 * Sept 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

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
import gov.noaa.nws.ncep.ui.pgen.producttypes.PgenLayer;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
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
 * 09/2010  	#151    	J. Wu		Initial Creation
 * 09/2011  	#335    	J. Wu		Added type/subtype/alias for 
 * 										PGEN activities.
 * 09/2011		#335      	J. Wu 		made cascading menu for activity type/subtype. 
 * 08/2012  	#?    	    J. Wu		Add the activity to the existing ones instead of "Replace"
 * 11/13		#1049		B. Yin		Handle outlook type defined in layer.
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 1
 */
public class ProductLauncher extends ContributionItem {
	
    protected LinkedHashMap<String, ProductType> prdTypeMap = null;
	
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
		prdTypeMap = ProductConfigureDialog.getProductTypes();
        ArrayList<String> typeUsed = new ArrayList<String>();
		
		//Create the menu item with all types
        int ii = 0;
        for ( String ptyp : prdTypeMap.keySet() ) {
            		
            ProductType prdType = prdTypeMap.get( ptyp );
            LinkedHashMap<String, String>  subtypesNalias = getSubtypes( prdType.getType(), true );
            
            if ( (ptyp.equals( prdType.getName() ) && 
            	 !prdType.getType().equals( prdType.getName() )) || 
            	 !hasSubtypes( subtypesNalias.values() ) ) {
                
            	MenuItem typeItem = new MenuItem( menu, SWT.PUSH, ii );
                
            	typeItem.setText( ptyp );
        		typeItem.addSelectionListener(new SelectionAdapter() {
        			public void widgetSelected(SelectionEvent e) {
        				/*
        				 * Create an active product with an active layer and add to the
        				 * product List .
        				 */	
       			         quickLaunch( ((MenuItem)e.widget).getText() );
        			}
        		});

            }            
            else {
                
            	if ( typeUsed.contains( prdType.getType() ) ) {
           		    continue;
            	}
            	else {
            		typeUsed.add( prdType.getType() );                	 
           	    }
 
            	MenuItem typeItem = new MenuItem( menu, SWT.CASCADE, ii );
            	
            	typeItem.setText( prdType.getType() );
        		Menu submenu = new Menu( typeItem );
        		typeItem.setMenu( submenu );
        		
        		for ( String styp : subtypesNalias.keySet() ) {
            	    MenuItem subtypeItem = new MenuItem( submenu, SWT.PUSH );
            	    subtypeItem.setText( subtypesNalias.get( styp ) );
            	    
            	    subtypeItem.setData( styp );

       		        subtypeItem.addSelectionListener( new SelectionAdapter() {
        			    public void widgetSelected(SelectionEvent e) {
        			    	/*
        			    	 * Create an active product with an active layer and add to the
        			    	 * product List .
        			    	 */	
        			    	quickLaunch( ((MenuItem)e.widget).getData().toString() );
        			    }
        		    });       			
        		}
            }
               		
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
	    prd.setOnOff( false ) ;
	    prd.setOutputFile( null );
        
	    List<PgenLayer> players = getPgenLayers( prdTypeMap.get( prdtype ) );

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
			    lyr.setMetaInfo( plyr.getMetaInfo() );
	
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
	    List<Product>  curPrdList = PgenSession.getInstance().getPgenResource().getProducts();
	    prd.setName( findUniqueActivityName( curPrdList, prd.getName() ) );
	    PgenSession.getInstance().getPgenResource().addProduct( productList );		    	
	    
	}
	
	/**
     *  Retrieve the layers defined in a product type
     */    
    private List<PgenLayer> getPgenLayers( ProductType ptype ) {
	    
    	List<PgenLayer> players = null;
 	    
    	if ( ptype != null ) {
	    	players = ptype.getPgenLayer();
	    	if ( players.size() <= 0 ) {
	    		players = null;
	    	}
	    }
	    	    	   	
    	return players;

    }
	

    /**
     *  Retrieve the all subtypes defined for an activity type
     *  
     *  Subtypes with alias could be excluded.
     *  
     */    
    private LinkedHashMap<String, String> getSubtypes( String ptype, boolean noAlias ) {
	    
    	LinkedHashMap<String, String> stypes = new LinkedHashMap<String, String>();
    	
    	for ( String typeID : prdTypeMap.keySet() ) {
    		ProductType  prdType = prdTypeMap.get( typeID );
    		if ( prdType.getType().equals( ptype ) ) {
    			if ( noAlias ) {
    				if ( prdType.getName() == null || prdType.getName().trim().length() == 0 
    					 || prdType.getName().equals( prdType.getType() ) ) {
        			    stypes.put( typeID, prdType.getSubtype() );
    				}
   			    }
    			else {
    			    stypes.put( typeID, prdType.getSubtype() );
    			}
    		}
    	}
	    	    	   	
    	return stypes;

    }
    
    /**
     *  Check if an activity type has no subtypes (except "None")
     *  
     */    
    private boolean hasSubtypes( Collection<String>  subtypes ) {
    	
    	boolean hasSubtypes = true;
    	if ( subtypes == null || subtypes.size() == 0  ) {
    		hasSubtypes = false;
    	}
    	else if ( subtypes.size() == 1 ) {   	        
        	for ( String st : subtypes ) {
        		if ( st.equalsIgnoreCase( "None" ) ) {
        			hasSubtypes = false;
        			break;
        		}
        	}
    	}
	    	    	   	
    	return hasSubtypes;

    }
    
    
	/*
	 * Build an activity name that is unique in the activity list. 
	 */
	private String findUniqueActivityName( List<Product> prds, String actName ) {
		
		String outName = new String( actName );
		
		int ii = 1;
		while ( !isUniqueActivityName( prds, outName ) ) {
			outName = new String( outName + " " + ii );
			ii++;
		}
		
		return outName;
		
	}

	/*
	 * Check if an activity name is unique in the activity list. 
	 */
	private boolean isUniqueActivityName( List<Product> prds, String actName ) {
		
		boolean isUnique = true;
		
		for ( Product prd : prds ) {
			if ( prd.getName().equalsIgnoreCase( actName ) ) {
				isUnique = false;
				break;
			}
		}
				
		return isUnique;
		
	}

}
