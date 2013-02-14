/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenLayeringControlDialog
 * 
 * July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.layering;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.GfaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;

import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


/**
 * This class controls PGEN layering in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/09	  	#131		J. Wu		Initial creation. 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class PgenLayeringControlDialog extends PgenLayeringDialog {

	/**
     * Default color for the active layer name button.
     */
    private final Color defaultLayerButtonColor = Color.lightGray;
    private final Color activeLayerButtonColor = Color.green;
                       
    /**
     * Layer name edit dialog.
     */
	protected PgenLayeringNameDialog layerNameDlg = null;
	protected PgenLayeringDisplayDialog displayDlg = null;
        
    
	/**
     * List of layers and buttons.
     */
    private ArrayList<Layer>	layerList = null;
    private ArrayList<Button>	layerNameBtns = null;
    private ArrayList<Button> 	displayOnOffBtns = null;
    private ArrayList<Button> 	colorModeBtns = null;
    
    private Button  allOnOffBtn = null;
      
    /**
     * The layer & color mode button in use.
     */
    private int	layerInUse = -1;
    private int colorModeBtnInUse = -1;
    private boolean allOnOff = false;  
    
    /**
     *  Open dialog in compact mode or full mode.
     */
    private Button arrowBtn = null;
    boolean compact = true;
    boolean openNameDialog = false;
    
    /**
     * Constructor.
     */
	public PgenLayeringControlDialog( Shell parentShell ) {		
		super( parentShell );        
	}

    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Layering" );        
    }
    
    
    /**
     *  Set the default location.
     * @param parent
     */
    public void setDefaultLocation( Shell parent ) {

    	if ( shellLocation == null ) {
        Point pt = parent.getLocation();
        shell.setLocation( pt.x + 255,  pt.y + 146 );
    }
		else {
			shell.setLocation(shellLocation);
		}

    }

    /**
     *  Pops up a second dialog - the window to edit the layer name.
     */    
    protected void popupSecondDialog() {       	
        if ( openNameDialog ) {
    	    editLayerName();
        }
    }

    
    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
        
    	// Initialize the product in PgenResource.
    	initialize(); 
    	
	    // Create "Add Layer" button.
        Composite addLayerComp = new Composite( shell, SWT.NONE );
                
        GridLayout gl = new GridLayout( 2, true );
        gl.makeColumnsEqualWidth = false;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        gl.verticalSpacing = 1;
        gl.horizontalSpacing = 1;
        addLayerComp.setLayout( gl );
        
        Button addLayerBtn = new Button( addLayerComp, SWT.NONE );
        addLayerBtn.setText( "Add Layer" );
        addLayerBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addLayer();
            }
        });
        
        // Create "Delete Layer" button.        
        if ( !compact ) {
            Button delLayerBtn = new Button( addLayerComp, SWT.NONE );
            delLayerBtn.setText( "Delete" );
            delLayerBtn.addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    deleteLayer();
                }
            });
        }
        
        addSeparator();
        
    	// Create buttons for layer name, display on/off, and color/fill mode.            				
        createLayers();
        
        addSeparator();
          	
	    // Create "All On" and "Edit Name" buttons.
        Composite centeredComp = new Composite( shell, SWT.NONE );
        centeredComp.setLayout( gl );
        
        allOnOffBtn = new Button( centeredComp, SWT.NONE );
        allOnOffBtn.setText( "All On" );
        allOnOffBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                  updateDisplayChecks();
            }
        });
        
        
        if ( !compact ) {
            
        	Button editNameBtn = new Button( centeredComp, SWT.NONE );
            editNameBtn.setText( "Edit Name" );
            editNameBtn.addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {               
                	editLayerName();
                }
            });
       	
        }

	    // Create "Exit" and expansion arrow buttons.       
        Composite exitComp = new Composite( shell, SWT.NONE );        
        exitComp.setLayout( gl );       
        
        Button exitBtn = new Button( exitComp, SWT.NONE );
        exitBtn.setText( "Exit" );
        exitBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {               
            	exitLayering();
            }
        });
 
        arrowBtn = new Button( exitComp, SWT.NONE );
        
        if ( compact ) {
            arrowBtn.setText( ">>" );         
        }
        else {
        	arrowBtn.setText( "<<" );      
        }
        
        arrowBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                
            	openNameDialog = false;
            	
            	if ( compact ) {
            		arrowBtn.setText( "<<" );
                }
                else {
                    arrowBtn.setText( ">>" );
                }
                
                compact = !compact;
                
                startLayering();
            }
        });
 
    }
    
    
    /*
     *  Create name, on/off, and color mode buttons for one layer
     */
    private void createLayers() {
        
    	Composite layersComp = new Composite( shell, SWT.NONE );
    	GridLayout gl = new GridLayout( 3, false );
        
    	if ( compact ) {
   	    	gl = new GridLayout( 2, false );
   	    }
   	    
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        gl.verticalSpacing = 1;
        gl.horizontalSpacing = 0;

        layersComp.setLayout( gl );
        
        int ii = 0;
    	for ( Layer lyr : layerList ) {
	     	
    	    Button nameBtn = new Button( layersComp, SWT.PUSH );
    	    nameBtn.setText( lyr.getName().replace("&", "&&") );
    	    setButtonColor( nameBtn, defaultLayerButtonColor );	    
    	    nameBtn.setData( ii );
    	    nameBtn.addSelectionListener( new SelectionAdapter() {
    	        public void widgetSelected(SelectionEvent event) {                		
    	        	switchLayer( Integer.parseInt( event.widget.getData().toString() ) );
    	        }
    	    });
    	
    	    layerNameBtns.add( nameBtn );
    	
    	    Button dispBtn = new Button( layersComp, SWT.CHECK );
    	    dispBtn.setSelection( lyr.isOnOff() );     	          
    	    dispBtn.setData( ii );
    	    dispBtn.addSelectionListener( new SelectionAdapter() {
    	        public void widgetSelected(SelectionEvent event) {                            	
    	        	turnOnDisplay( Integer.parseInt( event.widget.getData().toString() ) );
    	        }
    	    });
		
    	    displayOnOffBtns.add ( dispBtn );		
	        
    	    if ( !compact ) {
    	        Button  clrBtn = new Button( layersComp, SWT.PUSH );
    	        if ( lyr.isMonoColor() ) {
    	            clrBtn.setText( "M/F" );
    	        }
    	        else {
    	        	clrBtn.setText( "A/F" );
    	        }
    	        
        	    setButtonColor( clrBtn, lyr.getColor() );
    	        clrBtn.setData( ii );
    	    
    	        clrBtn.addSelectionListener( new SelectionAdapter() {
    	            public void widgetSelected(SelectionEvent event) {   	    	    
    	                    colorModeBtnInUse = Integer.parseInt( event.widget.getData().toString() );
    	                    editDisplayAttr();
    	                }
    	    	
    	            });
      
    	        colorModeBtns.add( clrBtn );
   	        
    	    }
    	    
    	    ii++;
        
    	}
    	
    	if ( layerInUse < 0 || layerInUse >=  layerList.size() ) {
    		layerInUse = layerList.size() - 1;
    	}
    	if ( layerList.contains( drawingLayer.getActiveLayer() ) ) {
    		layerInUse = layerList.indexOf( drawingLayer.getActiveLayer() );
    	}

	    setButtonColor( layerNameBtns.get( layerInUse ), activeLayerButtonColor );			
		displayOnOffBtns.get( layerInUse ).setSelection( true );
	    layerList.get( layerInUse ).setInUse( true );
	    
	    currentLayer = layerList.get( layerInUse );
	    drawingLayer.setActiveLayer( currentLayer );
		
    }
    
    
    /**
     * Initialize the product in the PgenResource.
     */
    public void initialize() {
        
    	if ( currentProduct == null ) {
			
        	currentProduct = new Product("Default", "Default", "Default",
      		      new ProductInfo(), new ProductTime(), new ArrayList<Layer>() );
        	
        	drawingLayer.addProduct( currentProduct );
        	drawingLayer.setActiveProduct( currentProduct );
        }
    	    	
        if ( currentLayer == null ) {
            
        	currentLayer = new Layer();
       	
            currentProduct.addLayer( currentLayer );
            
            drawingLayer.setActiveLayer( currentLayer );                              
        }
        
    	if ( currentLayer.getName().equalsIgnoreCase( "Default" ) )  {
            currentLayer.setName( "Layer_1" );
    	}
        
    	currentLayer.setInUse( true );
        
    	layerList = (ArrayList<Layer>)currentProduct.getLayers();
        
    	layerNameBtns = new ArrayList<Button>();
        displayOnOffBtns =  new ArrayList<Button>();
        colorModeBtns =   new ArrayList<Button>();
         
    }

    
    /*
     *  Update the active layer with a new name;
     */    	
    protected void updateActiveLayerName( String name ) {
    	
    	boolean update = false;
    	
    	/* 
    	 *  Update only if the new layer name is not empty and not
    	 *  the same as the layer names on the current product.
    	 *  Also, the name should not be any variations of "Default".
    	 *  
    	 *  Note: we assume the layer names should be unique within a product.
    	 */
    	if ( name != null && name.length() > 0 ) {
    		update = true;
    		for ( Layer lyr : currentProduct.getLayers() ) {
    			if ( lyr.getName().equals( name ) ) {
    			    update = false;
    				break;	
    			}
    		}
    		
    		if ( name.equalsIgnoreCase( "Default" ) ) {
    			update = false;
    		}
    	}
    	
    	
    	/*
    	 * Rebuilds and opens the layering control dialog since the size
    	 * of the button will change with the length of the new name.
    	 */
    	if ( update ) {
    		
    		layerNameBtns.get( layerInUse ).setText( name.replace("&", "&&") );
    	
    	    if ( layerInUse >= 0 ) {    	    
    		    layerList.get( layerInUse ).setName( name.replace("&", "&&") ); 
    		    currentLayer.setName( name );
    	    }
    	    
            drawingLayer.setActiveLayer( currentLayer );   
            
            openNameDialog = false;

    	    startLayering();
    	}
   	
    }
  
    /**
     *  Update active layer's colorMode, color and fill mode;
     */    	
    protected void updateDisplayAttr( boolean mono, Color clr, boolean fill ) {
    	   	
    	if ( colorModeBtnInUse >= 0 ) {
    		
    		if ( mono ) {
    			colorModeBtns.get( colorModeBtnInUse ).setText( "M/F" );
    		}
    		else {
    			colorModeBtns.get( colorModeBtnInUse ).setText( "A/F" );  			
    		}
    		
    		layerList.get( colorModeBtnInUse ).setMonoColor( mono );
    		layerList.get( colorModeBtnInUse ).setColor( clr );  
    		
    		setButtonColor( colorModeBtns.get( colorModeBtnInUse ), clr );
    		
    		layerList.get( colorModeBtnInUse ).setFilled( fill );    	
    		
    	}
    	  	
    }
 
    /*
     *  Retrieve active layer;
     */    	
    protected Layer getActiveLayer() {
    	  	
    	return currentLayer;
    	  	
    }
 
    /*
     *  Retrieve active layer;
     */    	
    protected Layer getLayerForColorMode() {
    	  	
    	return layerList.get( colorModeBtnInUse );
    	  	
    }
	
    /**
     *  Edit a selected layer's name
     */    
    private void editLayerName() {
       	
        /*
         * Pop up layer name editing window
         */
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if ( layerNameDlg == null) layerNameDlg = new PgenLayeringNameDialog( shell, this );
		
		if (  layerNameDlg != null && !(layerNameDlg.isOpen() ) ) {
			layerNameDlg.open();       
		}

    }

    /**
     *  Edit a selected layer's dispaly attributes ( color and fill mode)
     */    
    private void editDisplayAttr() {      	

        /*
         * Pop up layer name editing window
         */
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if ( displayDlg == null) displayDlg = new PgenLayeringDisplayDialog( shell, this );
		
		if ( (displayDlg.isOpen() ) ) {
			displayDlg.close();
		}
		
		displayDlg.open();

    }
    
 
    /**
     *  Add a new layer.
     */    
    private void addLayer() {        
    	
    	// Set the flag to pop up the layer name editing dialog.
        openNameDialog = true;
        
    	// Construct a unique layer name.
    	int size1 = layerList.size() + 1;   	
    	String name = new String( "Layer_" + size1 );

    	
        for ( int ii = 0; ii < layerList.size(); ii++ ) {
    		if ( name.equals( layerList.get( ii ).getName() ) ) {
    		    name = new String( "Layer_" + (size1++) );
    		    ii = 0;   		    	
    		}
    	}   	    

    	// Create a new layer and set as the new active layer.    	
    	currentLayer = new Layer();       
    	currentLayer.setName( name );
        
        drawingLayer.setActiveLayer( currentLayer );      
        currentProduct.addLayer( currentLayer );
        
        layerList = (ArrayList<Layer>)currentProduct.getLayers();
        
        layerInUse = layerList.size() - 1;
                        
    	// Re-open the layering control dialog.    	
        startLayering();         
                
    }
    
    /**
     * Switch to a given layer (used for switching from GFA hazard type).
     */
    public void switchLayer( String newLayer ) {
    	String clayer = layerList.get( layerInUse ).getName();
    	int which = -1;
    	
    	if ( !newLayer.equals( clayer ) ) {
    		for ( int ii = 0; ii < layerNameBtns.size(); ii++ ) {
    			if ( layerNameBtns.get(ii).getText().replace("&&", "&").equals( newLayer ) ) {
    				which = ii;
    				break;
    			}
    		}
    		
    		if ( which >= 0 ) {
    			switchLayer( which );
    		}
    	}
    	
    }

        
    /**
     *  Switch between layer.
     */    
   private void switchLayer( int which ) {
    
	   	// Switch the color for the active layers   
	    setButtonColor( layerNameBtns.get( layerInUse ), defaultLayerButtonColor );			    			
		
	    layerInUse = which;
	    
	    setButtonColor( layerNameBtns.get( layerInUse ), activeLayerButtonColor );			    			
	   	
	    // Turn the display for the layer   	    
//	    displayOnOffBtns.get( layerInUse ).setSelection( true );
		layerList.get( layerInUse ).setInUse( true );
//		layerList.get( layerInUse ).setOnOff( true );
		
		if ( layerNameDlg != null ) layerNameDlg.close();
		if ( displayDlg != null ) displayDlg.close();
		openNameDialog = false;
		
		currentLayer = layerList.get( layerInUse );
		
//    	currentLayer.setOnOff( true );
		
        drawingLayer.setActiveLayer( currentLayer );
        
        drawingLayer.removeGhostLine();
        
        if ( GfaAttrDlg.getInstance( this.getParent() ).isGfaOpen() ) {
        	if ( drawingLayer.getSelectedDE() != null ) {
        		GfaAttrDlg.getInstance( this.getParent() ).close();
        	}
        	else {
        	    GfaAttrDlg.getInstance( this.getParent() ).switchHazard( currentLayer.getName() );
        	}
        }
        else {
        	PgenUtil.setSelectingMode();
        }
        
        drawingLayer.removeSelected();
	    
        // Reset undo/redo and refresh   	       	
        PgenSession.getInstance().disableUndoRedo();
        
        PgenUtil.refresh();
        
    }
   
    /**
     *  Turn on/off the display check button for a layer.
     */    
    private void turnOnDisplay( int which ) {   		    
   		
    	if ( which == layerInUse ) {
    		displayOnOffBtns.get( which ).setSelection( true );	
    	}
	    
    	layerList.get( which ).setOnOff( displayOnOffBtns.get( which ).getSelection() );
		
    	PgenUtil.refresh();
    }

      
    /**
      *  Toggle the display on/off for all check buttons.
      */    
    private void updateDisplayChecks() {
		
        if ( allOnOff ) {
			allOnOff = false;        			
	        allOnOffBtn.setText( "All On" );
	    }
		else {
			allOnOff = true;
	        allOnOffBtn.setText( "All Off" );
	    }
		        		        		
    	for ( int ii = 0; ii < layerList.size(); ii++ ) {
			displayOnOffBtns.get( ii ).setSelection( allOnOff );
		    layerList.get( ii ).setOnOff( allOnOff );
    	}
    	
    	/*
    	 *  Always turn on the display for the active layer.
    	 */
//    	displayOnOffBtns.get( layerInUse ).setSelection( true );
//    	layerList.get( layerInUse ).setOnOff( true );
//    	currentLayer.setOnOff( true );

    	PgenUtil.refresh();
    
    }
    
        
    /**
     *  Return the name of the layer on which the color mode button is clicked.
     */    
    protected String getColorModeLayerName() {
		
    	if ( layerList != null && colorModeBtnInUse >= 0 ) {
			return layerList.get( colorModeBtnInUse ).getName();
		}
    	
		return null;
    }
    
    
    /**
     *  Exit layering.
     */    
    private void exitLayering() {
    	
    	/*
    	 * Create a new Default layer & move all DEs onto this layer.
    	 */
    	currentLayer = new Layer();
    	    	
    	for ( int ii = 0; ii < currentProduct.getLayers().size(); ii++ ) {
    		currentLayer.add( currentProduct.getLayer( ii ).getDrawables() );     		    		
    	}
    	
    	/*
    	 * Clear out the active product & add the new layer into it.
    	 */   	
    	currentProduct.clear();
		
    	currentProduct.addLayer( currentLayer );
    	       	
    	/*
    	 * Set the new layer & product as the active in the PgenResource.
    	 */   	
      	drawingLayer.setActiveLayer( currentLayer );
       	drawingLayer.setActiveProduct( currentProduct );
      	    	
       	layerList = (ArrayList<Layer>)currentProduct.getLayers();       
    	
    	/*
    	 * Refresh display and reset undo/redo as well.
    	 */   	
       	PgenUtil.refresh();
       	//PgenUtil.resetUndoRedo();
       	PgenSession.getInstance().disableUndoRedo();   	
       	
       	/*
    	 * Dispose all layering dialogs.
    	 */   	     	
    	if ( layerNameDlg != null ) layerNameDlg.close();       
        if ( displayDlg != null )   displayDlg.close();
    	
        close();
//        shell.dispose();
		        
    }
    
    /**
     *  Add a separator.
     */
    private void addSeparator() { 
        GridData gd = new GridData( GridData.FILL_HORIZONTAL );
        Label sepLbl = new Label( shell, SWT.SEPARATOR | SWT.HORIZONTAL );
        sepLbl.setLayoutData( gd );
    }

    /**
     *  Delete the current layer.
     */    
    private void deleteLayer() {
        
    	// Set the flag - do not pop up the layer name editing dialog.
    	openNameDialog = false;
    	
    	// Remove the current layer & set the next layer as the active  	
    	if ( currentProduct.getLayers().size() > 1 ) {
            currentProduct.removeLayer( layerInUse );
    	}
    	
    	if ( layerInUse >= currentProduct.getLayers().size() ) {
    		layerInUse--; 
    	}
    	
    	currentLayer = currentProduct.getLayer( layerInUse );
    	
    	currentLayer.setOnOff( true );
                
        drawingLayer.setActiveLayer( currentLayer );      
        
        layerList = (ArrayList<Layer>)currentProduct.getLayers();
    	
        // Re-open the layering dialog.                      
        startLayering(); 
        
        PgenUtil.refresh();
                     
    }
    
    /**
     *  Open the dialog.
     */    
    private void startLayering() {
         
    	 // Close the dialog first.
    	 if ( isOpen() ) {
        	 shell.dispose();
         }
     	  
    	 // Close other dialogs as well.
         if ( layerNameDlg != null && layerNameDlg.isOpen() ) {
         	layerNameDlg.close();
         	layerNameDlg = null;
         }
         
    	 // Build & open the dialog.
         open();        
         
    }
    
    /*
     *  Clean up before close the shell - default is to do nothing. 
     */
    protected void exit() { 
    	exitLayering();
    }  
   
}
