/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductNameDialog
 * 
 * September 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.producttypes.PgenLayer;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/**
 * This class allows the user to edit a product's attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09  		#151      	J. Wu 		Initial creation. 
 * 01/11  		#151      	J. Wu 		updated to show info in a product type. 
 * 09/11  		#335      	J. Wu 		updated with new activity type definition. 
 * 09/11  		#335      	J. Wu 		made cascading menu for activity type/subtype. 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class ProductNameDialog extends ProductDialog {

    private Text   nameText = null;      
    private Combo  typeCombo = null;     
    private Composite   typeComp = null;     
    private Text   typeText = null;     
    private ToolBar  typeToolBar = null;     
    private Text   forecasterText = null; 
    private Text   centerText = null; 
    private Button saveLayerBtn = null;
    private Text   outputFileTxt = null; 
    private Group  layersGrp = null;  
       
	private String initialOutput = null;
    
    private ProductManageDialog prdManageDlg = null;
    
    
	/**
     * Constructor.
     */
	public ProductNameDialog( Shell parentShell, ProductManageDialog dlg ) {
		
		super( parentShell );		
		
		prdManageDlg = dlg;
			
	}
    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Edit Product" );        
    }
    
    /**
     *  Creates the main layout for the shell.
     */
    public void setLayout() {
        
        GridLayout mainLayout = new GridLayout( 1, true );
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout( mainLayout );

    }
    
    
    /**
     *  Set the default location.
     * @param parent
     */
    public void setDefaultLocation( Shell parent ) {

    	if ( shellLocation == null ) {
	        Point pt = parent.getLocation();
	        shell.setLocation( pt.x + 420,  pt.y + 200 );
		} else {
			shell.setLocation(shellLocation);
		}

    }
    

    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
		
//    	initialOutput = prdManageDlg.getPrdOutputFile( prdManageDlg.getActiveProduct() );
      	initialOutput = prdManageDlg.getActiveProduct().getOutputFile();
       	
    	Composite main = new Composite( shell, SWT.NONE );
        GridLayout gl1 = new GridLayout( 1, false );
        main.setLayout( gl1 );
             
    	Composite top = new Composite( main, SWT.NONE );
        GridLayout gl = new GridLayout( 2, false );
        top.setLayout( gl );

        GridData gd = new GridData( SWT.FILL, SWT.DEFAULT, true, false );
        
        Label pname = new Label( top, SWT.NONE );
        pname.setText( "Name:");
        
        nameText = new Text( top, SWT.SINGLE | SWT.BORDER );                        
        nameText.setLayoutData( new GridData( 100, 20 ) );
        nameText.setEditable( true );   
        nameText.setText( prdManageDlg.getActiveProduct().getName() );

        Label ptype = new Label( top, SWT.NONE );
        ptype.setText( "Type:");
        
        String curType = prdManageDlg.getActiveProduct().getType();
        
    	typeComp = new Composite( top, SWT.LEFT );    	    	    	    	
    	typeComp.setLayout( new GridLayout( 2, false ) );
    	
   	    typeText = new Text( typeComp, SWT.LEFT | SWT.BORDER );	
   	    typeText.setSize( 300, 20 );
   	    typeText.setText( curType );
   	    typeText.setData( curType );
   	    typeText.setEditable( false );
        
    	typeToolBar = new ToolBar( typeComp, SWT.HORIZONTAL );
    	final ToolItem ti = new ToolItem( typeToolBar, SWT.DROP_DOWN );
    	
    	ti.setEnabled( true );
    	
    	final Menu mu = new Menu( shell.getShell(), SWT.POP_UP );
    	
    	MenuItem mi1 = new MenuItem( mu, SWT.PUSH, 0 );
		mi1.setText( "Default" );
       	mi1.setData( "Default" );
   		mi1.addSelectionListener(new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				typeText.setText( ((MenuItem)e.widget).getData().toString() );
  				typeText.pack();
 				typeComp.pack();
   				typeComp.getParent().pack();
            	switchProductType( "Default" );
   			}
   		});
        
		int ntyp = 1;
        ArrayList<String> typeUsed = new ArrayList<String>();
		for ( String ptypName : prdManageDlg.prdTypesMap.keySet() ) {
    		
            ProductType prdType = prdManageDlg.prdTypesMap.get( ptypName );
            LinkedHashMap<String, String>  subtypesNalias = prdManageDlg.getSubtypes( prdType.getType(), true );
            
            if ( (ptypName.equals( prdType.getName() ) && 
               	 !prdType.getType().equals( prdType.getName() ) ) ||
               	 !prdManageDlg.hasSubtypes( subtypesNalias.values() ) ) {
                   
                MenuItem typeItem = new MenuItem( mu, SWT.PUSH, ntyp );
                   
               	typeItem.setText( ptypName );
               	typeItem.setData( ptypName );
           		typeItem.addSelectionListener(new SelectionAdapter() {
           			public void widgetSelected(SelectionEvent e) {
           				String typeName = ((MenuItem)e.widget).getData().toString();
    			    	typeText.setText( typeName );
          				typeText.pack();
          				typeComp.pack();
           				typeComp.getParent().pack();
   	            	    switchProductType( typeName );
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
 
            	MenuItem typeItem = new MenuItem( mu, SWT.CASCADE, ntyp );
            	
            	typeItem.setText( prdType.getType() );
        		Menu submenu = new Menu( typeItem );
        		typeItem.setMenu( submenu );
        		
        		for ( String styp : subtypesNalias.keySet() ) {
            	    MenuItem subtypeItem = new MenuItem( submenu, SWT.PUSH );
            	    subtypeItem.setText( subtypesNalias.get( styp ) );
            	    
            	    subtypeItem.setData( styp );

       		        subtypeItem.addSelectionListener( new SelectionAdapter() {
        			    public void widgetSelected(SelectionEvent e) {
               				String typeName = ((MenuItem)e.widget).getData().toString();
        			    	typeText.setText( typeName );
               				typeText.pack();
               				typeComp.pack();
               				typeComp.getParent().pack();
       	            	    
               				switchProductType( typeName);
        			    }
        		    });       			
        		}
            }
               		
    		ntyp++;
        }      
            		    	
    	ti.addListener(SWT.Selection, new Listener() {
        	public void handleEvent(Event event) {
        		Rectangle bounds = ti.getBounds();
        		Point point = typeToolBar.toDisplay( bounds.x, bounds.y + bounds.height );
        		mu.setLocation( point );
        		mu.setVisible( true );
        	}
        });
    	
        Label pfst = new Label( top, SWT.NONE );
        pfst.setText( "Forecaster:");
        
        forecasterText = new Text( top, SWT.SINGLE | SWT.BORDER );                        
        forecasterText.setLayoutData( new GridData( 100, 20 ) );
        forecasterText.setEditable( true );   
        forecasterText.setText( prdManageDlg.getActiveProduct().getForecaster() );
       
        Label pcnt = new Label( top, SWT.NONE );
        pcnt.setText( "Center:");
        
        centerText = new Text( top, SWT.SINGLE | SWT.BORDER );                        
        centerText.setLayoutData( new GridData( 100, 20 ) );
        centerText.setEditable( true );   
        centerText.setText( prdManageDlg.getActiveProduct().getCenter() );
       	       
        Group typeInfoGrp = new Group( main, SWT.SHADOW_IN );       
        typeInfoGrp.setLayout( new GridLayout( 1, false) );
        typeInfoGrp.setText( "Product Type Information" );
    	
        Composite bot1 = new Composite( typeInfoGrp, SWT.NONE );
        bot1.setLayout( new GridLayout( 2, false) );
        
        Composite bot2 = new Composite( typeInfoGrp, SWT.NONE );
        bot2.setLayout( new GridLayout( 3, false) );
     
        Label psave = new Label( bot1, SWT.NONE );
        psave.setText( "Save Layers:");
        
        saveLayerBtn = new Button( bot1, SWT.CHECK ); 
        saveLayerBtn.setSelection( prdManageDlg.getActiveProduct().isSaveLayers() );
        
        //Create a composite for output file name        
        Label outputLbl = new Label( bot2, SWT.LEFT );
        outputLbl.setText("Output:");
              
        outputFileTxt = new Text( bot2,  SWT.SINGLE | SWT.BORDER );                        
        outputFileTxt.setLayoutData( new GridData( 150, 20 ) );
        outputFileTxt.setEditable( true );   

        if ( initialOutput != null ) {
        	outputFileTxt.setText( initialOutput  );
        }
        else {
        	outputFileTxt.setText( "" );
        }

        Button browseBtn = new Button( bot2, SWT.PUSH );
        browseBtn.setText( "Browse");
 
        layersGrp = new Group( typeInfoGrp, SWT.NONE );
        layersGrp.setLayout( new GridLayout( 1, false) );
        layersGrp.setText( "Layers Defined");
        
        String prevType = prdManageDlg.getActiveProduct().getType();
        createLayersTemplate( layersGrp , prevType );
        
        browseBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {            	
               	createFileText( outputFileTxt, initialOutput );
            }
        });  
        
        browseBtn.setEnabled( false );

        // Create control buttons
        Composite centeredComp = new Composite( shell, SWT.NONE );
        GridLayout gl2 = new GridLayout( 2, true );
        centeredComp.setLayout( gl2 );
        centeredComp.setLayoutData( gd );

        Button acceptBtn = new Button( centeredComp, SWT.NONE );
        acceptBtn.setText( "Accept" );
        acceptBtn.setLayoutData( gd );
        acceptBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {                           	
            	updateProduct();            	
            	close();
            }
        });
        
        Button cancelBtn = new Button( centeredComp, SWT.NONE );
        cancelBtn.setText( "Cancel" );
        cancelBtn.setLayoutData( gd );
        cancelBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	prdManageDlg.setOpenPrdNameDlg( false );
            	close();
            }
        });
           
    }
    

	/*
     *  Update the product attributes in the product control window.
     */
    private void updateProduct() {   	    	
    	
//    	selectProductType();
    	
    	HashMap<String, String> attr = new HashMap<String, String>();
    	
    	attr.put( "name", nameText.getText() );
    	attr.put( "type", typeText.getText() );
    	attr.put( "forecaster", forecasterText.getText() );
    	attr.put( "center", centerText.getText() );    	
    	if (saveLayerBtn.getSelection() ) {
    		attr.put( "saveLayers", "true" );
    	}
    	else {
    		attr.put( "saveLayers", "false" );  		
    	}
        attr.put( "outputfile", outputFileTxt.getText() );
    	    	
        if ( prdManageDlg != null ) {
            prdManageDlg.updateProductAttr( attr );
    	}
        
    } 
	
    
    /*
     *  Select a product type with confirmation.
     */
    private void selectProductType() {   	    	
    	
    	if ( !(typeCombo.getText().equals( prdManageDlg.getActiveProduct().getType() ) ) ) {
/*        	
        	MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Confirm type", null, 
            		"Are you sure you want to switch from type " +  
            		prdManageDlg.getActiveProduct().getType()
            		 + " to type " + typeCombo.getText() + "?",
            		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
            
        	confirmDlg.open();
            
            if ( confirmDlg.getReturnCode() != MessageDialog.OK ) {              		     
            	typeCombo.select( curType );    
            }
*/        	
            
//    		typeCombo.select( curType );

        }
    } 
    
	/**
	 * Create a file/path input dialog with a Text and a "Browse" button.
	 * 
	 * @param txt
	 * @return 
	 */
	private void createFileText( Text txt, String initialFile ) {
		
  	    String[] filterNames = new String[] { "*.xml", "All Files (*)" };
        String[] filterExtensions = new String[] {"*.xml", "*" };
//   	    String filterPath = PgenUtil.CURRENT_WORKING_DIRECTORY;
   	    String filterPath = PgenUtil.getWorkingDirectory();
   	    String defaultFile = new String( "default.xml" );
   	    
   	    if ( initialFile != null ) {
   	    	int index = initialFile.lastIndexOf('/');
   	    	if ( index >= 0 ) {
   	    	    defaultFile = initialFile.substring( index+1, initialFile.length() );
   	    	    filterPath = initialFile.substring( 0, index);
   	    	}
   	    	else {
   	    		defaultFile = new String( initialFile );
   	    	}
   	    }
   	    
    	String selectedFile = selectFile( shell, SWT.OPEN, filterNames,
    			    filterExtensions, filterPath, defaultFile, true );
    	if ( selectedFile != null ) {
    	    txt.setText( selectedFile );
    	}           	
       
	}
    
	/**
	 * Create a file selection dialog
	 * 
	 * @param sh
	 * @param mode
	 * @param nameFilter
	 * @param extensionFilter
	 * @param pathFilter
	 * @param defaultFile
	 * @param overWrite
	 * @return dialog.open()
	 */
	private String selectFile( Shell sh, int mode, String[] nameFilter,
								String[] extensionFilter, String pathFilter,
								String defaultFile, boolean overWrite ) {
		
        FileDialog dialog = new FileDialog( sh, mode );
        dialog.setFilterNames( nameFilter );
        dialog.setFilterExtensions( extensionFilter );
        dialog.setFilterPath( pathFilter );
        if ( defaultFile != null ) {
            dialog.setFileName ( defaultFile );
        }
        dialog.setOverwrite( overWrite );               
                
        return dialog.open();
            	               
	}
	
    /*
     *  Create name, on/off, and color mode buttons for defined layers
     */
    private void createLayersTemplate( Composite cmp, String typ ) {
        
        Control[] wids = cmp.getChildren();      	   
 	    for ( int jj = 0; jj < wids.length; jj++ ) {
    	    wids[jj].dispose();
    	}
 	    
 	    cmp.pack(); 
    	shell.pack( true );
 	    
 	    ProductType prdtype = prdManageDlg.prdTypesMap.get( typ );
        if ( prdtype == null || prdtype.getPgenLayer() == null || 
        		                prdtype.getPgenLayer().size() <= 0	) {
        	return;
        }
                       
    	Composite layersComp = new Composite( cmp, SWT.NONE );
    	GridLayout gl = new GridLayout( 3, false );
           	    
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        gl.verticalSpacing = 1;
        gl.horizontalSpacing = 10;

        layersComp.setLayout( gl );
    	       
    	for ( PgenLayer lyr : prdtype.getPgenLayer() ) {
	     	
    	    Button nameBtn = new Button( layersComp, SWT.PUSH );
    	    nameBtn.setText( lyr.getName() );
    	    	
    	    Button dispBtn = new Button( layersComp, SWT.CHECK );
    	    dispBtn.setSelection( lyr.isOnOff() );     	          
    	    dispBtn.setEnabled( false );     	          
			        
    	    Button  clrBtn = new Button( layersComp, SWT.PUSH );
    	    clrBtn.setText( "A/F" ); 
    	    
			Color clr = new Color( lyr.getColor().getRed(),
                    lyr.getColor().getGreen(),
                    lyr.getColor().getBlue(),
                    lyr.getColor().getAlpha() );

        	setButtonColor( clrBtn, clr );
   	    }
	    
    	cmp.pack();
    	shell.pack( true ); 	    		    		
    }
    
    
    /*
     *  Switch to a new type
     */
    private void switchProductType( String newType ) {       

        createLayersTemplate( layersGrp, newType ); 
        
        nameText.setText( newType );
        
        saveLayerBtn.setSelection( false );
        outputFileTxt.setText( ""  );
        outputFileTxt.setEditable( false );

        ProductType ptyp = prdManageDlg.prdTypesMap.get( newType );
        if ( ptyp != null ) {
        	if ( ptyp.getPgenSave() != null ) {            		    	
        		if ( ptyp.getPgenSave().getOutputFile() != null ) {               		    	
        			outputFileTxt.setText( ptyp.getPgenSave().getOutputFile()  );
        		}
        		if ( ptyp.getPgenSave().isSaveLayers() != null ) {
        			saveLayerBtn.setSelection( ptyp.getPgenSave().isSaveLayers() );
        		}
        	}
        }
	
    }

      
}


