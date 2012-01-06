/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductConfigureDialog
 * 
 * Sept 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productManage;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.PgenPreferences;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.Activator;

import gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;

import gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.productTypes.*;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;

/**
 * This class allows the user to configure and edit the product types.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09  		#151      	J. Wu 		Initial creation. 
 * 08/10  		#215      	J. Wu 		Added more potential options. 
 * 10/10  		#215      	J. Wu 		Implemented Layer and Save tab. 
 * 12/10		#359		B. Yin		Added Pgen settings tab
 * 01/11		?			B. Yin		Modified Product tab
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class ProductConfigureDialog extends ProductDialog {
	
	private static ProductConfigureDialog INSTANCE = null;
    
    private final String[] cntlBtnNames = { "Ok", "Add", "Delete", "Close" };
    private final String[] cntlBtnTips = { "Save changes and exit", "Apply changes and continue", 
    		                               "Delete current product type", "Ignore changes and exit" };
    private final String[] typeTabNames = { "Palette", "Settings", "Layer", "Save", "Filter", 
    										"Share", "Clip", "Products" };
	
    private HashMap<String, IConfigurationElement> itemMap = null;  // map of PGEN configuration elements
	
	private static ArrayList<String> controls = null;
	private static ArrayList<String> actions = null;
	private static ArrayList<String> classes = null;
	private static ArrayList<String> objects = null;
	private HashMap<String, Image> iconMap = null;    		  // map of all buttons on palette
	private HashMap<String, Image> iconMapSelected = null;    // map of all buttons if selected
	
	private final int numColumns = 8;
	private final int bgcolor = (0 * 65536 ) + ( 0 * 256 ) + 255;
	private final int fgcolor = (255 * 65536 ) + ( 255 * 256 ) + 255;
	
	private ProductTypes prdTyps = null;
	private List<ProdType> ptList = null; 
	
    private static String prdTypesTblLocal = System.getProperty("user.home");   
    private static String prdTypesTblFileName = "productTypes.xml";
    
    private Composite topComp = null;
    
    private TabFolder tabFolder = null;
    private Composite[] tabComposites = null;
    
    private Composite paletteComp = null;
    private Composite typeComp = null;
    private Group controlGroup = null;
    private Group actionGroup = null;
    private Group classGroup = null;   
    private Group objectGroup = null;
    private Composite controlBtnComp = null;      
    
    private Combo typeCombo = null;
    private Text typeText = null;
	
    private Button[] controlBtns = null;
    private Button[] actionBtns = null;
    private Button[] classChkBtns = null;       
    private Button[] classPushBtns = null;       
    private Button[] objectBtns = null; 
    
    private Button[] dlgCntlBtn = null;
    
    private Text shapeFileTxt = null;
    
    /*
     * Layer attributes
     */
    private Combo layerNameCombo = null;
    private Text layerNameTxt = null;
    private Button layerOnOffBtn = null;
    private Button layerMonoBtn = null;
    private ColorButtonSelector layerColorSelector = null;
    private Button layerFillBtn = null;
//    private Text inputFileTxt = null;
//    private Text outputFileTxt = null;
    private Group layerTempGrp = null;
    private ArrayList<Button> layerNameBtns = null;
    private Text settingsTxt = null;
    
    /*
     * Product "Save" tab attributes
     */
    private static String autoSavePath;
    private static int autoSaveInterval;    
//    private Text saveInputTxt = null;
    private Text saveOutputTxt = null;
    private Button saveIndividualLayerBtn = null;
    private Button autoSaveBtn = null;
    private Text   autoSaveFreqTxt = null;
    
    private Composite pdComp;
    
    /**
     * Default colors for the default and active product of layer name button.
     */
    private final Color defaultButtonColor = Color.lightGray;
    private final Color activeButtonColor = Color.green;
	
    // Filter Hour buttons
	private static String HOURS[] = {"0","0+", "3", "3+", "6","9","12","0-0","3-3","6-6","0-3","0-6","3-6","6-9",
		"6-12","9-12","AIRM","OTLK"};

           
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected ProductConfigureDialog( Shell parShell ) throws VizException {
		
        super( parShell );
		
        retrievePgenPalette();
      
    }
	
	/**
	 * Creates a product configuration dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static ProductConfigureDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new ProductConfigureDialog( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}
		
		return INSTANCE;
		
	}

    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Configure Product Types" );        
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
        Point pt = parent.getLocation();
        shell.setLocation( pt.x + 500,  pt.y + 150 );
    }
    
    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
	    
    	/*
    	 * Load product types
    	 */
    	prdTyps = loadProductTypes();
	    
    	/*
    	 * Find the autoSave directory and frequency in the preference
    	 */
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
		autoSaveInterval = prefs.getInt( PgenPreferences.P_AUTO_FREQ );
		autoSavePath = prefs.getString( PgenPreferences.P_RECOVERY_DIR );

    	/*
    	 * create main composite
    	 */
    	topComp = new Composite( shell, SWT.NONE );
  	    GridLayout gl = new GridLayout( 1, false );
        topComp.setLayout( gl );
        
        
    	/*
    	 * create product type composite
    	 */
    	createTypeComp( topComp  );
             
    	/*
    	 * create tab folders/items
    	 */
    	tabFolder = new TabFolder ( topComp, SWT.BORDER );
    	int ntabs = typeTabNames.length;
    	tabComposites = new Composite[ ntabs ];
    	
    	tabFolder.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {                
                editTabItems( tabFolder.getSelectionIndex() );
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });
    	
    	
    	/*
    	 * create tab folders/items
    	 */
   	   for ( int ii = 0; ii < typeTabNames.length ; ii++ ) {
        	
   		    tabComposites[ ii ] = new Composite( tabFolder, SWT.None );
        	//tabComposites[ ii ].setToolTipText( "item " + ii );
        	
        	TabItem item = new TabItem ( tabFolder, SWT.NONE );
    		item.setText ( typeTabNames[ii] );
    		item.setToolTipText( "item " + ii );
   		
    		if ( ii == 0 ) {
   			    createPaletteTab( tabComposites[ ii ] );
     		}
    		else if ( ii == 1 ) {
   			    createSettingsTab( tabComposites[ ii ] );
    		}
    		else if ( ii == 2 ) {
   			    createLayeringTab( tabComposites[ ii ] );
    		}
    		else if ( ii == 3 ) {
     			createSaveTab( tabComposites[ ii ] );
    		}
    		else if ( ii == 4 ) {
    			createFilterTab( tabComposites[ ii ] );
     		}
    		else if ( ii == 5 ) {
    			createShareTab( tabComposites[ ii ] );
    		}
    		else if ( ii == 6 ) {
    			createClipTab( tabComposites[ ii ] );
    		}
    		else if ( ii == 7 ) {
    			createProductsTab( tabComposites[ ii ] );
    		}
   		    else {
    	    	createDefaultTab( tabComposites[ ii ] );
    		}
		    
    		item.setControl ( tabComposites[ ii ] );
    		    		
    	}
   	 
    	/*
    	 * create control buttons
    	 */   	
    	createControlBtns( topComp );  	
   	    	
    }
    
    /**
     *  Edit each tab item.
     */
    private void editTabItems( int index ) {
            	
        if ( index == 1 ) {
   		    editLayeringTab();
   	    }
        else if ( index == 2 ){
        	editSaveTab();
        }
            	
    }
    
    /**
     *  Create the PGEN palette configuration page.
     */
    private void createPaletteTab( Composite cmp ) {
    	
   	    paletteComp = cmp;
		GridLayout gl = new GridLayout( 1, false );
        paletteComp.setLayout( gl );
        
        controlGroup = new Group(paletteComp, SWT.SHADOW_IN);
        controlGroup.setLayout( new GridLayout( numColumns, false) );
        controlGroup.setText( "Controls" );
 
        int ii = 0;
        for ( String str : controls ) {
            controlBtns[ ii ] = new Button( controlGroup, SWT.TOGGLE );
            controlBtns[ ii ].setData( str );
            controlBtns[ ii ].setImage( iconMap.get( str ) );
            controlBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            controlBtns[ ii ].setSelection( btnAlwaysOn( str ) );
           
            controlBtns[ ii ].addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {                           	
           	        
                	Button btn = (Button)event.widget;
           	        String name = event.widget.getData().toString();           	        
           	        
           	        if ( btnAlwaysOn( name ) ) {
           	        	btn.setSelection( true );
           	        }
           	        
           	        if ( btn.getSelection() ) {                       
           	        	btn.setImage( iconMapSelected.get( name ) );     	        	    	 
      	        	}
      	        	else {
     	        		 btn.setImage( iconMap.get( name ) ); 
      	        	}
           	        
                }
            });

            ii++;
        }
 
        actionGroup = new Group(paletteComp, SWT.SHADOW_IN);
        actionGroup.setLayout( new GridLayout(numColumns, false) );
        actionGroup.setText( "Actions" );
        
        ii = 0;
        for ( String str : actions ) {
         	actionBtns[ ii ] = new Button( actionGroup, SWT.TOGGLE );
       	    actionBtns[ ii ].setData( str );   
            actionBtns[ ii ].setImage( iconMap.get( str ) );
            actionBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            actionBtns[ ii ].setSelection( btnAlwaysOn( str ) );

            actionBtns[ ii ].addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {                           	
           	        
                	Button btn = (Button)event.widget;
           	        String name = event.widget.getData().toString();           	        
          	        
           	        
           	        if ( btnAlwaysOn( name ) ) {
           	        	btn.setSelection( true );
           	        }

           	        if ( btn.getSelection() ) {
                        btn.setImage( iconMapSelected.get( name ) );     	        	    	 
      	        	}
      	        	else {
           	        	btn.setImage( iconMap.get( name ) );      	        	    	 
      	        	}
                }
            });
           
            ii++;
        }
        
        classGroup = new Group( paletteComp, SWT.SHADOW_IN );
        classGroup.setLayout( new GridLayout(numColumns/2 + 1, false) );
        classGroup.setText( "Classes" );
        
        ii = 0;
        for ( String str : classes ) {
            Composite subComp = new Composite( classGroup, SWT.NONE );
            GridLayout gl2 = new GridLayout(2, false);
            gl2.marginRight = 0;
            gl2.horizontalSpacing = 0;
            gl2.verticalSpacing = 0;
            gl2.marginWidth = 0;
            gl2.marginHeight = 0;            
            subComp.setLayout( gl2 );

        	classChkBtns[ ii ] = new Button( subComp, SWT.CHECK );
        	classChkBtns[ ii ].setData( ii );
        	classChkBtns[ ii ].setSize(5, 5);
        	
            classChkBtns[ ii ].setSelection( btnAlwaysOn( str ) );
            
        	classChkBtns[ ii ].addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {                           	
                	
                	int which = Integer.parseInt( event.widget.getData().toString() );
                	String btnName = classPushBtns[ which ].getData().toString();
        	                  	        
           	        if ( btnAlwaysOn( btnName ) ) {
           	        	classChkBtns[ which ].setSelection( true );
           	        }

                	if ( classChkBtns[ which ].getSelection() ) {
                	    classPushBtns[ which ].setEnabled( true );
                	    classPushBtns[ which ].setImage( iconMapSelected.get( btnName ) );
                	}
                	else {
                		classPushBtns[ which ].setEnabled( false );
                	    classPushBtns[ which ].setImage( iconMap.get( btnName ) );
                	    
                	    if ( objectGroup != null && 
                	    	btnName.equals( objectGroup.getData().toString() ) ) {          	
                        	
                	    	createObjects( "None", true );
                	    }
                	    
                	}               	
                }
            });
       	
        	
        	classPushBtns[ ii ] = new Button( subComp, SWT.TOGGLE );
            classPushBtns[ ii ].setImage( iconMap.get( str ) );
            classPushBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            
            classPushBtns[ ii ].setSelection( btnAlwaysOn( str ) );
        	
            classPushBtns[ ii ].setData( str );        	
    		classPushBtns[ ii ].setEnabled( false );
    		
        	classPushBtns[ ii ].addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {                           	
                	createObjects( event.widget.getData().toString(), false ); 	
           	    }
            });
           
            ii++;
        }
                
        refreshPaletteSelections();
 
    } 

    /**
     *  Load the "productTypes.xml" into a ProductTypes instance.
     */
    static public ProductTypes loadProductTypes() {
    	
		/*
		 *  First Try to load from user's local directory; if not found, load from
		 *  the base directory.
		 */
		ProductTypes ptyps = new ProductTypes();
		
		String ptypFileName = new String( prdTypesTblLocal );
//        String 	currentWorkingDir = PgenUtil.CURRENT_WORKING_DIRECTORY;        
        String 	currentWorkingDir = PgenUtil.getWorkingDirectory();        
        if ( currentWorkingDir != null ) {
        	ptypFileName = new String( currentWorkingDir );
        }
		
    	String fullFileName = ptypFileName  + File.separator + prdTypesTblFileName;
		File prdTypesFile = new File( fullFileName );
						
		if ( !( prdTypesFile.exists() && prdTypesFile.isFile() && prdTypesFile.canRead() ) ) {
//			fullFileName = NmapCommon.getProductTypes();
			fullFileName = LocalizationManager.getInstance().getFilename("Pgen_Product_Types");
			prdTypesFile = new File( fullFileName );
		}				
		
		if ( prdTypesFile.exists() && prdTypesFile.isFile() && prdTypesFile.canRead() ) {
     	    ptyps = FileTools.readProductTypes( fullFileName );
		}
		
		return ptyps;
      	
    }

 
    /**
     *  Update the selections based on the product type.
     */
    private void refreshPaletteSelections() { 
        
    	String type = typeCombo.getText();
		ProductType curPrdTyp = getCurrentPrdTyp();
		
        typeText.setText( type );
    	                      
    	if ( type.equalsIgnoreCase( "new" ) ) {   	    	

    	    typeText.setText( "" );
    		if ( dlgCntlBtn[ 1 ] != null && !dlgCntlBtn[ 1 ].isDisposed() ) {
    			dlgCntlBtn[ 1 ].setText( "Add" );
    		}
            
    	    for ( Button btn : controlBtns ) {
            	
    	    	if ( btnAlwaysOn( btn.getData().toString() ) ) {
            		btn.setSelection( true );
                    btn.setImage( iconMapSelected.get( btn.getData().toString() ) );
            	}
            	else {
    	    	    btn.setSelection( false );
                   	btn.setImage( iconMap.get( btn.getData().toString() ) );
            	}
               	
            }
            
            for ( Button btn : actionBtns ) {
            	
            	if ( btnAlwaysOn( btn.getData().toString() ) ) {
           		    btn.setSelection( true );
           		    btn.setImage( iconMapSelected.get( btn.getData().toString() ) );
           	    }
           	    else {
                    btn.setSelection( false );
                   	btn.setImage( iconMap.get( btn.getData().toString() ) );
                }
               	
            }
   	        
            int ij = 0;
            for ( Button btn : classPushBtns ) {
            	
            	if ( btnAlwaysOn( btn.getData().toString() ) ) {
                	classChkBtns[ ij ].setSelection( true );          		    
                	btn.setEnabled( true );
           		    btn.setImage( iconMapSelected.get( btn.getData().toString() ) );
           	    }
            	else {           	
            	    classChkBtns[ ij ].setSelection( false );
            	    btn.setEnabled( false );
            	    btn.setImage( iconMap.get( btn.getData().toString() ) );
            	}
           	
                ij++;
            }
   	    
    	}  
    	else {    		
   		
 /*   		for ( ProductType ptype:prdTyps.getProductType() ) {
    			if ( ptype.getName().equalsIgnoreCase( type ) ) {
    				curPrdTyp = ptype;
    				break;
    			}
    		}
 */ 
    		if ( dlgCntlBtn[ 1 ] != null ) {
    			dlgCntlBtn[ 1 ].setText( "Apply");
    		}
            
    		dlgCntlBtn[ 2 ].setEnabled( true );
   		
     		for ( Button btn : controlBtns ) {
            	String str = btn.getData().toString();
     			if ( btnAlwaysOn( str ) || 
     				curPrdTyp.getPgenControls().getName().contains( str ) ) {
            		btn.setSelection( true );;
                   	btn.setImage( iconMapSelected.get( str ) );
           	    }
            	else {
            		btn.setSelection( false );;
                   	btn.setImage( iconMap.get( str ) );
           	    }
            }
     		
     		for ( Button btn : actionBtns ) {
            	String str = btn.getData().toString();          	
     			if ( btnAlwaysOn( str ) || 
     				curPrdTyp.getPgenActions().getName().contains( str ) ) {
            		btn.setSelection( true );;
                   	btn.setImage( iconMapSelected.get( str ) );
                }
            	else {
           		    btn.setSelection( false );;
                  	btn.setImage( iconMap.get( str ) );
           	    }
            }

     		int ii = 0;
     		for ( Button btn : classPushBtns ) {
				
     			String str = btn.getData().toString(); 
     			classChkBtns[ ii ].setSelection( false );
     			btn.setEnabled( false );
     			btn.setImage( iconMap.get( str ) );
     			
     			for ( PgenClass cls : curPrdTyp.getPgenClass() ) {
     				if ( btnAlwaysOn( str ) || cls.getName().equalsIgnoreCase( str ) ) {
     					classChkBtns[ ii ].setSelection( true );     					
    	     			btn.setEnabled( true ); 
    	     			btn.setImage( iconMapSelected.get( str ) );
     					break;
     				}
     			}
     			     			
                ii++; 
     		}

    	}
    	
    	refreshPgenPalette( curPrdTyp );
 
    }
    
 
    /**
     *  Dialog actions to be performed.
     */
    private void dialogActions( String btnName ) { 
      
    	if ( btnName.equalsIgnoreCase( cntlBtnNames[ 0 ] ) ) {
    		addNewProductType();  		  
    	}
    	else if ( btnName.equalsIgnoreCase( cntlBtnNames[ 1 ] ) ) {  	      
    		applyProductType();  
    	}
    	else if ( btnName.equalsIgnoreCase( cntlBtnNames[ 2 ] ) ) {
    		deleteProductType();
    	}
    	else {   	      
    		objectGroup = null;
    		shell.dispose();
    		PgenSession.getInstance().getPgenPalette().resetPalette( null );
    	}  	  

    }
 
    /**
     *  Add/save a new product type and exit.
     */
    private void addNewProductType() {    	
        
    	if ( typeCombo.getText().equalsIgnoreCase( "new" ) && 
    		( typeText.getText() == null || typeText.getText().trim().length() == 0 ) ) {    			 
    	}
    	else {   	
    	    if ( updateProductTypes() ) {        
                saveProductTypes();
    	    }
    	}
    	
        objectGroup = null;
        shell.dispose();
		PgenSession.getInstance().getPgenPalette().resetPalette( null );              	
        
    }

    /**
     *  Apply changes to a type and save.
     */
    private void applyProductType() {    	
        if ( updateProductTypes() ) {                    	
        	saveProductTypes();
//        	dlgCntlBtn[1].setText( "Apply" );
        } 
        
        refreshPaletteSelections();
        
    }
  
    /**
     *  Delete a type and save.
     */
    private void deleteProductType() { 
    	
    	String type = typeCombo.getText();
    	ProductType delType = null;
    	
    	int typeIdx = -1;
    	int ii = 0;
        for ( ProductType ptype:prdTyps.getProductType() ) {
    		if ( ptype.getName().equalsIgnoreCase( type ) ) {    			
    			delType = ptype;
    			typeIdx = ii;
    			break;
    		}
    		
    		ii++;
    	}
    		
    	
    	if ( delType != null ) {
            
        	MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Confirm Delete a Product Type", null, 
            		"Are you sure you want to delete type " + delType.getName() + "?",
            		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
            confirmDlg.open();
            
            if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
       		    
            	typeCombo.remove( delType.getName());
            	       		       		
            	prdTyps.getProductType().remove( delType );    		
    		    saveProductTypes();
            	
    		    typeCombo.select( Math.min(typeIdx, typeCombo.getItemCount() ) ); 
    		    
    		    switchProductType();
    		    
    		    shell.pack();
    		    shell.layout();
    		       		    
            }
       
    	}
    	
    	typeCombo.pack();
    	typeCombo.getParent().pack();

    	
    } 
    
    
    /**
     *  Save product types.
     */
    private void saveProductTypes() { 

		String ptypFileName = new String( prdTypesTblLocal );
//      String 	currentWorkingDir = PgenUtil.CURRENT_WORKING_DIRECTORY;        
        String 	currentWorkingDir = PgenUtil.getWorkingDirectory();  
        if ( currentWorkingDir != null ) {
        	ptypFileName = new String( currentWorkingDir );
        }
		
    	String fullFileName = ptypFileName  + File.separator + prdTypesTblFileName;
    	
//		fullFileName = LocalizationManager.getInstance().getFilename("Pgen_Product_Types");
 
/*		File getLocalizationFileDirectly( null,
				"pgen",
				String resourceLocalizationLevelStringValue,
				"productTypes.xml" );
*/		
    	FileTools.write( fullFileName, prdTyps );
   	
    } 
  
    
    /**
     *  Update changes to the current product type.
     */
    private boolean updateProductTypes() {
      	    	
    	ProductType pTyp = null; 	
        boolean validNewTypeName = true;
    	
    	String curTypName = typeCombo.getText();
		String inputName = typeText.getText();
				
		validNewTypeName = validatePrdTypName( curTypName, inputName );
		if (  validNewTypeName ) {
																
			int existingAt = -1;
			
			int ii = 0;
			for ( ProductType stype : prdTyps.getProductType() ) {
				if ( inputName.equalsIgnoreCase( stype.getName() ) ) {					
					existingAt = ii;					
					pTyp = stype;										
					break;
				}
				
				ii++;
			}
    	    
			if ( existingAt < 0 ) {
				pTyp = new ProductType();				
			}

    	    pTyp.setName( new String( inputName ) );  
    	    
			if ( getCurrentTabName().equals("Palette") ) {
				updatePalette( pTyp );
			}
			else if ( getCurrentTabName().equals("Layer") ) {
				updateLayers( pTyp );
				if ( existingAt < 0 ) updatePalette( pTyp );
			}
			else if ( getCurrentTabName().equals("Save") ) {
				updateSaveInfo( pTyp );
				if ( existingAt < 0 ) updatePalette( pTyp );
			}
			else if ( getCurrentTabName().equalsIgnoreCase("Settings") ){
				updateSettings( pTyp );
			}
			else if ( getCurrentTabName().equalsIgnoreCase("Products") ){
				pTyp.getProdType().clear();
				if ( ptList != null ) pTyp.getProdType().addAll(ptList);
				if ( existingAt < 0 ) updatePalette( pTyp );
			}
			
    	    if ( existingAt < 0 ) {
    	    	prdTyps.getProductType().add( pTyp );
    	    	typeCombo.add( inputName );
    	    	
    	    	typeCombo.pack();
    	    	typeCombo.getParent().pack();
    	    	
    	    	typeCombo.select( typeCombo.getItemCount() - 1 );

    	    }
    	    else {
    	    	prdTyps.getProductType().set( existingAt, pTyp ) ;	    	
    	    } 
    	    
    	}
 
		
		return validNewTypeName;
    	
    }
    
    /**
     * 
     */
    private void createObjects( String className, boolean disposeOnly ) {
    	
    	ArrayList<String> pgObjs =(ArrayList<String>)PgenSession.getInstance().getPgenPalette().getObjectNames( className );
    	objects = new  ArrayList<String>();
    	if ( className.equals( "Symbol") ) {
        	for ( String st : pgObjs ) {
        		if (!st.contains("TURBULENCE") ) {
        			objects.add( st ); 
        		}
        	}
        }
        else {
        	objects.addAll( pgObjs ); 
        }
    	
        if ( objectGroup != null ) {
           
        	Control[] wids = objectGroup.getChildren();
       	
        	if ( wids != null ) {
       	        for ( int kk = 0; kk < wids.length; kk++ ) {
        		    wids[kk].dispose();
        	    } 
        	}
        	
        	objectGroup.dispose(); 	

        } 

        Control[] wids = controlBtnComp.getChildren();
    	for ( int jj = 0; jj < wids.length; jj++ ) {
    		wids[jj].dispose();
    	}
    	        
    	controlBtnComp.dispose();

        
        // Note - pack it to force the changes to be cached.
        shell.pack();
        
        if ( disposeOnly ) {
            objectGroup = null;
        }
        else {
                   	
            objectGroup = new Group( paletteComp, SWT.SHADOW_IN );
            objectGroup.setText( className + " Objects" );
            objectGroup.setData( className );
            objectGroup.setSize( 400, 200 );            
    	
            int maxRow = 8;
            int ncol = numColumns;
            if ( objects.size() / numColumns > maxRow ) {
                ncol =  objects.size() / maxRow;        	
            }
            
            GridLayout ly = new GridLayout( ncol, false );
            ly.horizontalSpacing = 0;
            ly.makeColumnsEqualWidth = false;
            ly.marginHeight = 0;
            ly.marginWidth = 0;
            ly.verticalSpacing = 0;
            
            if ( ncol != numColumns ) {
                objectGroup.setLayout( ly );           	
            }
            else {         	
                objectGroup.setLayout( new GridLayout( ncol, false ) );           	
            }
   
            objectBtns = new Button[ objects.size() ]; 
        
            int ii = 0;
            for ( String str : objects ) {       	
            	
                objectBtns[ ii ] = new Button( objectGroup, SWT.TOGGLE );
                objectBtns[ ii ].setData( str ); 
                objectBtns[ ii ].setImage( iconMap.get( str ) );
                objectBtns[ ii ].setSelection( btnAlwaysOn( str ) );
            
                objectBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            
                String type = typeCombo.getText();            
                ArrayList<String>  objStr = new ArrayList<String>();
                for ( ProductType ptype : prdTyps.getProductType() ) {
                    if ( ptype.getName().equalsIgnoreCase( type ) ) {
            		
                        for ( PgenClass pclass : ptype.getPgenClass() ) {
                            if ( pclass.getName().equalsIgnoreCase( className ) ) {
                                for ( int kk = 0; kk < pclass.getPgenObjects().getName().size(); kk++ ) {
                                    objStr.add( pclass.getPgenObjects().getName().get(kk) );
                                }
                            }
                        }
                        
                        break;
                    }
                }
            
                if ( objStr != null ) {
                    if ( objStr.size() == 0 || btnAlwaysOn( str ) || objStr.contains( str ) ) {
                        objectBtns[ ii ].setSelection( true );
                        objectBtns[ ii ].setImage( iconMapSelected.get( str ) );                    
                    }
                }
            
                objectBtns[ ii ].addSelectionListener( new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent event) {                           	
           	        
                	    Button btn = (Button)event.widget;
           	            String name = event.widget.getData().toString();           	        
           	        
           	            if ( btnAlwaysOn( name ) ) {
           	        	    btn.setSelection( true );
           	            }

           	            if ( btn.getSelection() ) {
                            btn.setImage( iconMapSelected.get( name ) );     	        	    	     	        	    	 
     	        	    }
      	        	    else {
           	        	    btn.setImage( iconMap.get( name ) );      	        	    	     	        	    	 
      	        	    }
                    }
                });

                ii++;
            }

        }
                
        createControlBtns( topComp );        
        
        /*  
         * Note - pack again to realize the changes - if the size of the control
         * does not change, the control area will not be repainted by calling "layout".
         * Here, if the width and height of the area of "objectGroup" and "controlBtnComp"
         * do not change, no repaint will happen even though the number of object
         * buttons is changed in "objectGroup". So we called "pack" first when we disposed
         * the buttons in "objectGroup" and "pack" again after the new object buttons are
         * generated.
         */
        shell.pack();
        
        shell.layout();
          	 
   }
    
    /**
     *  Create control buttons for the dialog
     */
    private void createControlBtns( Composite comp ) {
        
    	controlBtnComp = new Composite( comp, SWT.NONE );
        GridLayout gl4 = new GridLayout( 4, true );
        GridData gd = new GridData( SWT.FILL, SWT.DEFAULT, true, false );
        controlBtnComp.setLayout( gl4 );
        controlBtnComp.setLayoutData( gd );
        
        int ii = 0;
        for ( String str : cntlBtnNames ) {
            dlgCntlBtn[ ii ] = new Button( controlBtnComp, SWT.NONE );
            dlgCntlBtn[ ii ].setText( str  );
            dlgCntlBtn[ ii ].setToolTipText( cntlBtnTips[ ii ]);       
            dlgCntlBtn[ ii ].setLayoutData( gd );
            dlgCntlBtn[ ii ].setData( str );
            dlgCntlBtn[ ii ].addSelectionListener( new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {                           	
            	    dialogActions( event.widget.getData().toString() ); 	
                }
            });
            
            ii++;
        }

    }
    
    /**
     *  Create the product type combo and text input area.
     */
    private void createTypeComp( Composite comp ) {
        
   	    typeComp = new Composite( comp, SWT.NONE );   	
        typeComp.setLayout( new GridLayout(3, false) );
    	
        Label typeLbl = new Label( typeComp, SWT.LEFT);
        typeLbl.setText("Product Type:");
        
        typeCombo = new Combo( typeComp, SWT.DROP_DOWN | SWT.READ_ONLY );       
               
        typeCombo.add( "New" );        
        for ( ProductType ptyp : prdTyps.getProductType() ) {
        	typeCombo.add( ptyp.getName() );
        }
                 
        typeCombo.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	switchProductType();
//           	createObjects( "None", true );
//            	refreshPaletteSelections(); 
            	
            }
        });
        
        typeCombo.select( 0 );
        
        typeText = new Text( typeComp,  SWT.SINGLE | SWT.BORDER );                        
        typeText.setLayoutData( new GridData( 100, 15 ) );
        typeText.setEditable( true );   
        typeText.setText( "" );

    }
    
    
    /**
     *  Retrieve all controls, actions, classes, objects names and icons from PGEN palette.
     */
    private void retrievePgenPalette() {    	
		
    	PgenPaletteWindow plt = PgenSession.getInstance().getPgenPalette();
        
    	itemMap = plt.getItemMap();
    	
    	controls = (ArrayList<String>)plt.getControlNames();
		actions =  (ArrayList<String>)plt.getActionNames();
		classes = (ArrayList<String>)plt.getClassNames();
		
		controlBtns = new Button[ controls.size() ];
		actionBtns = new Button[ actions.size() ];
		classChkBtns = new Button[ classes.size() ];
		classPushBtns = new Button[ classes.size() ];
		
		iconMap = new HashMap<String, Image>();
		iconMapSelected = new HashMap<String, Image>();
		
		Image img = null;
		for ( String str : controls ) {
			img = plt.getButtonImage( str );
			iconMap.put( str, img );
			iconMapSelected.put( str, plt.createNewImage(img, fgcolor, bgcolor) );
		}
		
		for ( String str : actions ) {
			img = plt.getButtonImage( str );
			iconMap.put( str, img );
			iconMapSelected.put( str, plt.createNewImage(img, fgcolor, bgcolor) );
		}

		for ( String str : classes ) {
			img = plt.getButtonImage( str );
			iconMap.put( str, img );
			iconMapSelected.put( str, plt.createNewImage(img, fgcolor, bgcolor) );
		}	
		
		for ( String str : plt.getObjectNames() ) {
			img = plt.getButtonImage( str );
			iconMap.put( str, img );
			iconMapSelected.put( str, plt.createNewImage(img, fgcolor, bgcolor) );
		}
		
		dlgCntlBtn = new Button[ cntlBtnNames.length ];
           
    }
    
    /**
     *  Retrieve the list of selected object for a given class.
     */
    private ArrayList<String> getSelectedObjects( String className ) {
       	    	
        ArrayList<String> objList = new ArrayList<String>();
        
        if ( objectGroup != null && className.equalsIgnoreCase( objectGroup.getData().toString() ) ) {
    		for ( Button obtn : objectBtns ) {	    		
    			if ( obtn.getSelection() ) {
    				objList.add( obtn.getData().toString() );
    			}
    		}          			        
        }
        else {
        	
        	String type = typeCombo.getText();
        	ProductType ptype = null;
        	
        	if ( type.equalsIgnoreCase("New") ) {       		
        		objList = (ArrayList<String>)PgenSession.getInstance().getPgenPalette().getObjectNames( className );
        	}
        	else {
    			for ( ProductType stype : prdTyps.getProductType() ) {
    				if ( type.equalsIgnoreCase( stype.getName() ) ) {   					    					
    					ptype = stype;   										
    					break;
    				}
    			}
    			
				PgenClass pclass = null;  				
			    if ( ptype != null ) { 		
				    for ( PgenClass cls : ptype.getPgenClass() ) {
					    if ( cls.getName().equalsIgnoreCase( className ) ) {   							
						    pclass = cls;    							  												
						    break;
					    }
				    }
				}
  				
  				if ( pclass != null ) {
  					objList.addAll( pclass.getPgenObjects().getName() );
  				}
  				else {
  	        		objList = (ArrayList<String>)PgenSession.getInstance().getPgenPalette().getObjectNames( className );  					
  				}

        	}
        	
        }
        		
		return objList;
    	
    }
    
    /**
     *  Check if a button should always be placed on the PGEN palette
     */
    private boolean btnAlwaysOn( String btnName ) {
    			
    	String always = itemMap.get( btnName ).getAttribute("alwaysVisible");
		
    	if ( (always == null) || always.equalsIgnoreCase( "false" ) ) {
    		return false; 
		}
		
    	return true;
    }
    
    /**
     * Initialize the dialog components.
     */
    private void createDefaultTab( Composite cmp ) {
		
    	GridLayout gl = new GridLayout( 1, false );
        cmp.setLayout( gl );
        
	    
    	Button button = new Button ( cmp, SWT.PUSH );
	    button.setText ( "This Page is Under Construction! " );  
	    
    }
    
    /**
     *  Create the layering control GUI
     */    
    private void createLayeringTab( Composite cmp ) {
        
    	Control[] wids = cmp.getChildren();      	   
 	    for ( int jj = 0; jj < wids.length; jj++ ) {
    	    wids[jj].dispose();
    	}
 	    
 	    cmp.pack(); 	    

        GridLayout gly = new GridLayout( 1, false );
        cmp.setLayout( gly );
   	
    	Group layerEditGrp = new Group( cmp, SWT.NONE );
        GridLayout gl0 = new GridLayout( 1, false );
        gl0.verticalSpacing = 0;
        layerEditGrp.setLayout( gl0 );
        layerEditGrp.setText( "Define/Edit Layers");
        
        // Create a composite for layer names
        Composite nameComp = new Composite( layerEditGrp, SWT.NONE );   	
        nameComp.setLayout( new GridLayout(4, false) );
    	
        Label typeLbl = new Label( nameComp, SWT.LEFT );
        typeLbl.setText("Layer:");
        
        layerNameCombo = new Combo( nameComp, SWT.DROP_DOWN | SWT.READ_ONLY );       
               
        layerNameCombo.add( "New" );
        
        ProductType curPrdType = getCurrentPrdTyp();
    	
    	if ( curPrdType != null ) {
    		for ( PgenLayer lyr : curPrdType.getPgenLayer() ) {
    			layerNameCombo.add( lyr.getName() );
    		}
    	}
                 
        layerNameCombo.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                refreshLayerInput();
            }
        });
        
        layerNameCombo.select( 0 );
        
        layerNameTxt = new Text( nameComp,  SWT.SINGLE | SWT.BORDER );                        
        layerNameTxt.setLayoutData( new GridData( 80, 15 ) );
        layerNameTxt.setEditable( true );   
        layerNameTxt.setText( "" );
        
	    Button  deleteBtn = new Button( nameComp, SWT.PUSH );
	    deleteBtn.setText( "Delete" );
	    setButtonColor( deleteBtn, activeButtonColor );
        deleteBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
                removeOneLayer();
            }
        });      

        
        //Create a composite for displaying attributes
        Composite dispComp = new Composite( layerEditGrp, SWT.NONE );
        GridLayout gl4 = new GridLayout( 5, false );
        gl4.marginWidth = 3;
        dispComp.setLayout( gl4 );
        
        Label dispLbl = new Label( dispComp, SWT.LEFT );
        dispLbl.setText("Display:");
        
	    layerOnOffBtn = new Button( dispComp, SWT.CHECK);
	    layerOnOffBtn.setText( "OnOff" );
	    layerOnOffBtn.setSelection( true );     	          
		        
	    layerMonoBtn = new Button( dispComp, SWT.CHECK );
	    layerMonoBtn.setText( "A/M" );
        
	    layerColorSelector = new ColorButtonSelector( dispComp );
        Color clr = Color.RED;
        layerColorSelector.setColorValue( new RGB( clr.getRed(),  clr.getGreen(), clr.getBlue() ) );
	                  
	    layerFillBtn = new Button( dispComp, SWT.CHECK );
	    layerFillBtn.setText( "Filled" );
	    
	    // Create a composite for layer input file name
/*        Composite infileComp = new Composite( layerEditGrp, SWT.NONE );
        GridLayout gl2 = new GridLayout( 3, false );
        gl2.marginWidth = 3;
        infileComp.setLayout( gl2 );
	    
        Label inputLbl = new Label( infileComp, SWT.LEFT );
        inputLbl.setText("Input:");
        
        inputFileTxt = new Text( infileComp,  SWT.SINGLE | SWT.BORDER );                        
        inputFileTxt.setLayoutData( new GridData( 190, 15 ) );
        inputFileTxt.setEditable( true );   
        inputFileTxt.setText( "" );

        Button nameBtn = new Button( infileComp, SWT.PUSH );
        nameBtn.setText( "Browse");
        
        nameBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
               	
            	String fileName = inputFileTxt.getText();
            	if ( fileName == null || fileName.trim().length() <= 0 ) {
            	
             	    fileName = createDefaultFileName( null,
            			               layerNameTxt.getText(), "in" );
            	}
            	
            	createFileText( inputFileTxt, fileName );
            }
        });      
       
        //Create a composite for layer output file name
        Composite outfileComp = new Composite( layerEditGrp, SWT.NONE );
        outfileComp.setLayout( gl2 );
        
        Label outputLbl = new Label( outfileComp, SWT.LEFT );
        outputLbl.setText("Output:");
      
        outputFileTxt = new Text( outfileComp,  SWT.SINGLE | SWT.BORDER );                        
        outputFileTxt.setLayoutData( new GridData( 190, 15 ) );
        outputFileTxt.setEditable( true );   
        outputFileTxt.setText( "" );

        Button browseBtn = new Button( outfileComp, SWT.PUSH );
        browseBtn.setText( "Browse");
        
        browseBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
            	
            	String fileName = outputFileTxt.getText();
            	if ( fileName == null || fileName.trim().length() <= 0 ) {
            	
             	    fileName = createDefaultFileName( null,
            			               layerNameTxt.getText(), "out" );
            	}
             	
                createFileText( outputFileTxt, fileName );
            }
        });      
*/
               
    	/*
    	 *  Generate a template for defined layers as those shown on product manage
    	 *  and layering control windows.
    	 */           				
        GridLayout gl = new GridLayout( 1, false );
        layerTempGrp = new Group( cmp, SWT.NONE );
        layerTempGrp.setLayout( gl );
        layerTempGrp.setText( "Layers Defined");
         
        if ( curPrdType!= null ) {
        	createLayersTemplate( layerTempGrp, curPrdType.getPgenLayer() );       
        }
        
        shell.pack( true );

    }
 

    /*
     *  Create name, on/off, and color mode buttons for one layer
     */
    private void createLayersTemplate( Composite cmp, List<PgenLayer> layers ) {
            	
        Control[] wids = cmp.getChildren();      	   
 	    for ( int jj = 0; jj < wids.length; jj++ ) {
    	    wids[jj].dispose();
    	}
 	    
 	    cmp.pack(); 	    
        
        int ncomp = 3;
        if ( layers.size() <= 8 ) {
        	ncomp = 1;
        }
   	
        Composite topComp = new Composite( cmp, SWT.NONE );
    	GridLayout gl = new GridLayout( ncomp, false );
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        gl.verticalSpacing = 1;
        gl.horizontalSpacing = 40;
	    topComp.setLayout( gl );
        
    	Composite layersComp = new Composite( topComp, SWT.NONE );
    	GridLayout gl2 = new GridLayout( 3, false );
           	    
        gl2.marginHeight = 1;
        gl2.marginWidth = 1;
        gl2.verticalSpacing = 1;
        gl2.horizontalSpacing = 5;

        layersComp.setLayout( gl2 );
        
        Composite layersComp2 = null;
        if ( ncomp > 1 ) {
        	GridData gd = new GridData( GridData.FILL_VERTICAL );
            Label sepLbl = new Label( topComp, SWT.SEPARATOR | SWT.VERTICAL );
            sepLbl.setLayoutData( gd );

            layersComp2 = new Composite( topComp, SWT.NONE );
        	GridLayout gl3 = new GridLayout( 3, false );
            gl3.marginHeight = 1;
            gl3.marginWidth = 1;
            gl3.verticalSpacing = 1;
            gl3.horizontalSpacing = 5;
            layersComp2.setLayout( gl2 );
        }
        
        Composite whichCmp;
        layerNameBtns = new ArrayList<Button>();
        
        int nlayers = layers.size();
        int mod = nlayers - nlayers / 2 * 2;
        int nleft = Math.max( 8, layers.size() / 2 + mod );        
        int ii = 0;
    	for ( PgenLayer lyr : layers ) {
	     	
    		if ( ii < nleft ) {
    			whichCmp = layersComp;
    		}
    		else {
    			whichCmp = layersComp2;
    		}
    		
    	    Button nameBtn = new Button( whichCmp, SWT.PUSH );
    	    nameBtn.setText( lyr.getName() );
	    	setButtonColor( nameBtn, defaultButtonColor );	
    	        	    
    	    nameBtn.setData( ii );
    	    nameBtn.addSelectionListener( new SelectionAdapter() {
    	        public void widgetSelected(SelectionEvent event) {                		    	        	
                    switchLayer( ((Button)event.widget).getText() );    	        		
     	        }
    	    });
    	    
    	    
    	    layerNameBtns.add( nameBtn );
    	    	
    	    Button dispBtn = new Button( whichCmp, SWT.CHECK );
    	    dispBtn.setSelection( lyr.isOnOff() );     	          
    	    dispBtn.setEnabled( false );     	          
			        
    	    Button  clrBtn = new Button( whichCmp, SWT.PUSH );
    	    clrBtn.setText( "A/F" ); 
    	    
			Color clr = new Color( lyr.getColor().getRed(),
                    lyr.getColor().getGreen(),
                    lyr.getColor().getBlue(),
                    lyr.getColor().getAlpha() );

        	setButtonColor( clrBtn, clr );
    	    
 //   	    Button  inoutBtn = new Button( layersComp, SWT.PUSH );
 //   	    inoutBtn.setText( "I/O" );    	    
	       	       	    
    	    ii++;       
    	}
	    
    	cmp.pack();
    	shell.pack( true );
    	    		    		
    }
    
	/**
	 * Creates forecast hour filter page
	 */
	private void createFilterTab( Composite parent ) {
		
		GridLayout gly = new GridLayout( 1, false );
        parent.setLayout( gly );
        
        Composite titleComp = new Composite( parent, SWT.NONE );
        GridLayout gl0 = new GridLayout( 2, true );
        titleComp.setLayout( gl0 );
       
        Label filters = new Label( titleComp, SWT.NONE );
        filters.setText( "Forecast Hour Filters:");
    	
        Button allOnBtn = new Button( titleComp, SWT.CHECK );     
        allOnBtn.setText( "All On");
        
        Composite hoursComp = new Composite( parent, SWT.NONE );
				
        GridLayout mainLayout = new GridLayout( HOURS.length / 3, false);

        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
       
        hoursComp.setLayout(mainLayout);
        
        Button[] hourBtns = new Button[ HOURS.length ];
        
        for ( int ii = 0; ii < HOURS.length; ii++ ){
        	hourBtns[ii] = new Button( hoursComp, SWT.CHECK );
        	hourBtns[ii].setText( HOURS[ii] );
        }
	}
	/**
	 * Creates PGEN settings page
	 */
	private void createSettingsTab( Composite parent ) {
				
		GridLayout gly = new GridLayout( 1, false );
        gly.verticalSpacing = 10;
		parent.setLayout( gly );
               
        Group prdComp = new Group( parent, SWT.NONE );
        RowLayout rl = new RowLayout( SWT.HORIZONTAL );
        rl.center = true;
        prdComp.setLayout( rl );
        
        Label tlbl = new Label( prdComp, SWT.NONE );
        tlbl.setText( "Load Settings From:");
    	        
        /*
         * Input file path/name
         */

        settingsTxt = new Text( prdComp,  SWT.SINGLE | SWT.BORDER );
        settingsTxt.setLayoutData( new RowData( 200, 15 ) );
        settingsTxt.setEditable( true );
        settingsTxt.setText( "" );
        
        Button nameBtn = new Button( prdComp, SWT.PUSH );
        nameBtn.setText( "Browse");
        
        nameBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
                	
               	    String[] filterNames = new String[] { "XML Files (*.xml)" };
                	String[] filterExtensions = new String[] { "*.xml" };
//                	String filterPath = PgenUtil.CURRENT_WORKING_DIRECTORY;
                	String filterPath = PgenUtil.getWorkingDirectory();
                	
                	String selectedFile = selectFile( shell, SWT.OPEN, filterNames,
                			    filterExtensions, filterPath, null, false );
                	if ( selectedFile != null ) {
                	    settingsTxt.setText(  selectedFile );
                	}           	
            }
        });      
 
	}
	
	/**
	 * Creates file save page
	 */
	private void createSaveTab( Composite parent ) {
				
		GridLayout gly = new GridLayout( 1, false );
        gly.verticalSpacing = 10;
		parent.setLayout( gly );
               
        Group prdComp = new Group( parent, SWT.NONE );
        GridLayout gl0 = new GridLayout( 1, true );
        prdComp.setLayout( gl0 );
        
        Label tlbl = new Label( prdComp, SWT.NONE );
        tlbl.setText( "Save This Product To:");
    	        
        /*
         * Input file path/name
         */
        GridLayout mainLayout = new GridLayout( 3, false);

        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
       
        Composite nameComp = new Composite( prdComp, SWT.NONE );
		       
        nameComp.setLayout(mainLayout);
        
        Label nameLbl = new Label( nameComp, SWT.NONE );
        nameLbl.setText( "File:");
 
        saveOutputTxt = new Text( nameComp,  SWT.SINGLE | SWT.BORDER );
        saveOutputTxt.setLayoutData( new GridData( 200, 15 ) );
        saveOutputTxt.setEditable( true );
        saveOutputTxt.setText( "" );
        
        Button nameBtn = new Button( nameComp, SWT.PUSH );
        nameBtn.setText( "Browse");
        
        nameBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
 
            	String fileName = saveOutputTxt.getText();
            	if ( fileName == null || fileName.trim().length() <= 0 ) {
            	
             	    fileName = createDefaultFileName( null,
            			               saveOutputTxt.getText(), "out" );
            	}
             	
                createFileText( saveOutputTxt, fileName );
            }
        });      
 
        
        /*
         * Check if each layer should be saved to a separate file.
         */
        Group titleComp = new Group( parent, SWT.NONE );
        titleComp.setLayout( gl0 );
        
        Label albl = new Label( titleComp, SWT.NONE );
        albl.setText( "Do you want to save each layer individually?");
       
        saveIndividualLayerBtn = new Button( titleComp, SWT.CHECK );
        saveIndividualLayerBtn.setText( "Save layers individually " );
        saveIndividualLayerBtn.setSelection( true );    
        saveIndividualLayerBtn.setSelection( false );    
       
        /*
         * Options for auto save.
         */
        Group autosaveComp = new Group( parent, SWT.NONE );	       
        autosaveComp.setLayout(mainLayout);
        
        GridLayout asLayout = new GridLayout( 3, false);

        asLayout.marginHeight = 3;
        asLayout.marginWidth = 3;
       
        autoSaveBtn = new Button( autosaveComp, SWT.CHECK );
        autoSaveBtn.setText( "Auto save");
        autoSaveBtn.setSelection( true );    
        autoSaveBtn.setEnabled( false );    
		
        Label lbl = new Label( autosaveComp, SWT.NONE );
        lbl.setText( "      Frequency (min): ");
        
        autoSaveFreqTxt = new Text( autosaveComp,  SWT.SINGLE | SWT.BORDER );
        autoSaveFreqTxt.setLayoutData( new GridData( 60, 15 ) );
        autoSaveFreqTxt.setEditable( true );
        autoSaveFreqTxt.setToolTipText( "Please enter a positive integer." );
        
        autoSaveFreqTxt.setText( "" + autoSaveInterval );
        autoSaveFreqTxt.setEnabled( false );
        
	}
	
	
	/**
	 * Creates Share page
	 */
	private void createShareTab( Composite parent ) {
		
		GridLayout gly = new GridLayout( 1, false );
        parent.setLayout( gly );
        
        Composite titleComp = new Composite( parent, SWT.NONE );
        GridLayout gl0 = new GridLayout( 1, true );
        titleComp.setLayout( gl0 );
       
        Label filters = new Label( titleComp, SWT.NONE );
        filters.setText( "Share This Product:");
    	        
        /*
         * Input who do you want to share
         */
        Composite whoComp = new Composite( parent, SWT.NONE );
				
        GridLayout mainLayout = new GridLayout( 2, false);

        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
       
        whoComp.setLayout(mainLayout);
        
        Label whoLbl = new Label( whoComp, SWT.NONE );
        whoLbl.setText( "Who:");
        
        Combo whoCmb = new Combo( whoComp, SWT.NONE );
        whoCmb.add( "AWC");
        whoCmb.add( "HPC");
        whoCmb.add( "CPC");
        whoCmb.add( "SPC");
        whoCmb.add( "Other");
        whoCmb.select( 0 );     
             
        /*
         * What to share
         */
        Composite whatComp = new Composite( parent, SWT.NONE );
		       
        whatComp.setLayout( mainLayout );
        
        Label whatLbl = new Label( whatComp, SWT.NONE );
        whatLbl.setText( "What:");
        
        Combo whatCmb = new Combo( whatComp, SWT.NONE );
        whatCmb.add( "All");
        whatCmb.add( "Graphics Only");
        whatCmb.add( "Final Products Only");
        whatCmb.add( "Other");
        whatCmb.select( 0 );     
       
	}
   
	/**
	 * Creates Products page
	 */
	private void createProductsTab( Composite parent ) {
		
		GridLayout gly = new GridLayout( 1, false );
        parent.setLayout( gly );
        
        Button newBtn = new Button( parent, SWT.PUSH );
        newBtn.setText( "New");
        
        newBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				ProdTypeDialog ptDlg = new ProdTypeDialog( ProductConfigureDialog.this );
				ptDlg.open();
			}
		 });
        
        pdComp = new Composite( parent, SWT.NONE );
        GridLayout gl0 = new GridLayout( 3, true );
        pdComp.setLayout( gl0 );
        
        ProductType curPrdType = getCurrentPrdTyp();
        if ( ptList != null ) ptList.clear();
        if ( curPrdType != null && curPrdType.getProdType() != null ) {
        	for (ProdType pt : curPrdType.getProdType() ){
        		addProdType(pt);
        	}
        }            
	}
	
	/**
	 * Creates Clipping page
	 */
	private void createClipTab( Composite parent ) {
		
		GridLayout gly = new GridLayout( 1, false );
        parent.setLayout( gly );
        
        Composite titleComp = new Composite( parent, SWT.NONE );
        GridLayout gl0 = new GridLayout( 2, true );
        gl0.marginHeight = 3;
        gl0.marginWidth = 3;
        titleComp.setLayout( gl0 );

        Label filters = new Label( titleComp, SWT.NONE );
        filters.setText( "Clip the Product:");
        
        Button pBtn = new Button( titleComp, SWT.CHECK );
           	        
        /*
         * Input boundary file name
         */
        GridLayout mainLayout = new GridLayout( 3, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
		               
        Composite nameComp = new Composite( parent, SWT.NONE );
		       
        nameComp.setLayout(mainLayout);
        
        Label nameLbl = new Label( nameComp, SWT.NONE );
        nameLbl.setText( "Shape File: ");
 
        shapeFileTxt = new Text( nameComp,  SWT.SINGLE | SWT.BORDER );
        shapeFileTxt.setLayoutData( new GridData( 180, 10 ) );
        shapeFileTxt.setEditable( true );
        
        Button nameBtn = new Button( nameComp, SWT.PUSH );
        nameBtn.setText( "Browse" );
        nameBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
            	
           	    String[] filterNames = new String[] { "All Files (*)" };
            	String[] filterExtensions = new String[] { "*" };
            	String filterPath = "/";
            	
            	String selectedFile = selectFile( shell, SWT.OPEN, filterNames,
            			    filterExtensions, filterPath, null, false );
            	if ( selectedFile != null ) {
            	    shapeFileTxt.setText(  selectedFile );
            	}           	
            }
        });      
              
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
	

	/**
	 * Get the name of the active tab
	 * 
	 * @param 
	 * @return current tab name
	 */
	private String getCurrentTabName() {
    	return tabFolder.getSelection()[0].getText();    	
	}
	
	
	/**
	 * Update the PGEN palette definitions for a given type
	 * 
	 * @param ptyp
	 * @return 
	 */
	private void updatePalette( ProductType ptyp ) {
		
		// Clear the previous selections of controls/actions 
		if ( ptyp.getPgenControls() != null &&
				ptyp.getPgenControls().getName() != null ) {
		    ptyp.getPgenControls().getName().clear();
		}
		
		if ( ptyp.getPgenActions() != null &&
				ptyp.getPgenActions().getName() != null ) {
			ptyp.getPgenActions().getName().clear();
		}

	    // Get controls
	    PgenControls pControls = new PgenControls();     	   	
	
	    for ( Button btn : controlBtns ) {
		
		    if ( btn.getSelection() ) {
			        pControls.getName().add( btn.getData().toString() );
		    }    		
	    }
	       	
	    ptyp.setPgenControls( pControls );
	
	    // Get actions   	
	    PgenActions pActions = new PgenActions();     	   	
	
	    for ( Button btn : actionBtns ) {
		
		    if ( btn.getSelection() ) {
			    pActions.getName().add( btn.getData().toString() );
		    }
		
	    }
	       	
	    ptyp.setPgenActions( pActions );
	
	    // Get classes and objects	    	     	   	   	
		int ii = 0;    	       	    
		if ( ptyp.getPgenClass() != null ) {
			 ptyp.getPgenClass().clear();
		}
		
		for ( Button btn : classChkBtns ) {   		   		
		    
		    if ( btn.getSelection() ) {
	    		
		    	PgenClass pClass = new PgenClass();
	    		pClass.setName( classPushBtns[ ii ].getData().toString() );

				ArrayList<String> selectedObjs = getSelectedObjects( classPushBtns[ ii ].getData().toString() );
                
				if ( selectedObjs != null ) {
			    	
					PgenObjects pObj = new PgenObjects();   		    									    
					for ( String objStr : selectedObjs ) {	    		    			    				
				    	pObj.getName().add( objStr );		    			
				    }
    				
					pClass.setPgenObjects( pObj );						
				}
				
			    ptyp.getPgenClass().add( pClass );	
				   			        
		    }
		
		    ii++;
		
	    }
	       		
	}

	/**
	 * Update the layer definitions for a given type
	 * 
	 * @param ptyp
	 * @return validateLayerName
	 */
	private boolean updateLayers( ProductType ptyp ) {
		
    	String curName = layerNameCombo.getText();
		String inputName = layerNameTxt.getText();
		        		
		boolean validateLayerName = validateLayerName( curName, inputName );
		
		List<PgenLayer> pLayers = ptyp.getPgenLayer();
		
		if ( validateLayerName ) {
	        
	        // Check if it is an existing layer or a new layer	 
			int layerAt = -1;
			int kk = 0;
			for ( PgenLayer plyr : pLayers ) {
				if ( plyr.getName().equals( inputName ) ) {		       
					layerAt = kk;			        
					break;
				}

				kk++;
			}
            
			// Build a new PgenLayer	    	     	   	   	    	       	        		    
			PgenLayer pgenlyr = new PgenLayer();
			
			RGB layerRGB = layerColorSelector.getColorValue();
			gov.noaa.nws.ncep.ui.pgen.productTypes.Color pclr = 
				new gov.noaa.nws.ncep.ui.pgen.productTypes.Color();
			pclr.setAlpha( 255 );
			pclr.setRed( layerRGB.red );
			pclr.setGreen( layerRGB.green );
			pclr.setBlue( layerRGB.blue );

			pgenlyr.setColor( pclr );

			pgenlyr.setFilled( layerFillBtn.getSelection() );
			pgenlyr.setMonoColor( layerMonoBtn.getSelection());
			pgenlyr.setName( inputName );
			pgenlyr.setOnOff(layerOnOffBtn.getSelection() );
//			pgenlyr.setInputFile( inputFileTxt.getText() );
//			pgenlyr.setOutputFile( outputFileTxt.getText() );

			//Add or replace using the new PgenLayer	    	     	   	   	    	       	        		    
			ProductType oldType = getCurrentPrdTyp();
			if ( oldType == null ||  ptyp.getName().equals( oldType.getName() ) ) {
			    if ( layerAt >= 0 ) {
				    ptyp.getPgenLayer().set( layerAt, pgenlyr );
			    }
			    else {				
				    ptyp.getPgenLayer().add( pgenlyr ); 
				    layerNameCombo.add( inputName );
				    layerNameCombo.select( layerNameCombo.getItemCount() - 1);
				    layerNameCombo.pack();
				    layerNameCombo.getParent().pack();
			    }
			}
			else {

				for ( PgenLayer plyr : oldType.getPgenLayer() ) {
					// Build a new PgenLayer	    	     	   	   	    	       	        		    
					PgenLayer newpgenlyr = new PgenLayer();
					
					newpgenlyr.setColor( plyr.getColor() );
					newpgenlyr.setFilled( plyr.isFilled() );
					newpgenlyr.setMonoColor( plyr.isMonoColor());
					newpgenlyr.setName(  plyr.getName() );
					newpgenlyr.setOnOff( plyr.isOnOff() );
					newpgenlyr.setInputFile( plyr.getInputFile() );
					newpgenlyr.setOutputFile( plyr.getOutputFile() );
                    
					ptyp.getPgenLayer().add( newpgenlyr );				    
				}
				
			}
			
			// Refresh the layers template.
			createLayersTemplate( layerTempGrp, ptyp.getPgenLayer() );			

		}
		
		return validateLayerName;
	       		
	}
	
	/**
	 *  Validate the name for the layers.
	 *  
	 * @param curLayerName
	 * @param inputName 
	 * @return current tab name  
	 */
	private boolean validateLayerName( String curLayerName, String inputName ) {
    			
        boolean validNewLayerName = true;
        String msg = new String( "Layer name " );
    	       
        if ( inputName == null || inputName.length() == 0 ) {		    
       		return validNewLayerName;
       	}
        else if ( inputName.equalsIgnoreCase( "New" )   ) {
        	validNewLayerName = false;	
        	msg = "cannot be any variations of 'New'.\n";      	
        }
        else {       	    			
    		if ( !curLayerName.equalsIgnoreCase( inputName ) ) {
    	        
    			for ( String stype : layerNameCombo.getItems() ) {
   			        if  ( inputName.equalsIgnoreCase( stype ) ) {
 	    			    msg = msg + "'" + inputName + "' has been used.\n";
   			        	validNewLayerName = false;
   					    break;
    			    }
    			}
    	    }
        }
		
	   			 		        		
		if ( !validNewLayerName ) {
	    	MessageDialog confirmDlg = new MessageDialog( 
    	           PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    	           "Confirm a new Type Name", null, msg + "Please specify another valid name.",
    	        	MessageDialog.WARNING, new String[]{"OK"}, 0);
    	        
    	    confirmDlg.open();   
		}
		
		return validNewLayerName;

	}
	
	/**
	 *  Validate the name for the product type.
	 *  
	 * @param curTypName
	 * @param inputName 
	 * @return current tab name  
	 */
	private boolean validatePrdTypName( String curTypName, String inputName ) {
    			
        boolean validNewTypeName = true;
        String msg = new String( "Product type name " );
        
        if ( inputName == null || inputName.length() == 0 ) {		    
        	validNewTypeName = false;	
        	msg += "cannot be blank.\n";
        }
        else if ( inputName.equalsIgnoreCase( "New" )   ) {
        	validNewTypeName = false;	
        	msg = "cannot be any variations of 'New'.\n";      	
        }
        else {       	    			
    		if ( !curTypName.equalsIgnoreCase( inputName ) ) {
    	        
    			for ( String stype : typeCombo.getItems() ) {
   			        if  ( inputName.equalsIgnoreCase( stype ) ) {
 	    			    msg = msg + "'" + inputName + "' has been used.\n";
   			        	validNewTypeName = false;
   					    break;
     			    }
   			    }
    	    }
        }
		
	   			 		        		
		if ( !validNewTypeName ) {
	    	MessageDialog confirmDlg = new MessageDialog( 
    	           PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    	           "Confirm a new Type Name", null, msg + "Please specify another valid name.",
    	        	MessageDialog.WARNING, new String[]{"OK"}, 0);
    	        
    	    confirmDlg.open();   
		}
		
		return validNewTypeName;

	}
    
	
	/**
     *  Edit/Update the layering input GUI
     */    
    private void editLayeringTab() {
        
        ProductType curPrdType = getCurrentPrdTyp();
 
    	layerNameCombo.removeAll();
    	
    	layerNameCombo.add( "New");
  	
    	if ( curPrdType != null ) {
    		for ( PgenLayer lyr : curPrdType.getPgenLayer() ) {
    			layerNameCombo.add( lyr.getName() );
    		}
    	}
    	
    	layerNameCombo.pack();
    	layerNameCombo.getParent().pack();
           	        
        layerNameCombo.select( 0 );
        refreshLayerInput();
                       
    	/*
    	 *  Generate a template for defined layers as those shown on product manage
    	 *  and layering control windows.
    	 */           				
        if ( curPrdType!= null ) {
   	        createLayersTemplate( layerTempGrp, curPrdType.getPgenLayer() );  
        }
        else {
            Control[] wids = layerTempGrp.getChildren();      	   
     	    for ( int jj = 0; jj < wids.length; jj++ ) {
        	       wids[jj].dispose();
        	}
     	    
     	    layerTempGrp.pack();
        }
        
    }
    
    
	/**
     *  Retrieve the current product type
     */    
    private ProductType getCurrentPrdTyp() {
        
    	String curPrdtypeName = typeCombo.getText();

        ProductType  curPrdType = null;
    	for ( ProductType ptyp : prdTyps.getProductType() ) {
        	if ( curPrdtypeName.equals( ptyp.getName() ) ) {
        		curPrdType = ptyp;
        		break;
        	}        	
        }
    	    	
    	return curPrdType;
    }
    
	
	/**
	 * Refresh the inputs for the layer definitions of a given type
	 * 
	 * @param 
	 * @return 
	 */
	private void refreshLayerInput() {
		
	    ProductType ptyp = getCurrentPrdTyp();
	    String layerName = layerNameCombo.getText();
	    
	    if ( layerName.equalsIgnoreCase( "New" ) ) {	    
	        setDefaultLayerInput(); 
	    }	    
	    else {
	    
	    	layerNameTxt.setText( layerName );

		    if ( ptyp != null ) {
    	        List<PgenLayer> pLayers = ptyp.getPgenLayer();
    	        int layerAt = -1;
    	        if ( pLayers != null ) {
    	    		        
			         int kk = 0;
			         for ( PgenLayer plyr : pLayers ) {
				         if ( plyr.getName().equals( layerName ) ) {		       
					         layerAt = kk;			        
					         break;
				         }
				         
				         kk++;
			         }
			     }
			     
		         if ( layerAt >= 0 ) {
            
			         PgenLayer pgenlyr = pLayers.get( layerAt );			

					 pgenlyr.getColor().getRed();
					 layerColorSelector.setColorValue( 
								new RGB( pgenlyr.getColor().getRed(), 
										 pgenlyr.getColor().getGreen(), 
										 pgenlyr.getColor().getBlue()) );

					 layerFillBtn.setSelection( pgenlyr.isFilled() );
					 layerMonoBtn.setSelection( pgenlyr.isMonoColor() );
					 layerOnOffBtn.setSelection( pgenlyr.isOnOff());
/*					 if ( pgenlyr.getInputFile() != null ) {
					     inputFileTxt.setText( pgenlyr.getInputFile());
					 }
					 else {
						 inputFileTxt.setText( "" );
					 }
					 
					 if ( pgenlyr.getOutputFile() != null ) {
					     outputFileTxt.setText( pgenlyr.getOutputFile());
					 }
					 else {
						 outputFileTxt.setText( "");
					 }						 
*/		        	
		         }
		         
		         // Set the color of the layer name buttons.
		         for ( Button btn : layerNameBtns ) {
		        	 if ( btn.getText().equals( layerName ) ) {
		        		 setButtonColor( btn, activeButtonColor );
		        	 }
		        	 else {
		        		 setButtonColor( btn, defaultButtonColor );		        	    	
		        	 }
		         }
    	    }	       		
	    }	    
    }
	
	
	/**
	 * Set default layer definitions for a new layer
	 * 
	 * @param 
	 * @return 
	 */
	private void setDefaultLayerInput() {
		
	    layerNameTxt.setText("");
		layerOnOffBtn.setSelection( true );
		layerMonoBtn.setSelection( false);
			
		Color layerClr = Color.YELLOW;
		layerColorSelector.setColorValue( 
			    new RGB( layerClr.getRed(), layerClr.getGreen(), layerClr.getBlue()) );
			
		layerFillBtn.setSelection( false );
			
//		inputFileTxt.setText("");
//		outputFileTxt.setText("");
		
    }
	
	/**
	 * Remove a layer from the current product type.
	 * Note: need to "Apply" to confirm the action.
	 * 
	 * @param 
	 * @return 
	 */
	private void removeOneLayer() {
	    
		ProductType ptyp = getCurrentPrdTyp();
	    String layerName = layerNameCombo.getText();
	    
		if ( !layerName.equalsIgnoreCase("New") && ptyp != null ) {
    	    
			List<PgenLayer> pLayers = ptyp.getPgenLayer();

    	    if ( pLayers != null ) {   	    		        

			    for ( PgenLayer plyr : pLayers ) {
				    if ( plyr.getName().equals( layerName ) ) {		       
                         layerNameCombo.remove( plyr.getName() );
                         pLayers.remove( plyr );		        
                         break;
				     }
			    }
				
			    layerNameCombo.select( pLayers.size() );			    
		   	    layerNameTxt.setText( layerNameCombo.getText() );
		   	    layerNameCombo.pack();
		   	    layerNameCombo.getParent().pack();
				createLayersTemplate( layerTempGrp, pLayers );  

    	    }			     
	    }		    		
    }
	
	/**
	 * Switch from one product type to another.
	 * 
	 * @param 
	 * @return 
	 */
	private void switchProductType() {				
   	    
		//Update palette selections from the new type
		createObjects( "None", true );
    	refreshPaletteSelections();
    	
    	//Update layer definitions from the new type
	    editLayeringTab();   
    	
	    //Update Save definitions from the new type
	    editSaveTab();   

		//update Pgen settings
	    editSettingsTab();
	    
	    //update Product tab
	    editProductsTab();
    }
	
	/**
	 * Refresh the inputs for the layer definitions of a given type
	 * 
	 * @param 
	 * @return 
	 */
	private void switchLayer( String layername ) {		
	    layerNameCombo.setText( layername );
	    refreshLayerInput();       	    
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
//   	String filterPath = PgenUtil.CURRENT_WORKING_DIRECTORY;
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
   	    
    	String selectedFile = selectFile( shell, SWT.SAVE, filterNames,
    			    filterExtensions, filterPath, defaultFile, true );
    	
    	if ( selectedFile != null ) {
    	    txt.setText(  selectedFile );
    	}
  	    
	}
	
	/**
	 * Create a default file name based on the type and layer names.
	 * 
	 * @param prdname	Product type name
	 * @param layername	layer name
	 * @param suffix	suffix to be added, if any
	 * @param input
	 * 
	 * @return createDefaultFileName()
	 */
	private String createDefaultFileName( String prdname,  String layername,
			                              String suffix ) {
		       
		String fileName = null;
		
		if ( prdname != null || layername != null ) {
			if ( prdname != null && prdname.trim().length() > 0 ) {
				fileName = new String( prdname );
			}

			if ( layername != null && layername.trim().length() > 0 ) {
				if ( fileName != null ) {
					fileName = new String( fileName + "_" + layername );
				}
				else {
					fileName = new String( layername );
				}
			}

			if ( fileName != null ) {
				
				if ( suffix != null ) {
			        fileName = new String( fileName + "_" + suffix + ".xml" );
				}
				else {
					fileName = new String( fileName + ".xml" );
				}
				
			}
		} 

		return fileName;
	
	}

	/**
     *  Edit/Update the Save input GUI
     */    
    private void editSaveTab() {
        
        ProductType curPrdType = getCurrentPrdTyp();
    	
        if ( curPrdType != null && curPrdType.getPgenSave() != null ) {
    		
        	PgenSave pSave = curPrdType.getPgenSave();
        	if (  pSave != null ) {
        	    String outFile = pSave.getOutputFile();
        	    if ( outFile != null ) {
        		    saveOutputTxt.setText( outFile );
        	    }
        	
        	    saveIndividualLayerBtn.setSelection( pSave.isSaveLayers() );
            	autoSaveBtn.setSelection( true );       		
            	autoSaveFreqTxt.setText( "" );

        	}
        }
        else {
                
        	saveOutputTxt.setText( "" );

        	saveIndividualLayerBtn.setSelection( false );
        	
        	autoSaveBtn.setSelection( true );       		
        	autoSaveFreqTxt.setText( "" );
        }
        	         
    }
    
	/**
     *  Edit/Update the Pgen settings 
     */    
    private void editSettingsTab() {
        
        ProductType curPrdType = getCurrentPrdTyp();
    	
        if ( curPrdType != null && curPrdType.getPgenSave() != null ) {
    		
        	String settingsFile = curPrdType.getPgenSettingsFile();
        	if (  settingsFile != null ) {
        		   settingsTxt.setText( settingsFile );
        	}
        }
        else {
        	settingsTxt.setText( "" );
        }
        	         
    }
    
   /**
    * Update the product tab 
    */
   private void editProductsTab() {
        
	    pdComp.dispose();
        pdComp = new Composite( tabComposites[7], SWT.NONE );
        GridLayout gl0 = new GridLayout( 3, true );
        pdComp.setLayout( gl0 );
	    
	    ProductType curPrdType = getCurrentPrdTyp();
        if ( ptList != null ) ptList.clear();
        if ( curPrdType != null && curPrdType.getProdType() != null ) {
        	for (ProdType pt : curPrdType.getProdType() ){
        		addProdType(pt);
        	}
        } 
        
        pdComp.pack();
        pdComp.layout();
        tabComposites[7].pack();
        tabComposites[7].layout();
    }
   
	/**
	 * Update the "Save" definitions for a given type
	 * 
	 * @param ptyp
	 */
	private void updateSaveInfo( ProductType ptyp ) {
		
		// Build a new PgenSave Class	    	     	   	   	    	       	        		    
		PgenSave pSave = ptyp.getPgenSave();
		if ( pSave == null ) {
			pSave = new PgenSave();
		}
		
		//Update info in PgenSave Class	    	     	   	   	    	       	        		    
	    pSave.setInputFile( null );
		
	    String outfile = saveOutputTxt.getText();
	    if ( outfile != null ) {
		    pSave.setOutputFile( outfile );
	    }
	    
	    pSave.setSaveLayers( saveIndividualLayerBtn.getSelection() );
	    pSave.setAutoSave( autoSaveBtn.getSelection() );
	    
	    int freqInt = 0;
	    if ( pSave.getAutoSaveFreq() != null ) {
	    	freqInt = pSave.getAutoSaveFreq();
	    }

		try {						
			freqInt = Math.abs( Integer.parseInt( autoSaveFreqTxt.getText() ) );
		} catch ( NumberFormatException e1 ) {
			
		}
		
		if ( freqInt == 0 ) { 
			freqInt = autoSaveInterval;
		}
		
		pSave.setAutoSaveFreq( freqInt );
		autoSaveFreqTxt.setText( "" + freqInt );
		
	    ptyp.setPgenSave( pSave );
	       		
	}
	
	/**
	 * Update Pgen Settings for a given type
	 * 
	 * @param ptyp
	 */
	private void updateSettings( ProductType ptyp ) {
		
		//Update settings 	    	     	   	   	    	       	        		    
	    String settingsFile = settingsTxt.getText();
	    if ( settingsFile != null && !settingsFile.isEmpty()) {
		    ptyp.setPgenSettingsFile( settingsFile );
		    
		    if (!AttrSettings.getInstance().loadPgenSettings(  settingsFile )){
       			MessageDialog infoDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Warning", null, "Pgen settings file does not exist or is invalid!",
						MessageDialog.WARNING, new String[]{"OK"}, 0);

				infoDlg.open();
		    }
	    }
	}
	
	/**
	 * Add a new Prod Type into the Configuration dialog
	 * @param pt
	 */
	public void addProdType( final ProdType pt ){
		
		if ( ptList == null ) ptList = new ArrayList<ProdType>();
		ptList.add( pt );
		
		//add a button with the type name
		final Button ptBtn = new Button( pdComp, SWT.PUSH );
		ptBtn.setLayoutData(new GridData( 120, 35 ));
		ptBtn.setText(pt.getName());

		//add a label with the output type(XML/KML/Text etc.
		final Label typeLbl = new Label( pdComp, SWT.NONE );
		GridData gd = new GridData( 140, 20 );
		gd.verticalAlignment = SWT.CENTER;
		gd.verticalIndent = 10;
		typeLbl.setLayoutData( gd );
		typeLbl.setText(pt.getType());
		
		ptBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				ProdTypeDialog ptDlg = new ProdTypeDialog( ProductConfigureDialog.this, pt, ptBtn, typeLbl);
				ptDlg.open();
			}
		});
		
	
		//add the delete button
		final Button delBtn = new Button( pdComp, SWT.PUSH );
		delBtn.setText( "Delete" );
		
		delBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				Iterator<ProdType> it = ptList.iterator();
				while ( it.hasNext() ){
					ProdType pt = it.next();
					if ( pt.getName().equalsIgnoreCase(ptBtn.getText() )){
						it.remove();
						break;
					}
				}
				ptBtn.dispose();
				typeLbl.dispose();
				delBtn.dispose();
				pdComp.pack();
				pdComp.layout();
			}
		});
		
		pdComp.pack();
		pdComp.layout();
		tabComposites[7].pack();
		tabComposites[7].layout();
		
	}
}
