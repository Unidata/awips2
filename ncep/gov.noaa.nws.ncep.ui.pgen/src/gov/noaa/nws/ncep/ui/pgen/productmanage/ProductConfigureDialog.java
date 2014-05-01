/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductConfigureDialog
 * 
 * Sept 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import java.awt.Color;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
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

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.PgenPreferences;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.Activator;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;

import gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.producttypes.*;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

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
 * 07/11        #450        G. Hull     NcPathManager. save to USER Context
 * 09/11		#335		J. Wu		Added type/subtype/alias/file template
 * 11/11		?			B. Yin		Put settings file in localization
 * 01/12		TTR463		J. Wu		Creating new activity from existing ones
 * 07/12  		#822        J. Wu    	Synchronize with the presence of PGEN Palette
 * 08/12  		#822        J. Wu    	Make this dialog "APPLICATION_MODAL"
 * 12/12  		#937        J. Wu    	Update G_Airmet layers/hazard - "C&V"
 * 11/13		#1049		B. Yin		Handle outlook type defined in layer.
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
    		                               "Delete current activity type", "Ignore changes and exit" };
    private final String[] typeTabNames = { "Palette", "Settings", "Layer", "Save", "Filter", 
    										"Share", "Clip", "Products" };
    private static final String PGEN_PRODUCT_TYPES   ="productTypes.xml";

    private HashMap<String, IConfigurationElement> itemMap = null;  // map of PGEN configuration elements
	
	private static ArrayList<String> controls = null;
	private static ArrayList<String> actions = null;
	private static ArrayList<String> classes = null;
	private static ArrayList<String> objects = null;
	private HashMap<String, Image> iconMap = null;    		  // map of all buttons on palette
	private HashMap<String, Image> iconMapSelected = null;    // map of all buttons if selected
	
	private final int numColumns = 10;
	private final int bgcolor = (0 * 65536 ) + ( 0 * 256 ) + 255;
	private final int fgcolor = (255 * 65536 ) + ( 255 * 256 ) + 255;
	
	// save the LocalizationFile for the Context level
	private static LocalizationFile prdTypesFile = null;
	private ProductTypes prdTyps = null;
	private List<ProdType> ptList = null; 
	
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
    
    private Group activityGrp = null;
    private Combo typeCombo = null;
    private Text typeText = null;

    private Combo subtypeCombo = null;
    private Text subtypeText = null;
    private static String DEFAULT_SUBTYPE = "None";
   
    private Text aliasText = null;
	
    private Button[] controlBtns = null;
    private Button[] actionBtns = null;
    private Button[] classChkBtns = null;       
    private Button[] classPushBtns = null;       
    private Button[] objectBtns = null; 
    private Button   allOnOffBtn = null; 
   
    private Button[] dlgCntlBtn = null;
    
    private Button clipChkBox;
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
    
    private Text currentSettingsTxt = null;
    private Text settingsTxt = null;
    
    private Text metaTxt;
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
    
    private Combo boundsNameCbo; 
    private Combo boundsListCbo;
    
    /**
     * Default colors for the default and active product of layer name button.
     */
    private final Color defaultButtonColor = Color.lightGray;
    private final Color activeButtonColor = Color.green;
    private final Color remindButtonColor = Color.yellow;
	
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
        shell.setText( "Configure Activity Types" );        
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
	        shell.setLocation( pt.x + 500,  pt.y + 100 );
		} else {
			shell.setLocation(shellLocation);
		}

    }
    
    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
	    
        /*
         * Retrieve PGEN pallete info - it may get disposed when palette is deactivated
         * so we need to make sure palette is there and retrieve info before starting to
         * build this GUI.
         */
    	retrievePgenPalette();
  	
    	
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
    	 * create main composite
    	 */
        activityGrp = new Group( topComp, SWT.SHADOW_IN );
        activityGrp.setLayout( new GridLayout(1, false) );
        activityGrp.setText( "Activity Identity" );
               
    	/*
    	 * create product type composite
    	 */
    	createTypeComp( activityGrp );
             
        /*
    	 * create main composite
    	 */
        Group configGrp = new Group( topComp, SWT.SHADOW_IN );
        configGrp.setLayout( new GridLayout(1, false) );
        configGrp.setText( "Activity Configuraton" );
 
    	/*
    	 * create tab folders/items
    	 */
    	tabFolder = new TabFolder ( configGrp, SWT.BORDER );
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
            	
        if ( index == 2 ) {
   		    editLayeringTab();
   	    }
        else if ( index == 3 ) {
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
            controlBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            
            if ( btnAlwaysOn( str ) ) {
                controlBtns[ ii ].setSelection( true );
                controlBtns[ ii ].setImage( iconMapSelected.get( str ) );
            }
            else {
                controlBtns[ ii ].setSelection( false );
                controlBtns[ ii ].setImage( iconMap.get( str ) );         	
            }
 
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
            actionBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            
            if ( btnAlwaysOn( str ) ) {
                actionBtns[ ii ].setImage( iconMapSelected.get( str ) );           	
                actionBtns[ ii ].setSelection( true );
            }
            else {
                actionBtns[ ii ].setImage( iconMap.get( str ) );           	           	
                actionBtns[ ii ].setSelection( false );
            }

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
        classGroup.setLayout( new GridLayout((numColumns+1)/2 + 1, false) );
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
                    	createObjects( classPushBtns[ which ].getData().toString(), false );                	    
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
            classPushBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
            
            classPushBtns[ ii ].setSelection( btnAlwaysOn( str ) );
        	
            classPushBtns[ ii ].setData( str );        	
    		classPushBtns[ ii ].setEnabled( false );
    		
            if ( btnAlwaysOn( str ) ) {
           		classPushBtns[ ii ].setEnabled( true );
                classPushBtns[ ii ].setSelection( true );          	
                classPushBtns[ ii ].setImage( iconMapSelected.get( str ) );
            }
            else {
           		classPushBtns[ ii ].setEnabled( false );
                classPushBtns[ ii ].setSelection( false );        	
                classPushBtns[ ii ].setImage( iconMap.get( str ) );
            }

    		
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
    	
		ProductTypes ptyps = new ProductTypes();
		
		prdTypesFile = PgenStaticDataProvider.getProvider().getStaticLocalizationFile(
				PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_PRODUCT_TYPES);
		
		if( prdTypesFile != null && prdTypesFile.getFile().exists() && 
				prdTypesFile.getFile().canRead() ) {
     	    ptyps = FileTools.readProductTypes( prdTypesFile.getFile().getAbsolutePath() );
		}
		
		return ptyps;
      	
    }

    /**
     *  Load the "productTypes.xml" into a ProductTypes instance.
     */
    static public LinkedHashMap<String, ProductType> getProductTypes() {
    	
    	LinkedHashMap<String, ProductType> prdTypMap = new LinkedHashMap<String, ProductType>();
    	ProductTypes ptyps = loadProductTypes();
		
        for ( ProductType ptype : ptyps.getProductType() ) {
    		String skey;
        	if ( ptype.getName() != null && ptype.getName().trim().length() > 0 ) {
        	    skey = new String( ptype.getName() );
        	}
        	else {
        		skey = new String( ptype.getType() );
        		String subtyp = ptype.getSubtype();
        		if ( subtyp != null && subtyp.trim().length() > 0 &&
        			 !subtyp.equalsIgnoreCase( DEFAULT_SUBTYPE ) ) {
        			skey = new String( ptype.getType() + "(" + subtyp + ")" );
        		}
        	}
        	
        	prdTypMap.put( skey,  ptype );
    	}
    			
		return prdTypMap;
      	
    }
 
    /**
     *  Update the selections based on the product type.
     */
    private void refreshPaletteSelections() { 
        
    	String type = typeCombo.getText();
		ProductType curPrdTyp = getCurrentPrdTyp();	
		
		if ( curPrdTyp == null ) {
			return;
		}
		
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
   		
    		if ( dlgCntlBtn[ 1 ] != null ) {
    			dlgCntlBtn[ 1 ].setText( "Apply");
    		}
            
    		dlgCntlBtn[ 2 ].setEnabled( true );
   		
     		for ( Button btn : controlBtns ) {
            	String str = btn.getData().toString();
     			if ( btnAlwaysOn( str ) || (curPrdTyp.getPgenControls() != null &&
     				curPrdTyp.getPgenControls().getName().contains( str ) ) ) {
            		btn.setSelection( true );;
                   	btn.setImage( iconMapSelected.get( str ) );
           	    }
            	else {
            		btn.setSelection( false );
            		btn.setImage( iconMap.get( str ) );
           	    }
            }
     		
     		for ( Button btn : actionBtns ) {
            	String str = btn.getData().toString();          	
     			if ( btnAlwaysOn( str ) || (curPrdTyp.getPgenActions() != null &&
     				curPrdTyp.getPgenActions().getName().contains( str ) ) ) {
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
     				if ( btnAlwaysOn( str ) || 
     					 ( cls != null && cls.getName().equalsIgnoreCase( str ) ) ) {
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
    	else if ( btnName.equalsIgnoreCase( cntlBtnNames[ 1 ] ) ||
    			  btnName.equalsIgnoreCase( "Apply" ) ) {  	      
    		applyProductType();  
    	}
    	else if ( btnName.equalsIgnoreCase( cntlBtnNames[ 2 ] ) ) {
   		    deleteProductType();
    	}
    	else {   	      
   		    objectGroup = null;
   		    close();
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
        close();
		if ( PgenSession.getInstance().getPgenPalette() != null ) {
			PgenSession.getInstance().getPgenPalette().resetPalette( null );              	
		}
        
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
    	
    	ProductType delType = getCurrentPrdTyp();
		int typeIdx = prdTyps.getProductType().indexOf( delType );
    	
    	if ( typeIdx >= 0  ) {
        	           
        	MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Confirm Delete a Product Type", null, 
            		"Are you sure you want to delete type " + findAlias( delType ) + "?",
            		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
            confirmDlg.open();
            
            if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
            	           	    
            	prdTyps.getProductType().remove( delType );
    		    saveProductTypes();
    		    
    		    //delete settings table for the product type
    		    try {
    		    	String pdName = typeText.getText();
    		    	String settings = getSettingFullPath(pdName);

    		    	if ( !settings.equalsIgnoreCase("settings_tbl.xml")){ //don't delete the default file

    		    		LocalizationContext userContext = PgenStaticDataProvider.getProvider().getLocalizationContext(
    		    				LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

    		    		LocalizationFile lFile = PgenStaticDataProvider.getProvider().getLocalizationFile( 
    		    				userContext, settings);

    		    		lFile.delete();
    		    	}
    		    }
    		    catch ( Exception e ){
    		    	e.printStackTrace();

    		    }
    		    
    		    int typeAt = -1;
    		    int jj = 0;
    		    for ( String tp : typeCombo.getItems() ) {
                    if ( tp.equals( delType.getType() ) ) {
    		    		typeAt =  jj;
    		    	}
    		    	jj++;
    		    }
    		    
           	    ArrayList<String> stypes = findSubtypes( delType.getType() );
            	if ( stypes.size() == 0  ) {
                	typeCombo.remove( delType.getType() );
        		    
                	int next = Math.min( typeAt, typeCombo.getItemCount() - 1 );
        		    typeCombo.select( next );
        		        
        		    subtypeCombo.removeAll();
        		    subtypeCombo.add( "New" );
        		    ArrayList<String> sntypes = findSubtypes( typeCombo.getText() );
        		    for ( String st : sntypes ) {
        		    	subtypeCombo.add( st );
        		    }
        		    
        		    if ( sntypes.size() > 0 ) {
        		        subtypeCombo.select( 1 );
        		        subtypeText.setText( subtypeCombo.getText() );
       		        }
        		    else {
        		    	subtypeCombo.select( 0 );
        		    }
               	
            	}
            	else {
            		
        		    int subtypeAt = -1;
            		String ss = delType.getSubtype();
        		    jj = 0;
        		    for ( String stp : subtypeCombo.getItems() ) {
        		    	if ( stp.equals( ss ) ) {
        		    		subtypeAt =  jj;
        		    	}
        		    	jj++;
        		    }

            		if ( ss != null && ss.trim().length() > 0  ) {
            		    subtypeCombo.remove( delType.getSubtype() );
            	    }
                   	
            		int next = Math.min( subtypeAt, subtypeCombo.getItemCount() - 1 );
        		    subtypeCombo.select( next ); 
            	}
  		    
    		    switchProductType();
    		    
    		    shell.pack();
    		    shell.layout();
    		       		    
            }
       
    	}
    	
    	if ( prdTyps.getProductType().size() == 0 ) {
    		typeText.setText("");
    		subtypeText.setText("");
    	}
    	
    	typeCombo.pack();
    	subtypeCombo.pack();
    	typeCombo.getParent().pack();
    	activityGrp.pack();
     	
    } 
    
    /**
     *  Save product types.
     */
    private void saveProductTypes() { 

    	// TODO : write code to check the context of the current productTypes and 
    	// check if it is BASE/SITE/DESK and create a new USER context. (also copy over the 
    	// localization type.)

    	// create a USER Level Localization File. 
    	//
    	try {
    		// NOTE: if the current file is not from the USER level then this will not prompt 
    		// for a confirmation.
    		//
    		if( prdTypesFile.getContext().getLocalizationLevel() ==
    			     LocalizationLevel.USER ) {
    	    	MessageDialog confirmDlg = new MessageDialog( 
    	    	           PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    	    	           "Confirm?", null, "The Product Types File Exists\n"+
    	    	           "Do you want to overwrite it?",
    	    	        	MessageDialog.WARNING, new String[]{"Yes", "No"}, 0);
    	    	
				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
					return;
    	    	}
    		}
    		else {  // create a user-level context and create/get a new localization file

    			LocalizationContext userContext = PgenStaticDataProvider.getProvider().getLocalizationContext(
    					prdTypesFile.getContext().getLocalizationType(), 
    					LocalizationLevel.USER );
    			prdTypesFile = PgenStaticDataProvider.getProvider().getLocalizationFile( 
    					userContext, PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_PRODUCT_TYPES );
    		}
    		File ptypFile = prdTypesFile.getFile();
    		
    		if( ptypFile == null ) {
    			throw new VizException( "Unable to create Localization File");
    		}
    		
    		FileTools.write( ptypFile.getAbsolutePath(), prdTyps );
    		
    		try {
    			prdTypesFile.save();
			} catch (LocalizationOpFailedException e) {
				throw new VizException( e );
			}
    	}
    	catch (VizException ve ) {
    		MessageDialog errDlg = new MessageDialog( 
	    	           PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
	    	           "Error", null, "Error saving Product Types.\n"+ve.getMessage(),
	    	        	MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    	}
    } 
  
    
    /**
     *  Update changes to the current product type.
     */
    private boolean updateProductTypes() {
    	
    	ProductType pTyp = null; 	
        boolean validNewTypeName = true;
    	
    	String typeCmbName = typeCombo.getText();
		String typeInputName = typeText.getText();
		
		String subtypeCmbName = subtypeCombo.getText();
		String subtypeInputName = subtypeText.getText();
		
		String aliasName = aliasText.getText();
						
		validNewTypeName = validatePrdTypName( typeCmbName, typeInputName, subtypeCmbName,
											   subtypeInputName, aliasName );
		
		if (  validNewTypeName ) {
	     							
			pTyp = findProductType( typeInputName, subtypeInputName );
			int existingAt = -1;
			if ( pTyp != null ) {  //edit an existing one
			    existingAt = prdTyps.getProductType().indexOf( pTyp ) ;
			}
			else {  //copy from an existing one or start new
				pTyp = copyProductType( findProductType( typeCmbName, subtypeCmbName ) );
				if ( pTyp == null ) {
					pTyp = new ProductType();
				}
			}
			           
			//update type, subtype, and alias
    	    pTyp.setType( new String( typeInputName ) );  
		    if ( subtypeInputName != null && subtypeInputName.trim().length() > 0 ) {
		    	pTyp.setSubtype( new String( subtypeInputName ) );
		    }
		    else {
		    	pTyp.setSubtype( DEFAULT_SUBTYPE );
		    }
		    
		    if ( aliasName != null && aliasName.trim().length() > 0 ) {
		    	pTyp.setName( aliasName );
		    }
		    else {
		    	pTyp.setName( "" );
		    }
   	    
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
			else if ( getCurrentTabName().equalsIgnoreCase("Clip") ){
				updateClip( pTyp );
			}
			else if ( getCurrentTabName().equalsIgnoreCase("Products") ){
				pTyp.getProdType().clear();
				if ( ptList != null ) pTyp.getProdType().addAll( ptList );
				if ( existingAt < 0 ) updatePalette( pTyp );
			}
			
    	    if ( existingAt < 0 ) {
				    	    	
    	    	ArrayList<String>  allTypes = findTypes();   	    	
    	    	prdTyps.getProductType().add( pTyp );
    	    	    	    	
    	    	//Update type combo    	    	
                if ( !allTypes.contains( typeInputName ) )  {
                	
                	typeCombo.add( typeInputName );
                    typeCombo.pack();
    	    		typeCombo.select( typeCombo.getItemCount() - 1 );
    	    		
    	    		//refresh subtypes for the new type.
            		subtypeCombo.removeAll();
            		subtypeCombo.add( "New" );            		
            		ArrayList<String> stypes = findSubtypes( typeInputName );                    
 
            		for ( String stp : stypes ) {
    	    			subtypeCombo.add( stp );
    	    		}
                 
            		int select= stypes.indexOf( pTyp.getSubtype() );
            		select++;  //Considering "New" is always the first;
            		
           		    subtypeCombo.select( select );
                    subtypeText.setText( subtypeCombo.getText() );
                }
                else { //add this new subtype to the existing one 
                    
                	int ii = 0;
                	for ( String tp : typeCombo.getItems() ) {
                		if ( tp.equals( typeInputName ) ) {
                			typeCombo.select( ii );
                			break;
                		}
                		ii++;
                	}
                	
                	String subtype = pTyp.getSubtype();
                    subtypeCombo.add( subtype );
                    subtypeText.setText( subtype );

                    subtypeCombo.pack();
                    subtypeCombo.select( subtypeCombo.getItemCount() - 1 );
                }
               
     	    	typeCombo.getParent().pack();
     	    	
    	    	//Set the PgenSave info.
    	    	updateSaveInfo( pTyp );
    	    	String fname = createDefaultOutputFile();
    	    	pTyp.getPgenSave().setOutputFile( fname );
       		    saveOutputTxt.setText( fname );

    	    }
    	    else {
    	    	prdTyps.getProductType().set( existingAt, pTyp ) ;	    	
    	    }
    	    
    	    aliasText.setText( pTyp.getName() );
    	    
    	    activityGrp.pack();
 	    	   	    
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
            ProductType ptype = getCurrentPrdTyp();
            for ( String str : objects ) {       	
            	
                objectBtns[ ii ] = new Button( objectGroup, SWT.TOGGLE );
                objectBtns[ ii ].setData( str ); 
                objectBtns[ ii ].setImage( iconMap.get( str ) );
                objectBtns[ ii ].setSelection( btnAlwaysOn( str ) );
                           
                objectBtns[ ii ].setToolTipText( itemMap.get( str ).getAttribute("label") );
                  
                ArrayList<String>  objStr = new ArrayList<String>();
                if ( ptype != null && ptype.getPgenClass() != null ) {            		
                	for ( PgenClass pclass : ptype.getPgenClass() ) {
                		if ( pclass.getName().equalsIgnoreCase( className ) ) {
                			for ( int kk = 0; kk < pclass.getPgenObjects().getName().size(); kk++ ) {
                				if ( pclass.getPgenObjects() != null ) {
                				    objStr.add( pclass.getPgenObjects().getName().get(kk) );
                				}
                			}
                		}
                	}
                }                               
            
                if ( objStr.contains( str ) ) {
                	// if ( objStr.size() == 0 || btnAlwaysOn( str ) || objStr.contains( str ) ) {
                	objectBtns[ ii ].setSelection( true );
                	objectBtns[ ii ].setImage( iconMapSelected.get( str ) );                    
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
            
            boolean allOff = true;
            for ( Button btn : objectBtns ) {
            	if ( btn.getSelection() == true ) {
            		allOff = false;
            		break;
            	}
            }
            
            allOnOffBtn = new Button( objectGroup, SWT.PUSH );
            setButtonColor( allOnOffBtn, remindButtonColor );
            
            if ( allOff ) {
                allOnOffBtn.setText( "ON "  );
                allOnOffBtn.setToolTipText( "Select ALL Objects" );       
                allOnOffBtn.setData( "ONN" );
            }
            else {
                allOnOffBtn.setText( "OFF"  );
                allOnOffBtn.setToolTipText( "Deselect ALL Objects" );       
                allOnOffBtn.setData( "OFF" );            	
            }
            
            allOnOffBtn.addSelectionListener( new SelectionAdapter() {
            	public void widgetSelected(SelectionEvent event) {                           	
            		String status = event.widget.getData().toString();
            		if ( status.equalsIgnoreCase( "ONN" ) ) {
            			allOnOffBtn.setText( "OFF"  );
                        allOnOffBtn.setToolTipText( "Deselect ALL Objects" );       
                        allOnOffBtn.setData( "OFF" );
                        
                        for ( Button btn : objectBtns ) {
                            btn.setSelection( true );
                            btn.setImage( iconMapSelected.get( btn.getData().toString() ) );                    
                        }
            		}
            		else {
                        allOnOffBtn.setText( "ON "  );
                        allOnOffBtn.setToolTipText( "Select ALL Objects" );       
                        allOnOffBtn.setData( "ONN" );         			           			
                        
                        for ( Button btn : objectBtns ) {
                            btn.setSelection( false );
                            btn.setImage( iconMap.get( btn.getData().toString() ) );                    
                        }
           		    }
            	}
            });           


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
        typeLbl.setText("Type:");
        
        typeCombo = new Combo( typeComp, SWT.DROP_DOWN | SWT.READ_ONLY );       
               
        typeCombo.add( "New" );        
        for ( String st : findTypes() ) {
            typeCombo.add( st );      
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
        typeText.setLayoutData( new GridData( 150, 15 ) );
        typeText.setEditable( true );   
        typeText.setText( "" );

        // Subtype
        Label stypeLbl = new Label( typeComp, SWT.LEFT);
        stypeLbl.setText("Subtype:");
        
        subtypeCombo = new Combo( typeComp, SWT.DROP_DOWN | SWT.READ_ONLY );       
               
        subtypeCombo.add( "New" );        
                 
        subtypeCombo.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
           	    switchProductSubtype();
            }
        });
        
        subtypeCombo.select( 0 );
        
        subtypeText = new Text( typeComp,  SWT.SINGLE | SWT.BORDER );                        
        subtypeText.setLayoutData( new GridData( 150, 15 ) );
        subtypeText.setEditable( true );   
        subtypeText.setText( "" );

        // Alias
        Label aliasLbl = new Label( typeComp, SWT.LEFT);
        aliasLbl.setText("Alias:");
               
        aliasText = new Text( typeComp,  SWT.SINGLE | SWT.BORDER );                        
        aliasText.setLayoutData( new GridData( 90, 15 ) );
        aliasText.setEditable( true );   
        aliasText.setText( "" );
       
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
        	
        	ProductType ptype = getCurrentPrdTyp();
        	
        	if ( ptype == null ) {       		
        		objList = (ArrayList<String>)PgenSession.getInstance().getPgenPalette().getObjectNames( className );
        	}
        	else {    			
				PgenClass pclass = null;  				
			    if ( ptype != null && ptype.getPgenClass() != null ) { 		
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
//	    layerOnOffBtn.setSelection( true );     	          
	    layerOnOffBtn.setSelection( false );     	          
		        
	    layerMonoBtn = new Button( dispComp, SWT.CHECK );
	    layerMonoBtn.setText( "A/M" );
        
	    layerColorSelector = new ColorButtonSelector( dispComp );
        Color clr = Color.RED;
        layerColorSelector.setColorValue( new RGB( clr.getRed(),  clr.getGreen(), clr.getBlue() ) );
	                  
	    layerFillBtn = new Button( dispComp, SWT.CHECK );
	    layerFillBtn.setText( "Filled" );
	    
	    Composite metaComp = new Composite( layerEditGrp, SWT.NONE );
        GridLayout glMeta = new GridLayout( 2, false );
        glMeta.marginWidth = 3;
        metaComp.setLayout( glMeta );
	    
        Label metaLbl = new Label( metaComp, SWT.LEFT );
        metaLbl.setText("Meta-Info:");
        
        metaTxt = new Text( metaComp,  SWT.SINGLE | SWT.BORDER );                        
        metaTxt.setLayoutData( new GridData( 190, 15 ) );
        metaTxt.setEditable( true );   
        metaTxt.setText( "" );
        
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
    	    nameBtn.setText( lyr.getName().replace("&", "&&") );
	    	setButtonColor( nameBtn, defaultButtonColor );	
    	        	    
    	    nameBtn.setData( ii );
    	    nameBtn.addSelectionListener( new SelectionAdapter() {
    	        public void widgetSelected(SelectionEvent event) {                		    	        	
                    switchLayer( ((Button)event.widget).getText().replace("&&", "&") );    	        		
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
              		
		Label currentSettingLbl = new Label( parent, SWT.NONE );
		currentSettingLbl.setText( "Settings: ");

		currentSettingsTxt = new Text( parent,  SWT.SINGLE | SWT.BORDER );
		//currentSettingsTxt.setLayoutData( new RowData( 300, 15 ) );
		currentSettingsTxt.setEditable( false );
	    currentSettingsTxt.setText( "Localization\\NCP\\PGEN\\" + this.getCurrentSetting( typeText.getText()) );
	    currentSettingsTxt.setEnabled(false);
		
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
        
        saveOutputTxt.setText( createDefaultOutputFile() );

        Button nameBtn = new Button( nameComp, SWT.PUSH );
        nameBtn.setText( "Browse");

        /*
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
        */
        
        nameBtn.setEnabled( false );
 
        
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

        Label flagLbl = new Label( titleComp, SWT.NONE );
        flagLbl.setText( "Clip the Product:");
        
        clipChkBox = new Button( titleComp, SWT.CHECK );
        clipChkBox.addSelectionListener( new SelectionListener(){

			@Override
			public void widgetSelected(SelectionEvent e) {
		        if ( ((Button)e.widget).getSelection()){
		        	boundsListCbo.setEnabled(true);
		        	boundsNameCbo.setEnabled(true);
		        }
		        else {
		        	boundsListCbo.setEnabled(false);
		        	boundsNameCbo.setEnabled(false);
		        }
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}
        	
        });
        
        
        Label boundsListLbl = new Label( titleComp, SWT.NONE );
        boundsListLbl.setText( "Bounds Table: ");
        
        boundsListCbo= new Combo( titleComp, SWT.DROP_DOWN | SWT.READ_ONLY );
		
		int init = 0;
		int ii = 0;
        for ( String str : PgenStaticDataProvider.getProvider().getBoundsTableList() ) {
        	boundsListCbo.add(str);
        	if ( str.equals("bwus_bnd")) init = ii;
        	ii++;
        }
        boundsListCbo.select( init );
        
        boundsListCbo.addSelectionListener( new SelectionListener(){

			@Override
			public void widgetSelected(SelectionEvent e) {
		        populateBoundsNames( ((Combo)e.widget).getText());
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}
        	
        });
        
        Label boundsNameLbl = new Label( titleComp, SWT.NONE );
        boundsNameLbl.setText( "Bounds Name: ");
        
        boundsNameCbo = new Combo( titleComp, SWT.DROP_DOWN | SWT.READ_ONLY );
		
        populateBoundsNames( boundsListCbo.getText());
        
        
        /*
         * Input boundary file name
         */
  /*      GridLayout mainLayout = new GridLayout( 3, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
		               
        Composite nameComp = new Composite( parent, SWT.NONE );
		       
        nameComp.setLayout(mainLayout);
        
        Label nameLbl = new Label( nameComp, SWT.NONE );
        nameLbl.setText( "Clip Bounds: ");
 
        
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
    */    
        if (!clipChkBox.getSelection()){
        	boundsListCbo.setEnabled(false);
        	boundsNameCbo.setEnabled(false);
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
/*		if ( ptyp.getPgenClass() != null ) {
			 ptyp.getPgenClass().clear();
		}
*/		
		ArrayList<PgenClass> pClassList = new ArrayList<PgenClass>();
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
					
					pClassList.add( pClass );
					
				}
				
//				pClassList.add( pClass );
//			    ptyp.getPgenClass().add( pClass );	
				   			        
		    }
		
		    ii++;
		
	    }
		
		if ( ptyp.getPgenClass() != null ) {
		     ptyp.getPgenClass().clear();
	    }
		
		for ( PgenClass pcls : pClassList ) {
			ptyp.getPgenClass().add( pcls );
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
			gov.noaa.nws.ncep.ui.pgen.producttypes.Color pclr = 
				new gov.noaa.nws.ncep.ui.pgen.producttypes.Color();
			pclr.setAlpha( 255 );
			pclr.setRed( layerRGB.red );
			pclr.setGreen( layerRGB.green );
			pclr.setBlue( layerRGB.blue );

			pgenlyr.setColor( pclr );

			pgenlyr.setFilled( layerFillBtn.getSelection() );
			pgenlyr.setMonoColor( layerMonoBtn.getSelection());
			pgenlyr.setName( inputName );
			pgenlyr.setOnOff(layerOnOffBtn.getSelection() );
			pgenlyr.setMetaInfo(metaTxt.getText());

			//Add or replace using the new PgenLayer	    	     	   	   	    	       	        		    
			ProductType oldType = getCurrentPrdTyp();
			if ( oldType == null || ( ptyp.getType().equals( oldType.getType() ) &&
					                  ptyp.getSubtype().equals( oldType.getSubtype() ) ) ) {
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
    	       
        if ( inputName == null || inputName.trim().length() == 0 ) {		    
        	validNewLayerName = false;	
        	msg = "Layer name cannot be empty.\n";      	
        	
//        	return validNewLayerName;
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
    	           "Confirm a new Layer Name", null, msg + "Please specify another valid name.",
    	        	MessageDialog.WARNING, new String[]{"OK"}, 0);
    	        
    	    confirmDlg.open();   
		}
		
		return validNewLayerName;

	}
	
	/**
	 *  Validate the type/subtype/alias inputs for the product type.
	 *  
	 * @param typeName
	 * @param typeInputName 
	 * @param subtypeName
	 * @param subtypeInputName 
	 * @param alias Name 
	 * 
	 * @return boolean - if the type/subtype/alias are valid  
	 */
	private boolean validatePrdTypName( String typeName, String typeInputName,
										String subtypeName, String subtypeInputName,
										String aliasName ) {
    			
        boolean isValid = true;
        StringBuilder msg = new StringBuilder();
        
        /*
         * 1. Type cannot be null, empty, or any variations of "New".
         * 2. Type cannot be duplicated.
         * 3. Subtype cannot be any variations of "New" or duplcated within a Type.
         * 4. Alias can be empty, but cannot be duplicated. It links to
         *    a product type/subtype combo.  When it is null or empty, 
         *    we will use "type[(subtype)]" as the alias (subtype presented
         *    only if it is not null and empty.
         */
    	if ( typeInputName == null || typeInputName.trim().length() == 0 ) {		    
    		isValid = false;	
    		msg.append( "Activity type name cannot be blank.\n" );
    	}
    	else if ( typeInputName.equalsIgnoreCase( "New" )   ) {
    		isValid = false;	
    		msg.append( "Activity type name cannot be any variations of 'New'.\n" );      	
    	} 
    	else if ( subtypeInputName != null && subtypeInputName.trim().length() > 0 && 
    			  subtypeInputName.equalsIgnoreCase( "New" ) ) {
    		isValid = false;	
    		msg.append( "Activity subtype name cannot be any variations of 'New'.\n" );      	    		
    	}
    	else if ( aliasName != null && aliasName.trim().length() > 0 ) {    		   	

    		ProductType ityp = findProductType( typeInputName, subtypeInputName );
    		for ( ProductType ptp : prdTyps.getProductType() ) {
    			if  ( ityp == null && ptp.getName().equalsIgnoreCase( aliasName ) ) {
    				
    				msg.append( "Alias name '" + aliasName + "' has been used.\n" );
    				isValid = false;
    				break;
   				
    			}
    		} 
    	}
    	
        /*
         * When creating a new activity (typeCombo = "New" or subtypeCombo = "New"),
         * the new activity "typeInputText[subtypeInputText]" can not be the same as
         * activity identified by "typeCombo".    
         */
        ProductType ptype = null;   	    
        if ( isValid ) {
        	if ( typeName.equalsIgnoreCase( "New" ) ||
        		 subtypeName.equalsIgnoreCase( "New" )	) { 

        		ptype = findProductType( typeInputName, subtypeInputName );
       		    if ( ptype != null ) {       		
               	    isValid = false;
        			String aName = new String( typeInputName );
        			if ( subtypeInputName != null && subtypeInputName.trim().length() > 0 ) {
        				aName = new String( aName + "(" + subtypeInputName + ")" );
        			}
 
        			msg.append( "Activity name '" + aName + "' has been used.\n" );       			
        		} 
        	}
        }
        
        /*
         * When the activity identified by "typeText[subtypeText]" does not exist, we can 
         * create a new activity from the one identified by "typeCombo[subtypeCombo]".
         * 
         * When the activity identified by "typeText[subtypeText]" exists and is the same 
         * as the one identified by "typeCombo[subtypeCombo]", we can update this activity.
         * 
         * Otherwise, "typeText[subtypeText]" is an invalid activity combo.
         * 
         */
        if ( isValid )  {
        	ptype = findProductType( typeInputName, subtypeInputName );
        	if ( ptype != null ) {
        	    ProductType otype = findProductType( typeName, subtypeName );
        	    if ( !(ptype.equals( otype )) ) {       		
        			isValid = false;
        			String aName = new String( typeInputName );
        			if ( subtypeInputName != null && subtypeInputName.trim().length() > 0 ) {
        				aName = new String( aName + "(" + subtypeInputName + ")" );
        			}
        			msg.append( "Activity name '" + aName + "' has been used.\n" );       	
        	    }
        	}
        }   	   	
    	
        if ( !isValid ) {
    		MessageDialog confirmDlg = new MessageDialog( 
    				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    				"Confirm a new Type Name", null, msg + "Please specify another valid name.",
    				MessageDialog.WARNING, new String[]{"OK"}, 0);

    		confirmDlg.open();   
    	}
		
		return isValid;

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
    	String curPrdSubtypeName = subtypeCombo.getText();
    	
        ProductType curPrdType = findProductType( curPrdtypeName,  curPrdSubtypeName );

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
					 if (pgenlyr.getMetaInfo() != null) {
						 metaTxt.setText(pgenlyr.getMetaInfo());
					 }
					 else {
						 metaTxt.setText("");
					 }
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
		        	 if ( btn.getText().replace("&&", "&").equals( layerName ) ) {
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
//		layerOnOffBtn.setSelection( true );
		layerOnOffBtn.setSelection( false);
		layerMonoBtn.setSelection( false);
			
		Color layerClr = Color.YELLOW;
		layerColorSelector.setColorValue( 
			    new RGB( layerClr.getRed(), layerClr.getGreen(), layerClr.getBlue()) );
			
		layerFillBtn.setSelection( false );
		metaTxt.setText("");	
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
   	    
		if ( typeCombo.getSelectionIndex() == 0 ) { //"New"
			typeText.setText( "" );
		}
		
		//Update subtype menu
        subtypeCombo.removeAll();        
		
        subtypeCombo.add( "New" );       
		ArrayList<String> stypes = findSubtypes( typeCombo.getText() );
		for ( String stp : stypes ) {
			subtypeCombo.add( stp );
		}
		
		subtypeCombo.pack();
		if ( stypes.size() > 0 ) {
		    subtypeCombo.select( 1 );
			subtypeText.setText( subtypeCombo.getText() );
		}
		else {
		    subtypeCombo.select( 0 );			
		    subtypeText.setText( "" );			
		}
				
		ProductType ctype = getCurrentPrdTyp();
		if ( ctype != null && ctype.getName() != null ) {
			aliasText.setText( ctype.getName() );
		}
		else {
			aliasText.setText( "" );
		}
				
		//Update palette selections from the new type
		createObjects( "None", true );
    	refreshPaletteSelections();
    	
    	//Update layer definitions from the new type
	    editLayeringTab();   
    	
	    //Update Save definitions from the new type
	    editSaveTab();   

		//update Pgen settings
	    editSettingsTab();
	    
		//update Pgen clip tab
	    editClipTab();
	    
	    //update Product tab
	    editProductsTab();
	    
	    activityGrp.pack();
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
        
        String tmpFile = createDefaultOutputFile();        
        
        if ( curPrdType != null && curPrdType.getPgenSave() != null ) {
    		
        	PgenSave pSave = curPrdType.getPgenSave();
        	if (  pSave != null ) {
        	    String outFile = pSave.getOutputFile();
       	        if ( outFile != null && outFile.length() > 7 &&
       	        	 !outFile.substring(0, 7 ).equalsIgnoreCase( "Default") ) {
        		    saveOutputTxt.setText( outFile );
        	    }
        	    else {
           		    saveOutputTxt.setText( tmpFile );
        	    }
        	
        	    saveIndividualLayerBtn.setSelection( pSave.isSaveLayers() );
            	autoSaveBtn.setSelection( true );       		
            	autoSaveFreqTxt.setText( "" );

        	}
        }
        else {               
        	saveOutputTxt.setText( tmpFile );

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
	    currentSettingsTxt.setText( "Localization\\NCP\\PGEN\\" + this.getCurrentSetting( typeText.getText()) );
        
        if ( curPrdType != null && curPrdType.getPgenSettingsFile() != null ) {
    		
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
     *  Edit/Update the Pgen clipping
     */    
    private void editClipTab() {
        
        ProductType curPrdType = getCurrentPrdTyp();
        
        if ( curPrdType != null && curPrdType.getClipFlag() != null && curPrdType.getClipFlag() ) {
        	
        	clipChkBox.setSelection(true);
        	boundsListCbo.setEnabled(true);
        	boundsNameCbo.setEnabled(true);
        	
        	if ( curPrdType.getClipBoundsTable() != null ){
        		boundsListCbo.setText( curPrdType.getClipBoundsTable() );
        		if ( curPrdType.getClipBoundsName() != null ){
        	        populateBoundsNames( boundsListCbo.getText());
            		boundsNameCbo.setText( curPrdType.getClipBoundsName() );
        		}
        	}
    		
        }
        else {
        	
        	clipChkBox.setSelection(false);
        	boundsListCbo.setEnabled(false);
        	boundsNameCbo.setEnabled(false);
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
	 * Update clip information for a given type
	 * 
	 * @param ptyp
	 */
	private void updateClip( ProductType ptyp ) {
	
		String pdName = typeText.getText(); 
		if ( pdName == null || pdName.isEmpty() ){
			return;
		}
		
		ptyp.setClipFlag( clipChkBox.getSelection() );
		if (clipChkBox.getSelection() ){
			ptyp.setClipBoundsTable( boundsListCbo.getText());
			ptyp.setClipBoundsName(boundsNameCbo.getText());
		}
		
	}
	
	/**
	 * Update Pgen Settings for a given type
	 * 
	 * @param ptyp
	 */
	private void updateSettings( ProductType ptyp ) {
	
		String pdName = typeText.getText(); 
		if ( pdName == null || pdName.isEmpty() ){
			return;
		}
		
		//Update settings 	    	     	   	   	    	       	        		    
	    String settingsFile = settingsTxt.getText();
	    if ( settingsFile != null && !settingsFile.isEmpty()) {
		    File sFile = new File(settingsFile);

		    if ( !sFile.canRead() || FileTools.validate(settingsFile, null) ){
       			MessageDialog infoDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Warning", null, "Pgen settings file " + settingsFile +" does not exist or is invalid!",
						MessageDialog.WARNING, new String[]{"OK"}, 0);

				infoDlg.open();
		    }
		    else {
		    	ptyp.setPgenSettingsFile( "" );
		    	
		    	//put into localization
			    LocalizationContext userContext = PgenStaticDataProvider.getProvider().getLocalizationContext(
						LocalizationType.CAVE_STATIC, LocalizationLevel.USER );
			    
			    LocalizationFile lFile = PgenStaticDataProvider.getProvider().getLocalizationFile( 
			    		userContext, getSettingFullPath(pdName));
			    
			    //check file
			    InputStream is;
			    try {
			    	is = new FileInputStream( sFile );

			    	// Get the size of the file
			    	long length = sFile.length();

			    	// ensure that file is not larger than Integer.MAX_VALUE.
			    	if (length > Integer.MAX_VALUE) {
			    		return;
			    	}

			    	// Create the byte array to hold the data
			    	byte[] bytes = new byte[(int)length];

			    	// Read in the bytes
			    	int offset = 0;
			    	int numRead = 0;
			    	while (offset < bytes.length
			    			&& (numRead=is.read(bytes, offset, bytes.length-offset)) >= 0) {
			    		offset += numRead;
			    	}

			    	lFile.write( bytes );

			    } catch (FileNotFoundException e1) {
			    	// TODO Auto-generated catch block
			    	e1.printStackTrace();
			    } catch (LocalizationException e) {
			    	// TODO Auto-generated catch block
			    	e.printStackTrace();
			    } catch (IOException e) {
			    	// TODO Auto-generated catch block
			    	e.printStackTrace();
			    }
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
						
					//	try {
							//remove style sheets
					//		String xsltFile = ProdTypeDialog.getStyleSheetFileName( pt.getName());
					//		if ( xsltFile != null && !xsltFile.isEmpty()){
					//			LocalizationContext userContext = NcPathManager.getInstance().getContext(
					//					LocalizationType.CAVE_STATIC, LocalizationLevel.USER );

					//			LocalizationFile lFile = NcPathManager.getInstance().getLocalizationFile( 
					//					userContext, xsltFile);

					//			lFile.delete();
					//		}
					//	}
					//	catch (Exception e ){
					//		e.printStackTrace();
					//	}
    		    		
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
		
	/*
	 * Find a product type by a combo of type and subtype.
	 */
	private ProductType findProductType( String type,  String subtype ) {
			
		ProductType atyp = null;
		String ss = null;
		if ( subtype == null || subtype.trim().length() == 0  ) {
			ss = new String( DEFAULT_SUBTYPE );
		}
		else {
		    ss = new String( subtype );
		}
		
		if ( type != null && type.trim().length() > 0 ) {
			for ( ProductType stype : prdTyps.getProductType() ) {
				if ( stype.getType().equals( type ) ) {
					if ( stype.getSubtype().equals( ss ) ) {
						atyp = stype;										
						break;						
					}
				}
			}
		}
				
		return atyp;

	}

	/*
	 * Find all major activity types defined
	 * 
	 * Note: an activity type can have 0 or more subtypes. 
	 */
	private ArrayList<String> findTypes() {
		
		ArrayList<String> types = new ArrayList<String>();
	    for ( ProductType type : prdTyps.getProductType() ) {
	    	if ( !types.contains( type.getType() ) ) {
	    		types.add( type.getType() );
	    	}
	    }
				
		return types;

	}

	/*
	 * Find all product subtypes defined for an activity
	 * 
	 * Note: an activity without "subtype" uses DEFAULT_SUBTYPE "none".
	 */
	private ArrayList<String> findSubtypes( String type ) {
		
		ArrayList<String> subtypes = new ArrayList<String>();
		if ( type != null && type.trim().length() > 0 ) {
			for ( ProductType stype : prdTyps.getProductType() ) {
				if ( stype.getType().equals( type ) ) {
					String stp = stype.getSubtype();
					if ( stp != null && stp.trim().length() > 0 ) {
					    subtypes.add( stype.getSubtype() );
					}
					else {
						subtypes.add( DEFAULT_SUBTYPE );
					}
				}
			}
		}
				
		return subtypes;

	}
	
	/*
	 * Find alias for an activity type.
	 */
	private String findAlias( ProductType ptype ) {
		String alias = null;
		if ( ptype != null ) {
		   alias = ptype.getName();
		   if ( alias == null || alias.trim().length() == 0 ) {
			   alias = new String( ptype.getType() );
			   String stp = ptype.getSubtype();
			   if ( stp != null && stp.trim().length() > 0 ) {
				   alias = new String( alias + "(" + stp + ")" );
			   }
		   }
		}
		
		return alias;
		
	}
	
	/**
	 * Switch from one product subtype type to another.
	 * 
	 * @param 
	 * @return 
	 */
	private void switchProductSubtype() {				
   	    		
		ProductType ctype = getCurrentPrdTyp();
		
		subtypeText.setText( "" );
		if ( ctype != null && ctype.getSubtype() != null ) {
			subtypeText.setText( ctype.getSubtype() );
		}
		else {
			subtypeText.setText( "" );
		}
		
		if ( ctype != null && ctype.getName() != null ) {
			aliasText.setText( ctype.getName() );
		}
		else {
			aliasText.setText( "" );
		}
				
		//Update palette selections from the new subtype
		createObjects( "None", true );
    	refreshPaletteSelections();
    	
    	//Update layer definitions from the new subtype
	    editLayeringTab();   
    	
	    //Update Save definitions from the new subtype
	    editSaveTab();   

		//update Pgen settings
	    editSettingsTab();
	
		//update Pgen clipping tab
	    editClipTab();
	    
	    //update Product tab
	    editProductsTab();
    }
	
	/**
	 * Create a default output file name in the following pattern.
	 * 
	 * type.[subtype].DDMMYYYY.HH.xml
	 * 
	 * @param 
	 * @return 
	 */
	private String createDefaultOutputFile() {
		
		StringBuilder fname = new StringBuilder();
		
		ProductType ptype = getCurrentPrdTyp();
		if ( ptype == null ) {
			fname.append( "Default." );
		}
		else {
			
			fname.append( ptype.getType().replace( ' ', '_' ) + "." );
			if ( ptype.getSubtype() != null && ptype.getSubtype().trim().length() > 0 &&
				 !ptype.getSubtype().equalsIgnoreCase( DEFAULT_SUBTYPE ) )	 {
				fname.append( ptype.getSubtype().replace( ' ', '_' ) + "." );				
			}			
		}

		fname.append( "DDMMYYYY.HH.xml" );
						
		return fname.toString();
	}
	
	/**
	 * Get the full path of the setting file in localization.
	 * @param prodType
	 * @return
	 */
	static public String getSettingFullPath( String prodType ){
		
		return PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() +  getSettingFileName( prodType );
	}
	
	/**
	 * Gets the name of the settings file(only file name, not full path)
	 * @param prodType
	 * @return
	 */
	static public String getSettingFileName( String prodType ){
		String ret = "settings_tbl.xml";
		if ( prodType == null || prodType.isEmpty() ) return ret;
		
		String pt = prodType.replaceAll(" ", "_");
		
		return "settings_tbl_" + pt + ".xml";
	}
	
	/**
	 * Gets the current settings file name and the localization level(Base/Site/Desk/User)
	 * @param prodType
	 * @return
	 */
	private String getCurrentSetting( String prodType ){
		File settingsFile = PgenStaticDataProvider.getProvider().getStaticFile( 
				 getSettingFullPath( prodType));
		
		String fileName = "";
		if ( settingsFile == null || !settingsFile.exists() ){
			fileName = "settings_tbl.xml";
			settingsFile = PgenStaticDataProvider.getProvider().getStaticFile( 
					PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + AttrSettings.settingsFileName);
		}
		else {
			fileName = getSettingFileName( prodType );
		}
		
		String path = settingsFile.getAbsolutePath();
		if ( path.contains( "user") ){
			fileName += "(USER)";
		}
		else if ( path.contains( "desk") ){
			fileName += "(DESK)";
		}
		else if ( path.contains( "site") ){
			fileName += "(SITE)";
		}
		else {
			fileName += "(Base)";
		}
		
		return fileName;
	}
	
	/**
	 * Make a deep copy of a given activity type definition
	 * @param prodType
	 * @return
	 */
	private ProductType copyProductType( ProductType typeIn ){
	    
		if ( typeIn == null ) {
			return null;
		}
		
		ProductType outType = new ProductType();

		outType.setName( nvl( typeIn.getName() ) );
		
		outType.setType( nvl( typeIn.getType() ) );
		
		outType.setSubtype( nvl( typeIn.getSubtype() ) );
		
		outType.setPgenSettingsFile( nvl( typeIn.getPgenSettingsFile() ) );
		
		outType.setClipFlag( typeIn.getClipFlag() );
		
	    //Copy PgenActions
		PgenActions pact = new PgenActions();
		for ( String name : typeIn.getPgenActions().getName() ) {
		    pact.getName().add( new String( name ) );
		}
		outType.setPgenActions( pact );

	    //Copy PgenControls
		PgenControls pcntl = new PgenControls();
		for ( String name : typeIn.getPgenControls().getName() ) {
		    pcntl.getName().add( new String( name )  );
		}
		outType.setPgenControls(  pcntl );		

	    // Copy  List<PgenClass>		
		for ( PgenClass pcs : typeIn.getPgenClass() ) {
			PgenClass pcls = new PgenClass();
		    pcls.setName( new String( pcs.getName() )  );

		    PgenObjects pobj = new PgenObjects();
		    
		    for ( String obj : pcs.getPgenObjects().getName() ) {
			    pobj.getName().add( new String( obj ) );		    	
		    }
		    
		    pcls.setPgenObjects( pobj );
		    
			outType.getPgenClass().add( pcls );				    		    
		}	
		
	    //Copy List<PgenLayer>
		for ( PgenLayer plyr : typeIn.getPgenLayer() ) {
			PgenLayer lyr = new PgenLayer();
			gov.noaa.nws.ncep.ui.pgen.producttypes.Color clr = 
				       new gov.noaa.nws.ncep.ui.pgen.producttypes.Color();
			clr.setAlpha( plyr.getColor().getAlpha() );
			clr.setBlue( plyr.getColor().getBlue() );
			clr.setGreen( plyr.getColor().getGreen() );
			clr.setRed( plyr.getColor().getRed() );
			
			lyr.setColor( clr );

			lyr.setFilled( plyr.isFilled() );
            
			lyr.setInputFile( nvl( plyr.getInputFile() ) );
			
			lyr.setMonoColor( plyr.isMonoColor() );
			
			lyr.setName( nvl( plyr.getName() ) );
			lyr.setOnOff( plyr.isOnOff() );
			
			lyr.setOutputFile( nvl( plyr.getOutputFile() ) );
						
			outType.getPgenLayer().add( lyr );	
		}

	    //Copy PgenSave
		PgenSave psave = new PgenSave();
		psave.setAutoSave( typeIn.getPgenSave().isAutoSave() );
		psave.setAutoSaveFreq( typeIn.getPgenSave().getAutoSaveFreq() );		
		psave.setInputFile( nvl( typeIn.getPgenSave().getInputFile() ) );
	    psave.setOutputFile( nvl( typeIn.getPgenSave().getOutputFile() ) );			
		psave.setSaveLayers( typeIn.getPgenSave().isAutoSave() );
		
		outType.setPgenSave( psave );
		
        //Copy List<ProdType>
		for ( ProdType ptype : typeIn.getProdType() ) {
		    ProdType ptp = new ProdType();
		    ptp.setName( new String( ptype.getName() ) );
	    	ptp.setOutputFile( nvl( ptype.getOutputFile() ) );
	    	ptp.setStyleSheetFile( nvl( ptp.getStyleSheetFile() ) );		    		    
		    ptp.setType( nvl( ptype.getType() ) );
			
		    outType.getProdType().add( ptp );
		}
		
	    return outType;
	}
    
	/*
	 * Return a non-null new string
	 */
	private static String nvl(String value) {
		return value == null ? "" : (new String( value ) );
	}
	
    /**
     * Create a new primary_modal shell for this dialog.
     * 
     * @return Return shell.
     */
	@Override
	protected Shell createShell( Shell parent ) {
        Shell newShell = new Shell( parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL );
    	return newShell;
    }

	/**
	 * Sets the bounds name combo from the database table.
	 * @param table
	 */
	private void populateBoundsNames( String table ){
		
		boundsNameCbo.removeAll();
        for ( String str : PgenStaticDataProvider.getProvider().getBoundsNames( table )) {
        	boundsNameCbo.add(str);
        }
        boundsNameCbo.select(0);
	}

}
