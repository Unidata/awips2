/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageDialog
 * 
 * 11 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.io.File;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Scanner;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;

import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.nws.ncep.viz.common.ui.*;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;
import gov.noaa.nws.ncep.ui.pgen.file.*;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenSnapJet;

import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;


/**
 * Create a dialog to Open/Save PGEN product files.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	-----------------------------------
 * 02/09		#63			Jun Wu		Initial creation
 * 09/09		#151		Jun Wu		Allow multiple save (one product per
 *                                      file when having multiple products 
 *                                      in PgenResource
 * 03/10        #228        Archana     Added logic to sort files by name and date
 *                                      Removed the Booleans sortByName and sortByDate                          
 * 03/10        #226        Greg Hull   Change dirLIst to ListViewer with a SubDirContentProvider
 * 04/10		#165		Gang Zhang	Add Non-drawable Volcano Product handling
 * 08/10        #273        Greg Hull   ResourceDefnsMngr to get PGEN resources.
 * 08/10		#63			Jun Wu	    Roll back to the original design - Local will be the
 * 										location where the application starts and no "PRM" files
 * 										for PGEN product files.	
 * 10/10		#151		Jun Wu	    Allow open/save for multiple layers defined through 
 * 										product	configuration
 * 10/10		#151		Jun Wu	    Redefined "Append" and added "Merge"
 * 09/11		#335		Jun Wu	    Implemented the file structure to organize PGEN files.
 * 08/11        #450        Greg Hull   Call ResourceDefnsMngr to get list of PGEN Resource Defns
 * 10/11		?			B. Yin		Fixed the problem of jet flight level text location 
 * 										when zooming after loading file.
 * 10/11		?			Jun Wu	    Add capability to load legacy layer product file (LPF).
 * 12/11		#540		Jun Wu	    Enhanced the capability to load layer product file (LPF).
 * 08/12		#593		Jun Wu	    Add "Advanced" capability to import layers selectively.
 * 										
 * </pre>
 * 
 * @author
 * @version 1
 */
public class PgenFileManageDialog1 extends CaveJFACEDialog {
    
	public static enum PgenFileMode { OPEN, SAVE, SAVE_AS, SAVE_ALL };
	
	private String title = null;
    
    private Shell shell;

    private Button sortByNameBtn = null;
    private Button sortByDateBtn = null;
    
    private List dirList = null;
    private ListViewer fileListViewer= null; 
    private static Text fileNameText = null;
    
    private Button browseBtn = null;
    private FileDialog browseDlg = null;
    
    private Button autoSaveOffBtn = null;   
    private Button autoSaveOnBtn = null;

	private static final int ADD_ID = IDialogConstants.CLIENT_ID + 7587;
	private static final String ADD_LABEL = "Add";
	private static final int REPLACE_ID = IDialogConstants.CLIENT_ID + 7586;
	private static final String REPLACE_LABEL = "Replace";
	private static final int ADVANCE_ID = IDialogConstants.CLIENT_ID + 7588;
	private static final String ADVANCE_LABEL = "Advanced";
	private static final int SAVE_ID = IDialogConstants.CLIENT_ID + 7589;
	private static final String SAVE_LABEL = "Save";
	private static final int CLOSE_ID = IDialogConstants.CLIENT_ID + 7590;
	private static final String CLOSE_LABEL = "Close";

	private Button replaceBtn = null;   
    private Button addBtn = null;
    private Button appendBtn = null;
    private Button cancelBtn = null;
    private Button saveBtn = null;   
 
    private static LinkedHashMap<String, String>  dirTableMap = null;
    private String dirLocal = ".";
    
    private static String selectedDir = null;      
    private static String fileName = null; 
    private static String fullName = null; 

    // Attributes' names defined in a legacy Layer Product File
    private static final String LPF_LAYER_NAME = "name";
    private static final String LPF_LAYER_INPUT_FILE = "file";
    private static final String LPF_LAYER_OUTPUT_FILE = "output_file";
    private static final String LPF_LAYER_COLOR_MODE = "color_mode";
    private static final String LPF_LAYER_COLOR_ID = "color_id";
    private static final String LPF_LAYER_FILL_MODE = "fill_mode";
//    private static final String LPF_LAYER_GROUP_TYPE = "group_type";
    private static final String LPF_LAYER_DISPLAY_MODE = "display_mode";
  
    private static PgenFileMode fileMode = PgenFileMode.OPEN;
    
    private static int lastDirPos = -1;
    
    /*
     *  The Color map used in legacy PGEN with 32 colors.
     */
    private static HashMap<Integer, Integer[]> nmapColors = null;
      
    /*
     *  Constructor
     */
    public PgenFileManageDialog1( Shell parShell, String btnName ) throws VizException {
     	
    	super ( parShell );   	       	
    	
    	setFileMode( btnName );   
    	
        String 	currentWorkingDir = PgenUtil.getWorkingDirectory();        

        if ( currentWorkingDir != null ) {
        	dirLocal = new String( currentWorkingDir );
        }               
    	       	
	    loadUserTable();
    }  
 
    /*
     * Set up the file mode.
     */
    private void setFileMode( String btnName ) {

    	if ( btnName.equals( "Open" ) ) {
        	fileMode = PgenFileMode.OPEN;
    	    title = "Open a PGEN Product file";
    	}
    	else if ( btnName.equals( "Save" ) || btnName.equals( "Save All" )) {
           	fileMode = PgenFileMode.SAVE;
    	    title = "Save the PGEN Product";
    	}
    	else if ( btnName.equals( "Save As" ) ) {
           	fileMode = PgenFileMode.SAVE_AS;
    	    title = "Save the PGEN Product as";
    	}
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        this.setShellStyle( SWT.RESIZE | SWT.PRIMARY_MODAL ); 
        
        this.shell = shell;
        if ( title != null ) {
            shell.setText( title );
        }
    }
            
    /**
     * (non-Javadoc)
     * Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        
    	Composite dlgAreaForm = (Composite) super.createDialogArea(parent);

        Composite topForm = new Composite( dlgAreaForm, SWT.NONE );        
    	topForm.setLayout( new FormLayout() );
           	
        /*
         *  Create a label and two radio buttons - how to sort the files
         */
        Composite sortForm = new Composite( topForm, SWT.NONE );        
    	sortForm.setLayout( new FormLayout() );
          	
    	sortByNameBtn = new Button( sortForm, SWT.RADIO);
        sortByNameBtn.setText("Sort Alphabetically");
        
        FormData layoutData1 = new FormData( 370, 25 );
        layoutData1.top   = new FormAttachment( 0, 0 );
        layoutData1.left  = new FormAttachment( 0, 0 );
        
        sortByNameBtn.setLayoutData( layoutData1 );

        sortByNameBtn.setSelection(true);
        
        /*
         * Sort the files by name. when the corresponding radio button is selected
         */
        sortByNameBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
                fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml" }, new FileNameComparator()) );     
                fileListViewer.setLabelProvider( NmapCommon.createFileLabelProvider());
      		    fileListViewer.refresh(true);
       		 
         	}
        });
        
        sortByDateBtn = new Button( sortForm, SWT.RADIO );
        sortByDateBtn.setText("Sort By Date");       
        
        FormData layoutData3 = new FormData();
        layoutData3.top   = new FormAttachment( sortByNameBtn, 5, SWT.BOTTOM );
        layoutData3.left  = new FormAttachment( sortByNameBtn, 0, SWT.LEFT );       
        sortByDateBtn.setLayoutData( layoutData3 );

        /*
         * Sort the files by date. when the corresponding radio button is selected
         */
        sortByDateBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
                fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml" }, new FileDateComparator()) );     
                fileListViewer.setLabelProvider( NmapCommon.createFileLabelProvider());
       		    fileListViewer.refresh(true);
       		 
         	}
        });        
        
        /*
         *  Create a list of directories to select from
         */
        Label dirLbl = new Label(topForm, SWT.NONE);
        dirLbl.setText("Select Directory:");
        
        FormData layoutData5 = new FormData();
        layoutData5.top   = new FormAttachment( sortForm , 15, SWT.BOTTOM );
        layoutData5.left  = new FormAttachment( sortForm, 0, SWT.LEFT );       
        
        dirLbl.setLayoutData( layoutData5 );
        
        dirList = new List( topForm, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL );
        
        //  TODO : would like to get the list of available pgen resources (ie. subDirectories)
        //  from the ResourceDefnsMngr but this created some access restriction problems so for now 
        //  just keep with 
//			try {
//				Map<String,ResourceDefinition> pgenRscDefns = ResourceDefnsMngr.getInstance().getPgenResourceDefinitions();
//			} catch (VizException e1) {
//				// TODO Auto-generated catch block
//				e1.printStackTrace();
//			}
	    
        for ( String str : dirTableMap.keySet() ) {
        	dirList.add( str );
        }        

        if ( lastDirPos < 0 ) {
        	lastDirPos =  (dirList.getItemCount() - 1);
        }
                
        dirList.setSelection( lastDirPos );
	    selectedDir = "" + dirTableMap.get( dirList.getSelection()[0] );
                
        FormData layoutData6 = new FormData( 350, 200 );
        layoutData6.top  = new FormAttachment( dirLbl, 5, SWT.BOTTOM );
        layoutData6.left = new FormAttachment( dirLbl, 0, SWT.LEFT );
        dirList.setLayoutData( layoutData6 );        
        	
        dirList.addListener ( SWT.Selection, new Listener() {
    		public void handleEvent (Event e) {
    			if ( dirList.getSelectionCount() > 0 ) {
    			    
    				selectedDir = "" + dirTableMap.get( dirList.getSelection()[0] );

    				dirList.setToolTipText( selectedDir );
    				
   		            fileListViewer.setInput( new File( selectedDir ) );
  		            lastDirPos = dirList.getSelectionIndex();
   		            fileListViewer.refresh();
    			    
   		            //Update the full file name with the new path
    				fullName = null;
   		            if ( fileMode != PgenFileMode.OPEN ) {
    					fullName = new String( selectedDir + File.separator + fileNameText.getText() );    					
    				}
    			}
    		}
    	});


        /*
         *  Create a list to display the product files  in a selected directory
         */
        Label fileLbl = new Label(topForm, SWT.NONE);
        fileLbl.setText("Select a Product File:");
        
        FormData layoutData8 = new FormData();
        layoutData8.top   = new FormAttachment( dirList, 20, SWT.BOTTOM );
        layoutData8.left  = new FormAttachment( dirList, 0, SWT.LEFT );       
       
        fileLbl.setLayoutData( layoutData8 );
      
        fileListViewer = new ListViewer(topForm, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);

        FormData layoutData9 = new FormData( 350,200 );
        layoutData9.top  = new FormAttachment( fileLbl, 5, SWT.BOTTOM );
        layoutData9.left = new FormAttachment( fileLbl, 0, SWT.LEFT );
  
        fileListViewer.getList().setLayoutData( layoutData9 );
        
    	if ( sortByNameBtn.getSelection() ) {
            fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml", ".lpf" }, new FileNameComparator()) );     
    	}
    	else {
            fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml", ".lpf"}, new FileDateComparator()) );    		
    	}
 
        fileListViewer.setLabelProvider( NmapCommon.createFileLabelProvider( new String[]{""} ) ); 

        fileListViewer.setInput( new File( selectedDir ) ); 
        fileListViewer.refresh();
        
        if ( fileName != null ) {
        	int selFileInd = fileListViewer.getList().indexOf( fileName );
        	if ( selFileInd >= 0 ) {
        		fileListViewer.getList().select( selFileInd );
        	}
        	else {
        		fileName = null;
        		fullName = null;
        	}
        }        	
      
        fileListViewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
             	String[] selectedFile = fileListViewer.getList().getSelection();
            	            	    
            	if( selectedFile.length == 0 )	{
            		if ( fileMode != PgenFileMode.OPEN ) {
            		    saveBtn.setEnabled(false);
            		}
            		else {            		
            			replaceBtn.setEnabled(false);
         	            addBtn.setEnabled(false);
            		}     	
        	    }
            	else {
               		
       	            fileName = "" + selectedFile[0];
               	    fullName = "" + selectedDir + File.separator + fileName;
           		    
               	    if ( fileMode != PgenFileMode.OPEN ) {       	        
               		    fileNameText.setText( fileName );
               	    	saveBtn.setEnabled( true );
            		}
               		else {     	            
            		    replaceBtn.setEnabled(true);
    	                addBtn.setEnabled(true);           
               		}                          	
               	}        	         
            }
       	});       	   

        
        /*
         *  Create a text field to input a file name to save the product
         */
        if ( fileMode != PgenFileMode.OPEN ) {
        	Label fileNameLbl = new Label(topForm, SWT.NONE);
            fileNameLbl.setText("Or Enter a File Name");
        
            FormData layoutData9_1 = new FormData();
            layoutData9_1.top   = new FormAttachment( fileListViewer.getList(), 25, SWT.BOTTOM );
            layoutData9_1.left  = new FormAttachment( fileListViewer.getList(), 0, SWT.LEFT );       
            fileNameLbl.setLayoutData( layoutData9_1 );
        
            fileNameText = new Text (topForm,  SWT.SINGLE | SWT.BORDER);
        
            FormData layoutData9_2 = new FormData(355, 15);
            layoutData9_2.top   = new FormAttachment( fileNameLbl, 10, SWT.BOTTOM );
            layoutData9_2.left  = new FormAttachment( fileNameLbl, 0, SWT.LEFT );       
            fileNameText.setLayoutData( layoutData9_2 );     
        
        	fileNameText.setText( "" );
            
        	//If the current product has not been saved, should use its configured path/filename
            //otherwise, use its last saved path/file name.
        	String curFile = PgenSession.getInstance().getPgenResource().getActiveProduct().getOutputFile();
        	String configuredFile = getConfiguredFile();
            if ( curFile == null && configuredFile != null ) {
            	fileName = new String( configuredFile );
            	fullName = new String ( "" + selectedDir + File.separator + fileName );
            }
            
            if ( fileName != null ) {
            	fileNameText.setText(fileName );
            }

            
            fileNameText.setSize( 310, 10 );
            fileNameText.setEditable( true );
            
            fileNameText.addModifyListener( new ModifyListener() {
                public void modifyText( ModifyEvent  event ) {           
                	
                	String usrFile = fileNameText.getText().trim();
                                    
                    if ( !usrFile.isEmpty() ) {                    	
                    	if ( !usrFile.contains( File.separator ) ) { 
                			usrFile = selectedDir + File.separator + usrFile; 
                		}                		
                	}
                     
            		if ( !usrFile.endsWith( ".xml" ) ) {
            			usrFile = usrFile + ".xml"; 
            		}
            		
            		fullName = usrFile;            

                }          		            	 	
            } );       

        }
        
        /**
         *  Create a browse button to open a file dialog to navigate & select a file.
         */
        browseBtn = new Button( topForm, SWT.PUSH );      
       
        FormData layoutData10 = new FormData(355, 25);
        if ( fileMode != PgenFileMode.OPEN ) {
        	layoutData10.top   = new FormAttachment( fileNameText, 20, SWT.BOTTOM );
            layoutData10.left  = new FormAttachment( fileNameText, 0, SWT.LEFT );       
        }
        else {
            layoutData10.top   = new FormAttachment( fileListViewer.getList(), 20, SWT.BOTTOM );
            layoutData10.left  = new FormAttachment( fileListViewer.getList(), 0, SWT.LEFT );             	
        }
    
        browseBtn.setLayoutData( layoutData10 );
        browseBtn.setSize(330, 20);
        browseBtn.setText("Browse"); 
        
        browseBtn.addListener( SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {           
         		if ( fileMode != PgenFileMode.OPEN ) {
            	    browseDlg = new FileDialog( shell, SWT.SAVE );
        	    	browseDlg.setFileName( "default.xml");           	  
         		}        		
         		else {
         			browseDlg = new FileDialog( shell, SWT.OPEN );
         		}

         		browseDlg.setFilterNames (new String [] {"Product files (*.xml, *.lpf)" });
         		browseDlg.setFilterExtensions (new String [] {"*.xml", "*.lpf"});
         		browseDlg.setFilterPath ( dirLocal ); 
         		
                String s = browseDlg.open();
                if ( s != null && !s.isEmpty() ) {                         		
         		    if ( !browseDlg.getFileName().isEmpty() ) {
         		        selectedDir = browseDlg.getFilterPath();
         		        fileName = browseDlg.getFileName();
         		        fullName = "" + selectedDir + File.separator + fileName;
         		        if ( fileMode != PgenFileMode.OPEN ) {
         		    	    fileNameText.setText( fullName );
         		        }
         		    }
                }
            }          		            	 	
        } );       
        
        /*
         *  Create a label and two radio buttons to turn "auto save" on or off.
         */
        Label autoSaveLbl = new Label(topForm, SWT.NONE);
        autoSaveLbl.setText("Auto Save:");
        
        FormData layoutData11 = new FormData();
        layoutData11.top   = new FormAttachment( browseBtn, 20, SWT.BOTTOM );
        layoutData11.left  = new FormAttachment( browseBtn, 0, SWT.LEFT );       
        autoSaveLbl.setLayoutData( layoutData11 );
      
        autoSaveOffBtn = new Button(topForm, SWT.RADIO );
        autoSaveOffBtn.setText("Off");       
        autoSaveOffBtn.setSelection(true);
        
        FormData layoutData12 = new FormData();
        layoutData12.top   = new FormAttachment( autoSaveLbl, 0, SWT.TOP );
        layoutData12.left  = new FormAttachment( autoSaveLbl, 10, SWT.RIGHT );       
        autoSaveOffBtn.setLayoutData( layoutData12 );
         
        autoSaveOnBtn = new Button(topForm, SWT.RADIO );
        autoSaveOnBtn.setText("On");       
        
        FormData layoutData13 = new FormData();
        layoutData13.top   = new FormAttachment( autoSaveOffBtn, 0, SWT.TOP );
        layoutData13.left  = new FormAttachment( autoSaveOffBtn, 10, SWT.RIGHT );       
        autoSaveOnBtn.setLayoutData( layoutData13 );
    	
        return dlgAreaForm;
     }
    
    /**
     *  Create Replace/Append/Cancel button for "Open" a product file or
     *         Save/Cancel button for "Save" a product file.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {       
           	
    	if ( fileMode == PgenFileMode.OPEN ) {
           
            addBtn = createButton( parent, ADD_ID, ADD_LABEL, true );
    		replaceBtn = createButton( parent, REPLACE_ID, REPLACE_LABEL, true );
            appendBtn = createButton( parent, ADVANCE_ID, ADVANCE_LABEL, true );
           
            replaceBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {
                	openProducts( true );
                }          		            	 	
            });
 
            addBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) { 
                   openProducts( false );               	
                }          		            	 	
            });
            
            appendBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {
                    appendProducts();
                }          		            	 	
            });
            
   	    }
    	else {

    		saveBtn = createButton( parent, SAVE_ID, SAVE_LABEL, true );

            saveBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) { 
                	saveProducts();
                }
            });
            
    	}
    	
        cancelBtn = createButton( parent, CLOSE_ID, CLOSE_LABEL, true );
        cancelBtn.addListener( SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) { 
                close();
            }          		            	 	
        });
	
    }
    
    /**
     *  Load the user-defined directories in old vgf.nmap
     */
     private void loadUserTable() {
//    	dirTableMap = PgenFileOrganizer.getPgenFilePaths();
     	dirTableMap = new LinkedHashMap<String, String>();
/*
     	dirTableMap.put( "test_data",	"$GEMDATA/vgf" );
        dirTableMap.put( "bawx",		"/export/hbawx1/bawx" );
        dirTableMap.put( "eff1",		"/export/heff1/eff1" );
        dirTableMap.put( "eff2",		"/export/hww1/eff2" );
        dirTableMap.put( "eff3",		"/export/lnx159/eff3" );
        dirTableMap.put( "eff4", 		"/export/lnx162/eff4" );
        dirTableMap.put( "nppu2", 		"/export/hqpf1/nppu2" );
        dirTableMap.put( "nppu3", 		"/export/hqpf2/nppu3" );
        dirTableMap.put( "medr",		"/export/hmedr2/medr" );
        dirTableMap.put( "medr3", 		"/export/hmedr1/medr3" );
        dirTableMap.put( "avn3", 		"/export/hdwm/avn3" );
        dirTableMap.put( "sfc1",		"/export/hsfc1/sfc1" );
        dirTableMap.put( "dwm", 		"/export/hdwm/dwm" );
        dirTableMap.put( "id0", 		"/export/sgi112/id0" );
        dirTableMap.put( "id1", 		"/export/lnx114/id1" );
        dirTableMap.put( "id2", 		"/export/lnx130/id2" );
        dirTableMap.put( "id3", 		"/export/hp61/id3" );
        dirTableMap.put( "id5", 		"/export/sgi20/id5" );
        dirTableMap.put( "id6", 		"/export/sgi112/id6" );
        dirTableMap.put( "prism", 		"/export-2/hp46/modtdg/joey/prism" );
        dirTableMap.put( "Pac_HS", 		"/home/hp2/marine" );
        dirTableMap.put( "Atl_HS", 		"/home/hp12/marine2" );
        dirTableMap.put( "Pac_Reg", 	"/home/hp41/marine3" );
        dirTableMap.put( "Atl_Reg", 	"/home/hp49/marine4" );
        dirTableMap.put( "HS_Outlook",	"/home/hp68/marine5" );
*/
     	// Load the product types.     	
        String pbase = PgenUtil.getPgenOprDirectory();
        String curPrdType = PgenSession.getInstance().getPgenResource().getActiveProduct().getType();
     	
        LinkedHashMap<String, ProductType> prdTypes = ProductConfigureDialog.getProductTypes();
        
        ProductType curAct = prdTypes.get( curPrdType );
        if ( curAct != null ) {
        	 curPrdType = new String( curAct.getType() );
        }
        
        ArrayList<String> uniPrdTypeNames = new ArrayList<String>();
       
		for ( String ptypName : prdTypes.keySet() ) {
			String mtyp = prdTypes.get( ptypName ).getType();
			if ( !uniPrdTypeNames.contains( mtyp ) ) {
				uniPrdTypeNames.add( mtyp );
			}
		}
     	
     	int ii = 0;
		lastDirPos = -1;
		for ( String ptypName : uniPrdTypeNames ) {
            			
			String sdir = new String ( pbase + File.separator + 
	                                   ptypName.replace(' ', '_' ) + 
					                   File.separator + "xml" ) ;   	    
            
            dirTableMap.put( ptypName,  sdir  ); 
     		
     		if ( ptypName.equalsIgnoreCase( curPrdType ) ) {
     			lastDirPos = ii;
     		}
     		ii++;
     	}
    	
  	    dirTableMap.put( "Local",	dirLocal ); 

    }       


    /**
     *  Open a product file to replace or append to the current product list
     *  in the current PGEN session.
     */
    private void openProducts( boolean replace ) {
   	
    	if ( !validProductFile( fullName ) ) {
    		return;
    	}
    	
    	Products products = null;
    	if ( fullName.endsWith(".lpf") ) {    		
    	    products = loadLpfFile( fullName );
    	}
    	else {
    	    products = FileTools.read( fullName );
    	}
    	
   	    if ( products == null ) {
    		MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Invalid PGEN Product File", null, 
            		"File " + fileName + " is not a valid PGEN product file. \nPlease select another one.",
            		MessageDialog.INFORMATION, new String[]{"OK"}, 0);
            
        	confirmDlg.open();

   	    	return;
   	    }
    	  	    
   	    /*
   	     * some Volcano Products are pure texts: TEST/RESUME
   	     * and cannot be drawn.
   	     */
	   	if( VaaInfo.isNoneDrawableTxt(products) ){ 
	   		VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);
	   		return;
	   	}
	   	
	   	/*
	   	 * Confirm the action
	   	 */
   	    PgenResource pgen = PgenSession.getInstance().getPgenResource();
   	       	       	    
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenProds =
   	    									ProductConverter.convert( products );  
   	    
   	    // Force all product/layer display onOff flag to be false at the start.
/*   	    for ( gov.noaa.nws.ncep.ui.pgen.elements.Product  prd : pgenProds ) {
   	    	prd.setOnOff( false );
   	    	for ( gov.noaa.nws.ncep.ui.pgen.elements.Layer  lyr : prd.getLayers() ) {
   	    		lyr.setOnOff( false );
   	    	}
   	    }
*/   	    
		if ( replace ) {
		   	MessageDialog confirmOpen = new MessageDialog( 
	        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
	        		"Confirm File Replace", null, 
	        		"Replace Activity <" + pgen.getActiveProduct().getType() + 
	        		"> with New Activity <" + pgenProds.get(0).getType() + "> ?",
	        		MessageDialog.INFORMATION, new String[]{ "Yes", "No" }, 0 );
		   	
		   	confirmOpen.open();
		   	
			if ( confirmOpen.getReturnCode() != MessageDialog.OK ) {				
				return;
			}

		}
		
		pgen.setAutosave( autoSaveOnBtn.getSelection() );
   	    if ( fullName.endsWith( ".lpf") ) {
   	        pgen.setAutoSaveFilename( fullName.replace( ".lpf", "xml") );  	    	
   	    }
   	    else {
   	        pgen.setAutoSaveFilename( fullName );
   	    }
   	    
   	    pgenProds.get(0).setInputFile( fullName );
   	       	    
   	    this.setJetTool( pgenProds);
   	    
  	    close();
 	        	
	   	/*
	   	 * Replace the active product or add the product to the end
	   	 */
   	    if ( replace ) {
   	   	    //Reset the output file name.
   	    	for ( gov.noaa.nws.ncep.ui.pgen.elements.Product pp : pgenProds ) {
   	   	    	pp.setOutputFile( null );
   	   	    }
   	   	    
   	        PgenFileNameDisplay.getInstance().setFileName( fullName );
   	        
   	        pgen.replaceProduct( pgenProds );
   	    }
   	    else {
   	   	    if ( pgen.getActiveProduct() == null ) {
  	   	        PgenFileNameDisplay.getInstance().setFileName( fullName );
   	   	    }
   	   	    
	        pgen.addProduct( pgenProds );     
   	    }
  	       	    
   	    PgenUtil.refresh();             	                	    

    }

    /**
     *  Append the products in a file with those in the current product list.
     */
    private void appendProducts() {
    	   	
    	/*
    	 * Validate
    	 */
    	if ( !validProductFile( fullName ) ) {
    		return;
    	}
    	
    	Products products = null;
    	if ( fullName.endsWith(".lpf") ) {    		
    	    products = loadLpfFile( fullName );
    	}
    	else {
    	    products = FileTools.read( fullName );
    	}
    	
   	    if ( products == null ) {
    		MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Invalid PGEN Product File", null, 
            		"File " + fileName + " is not a valid PGEN product file. \nPlease select another one.",
            		MessageDialog.INFORMATION, new String[]{"OK"}, 0);
            
        	confirmDlg.open();

   	    	return;
   	    }
   	    
   	    /*
   	     * some Volcano Products are pure texts: TEST/RESUME
   	     * and cannot be drawn.
   	     */
	   	if( VaaInfo.isNoneDrawableTxt(products) ){ 
	   		VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);
	   		return;
	   	} 
 
	   	PgenResource pgen = PgenSession.getInstance().getPgenResource();
   	    
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenProds =
   	    									ProductConverter.convert( products );
	   	
	   	PgenLayerMergeDialog layerMergeDlg = null;
	   	try {
	   		layerMergeDlg = new PgenLayerMergeDialog( shell, pgenProds.get(0), fullName );
	   	}
	   	catch (Exception e) {
	   		e.printStackTrace();
	   	}
	   		   	

	   	/*
   	    PgenResource pgen = PgenSession.getInstance().getPgenResource();
   	    
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenProds =
   	    									ProductConverter.convert( products );
   	    
   	    MessageDialog confirmOpen = new MessageDialog( 
   	    		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
   	    		"Confirm File Append", null, 
   	    		"Append Contents in Activity <" + pgenProds.get(0).getType() + 
   	    		"> to Current Activity <" + pgen.getActiveProduct().getType() + "> ?",
   	    		MessageDialog.INFORMATION, new String[]{ "Yes", "No" }, 0 );

   	    confirmOpen.open();

   	    if ( confirmOpen.getReturnCode() != MessageDialog.OK ) {				
   	    	return;
   	    }
        */
	   	
	   	if ( layerMergeDlg != null ) {
	   		
	   		layerMergeDlg.open();
	   	    if ( layerMergeDlg.getReturnCode() == MessageDialog.OK ) {
	   	    		   	
	   	    	pgen.setAutosave( autoSaveOnBtn.getSelection() );
	   	    	if ( fullName.endsWith( ".lpf") ) {
	   	    		pgen.setAutoSaveFilename( fullName.replace( ".lpf", "xml") );  	    	
	   	    	}
	   	    	else {
	   	    		pgen.setAutoSaveFilename( fullName );
	   	    	}

	   	    	this.setJetTool( pgenProds);

	   	    	close();

	   	    	pgen.getResourceData().startProductManage();
	   	    }
	   	}	   	
          	    
    } 
    
    /**
     *  Save the products to file.
     */
    private void saveProducts() {            		
    	
    	if ( !validProductFile( fullName ) ) {
    		return;
    	}
    	    	
		// check and inform duplicate product names.
   	    PgenResource rsc = PgenSession.getInstance().getPgenResource();

		boolean dup = false;
		for ( Product p : rsc.getProducts() ) {
			for ( Product p1 : rsc.getProducts() ) {
				if ( !p1.equals( p ) && p1.getName().equals( p.getName() ) ) {
		    		dup = true;		 
					break;
				}
			}
		}			
        
		if ( dup ) {
			MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Duplicate Product Name", null, 
            		"There are duplicate product names. \nPlease make them unique before saving!",
            		MessageDialog.WARNING, new String[]{"OK"}, 0);
            
        	confirmDlg.open();

        	/*
        	if ( (confirmDlg.getReturnCode() == MessageDialog.OK) ) {
            	close();
            	return;
            }
            */
		
		}
      
        //Ask for confirmation
    	boolean saveFile = true;
      	File infile = new File( fullName );  	   	
    	String msg = "Are you sure you want to save to: " + fullName + "?";
    	if ( infile.exists() ) {
    	    msg = "File " + fullName + " exists. Are you sure you want to overwrite it?";
    	}

    	MessageDialog confirmDlg = new MessageDialog( 
        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
        		"Saving a PGEN file", null, msg,
        		MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
        
    	confirmDlg.open();

        if ( !(confirmDlg.getReturnCode() == MessageDialog.OK) ) {
        	saveFile = false;
        }
        
        // Save now
        if ( saveFile ) {           
        	
            rsc.setAutosave( autoSaveOnBtn.getSelection() );
            rsc.setAutoSaveFilename(fullName);
        	
            close(); 
        	PgenSession.getInstance().getPgenPalette().setActiveIcon( "Select" );

            rsc.getActiveProduct().setInputFile( fullName );      
            rsc.getActiveProduct().setOutputFile( fullName );      
            
            rsc.saveCurrentProduct( fullName );  
            
		    PgenFileNameDisplay.getInstance().setFileName( fullName );
        	
            PgenUtil.setSelectingMode();
        }       
   	    
    } 
    
    /**
     *  Check if the selected file is a valid PGEN product file
     */
    private boolean validProductFile( String vfile ) {
    	
    	String msg = null;
    	if ( vfile == null ) {
    	    if ( fileMode == PgenFileMode.OPEN ) {
    		    msg = "Please select a file!";
    	    }
    	    else {
    	    	msg = "Please input a file name!";
    	    }
    	}
    	else {    	
    	    File infile = new File( vfile );
    	    if ( fileMode == PgenFileMode.OPEN ) {
    	        if ( !infile.exists() && !infile.canRead() ) {
    		        msg = "File " + vfile + " cannot be read!";
    	        }
    	    }
    	    else {
    	    	//Check write permission
    	        String fpath = fullName.substring( 0 , fullName.lastIndexOf( "/") );
    	    	File indir = new File( fpath );
    			if ( indir.exists() && !indir.canWrite() ) {
    	        	msg = "No write permission to directory: " + fpath + "!";
    	        }
    	    }
    	}

    	if ( msg != null ) {
    		MessageDialog confirmDlg = new MessageDialog( 
            		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
            		"Invalid PGEN Product File", null, msg,
            		MessageDialog.INFORMATION, new String[]{"OK"}, 0 );
            
        	confirmDlg.open();
        	
        	return false;
    	}

    	return true;
    }
   
    
    /** 
     * Sets the jet snap tool in order to zoom tghe jet correctly.
     * @param prods
     */
    private void setJetTool(java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prods){
    	
//    	PgenSnapJet st = new PgenSnapJet(PgenSession.getInstance().getPgenResource().getDescriptor(), 
//    						NmapUiUtils.getActiveNatlCntrsEditor(), null);
    	PgenSnapJet st = new PgenSnapJet(PgenSession.getInstance().getPgenResource().getDescriptor(), 
				PgenUtil.getActiveEditor(), null);
    	
    	for(gov.noaa.nws.ncep.ui.pgen.elements.Product prod : prods){
    		for ( Layer layer : prod.getLayers() ) {

    			Iterator<AbstractDrawableComponent> iterator = layer.getComponentIterator();
    			while ( iterator.hasNext()){    
    				AbstractDrawableComponent adc = iterator.next();
    				if ( adc instanceof Jet ){
    					((Jet)adc).setSnapTool(st);
    				//	st.snapJet((Jet)adc);
    				}
    			}
    		}
    	}

    }
    
    
    /** 
     * Retrieved the configured output file for the current product type
     * @param prods
     */
    private String getConfiguredFile() {
    	
    	String filename = PgenSession.getInstance().getPgenResource().buildFileName( 
    			          PgenSession.getInstance().getPgenResource().getActiveProduct() );
    	
    	filename = filename.substring( filename.lastIndexOf( File.separator ) + 1 );

    	return filename;
    	
    }
 
    /* 
     * Load a full-path LPF file into a PGEN product
     * @param prods
     */
    private Products loadLpfFile( String fname ) {
    
    	Products pgenFilePrds = null;
        
    	LinkedHashMap<String, ProductType> prdTypes = ProductConfigureDialog.getProductTypes();
	    
    	HashMap<String, String> lpfMap = loadLpfParameters( fname );

    	ArrayList<String> layerPrefixes = getLayerPrefixes( lpfMap );
        
    	String masterType = matchActivityType( lpfMap, prdTypes);
  	
        //Load layers.
		if ( layerPrefixes.size() > 0 ) {
			pgenFilePrds = new 	Products();				
			for ( String layerPre : layerPrefixes ) {
				
				Products layerFp = null; 
				
				String layerName = lpfMap.get( layerPre + "_"+ LPF_LAYER_NAME );
				
				if ( layerName == null || layerName.trim().length() == 0 ) {
					continue;
				}
				
				String layerOutputFile = lpfMap.get( layerPre + "_" + LPF_LAYER_OUTPUT_FILE );
				String layerColorMode = lpfMap.get( layerPre + "_" + LPF_LAYER_COLOR_MODE );
				String layerColorID = lpfMap.get( layerPre + "_" + LPF_LAYER_COLOR_ID );
				String layerFillMode = lpfMap.get( layerPre + "_" + LPF_LAYER_FILL_MODE );
//				String layerGroupType =  lpfMap.get( layerPre + "_" + LPF_LAYER_GROUP_TYPE );
				String layerDisplayMode =  lpfMap.get( layerPre + "_" + LPF_LAYER_DISPLAY_MODE );
				
				String layerFile = lpfMap.get( layerPre + "_" + LPF_LAYER_INPUT_FILE );
				
				/*
				 * Look for a full-path input file for the layer.
				 * 
				 * If no path provided, first look in the LPF's defined activity path, if any;
				 * if still not found, look in the same directory where this lpf exists.
				 */
				String layerInputFile = PgenUtil.parsePgenFileName( layerFile );
				if ( !layerInputFile.contains( File.separator ) ) {
					if ( masterType != null && !masterType.equalsIgnoreCase( "Default") ) {
					    String fnm = new String ( PgenUtil.getPgenOprDirectory() + File.separator + 
					                 masterType.replace(" ", "_" ) + File.separator + "xml" +
					                 File.separator + layerInputFile );

						if ( fnm.endsWith( "vgf" ) ) {
						    fnm = fnm.replace(".vgf", ".xml" );
						}
						
					    File ff = new File( fnm );
						if ( ff.exists() && ff.canRead() ) {
							layerInputFile = new String( fnm );
						}
						else {
							layerInputFile = new String ( fname.substring( 0, fname.lastIndexOf( File.separator ) )+
									File.separator + layerInputFile );
						}					    						
					}
					else {
						layerInputFile = new String ( fname.substring( 0, fname.lastIndexOf( File.separator ) )+
								File.separator + layerInputFile );						
					}
				}
				
				//Read in the contents in the specified file.
				if ( layerInputFile != null ) {
					if ( layerInputFile.endsWith( "vgf" ) ) {
					    layerInputFile = layerInputFile.replace(".vgf", ".xml" );
					}
					
					File ft = new File( layerInputFile );
					if ( ft.exists() && ft.canRead() ) {
						layerFp = FileTools.read( layerInputFile );
					}
				}
				
				//Create a default pgen product if there is no file specified for the layer.
				if ( layerFp == null ) {
				    Product dfltp = new Product("Default", "Default", "Default",
			    		      new ProductInfo(), new ProductTime(), new ArrayList<Layer>() );
				    
				    Layer dfltly = new Layer();
				    dfltp.addLayer( dfltly );
				    
				    ArrayList<Product>  dfltPrds = new ArrayList<Product>();
				    dfltPrds.add( dfltp );
				    
				    layerFp = ProductConverter.convert( dfltPrds );
				}
				
				//Add to the product list and set the activity type
				if ( !VaaInfo.isNoneDrawableTxt( layerFp ) ) {
					if ( pgenFilePrds.getProduct().size() == 0  ) {
					    pgenFilePrds.getProduct().addAll( layerFp.getProduct() );
			        	if ( masterType != null && !masterType.equalsIgnoreCase( "Default") ) {
					        pgenFilePrds.getProduct().get(0).setType( masterType );
			        	    pgenFilePrds.getProduct().get(0).setName( masterType );
			        	}
					}
				    else {	
				        pgenFilePrds.getProduct().get(0).getLayer().addAll( 
						            layerFp.getProduct().get(0).getLayer() );
				    }
				    
				    /*
				     * If no activity type found in LPF file, try to update 
				     * from each layer's file.
				     */
		        	if ( pgenFilePrds.getProduct().get(0).getType().equalsIgnoreCase("Default") ) {
				        String ptype = layerFp.getProduct().get(0).getType();
				        ptype = matchActivityType( ptype, prdTypes );
				        if ( !ptype.equalsIgnoreCase( "Default") ) {
				        	pgenFilePrds.getProduct().get(0).setType( ptype );
				        	pgenFilePrds.getProduct().get(0).setName( ptype );
				        }
				    }
				}
				
				//Update layer attributes
				int nly = pgenFilePrds.getProduct().get(0).getLayer().size();
				gov.noaa.nws.ncep.ui.pgen.file.Layer clayer = 
					     pgenFilePrds.getProduct().get(0).getLayer().get( nly - 1 );
				clayer.setName( layerName );
				
				// Color mode
				if ( layerColorMode != null && layerColorMode.trim().length() > 0 &&
						layerColorMode.trim().toUpperCase().startsWith( "A" ) ) {
					clayer.setMonoColor( false );
				}
				else {
					clayer.setMonoColor( true );
				}
                
				// Fill mode
				if ( layerFillMode != null && layerFillMode.trim().length() > 0 &&
						layerFillMode.trim().equalsIgnoreCase( "On" ) ) {
					clayer.setFilled( true );
				}
				else {
					clayer.setFilled( false );
				}

				// Display mode
				if ( layerDisplayMode != null && layerDisplayMode.trim().length() > 0 &&
						layerDisplayMode.trim().equalsIgnoreCase( "On" ) ) {
					clayer.setOnOff( true );
				}
				else {
					clayer.setOnOff( false );
				}

				// Output file
				if ( layerOutputFile != null && layerOutputFile.trim().length() > 0 ) {
					clayer.setOutputFile( layerOutputFile );
				}
				else {
					clayer.setOutputFile( null );
				}
				
				// layer color
				if ( layerColorID != null && layerColorID.trim().length() > 0 ) {
					int colorNum = Integer.parseInt( layerColorID );
					if ( colorNum > 0 && colorNum <= 32 ) {
					    Integer[] nmapColor = getNmapColors().get( colorNum );
					    if ( nmapColor != null ) {
					    	if ( clayer.getColor() == null ) {
					    		clayer.setColor( new Color() );
					    		clayer.getColor().setAlpha( 255 );
					    	}
					        
					    	clayer.getColor().setRed( nmapColor[0] );
					        clayer.getColor().setGreen( nmapColor[1] );
					        clayer.getColor().setBlue( nmapColor[2] );
					    }
					}
				}				
			}
    	}
		   	
    	return pgenFilePrds;
    	
    }
   
    /* 
     * Find all layer prefixes in a loaded LPF file (e.g., layer1, layer3, ...)
     * @param 	lpfMap HashMap<String, String>
     * @return 	ArrayList<String>
     */
    private ArrayList<String> getLayerPrefixes( HashMap<String, String> lpfMap ) {
        
    	ArrayList<String> layerKeys = new  ArrayList<String>();
    	if ( lpfMap != null && lpfMap.size() > 0 ) {    		
    		for ( String key : lpfMap.keySet() ) {
    			if ( key != null && key.contains( LPF_LAYER_NAME ) ) {
    				layerKeys.add( new String( key.substring( 0, key.indexOf("_") ) ) );
    			}
    		}
    	}
   	
    	return layerKeys;
    	
    }
    
    /*
     *  Load the LPF file contents into a HashMap.
     *  
     *  Note: valid entry should be in format of "<tag>	value"
     *  
     */
    private LinkedHashMap<String, String> loadLpfParameters( String fname ) {
    	
    	LinkedHashMap<String, String> params = new LinkedHashMap<String, String>();
		
    	//Check if the given file exists and readable. 
        File thisFile = null;                
        if ( fname != null ) {
             thisFile = new File( fname );
             if ( !thisFile.exists() || !thisFile.canRead() ) {
        	    thisFile = null;
             }
        }
        
        if ( thisFile == null ) {
        	return params;
        }
      
   	    try {
       	    
         	Scanner fileScanner = new Scanner( thisFile );       	 
            
            try {
                //first use a Scanner to get each line
                while ( fileScanner.hasNextLine() ) {
                    String nextLine = fileScanner.nextLine().trim();
                	              
                    //process each line
                    if ( !(nextLine.startsWith("!")) ) {                    	
                        
                        int start = nextLine.indexOf( "<");
                        int end = nextLine.indexOf( ">");
                        
                        if ( start >= 0 && end > 0 && end > start && 
                        		  nextLine.length() > end ) {
                            
                        	String name = nextLine.substring( start + 1, end ).trim();
                        	if ( name.length() > 0  ) {
                            	String value = nextLine.substring( end + 1 ).trim();
                        		
                            	if ( value.length() > 0 ) {
                            	    params.put( name, value );
                            	}                       		
                        	}                       	
                        }
                    }
                }
            }
            finally {
                fileScanner.close();
            }
    	}
        catch ( IOException e ) {
        	e.printStackTrace();
        }
                
   	    return params;   	
    } 

    /*
     *  The Color map used in legacy PGEN with 32 colors.
     *  @return HashMap<Integer, Integer[]>
     */
    private static HashMap<Integer, Integer[]> getNmapColors() {
    	if ( nmapColors == null ) {
    		nmapColors = new HashMap<Integer, Integer[]>();
    		nmapColors.put( 0,  new Integer[]{  0, 	   0, 	    0} );
       		nmapColors.put( 1,  new Integer[]{255,    228,    220} );
       		nmapColors.put( 2,  new Integer[]{255,      0,      0} );
       		nmapColors.put( 3,  new Integer[]{  0,    255,      0} );
       		nmapColors.put( 4,  new Integer[]{  0,      0,    255} );
       		nmapColors.put( 5,  new Integer[]{255,    255,      0} );
       		nmapColors.put( 6,  new Integer[]{  0,    255,    255} );
       		nmapColors.put( 7,  new Integer[]{255,      0,    255} );
       		nmapColors.put( 8,  new Integer[]{139,     71,     38} );
      		nmapColors.put( 9,  new Integer[]{255,    130,     71} );
      		nmapColors.put( 10, new Integer[]{255,   165,     79} );
      		nmapColors.put( 11, new Integer[]{255,    174,    185} );
      		nmapColors.put( 12, new Integer[]{255,    106,    106} );
      		nmapColors.put( 13, new Integer[]{238,     44,     44} );
      		nmapColors.put( 14, new Integer[]{139,      0,      0} );
      		nmapColors.put( 15, new Integer[]{205,      0,      0} );
      		nmapColors.put( 16, new Integer[]{238,     64,      0} );
      		nmapColors.put( 17, new Integer[]{255,    127,      0} );
      		nmapColors.put( 18, new Integer[]{205,    133,      0} );
      		nmapColors.put( 19, new Integer[]{255,    215,      0} );
      		nmapColors.put( 20, new Integer[]{238,    238,      0} );
      		nmapColors.put( 21, new Integer[]{127,    255,      0} );
      		nmapColors.put( 22, new Integer[]{  0,    205,      0} );
      		nmapColors.put( 23, new Integer[]{  0,    139,      0} );
      		nmapColors.put( 24, new Integer[]{ 16,     78,    139} );
      		nmapColors.put( 25, new Integer[]{ 30,    144,    255} );
      		nmapColors.put( 26, new Integer[]{  0,    178,    238} );
      		nmapColors.put( 27, new Integer[]{  0,    238,    238} );
      		nmapColors.put( 28, new Integer[]{137,    104,    205} );
      		nmapColors.put( 29, new Integer[]{145,     44,    238 } );
      		nmapColors.put( 30, new Integer[]{139,      0,   139} );
      		nmapColors.put( 31, new Integer[]{255,    255,    255} );
      		nmapColors.put( 32, new Integer[]{  0,      0,      0 } );
  	    }
    	
    	return nmapColors;
    }
    
    /*
     * Try to match an activity type name to an existing activity type.
     * 
     * First try to match the alias of an existing type.  If not found, then match
     * "type(subtype)".  If still not found, "Default" is used.
     */
    private String matchActivityType( String typeName, HashMap<String, ProductType> prdTypes) {
    	
 	    String tpName = typeName; 	    
 	            
        boolean matchFound = false;
        
        // If typeName is a unique ProductType alias, return as is.
        for ( String name : prdTypes.keySet() ) {
        	String palias = prdTypes.get( name ).getName();
        	if ( palias != null && typeName.equalsIgnoreCase( palias ) ) {
        		matchFound = true;
        		break;
        	}
        }
        
        if ( !matchFound ) {
        	for ( String name : prdTypes.keySet() ) {
        		String fullTypeStr = new String( prdTypes.get( name ).getType() );
        		String subtype = prdTypes.get( name ).getSubtype();
        		if ( subtype != null && !subtype.equalsIgnoreCase( "None" ) ) {
        			fullTypeStr = new String( fullTypeStr + "(" + subtype + ")");
        		}
       		
        		if ( typeName.equalsIgnoreCase( fullTypeStr ) ) {
        			String st = prdTypes.get( name ).getName();
        			if ( st != null && st.trim().length() > 0 ) {
        				tpName = new String( st );
        			}
        			else {
        				tpName = fullTypeStr;
        			}
       			
            		matchFound = true;
            		break;
        		}
        	}
        }
        
        if ( !matchFound ) {
        	tpName = new String( "Default" );
        }
                
        return tpName;
    }
    
    /* 
     * Try to find a matching activity defined in LPF file (alias, type, subtype)
     * @param 	lpfMap HashMap<String, String>
     * @param 	lpfMap HashMap<String, String>
     * @return 	HashMap<String, String>
     */
    private String matchActivityType( HashMap<String, String> lpfMap, HashMap<String, ProductType> prdTypes ) {
        
    	HashMap<String, String> actMap = getActivityInfoMap( lpfMap );
    	
    	String actType = new String( "Default" );
    	
    	if ( actMap.size() > 0 ) {    		
			String alias = null;
			String type = null;
			String subtype = null;
			
   		    for ( String key : actMap.keySet() ) {
    			if ( key.contains( "alias") ) {
    				alias = actMap.get( key );
   		        }
    			
    			if ( key.contains( "type") ) {
    				type= actMap.get( key );
   		        }
    			
    			if ( key.contains( "subtype") ) {
    				subtype = actMap.get( key );
   		        }
   		    }
   		    
   		    if ( alias != null ) {
   		    	actType = matchActivityType( alias, prdTypes);
    		}
   		    
   		    if ( actType == null || actType.equalsIgnoreCase( "Default") ) {
   		    	if ( type != null ) {
   		    	    String fullTypeName = new String( type ); 
   		    		if ( subtype != null ) {
   		    			fullTypeName = new String( fullTypeName + "(" + subtype + ")" );
   		    		}
   		    		
   		    		actType = matchActivityType( fullTypeName, prdTypes );
   		    	}
   		    }
    	}
		  	
    	return actType;    	
    }
    
    /* 
     * Find all activity info in a loaded LPF file (e.g., alias, type, subtype)
     * @param 	lpfMap HashMap<String, String>
     * @return 	HashMap<String, String>
     */
    private HashMap<String, String> getActivityInfoMap( HashMap<String, String> lpfMap ) {
        
    	HashMap<String, String>  actInfo = new HashMap<String, String>();
    	if ( lpfMap != null && lpfMap.size() > 0 ) {    		
    		for ( String key : lpfMap.keySet() ) {
    			if ( key != null && key.contains( "activity" ) ) {
    				actInfo.put( key, lpfMap.get( key ) );
    			}
    		}
    	}
   	
    	return actInfo;    	
    }
       
}
