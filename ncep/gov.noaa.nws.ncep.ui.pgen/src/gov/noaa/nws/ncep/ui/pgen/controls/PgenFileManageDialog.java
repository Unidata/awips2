/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageDialog
 * 
 * 11 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.io.File;

import java.util.ArrayList;
import java.util.LinkedHashMap;

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

import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.*;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo;


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
 * 07/11		#335		Jun Wu	    Implemented the file structure to organize PGEN files.
 * 										
 * </pre>
 * 
 * @author
 * @version 1
 */
public class PgenFileManageDialog extends CaveJFACEDialog {
    
	public static enum PgenFileMode { OPEN, SAVE, SAVE_AS };
	
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

	private static final int REPLACE_ID = IDialogConstants.CLIENT_ID + 7586;
	private static final String REPLACE_LABEL = "Replace";
	private static final int APPEND_ID = IDialogConstants.CLIENT_ID + 7587;
	private static final String APPEND_LABEL = "Append";
	private static final int MERGE_ID = IDialogConstants.CLIENT_ID + 7588;
	private static final String MERGE_LABEL = "Merge";
	private static final int SAVE_ID = IDialogConstants.CLIENT_ID + 7589;
	private static final String SAVE_LABEL = "Save";
	private static final int CANCEL_ID = IDialogConstants.CLIENT_ID + 7590;
	private static final String CANCEL_LABEL = "Cancel";

	private Button replaceBtn = null;   
    private Button appendBtn = null;
    private Button mergeBtn = null;
    private Button cancelBtn = null;
    private Button saveBtn = null;   
 
    private static LinkedHashMap<String, String>  dirTableMap = null;
    private String dirLocal = ".";
    
    private static String selectedDir = null;      
    private static String fileName = null; 
    private static String fullName = null; 
       
    private static PgenFileMode fileMode = PgenFileMode.OPEN;
    
    private static int lastDirPos = -1;
   
    /*
     *  Constructor
     */
    public PgenFileManageDialog( Shell parShell, String btnName ) throws VizException {
     	
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
    	else if ( btnName.equals( "Save" ) ) {
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
        
        FormData layoutData1 = new FormData( 340, 25 );
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
        
        for ( String str : dirTableMap.keySet() ) {
        	dirList.add( str );
        }
 
        if ( lastDirPos < 0 ) {
        	lastDirPos =  (dirList.getItemCount() - 1);
        }
        dirList.setSelection( lastDirPos );
	    selectedDir = "" + dirTableMap.get( dirList.getSelection()[0] );      
        
        FormData layoutData6 = new FormData( 320, 200 );
        layoutData6.top  = new FormAttachment( dirLbl, 5, SWT.BOTTOM );
        layoutData6.left = new FormAttachment( dirLbl, 0, SWT.LEFT );
        dirList.setLayoutData( layoutData6 );        
        	
        dirList.addListener ( SWT.Selection, new Listener () {
    		public void handleEvent (Event e) {
    			if ( dirList.getSelectionCount() > 0 ) {
    			    
    				selectedDir = "" + dirTableMap.get( dirList.getSelection()[0] );

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

        FormData layoutData9 = new FormData( 320,200 );
        layoutData9.top  = new FormAttachment( fileLbl, 5, SWT.BOTTOM );
        layoutData9.left = new FormAttachment( fileLbl, 0, SWT.LEFT );
  
        fileListViewer.getList().setLayoutData( layoutData9 );
        
    	if ( sortByNameBtn.getSelection() ) {
            fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml" }, new FileNameComparator()) );     
    	}
    	else {
            fileListViewer.setContentProvider( NmapCommon.createFileContentProvider(new String[]{".xml" }, new FileDateComparator()) );    		
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
         	            appendBtn.setEnabled(false);
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
    	                appendBtn.setEnabled(true);           
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
        
            FormData layoutData9_2 = new FormData(320, 15);
            layoutData9_2.top   = new FormAttachment( fileNameLbl, 10, SWT.BOTTOM );
            layoutData9_2.left  = new FormAttachment( fileNameLbl, 0, SWT.LEFT );       
            fileNameText.setLayoutData( layoutData9_2 );     
        
        	fileNameText.setText( "" );
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
       
        FormData layoutData10 = new FormData(330, 25);
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

         		browseDlg.setFilterNames (new String [] {"Product files (*.xml)" });
         		browseDlg.setFilterExtensions (new String [] {"*.xml"});
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
           
    		replaceBtn = createButton( parent, REPLACE_ID, REPLACE_LABEL, true );
            appendBtn = createButton( parent, APPEND_ID, APPEND_LABEL, true );
           mergeBtn = createButton( parent, MERGE_ID, MERGE_LABEL, true );
           
           replaceBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {
                    openProducts( true );
                }          		            	 	
            });
 
            appendBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) { 
                   openProducts( false );               	
                }          		            	 	
            });
            
            mergeBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {
                    mergeProducts();
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
    	
        cancelBtn = createButton( parent, CANCEL_ID, CANCEL_LABEL, true );
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

    	dirTableMap.put( "Local",		dirLocal );         
    }       


    /**
     *  Open a product file to replace or append to the current product list
     *  in the current PGEN session.
     */
    private void openProducts( boolean replace ) {
   	       	
    	if ( !validProductFile( fullName ) ) {
    		return;
    	}
    	
    	Products products = FileTools.read( fullName );
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
   	    pgen.setAutosave( autoSaveOnBtn.getSelection() );
   	    pgen.setAutoSaveFilename(fullName);
   	       	    
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenProds =
   	    									ProductConverter.convert( products );
   	    
/*
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenNewProds = new 
	             ArrayList<gov.noaa.nws.ncep.ui.pgen.elements.Product>();
   	    
   	    for ( gov.noaa.nws.ncep.ui.pgen.elements.Product pgPrd: pgenProds ) {
   	    	java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> singleLoad =
   	    		             loadSingleProductFile( pgPrd.getInputFile() );
   	    	if ( singleLoad != null ) {
   	    	    pgenNewProds.addAll( singleLoad );
   	    	}
   	    	else {
   	  	      	pgenNewProds.add( pgPrd );
   	    	}
   	    }
 	       	 
   	    if ( replace ) {
   	        pgen.replaceProduct( pgenNewProds );     
   	    }
   	    else {
	        pgen.appendProduct( pgenNewProds );     
   	    }
*/

  	    close();
    	
   	    if ( replace ) {
   	        pgen.replaceProduct( pgenProds );     
   	    }
   	    else {
	        pgen.appendProduct( pgenProds );     
   	    }
   	    
   	    PgenUtil.refresh();             	                	    

    }

    /**
     *  Merge the products in a file with those in the current product list.
     */
    private void mergeProducts() {
    	   	
    	/*
    	 * Validate
    	 */
    	if ( !validProductFile( fullName ) ) {
    		return;
    	}
    	
    	Products products = FileTools.read( fullName );
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
   	    pgen.setAutosave( autoSaveOnBtn.getSelection() );
   	    pgen.setAutoSaveFilename(fullName);
   	    
   	    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> pgenProds =
   	    									ProductConverter.convert( products );

  	    close();
    	
	    pgen.mergeProduct( pgenProds );
  	       	    
   	    PgenUtil.refresh();             	                	    
          	    
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
            		MessageDialog.ERROR, new String[]{"OK"}, 0);
            
        	confirmDlg.open();
            
        	if ( (confirmDlg.getReturnCode() == MessageDialog.OK) ) {
            	close();
            	return;
            }
		
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

            rsc.saveProducts( fullName );        
        	
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
    			if ( !indir.canWrite() ) {
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
     *  Load a product file into the a product
     */
    private java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> loadSingleProductFile( String prdFileName ) {
   	    
    	java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prdList = null;
    	
    	if ( prdFileName == null ) {
    		return null;
    	}
    	
    	File infile = new File( prdFileName );
    	if ( !infile.exists() || !infile.canRead() ) {
    		return null;
    	}
    	    	    
    	prdList = new ArrayList<gov.noaa.nws.ncep.ui.pgen.elements.Product>();
        Products fproducts = FileTools.read( prdFileName );
        
        java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> inPrdList =
        	              ProductConverter.convert( fproducts );
        	
     	    
   	    /*
   	     * some Volcano Products are pure texts: TEST/RESUME
   	     * and cannot be drawn.
   	     */
    	if( VaaInfo.isNoneDrawableTxt( fproducts ) ){ 
	   		VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);	   			   	        
    	    return null;
    	}
	    
    	for ( gov.noaa.nws.ncep.ui.pgen.elements.Product prd : inPrdList ) {
            String sinfile = prd.getInputFile();
    		if ( sinfile != null && !sinfile.equals( prdFileName ) ) {
                prdList.addAll( loadSingleProductFile( sinfile ) );
            }
    		else { 
    			gov.noaa.nws.ncep.ui.pgen.elements.Product prdbackup = prd.copy(); 
    			prdbackup.clear();
    			
    			int ii = 0;
       	    	for ( gov.noaa.nws.ncep.ui.pgen.elements.Layer lyr : prd.getLayers() ) {
    			    java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Layer> singleLoad =
  		                  loadSingleLayerFile( lyr.getInputFile() );
    			    
  	                if ( singleLoad != null ) {
                        
  	            	    for ( gov.noaa.nws.ncep.ui.pgen.elements.Layer onelyr : singleLoad ) {
  	                        prdbackup.addLayer( onelyr );
  	            	    }
  	                }
  	                else {
  	                	prdbackup.addLayer( lyr );
  	                }
  	                
  	                ii++;
  	                
 	            }
       	    	      	    	
	            prdList.add( prdbackup );
    		}   	    	    
        }
  	   	       	    	   		   	
	   	return prdList;
    }
    
    /**
     *  Open a file into a layer
     */
    private java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Layer> loadSingleLayerFile( String lyrFileName ) {
   	    
    	java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Layer> lyrList = null;
    	
    	if ( lyrFileName == null ) {
    		return null;
    	}
    	
    	File infile = new File( lyrFileName );
    	if ( !infile.exists() || !infile.canRead() ) {
    		return null;
    	}
    	    	    
    	lyrList = new ArrayList<gov.noaa.nws.ncep.ui.pgen.elements.Layer>();
        Products fproducts = FileTools.read( lyrFileName );
        
        java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> inPrdList =
        	              ProductConverter.convert( fproducts );
        	
     	    
   	    /*
   	     * some Volcano Products are pure texts: TEST/RESUME
   	     * and cannot be drawn.
   	     */
    	if( VaaInfo.isNoneDrawableTxt( fproducts ) ){ 
	   		VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);	   			   	        
    	    return null;
    	}
    	else if ( inPrdList.size() <= 0 ) {
    		return null;
    	}
        
    	gov.noaa.nws.ncep.ui.pgen.elements.Product prd = inPrdList.get(0);
    	
		String sinfile = null;
    	for ( gov.noaa.nws.ncep.ui.pgen.elements.Layer lyr : prd.getLayers() ) {
            sinfile = lyr.getInputFile();
            if ( sinfile != null && !sinfile.equals( lyrFileName ) ) {
                lyrList.addAll( loadSingleLayerFile( sinfile ) );
            }
            else {
           	    lyrList.add( lyr );           	
            }          	
    	}
  	   	       	    	   		   	
	   	return lyrList;
    }
       
}
