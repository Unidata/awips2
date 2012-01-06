/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageDialog
 * 
 * 11 February 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.pgen;

import java.io.File;
//import java.util.ArrayList;
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

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;

import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.nws.ncep.viz.common.ui.*;

//import gov.noaa.nws.ncep.ui.pgen.file.*;
//import gov.noaa.nws.ncep.ui.pgen.PgenSession;
//import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
//import gov.noaa.nws.ncep.ui.pgen.elements.Product;
//import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

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
 * 01/10        #202        B. Hebbard  Swiped from PGEN proper; variant used
 *                                      for PGEN display resource file select
 *                                      -- intended to be replaced by selection
 *                                      built in to Resource Bundle Definition
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
    
    private Button replaceBtn = null;   

    private Button cancelBtn = null;
    private Button saveBtn = null;   
 
    private static LinkedHashMap<String, String>  dirTableMap = null;
    private static String dirLocal = ".";
          
    private Boolean sortByName = true;
    private Boolean sortByDate= false;
        
    private static String selectedDir = null;      
    private static String fileName = null; 
    private static String fullName = null; 
       
    private static PgenFileMode fileMode = PgenFileMode.OPEN;
   
    /*
     *  Constructor
     */
    public PgenFileManageDialog( Shell parShell, String btnName ) throws VizException {
     	
    	super ( parShell );   	       	
    	
    	setFileMode( btnName );    	
    	       	
    	if ( dirTableMap == null )  loadUserTable();
    }  
 
    /*
     * Set up the file mode.
     */
    private void setFileMode( String btnName ) {

    	if ( btnName.equals( "Open" ) ) {
        	fileMode = PgenFileMode.OPEN;
    	    title = "Open a Product file";
    	}
    	else if ( btnName.equals( "Save" ) ) {
           	fileMode = PgenFileMode.SAVE;
    	    title = "Save the Product";
    	}
    	else if ( btnName.equals( "Save As" ) ) {
           	fileMode = PgenFileMode.SAVE_AS;
    	    title = "Save the Product as";
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
            
    /*
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
        Label sortLbl = new Label(topForm, SWT.NONE);
        sortLbl.setText("Display Files In Order By:");
        
        FormData layoutData1 = new FormData( 340, 25 );
        layoutData1.top   = new FormAttachment( 0, 5 );
        layoutData1.left  = new FormAttachment( 0, 5 );
        sortLbl.setLayoutData( layoutData1 );
        
        sortByNameBtn = new Button(topForm, SWT.RADIO);
        sortByNameBtn.setText("Name");
        sortByNameBtn.setEnabled( true );
        
        FormData layoutData2 = new FormData();
        layoutData2.top   = new FormAttachment( sortLbl, 5, SWT.BOTTOM );
        layoutData2.left  = new FormAttachment( sortLbl, 0, SWT.LEFT );
        sortByNameBtn.setLayoutData( layoutData2 );

        sortByNameBtn.addListener( SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {           
         		sortByName = true;
         		sortByDate = false;      		
            }          		            	 	
        } );       
        
        sortByDateBtn = new Button( topForm, SWT.RADIO );
        sortByDateBtn.setText("Date");       
        sortByDateBtn.setEnabled( true );
        
        FormData layoutData3 = new FormData();
        layoutData3.top   = new FormAttachment( sortByNameBtn, 5, SWT.BOTTOM );
        layoutData3.left  = new FormAttachment( sortByNameBtn, 0, SWT.LEFT );       
        sortByDateBtn.setLayoutData( layoutData3 );

        sortByDateBtn.addListener( SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {           
         		sortByName = false;
         		sortByDate = true;
            }          		            	 	
        } );       
           
        /*
         *  Create a list of directories to select from
         */
        Label dirLbl = new Label(topForm, SWT.NONE);
        dirLbl.setText("Select Directory:");
        
        FormData layoutData5 = new FormData();
        layoutData5.top   = new FormAttachment( sortByDateBtn , 15, SWT.BOTTOM );
        layoutData5.left  = new FormAttachment( sortByDateBtn, 0, SWT.LEFT );       
        
        dirLbl.setLayoutData( layoutData5 );
        
        dirList = new List( topForm, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL );
      
        for ( String str : dirTableMap.keySet() ) {
        	dirList.add( str );
        }
        
        dirList.setSelection( (dirList.getItemCount() - 1) );
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
   		            fileListViewer.refresh();  		            
    			
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
        
        fileListViewer.setContentProvider( NmapCommon.createFileContentProvider( new String[]{".xml", ".txt" } ) );          
        fileListViewer.setLabelProvider( NmapCommon.createFileLabelProvider( new String[]{""} ) ); 

        fileListViewer.setInput( new File( selectedDir ) ); 
        fileListViewer.refresh();
       
        fileListViewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
             	String[] selectedFile = fileListViewer.getList().getSelection();
            	            	    
            	if( selectedFile.length == 0 )	{
            		if ( fileMode != PgenFileMode.OPEN ) {       	        
            		    saveBtn.setEnabled(false);
            		}
            		else {            		
            			replaceBtn.setEnabled(false);
         	            //appendBtn.setEnabled(false);
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
    	                //appendBtn.setEnabled(true);           
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
            fileNameText.setSize( 310, 10 );
            fileNameText.setEditable( true );
            
            fileNameText.addModifyListener( new ModifyListener() {
                public void modifyText( ModifyEvent  event ) {           
                	
                	String usrFile = fileNameText.getText().trim();
                                    
                    if ( !usrFile.isEmpty() ) {                    	
                    	if ( !usrFile.contains( File.separator ) ) {
                			usrFile = dirLocal + File.separator + usrFile; 
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
         		browseDlg = new FileDialog( shell, SWT.OPEN );
         		browseDlg.setFilterNames (new String [] {"SVG files (*.xml)" });
         		browseDlg.setFilterExtensions (new String [] {"*.xml"});
         		browseDlg.setFilterPath ( dirLocal ); 
         		browseDlg.open();
         		
         		if ( !browseDlg.getFileName().isEmpty() ) {
         		    selectedDir = browseDlg.getFilterPath();
         		    fileName = browseDlg.getFileName();
         		    fullName = "" + selectedDir + File.separator + fileName;
         		    if ( fileMode != PgenFileMode.OPEN ) {
         		    	fileNameText.setText( fullName );
         		    }
                }
            }          		            	 	
        } );       
        
        /*
         *  Create a label and two radio buttons to turn "auto save" on or off.
         */
        /*
        Label autoSaveLbl = new Label(topForm, SWT.NONE);
        autoSaveLbl.setText("Auto Save:");
        
        FormData layoutData11 = new FormData();
        layoutData11.top   = new FormAttachment( browseBtn, 20, SWT.BOTTOM );
        layoutData11.left  = new FormAttachment( browseBtn, 0, SWT.LEFT );       
        autoSaveLbl.setLayoutData( layoutData11 );
      
        autoSaveOffBtn = new Button(topForm, SWT.CHECK);
        autoSaveOffBtn.setText("Off");       
        
        FormData layoutData12 = new FormData();
        layoutData12.top   = new FormAttachment( autoSaveLbl, 0, SWT.TOP );
        layoutData12.left  = new FormAttachment( autoSaveLbl, 10, SWT.RIGHT );       
        autoSaveOffBtn.setLayoutData( layoutData12 );
         
        autoSaveOnBtn = new Button(topForm, SWT.CHECK);
        autoSaveOnBtn.setText("On");       
        
        FormData layoutData13 = new FormData();
        layoutData13.top   = new FormAttachment( autoSaveOffBtn, 0, SWT.TOP );
        layoutData13.left  = new FormAttachment( autoSaveOffBtn, 10, SWT.RIGHT );       
        autoSaveOnBtn.setLayoutData( layoutData13 );
        */
        
        return dlgAreaForm;
     }
    
    /**
     *  Create Replace/Append/Cancel button for "Open" a product file or
     *         Save/Cancel button for "Save" a product file.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {       
           	
    	if ( fileMode == PgenFileMode.OPEN ) {
            replaceBtn = createButton( parent, IDialogConstants.OK_ID, "Use", true );
        
            // appendBtn = createButton( parent, IDialogConstants.OK_ID, "Append", false );
        
            cancelBtn = createButton( parent, IDialogConstants.CANCEL_ID, "Cancel", false );

            replaceBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {           
                    openProducts( true );
                }          		            	 	
            });
 
            // appendBtn.addListener( SWT.MouseUp, new Listener() {
            //     public void handleEvent(Event event) {           
            //         openProducts( false );               	
            //     }          		            	 	
            // });
    	}
    	else {
            saveBtn = createButton( parent, IDialogConstants.OK_ID, "Save", true );
            cancelBtn = createButton( parent, IDialogConstants.CANCEL_ID, "Cancel", false );

            saveBtn.addListener( SWT.MouseUp, new Listener() {
                public void handleEvent(Event event) {               	                    
                	saveProducts();
                }
            });
            
    	}

    }
    
    /**
     *  Load the user-defined directories in old vgf.nmap
     */
    private static void loadUserTable() {
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
     *  Open a product file to replace or append to the current product list.
     */
    private void openProducts( boolean replace ) {

    	/*
    	FileTools fileTool = new FileTools( jaxbPackage, Products.class );
   	    Products products = fileTool.read( fullName );
   	       	    
   	    if ( replace ) {
   	        PgenSession.getInstance().getPgenResource().replaceProduct( ProductConverter.convert( products ) );     
   	    }
   	    else {
   	        PgenSession.getInstance().getPgenResource().appendProduct( ProductConverter.convert( products ) );
   	    }
   	      	    
   	    PgenUtil.refresh();
   	    */

    }

    
    /**
     *  Save the products to file.
     */
    private void saveProducts() {
    }

	public Text getFileNameText() {
		return fileNameText;
	}

	public String getFileName() {
		return fileName;
	}

	public String getFullName() {
		return fullName;
	}
   
}



