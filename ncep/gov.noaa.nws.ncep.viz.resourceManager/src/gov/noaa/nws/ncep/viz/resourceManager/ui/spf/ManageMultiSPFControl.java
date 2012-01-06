package gov.noaa.nws.ncep.viz.resourceManager.ui.spf;

import gov.noaa.nws.ncep.viz.common.ui.FileDateComparator;
import gov.noaa.nws.ncep.viz.common.ui.FileNameComparator;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
//import gov.noaa.nws.ncep.viz.resource.manager.util.OpenedNcMapEditorInfoManager;
import gov.noaa.nws.ncep.viz.resources.manager.NmapResourceUtils;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
//import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.UiPlugin;



/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/29/11		  #416		 M. Gao      Created
 * 07/11/11                  Greg Hull   Get Displays instead of storing RscBundleDisplayMngr
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class ManageMultiSPFControl extends Composite {

    private Shell shell;
    
    private final int YES_TO_ALL_INT_VALUE = 0; 
    private final int NO_SKIP_INT_VALUE = 1; 
    private final int NO_NEW_NAME_INT_VALUE = 2; 
    private final int YES_INT_VALUE = 3; 
    
    private boolean yesToAll; 

    public boolean isYesToAll() {
		return yesToAll;
	}
	public void setYesToAll(boolean yesToAll) {
		this.yesToAll = yesToAll;
	}
    
	private Button display_by_name = null;
    private Button display_by_date = null;

    private Combo spf_group_combo = null;

    private Text spf_name_text; 
    
    private Button save_ref_time_btn = null;
    
    private ListViewer spf_name_lviewer = null;
    
    private Button save_btn = null;
    
	private String seldSpfGroup = null;
	
	private boolean saveRefTime;
	
	private String savedRbdFilename; 

	public ManageMultiSPFControl(Composite parent) {
		super(parent, SWT.NONE);
		
		shell = parent.getShell(); 

        Composite topComposite = this;        

        topComposite.setLayout( new FormLayout() );

        FormData fd = new FormData( );

        Group display_by_grp = new Group( topComposite, SWT.SHADOW_NONE );
        display_by_grp.setText("Save Displays to SPF"); 
        display_by_grp.setLayout( new FormLayout() );
        fd.top = new FormAttachment( 0, 10 );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment( 40, 0 );
        fd.bottom = new FormAttachment(15, 0); 
        display_by_grp.setLayoutData( fd );    	
        
        display_by_name = new Button( display_by_grp, SWT.RADIO );
        display_by_name.setText("Sort Alphabetically");
        fd = new FormData();
        fd.top = new FormAttachment( 0, 20 );
        fd.left  = new FormAttachment( 10, 0);
        display_by_name.setLayoutData( fd );
        display_by_name.setSelection( true );
        display_by_name.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
              spf_name_lviewer.setContentProvider( NmapCommon.createSubDirContentProvider( new FileNameComparator()) );     
              spf_name_lviewer.setLabelProvider( NmapCommon.createFileLabelProvider());
     		  spf_name_lviewer.refresh(true);
     		 
       		}
        });
     
        display_by_date = new Button( display_by_grp, SWT.RADIO );
        display_by_date.setText("Sort By Date");
        fd = new FormData();
        fd.top = new FormAttachment( display_by_name, 5, SWT.BOTTOM );
        fd.left  = new FormAttachment( display_by_name, 0,SWT.LEFT );

        display_by_date.setLayoutData( fd );    	
        
        display_by_date.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
                spf_name_lviewer.setContentProvider( NmapCommon.createSubDirContentProvider( new FileDateComparator() ) );     
                spf_name_lviewer.setLabelProvider( NmapCommon.createFileLabelProvider() );
       		    spf_name_lviewer.refresh(true);
       		 
         		}
          });

        spf_group_combo = new Combo( topComposite, SWT.DROP_DOWN );
        fd = new FormData();
        fd.top = new FormAttachment( display_by_grp, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( display_by_grp, 20, SWT.LEFT );
        fd.right  = new FormAttachment( display_by_grp, -20, SWT.RIGHT );
        spf_group_combo.setLayoutData( fd );    	

        Label spf_grp_lbl = new Label( topComposite, SWT.NONE);
        spf_grp_lbl.setText("SPF Group");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spf_group_combo, -3, SWT.TOP );
        fd.left  = new FormAttachment( spf_group_combo, 0, SWT.LEFT );
        spf_grp_lbl.setLayoutData( fd );
        
        Group spf_name_grp = new Group( topComposite, SWT.SHADOW_NONE );
        spf_name_grp.setText("Available SPFs ");
        spf_name_grp.setLayout( new FormLayout() );
        fd = new FormData(260,300);
        fd.top = new FormAttachment( spf_group_combo, 20, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment(40, 0);
        spf_name_grp.setLayoutData( fd );    	

        spf_name_lviewer = new ListViewer(spf_name_grp, 
        		SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL| SWT.H_SCROLL);
        fd = new FormData();
        fd.height = 250;
        fd.top = new FormAttachment( 0, 5 );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment( 100, -10 );
        fd.bottom = new FormAttachment( 100, -65 );
        spf_name_lviewer.getList().setLayoutData( fd );    	


        spf_name_text = new Text(spf_name_grp, SWT.SINGLE);
        fd = new FormData();
        fd.top = new FormAttachment( spf_name_lviewer.getList(), 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.LEFT );  
        fd.right  = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.RIGHT );  
        spf_name_text.setLayoutData( fd );

        Label spf_name_lbl = new Label( spf_name_grp, SWT.NONE);
        spf_name_lbl.setText("SPF Name");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spf_name_text, -3, SWT.TOP );
        fd.left  = new FormAttachment( spf_name_text, 0, SWT.LEFT );
        spf_name_lbl.setLayoutData( fd );

        save_ref_time_btn = new Button( topComposite, SWT.CHECK );
        fd = new FormData();
        save_ref_time_btn.setText("Save Reference Time");
        fd.top = new FormAttachment( spf_name_grp, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( spf_name_grp, 0, SWT.LEFT );
        save_ref_time_btn.setLayoutData( fd );

        save_ref_time_btn.setSelection( saveRefTime );
        
        save_ref_time_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveRefTime = save_ref_time_btn.getSelection();
       		}
        });

        save_btn = new Button( topComposite, SWT.PUSH );
        save_btn.setText("  Save All RBDs  ");
        fd = new FormData();
    	fd.top = new FormAttachment(save_ref_time_btn, 15, SWT.BOTTOM);
    	fd.left = new FormAttachment( save_ref_time_btn, 0, SWT.LEFT);
        save_btn.setLayoutData( fd );

        save_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveAllRBD();
       		}
        });

    	initWidgets();
    	
	}


    private void initWidgets() {
    	
    	spf_group_combo.addSelectionListener(new SelectionListener() {
   			public void widgetSelected(SelectionEvent e) {
   				setSeldSpfGroup( spf_group_combo.getText() );
   			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
   				setSeldSpfGroup( spf_group_combo.getText() );
			} 
   		});
    	
    	spf_name_text.addSelectionListener(new SelectionListener() {
   			public void widgetSelected(SelectionEvent e) {
   				setSeldSpfName( spf_name_text.getText() );
   			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
   				setSeldSpfName( spf_name_text.getText() );
			} 
   		});

    	
    	spf_group_combo.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
    			save_btn.setEnabled( 
    					!spf_name_text.getText().isEmpty() && 
    					!spf_group_combo.getText().isEmpty() ); 			
			}
    	});

    	spf_name_text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
    			save_btn.setEnabled( 
    					!spf_name_text.getText().isEmpty() && 
    					!spf_group_combo.getText().isEmpty() ); 			
			}
    	});

        spf_name_lviewer.setContentProvider( NmapCommon.createSubDirContentProvider() );     
        spf_name_lviewer.setLabelProvider( NmapCommon.createFileLabelProvider( ) );

        spf_name_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged( SelectionChangedEvent event ) {
            	StructuredSelection seldSpfs = (StructuredSelection)spf_name_lviewer.getSelection();  

            	File spfFile = (File)seldSpfs.getFirstElement();
            	
            	setSeldSpfName( spfFile.getName() );
            }
        });

        spf_group_combo.setItems( NmapResourceUtils.getAvailSPFGroups() );
        
        // if the user has pre selected a group then select it
        //
        if( seldSpfGroup != null ) {
        	for( int g=0 ; g<spf_group_combo.getItemCount() ; g++ ) {
        		if( seldSpfGroup.equals( spf_group_combo.getItem(g) ) ) {
            		spf_group_combo.select(g);
            		setSeldSpfGroup( seldSpfGroup );        			
        		}
        	}
        }
        else if( spf_group_combo.getItemCount() > 0 ) {
    		spf_group_combo.select(0);
    		setSeldSpfGroup( spf_group_combo.getText() );        			

        }
        
    }

    
	private void setSeldSpfGroup(String spfGroup) {
		seldSpfGroup = spfGroup;

		File spfGroupDir = new File( NmapResourceUtils.getSpfGroupsDir(), 
                						spf_group_combo.getText() );	

		// if this is a valid spf group then get set the content provider 
    	// of the spf name viewer and select one if not already set. 
    	//
    	if( spfGroupDir.exists() && spfGroupDir.isDirectory() ) {
    		spf_name_lviewer.setInput( spfGroupDir );    
    	}
    	else {
    		spf_name_lviewer.setInput( null );
//    		rbd_name_combo.setText("");
    		return;
    	}

    }
    
	private void setSeldSpfName( String spfName ) {
		spf_name_text.setText( spfName );
			
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
    
    private void saveAllRBD() {    
      try {
    	File spfGroupDir = new File( NmapResourceUtils.getSpfGroupsDir( ),  
    			spf_group_combo.getText() );	
    	String seldSpfName = spf_name_text.getText();

    	// get the name of the directories and create them if needed
    	if( !spfGroupDir.exists() && !spfGroupDir.mkdir() ) {
    		System.out.println("Error creating SPF group directory : " + 
    				spfGroupDir.getAbsolutePath() );
    		throw new VizException("Error creating SPF group directory : " + 
    				spfGroupDir.getAbsolutePath() );
    	}   			
    	File spfDir = new File( spfGroupDir, seldSpfName );

    	if( !spfDir.exists() && !spfDir.mkdir() ) {
    		System.out.println("Error creating SPF : " + 
    				spfDir.getAbsolutePath() );
    		throw new VizException("Error creating SPF : " + 
    				spfDir.getAbsolutePath() );
    	}   			

    	//????? do we still need saveRefTime flag ????

    	List<NCMapEditor> displayList = NmapUiUtils.getAllNCDisplays(); 
    	Integer numRBDs = 0;
    	
    	// loop thru all the active Displays and create an RbdBundle for each.
    	//
    	for( NCMapEditor display : displayList) {
    		RbdBundle rbd = new RbdBundle();
    		
    		rbd.setNcEditor( display );
    		
    		rbd.initFromEditor();   

    		File rbdFile = getRbdFile( spfDir, display.getApplicationName() );
    		
    		try {
        		if( rbdFile != null ) { // if null assume duplicate name and user choose to skip 
        			SerializationUtil.jaxbMarshalToXmlFile( rbd, rbdFile.getAbsolutePath() );
        			numRBDs++;
        		}
    		} catch ( SerializationException e ) {
    			VizException ve = new VizException( e );
    			throw ve;
    		} 
    	}

    	final Integer createdRBDs = numRBDs; 

    	VizApp.runSync(new Runnable() {
    		public void run() {
    			MessageBox mb = new MessageBox( shell, SWT.OK );         								
    			mb.setText( "SPF Created" );
    			mb.setMessage( "SPF Created with "+ createdRBDs + " RBDs" );
    			mb.open();
    		}
    	});
      }
      catch( VizException e ) {
		final String msg = e.getMessage();
		VizApp.runSync(new Runnable() {
			public void run() {
				Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0, msg, null );
				ErrorDialog.openError(Display.getCurrent().getActiveShell(),
						"ERROR", "Error.", status);
			}
		});
	  }    	
    }

    private File getRbdFile(File parentDir, String rbdBundleName) {
    	File rbdFile = null; 
    	if(isDirectoryFileValid(parentDir)) {
	    	savedRbdFilename = rbdBundleName + ".xml"; 
    		if(isFileOKToBeSaved(parentDir, savedRbdFilename, isYesToAll())) {
    	    	rbdFile = new File(parentDir, savedRbdFilename);
    		}
    	}
    	return rbdFile; 
    }
    
	private boolean isDirectoryFileValid(File file) {
		boolean isDirectoryFileValid = true; 
		if(file != null) {
			if(!file.exists() && !file.mkdir())
				isDirectoryFileValid = false; 
		}
		return isDirectoryFileValid; 
	}

	private boolean isFileValid(File file) {
		boolean isFileValid = true; 
		if(file == null) {
			isFileValid = false; 
		}
		return isFileValid; 
	}

    private boolean isFileOKToBeSaved(File parentDirFile, String filename, boolean isYesToAll) {
    	boolean isOKflag = true; 
    	if(!isYesToAll) {
        	String[] existFilenameArray = parentDirFile.list(); 
        	if( isFilenameAlreadyExist(existFilenameArray, filename) ) {
        		MessageDialog confirmDlg = new MessageDialog( 
        				NmapUiUtils.getCaveShell(), 
        				"Create RBD", null, 
        				"RBD " +filename+" already exists in this SPF.\n\n"+
        				"Do you want to overwrite it?",
        				MessageDialog.QUESTION, new String[]{"Yes to All", "No, Skip", "No, New Name", "Yes"}, 0);
        		confirmDlg.open();
        		int returnCode = confirmDlg.getReturnCode(); 
        		if(returnCode == YES_TO_ALL_INT_VALUE) {
        			setYesToAll(true); 
        		} else if(returnCode == NO_SKIP_INT_VALUE) {
            		isOKflag = false; 
        		} else if(returnCode == NO_NEW_NAME_INT_VALUE) {
        			NewSpfFileNameDialog newSpfFileNameDialog = new NewSpfFileNameDialog(shell, parentDirFile); 
        			newSpfFileNameDialog.open(); 
        			savedRbdFilename = newSpfFileNameDialog.getSelectedSpfFilename(); 
        			newSpfFileNameDialog = null; 
        		}
        	}
    	}
    	return isOKflag; 
    }
    
    private boolean isFilenameAlreadyExist(String [] existFilenameArray, String filename) {
    	boolean isFileExist = false; 
    	for(String eachFilename : existFilenameArray) {
    		if(eachFilename.equals(filename)) {
    			isFileExist = true; 
    			break; 
    		}
    	}
    	return isFileExist; 
    }
    
//    public void saveAllRBDs( boolean new_pane ) {
//    	try {    		
//			NCTimeMatcher timeMatcher = timelineControl.getTimeMatcher();
//    		boolean saveRefTime = !timeMatcher.isCurrentRefTime();
//    		
//    		// get the filename to save to.
//    		SaveRbdDialog saveDlg = new SaveRbdDialog( shell,
//    				savedSpfGroup, savedSpfName, rbd_name_txt.getText(), saveRefTime ); 
//    		
//    		File rbdFile = (File)saveDlg.open();
//    		
//    		if( rbdFile == null ) {
//    			return; 
//    		}
//
//    		/*
//    		 * repeated logic??? Gao's comment
//    		 */
////    		savedSpfGroup = saveDlg.getSeldSpfGroup();
////    		savedSpfName  = saveDlg.getSeldSpfName();
//    		String savedRbdName = saveDlg.getSeldRbdName(); 
//    		String rbdFilePathPrefix = getRbdFilePathPrefix(rbdFile.getAbsolutePath()); 
//    		
//    		saveRefTime = saveDlg.getSaveRefTime();   ////???????? Gao's comment 
//   
//    		/*
//    		 * disable this section at this moment because do not know how to implement it for
//    		 * multiple editors
//    		 */
////    		// Set the name to the name that was actually used 
////    		// to save the RBD.
////    		// TODO : we could store a list of the RBDNames and load these
////    		// as items in the combo.
///////    		rbd_name_txt.setText( saveDlg.getSeldRbdName() );
////    		
////    		rbdMngr.setGeoSyncPanes( geo_sync_panes.getSelection() );
////    		rbdMngr.setAutoUpdate( auto_update_btn.getSelection() );
//    		
//    		// if the user elects not to save out the refTime then don't marshal it out. 
//
//			if( !saveRefTime ) {
//    			timeMatcher.setCurrentRefTime();     			
//    		}
//			
////			RscBundleDisplayMngr rscBundleDisplayManager = null; 
////			RscBundleDisplayMngr rscBundleDisplayManager = new RscBundleDisplayMngr(); 
////			rscBundleDisplayManager.init(); 
//			
//			/*
//			 * Now retrieved all opened NcmapEditors
//			 */
//    		int fileIndexInt = 1; 
//			List<NCMapEditor> ncMapEditorList = NmapUiUtils.getAllNCDisplays(); 
//			for(NCMapEditor eachNCMapEditor : ncMapEditorList) {
//				String rbdBundleName = eachNCMapEditor.getApplicationName(); 
//				PaneLayout eachPaneLayout = eachNCMapEditor.getPaneLayout(); 
//				RscBundleDisplayMngr eachRscBundleDisplayMngr = OpenedNcMapEditorInfoManager.getRscBundleDisplayMngrByNcMapEditor(eachNCMapEditor); 
//				if(eachRscBundleDisplayMngr != null) {
//					RbdBundle eachRbdBundle = eachRscBundleDisplayMngr.createRbdBundle(rbdBundleName, timeMatcher, eachPaneLayout); 
//		    		try {
//		    			SerializationUtil.jaxbMarshalToXmlFile( eachRbdBundle, rbdFile.getAbsolutePath() );
////		    			SerializationUtil.jaxbMarshalToXmlFile( rbdBundleCollectionObject, rbdFile.getAbsolutePath() );
//		    		} catch (SerializationException e) {
//		    			VizException ve = new VizException( e );
//		    			throw ve;
//		    		} 
//		    		final String msg = new String("Resource Bundle Display "+
//    						rbdBundleName + " Saved." );
//		    		VizApp.runSync(new Runnable() {
//		    			public void run() {
//		    				MessageBox mb = new MessageBox( shell, SWT.OK );         								
//		    				mb.setText( "RBD Saved" );
//		    				mb.setMessage( msg );
//		    				mb.open();
//		    				
//		    				rbdMngr.setRbdModified( false );
//		    			}
//		    		});
//		    		rbdFile = getNextRbdFile(rbdFilePathPrefix, savedRbdName, fileIndexInt); 
//		    		fileIndexInt = getNewFileIndexInt(rbdFile.getName()); 
//				}
//			}
//
//    		
////    		try {
////    			SerializationUtil.jaxbMarshalToXmlFile( rbdBndl, rbdFile.getAbsolutePath() );
//////    			SerializationUtil.jaxbMarshalToXmlFile( rbdBundleCollectionObject, rbdFile.getAbsolutePath() );
////    		} catch (SerializationException e) {
////    			VizException ve = new VizException( e );
////    			throw ve;
////    		} 
////    		
////    		VizApp.runSync(new Runnable() {
////    			public void run() {
////    				String msg = null;
////    				msg = new String("Resource Bundle Display "+
////    						rbd_name_txt.getText() + " Saved." );
////    				MessageBox mb = new MessageBox( shell, SWT.OK );         								
////    				mb.setText( "RBD Saved" );
////    				mb.setMessage( msg );
////    				mb.open();
////    				
////    				rbdMngr.setRbdModified( false );
////    			}
////    		});
//    	}
//    	catch( VizException e ) {
//    		final String msg = e.getMessage();
//    		VizApp.runSync(new Runnable() {
//    			public void run() {
//    				Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0, msg, null );
//    				ErrorDialog.openError(Display.getCurrent().getActiveShell(),
//    						"ERROR", "Error.", status);
//    			}
//    		});
//    	}
//    }

}
