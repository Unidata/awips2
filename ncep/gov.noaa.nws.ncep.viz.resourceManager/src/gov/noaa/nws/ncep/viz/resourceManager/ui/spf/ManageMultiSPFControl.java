package gov.noaa.nws.ncep.viz.resourceManager.ui.spf;

import gov.noaa.nws.ncep.viz.common.ui.FileDateComparator;
import gov.noaa.nws.ncep.viz.common.ui.FileNameComparator;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
//import gov.noaa.nws.ncep.viz.resource.manager.util.OpenedNcMapEditorInfoManager;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
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
 * 08/04/11      #450        Greg Hull   SpfsManager
 * 02/15/2012     627        Archana    Removed the call to setNcEditor() and updated initFromEditor(0
 *                                      to take an editor as one of the arguments  
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
    
    private boolean yesToAll = false; 
	    
	private Button display_by_name = null;
    private Button display_by_date = null;

    private Combo spf_group_combo = null;

    private Text spf_name_text; 
    
    private Button save_ref_time_btn = null;
    
    private ListViewer spf_name_lviewer = null;
    
    private Button save_btn = null;
    
	private String seldSpfGroup = null;
	
	private boolean saveRefTime;
	
	private String savedRbdName; 

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


        spf_name_text = new Text(spf_name_grp, SWT.SINGLE | SWT.BORDER );
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

        spf_name_lviewer.setContentProvider(new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				// TODO : check display_by_name selection and order appropriately.
				// (add support in SpfsManager to get the time. Not sure how to do this
				// now that it is possible to have the SPF in 2 contexts?)
				return SpfsManager.getInstance().getSpfNamesForGroup( (String)inputElement );
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });          
        
//        spf_name_lviewer.setLabelProvider( NmapCommon.createFileLabelProvider( ) );

        spf_name_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged( SelectionChangedEvent event ) {
            	StructuredSelection seldSpfs = (StructuredSelection)spf_name_lviewer.getSelection();  

            	setSeldSpfName( (String)seldSpfs.getFirstElement() );
            }
        });

        spf_group_combo.setItems( SpfsManager.getInstance().getAvailSPFGroups() );
        
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

		String spfGroupName = spf_group_combo.getText();	

		spf_name_lviewer.setInput( spfGroupName );    
    }
    
	private void setSeldSpfName( String spfName ) {
		spf_name_text.setText( spfName );
			
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
    
    private void saveAllRBD() {    
      try {
    	String spfGroup = spf_group_combo.getText();	
    	String spfName  = spf_name_text.getText();

    	//????? do we still need saveRefTime flag ????

    	List<NCMapEditor> displayList = NmapUiUtils.getAllNCDisplays(); 
    	Integer numRBDs = 0;
    	
    	// loop thru all the active Displays and create an RbdBundle for each.
    	//
    	for( NCMapEditor display : displayList) {
    		RbdBundle rbd = new RbdBundle();
    		
 //   		rbd.setNcEditor( display );
    		
    		rbd.initFromEditor( display );   

    		savedRbdName = rbd.getRbdName();
        	
        	if( !yesToAll ) {        		
        		if( SpfsManager.getInstance().doesRbdExistInUserContext( 
        							spfGroup, spfName, savedRbdName ) ) {
        		
            		MessageDialog confirmDlg = new MessageDialog( 
            				getShell(), 
            				"Create RBD", null, 
            				"RBD " +rbd.getRbdName()+" already exists in this SPF.\n\n"+
            				"Do you want to overwrite it?",
            				MessageDialog.QUESTION, new String[]
            				          {"Yes to All", "No, Skip", "No, New Name", "Yes"}, 0);
            		confirmDlg.open();
            		
            		int returnCode = confirmDlg.getReturnCode(); 
            		
            		if(returnCode == YES_TO_ALL_INT_VALUE) {
            			yesToAll = true; 
            		} 
            		else if(returnCode == NO_SKIP_INT_VALUE) {
            			savedRbdName = null; 
            		} 
            		else if(returnCode == NO_NEW_NAME_INT_VALUE) {
            			NewRbdNameDialog newSpfFileNameDialog = 
            				   new NewRbdNameDialog( shell, 
            						   spfGroup, spfName, rbd.getRbdName() ); 
            			newSpfFileNameDialog.open(); 
            			savedRbdName = newSpfFileNameDialog.getNewRbdName(); 
            			newSpfFileNameDialog = null;
            			
            			rbd.setRbdName( savedRbdName );
            		}
            	}
        	}

    		try {
        		if( savedRbdName != null ) { // if null assume duplicate name and user choose to skip 
        			SpfsManager.getInstance().saveRbdToSpf(spfGroup, spfName, rbd );    			
        			numRBDs++;
        		}
    		} catch ( VizException e ) {
    			throw e;
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

}
