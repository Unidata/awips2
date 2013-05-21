package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


/**
 *  Dialog displayed from RBD Mngr window when the 'Import SPF...' is selected. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/16/10      #226       Greg Hull    Initial Creation
 * 03/08/10      #228       Archana      Added logic to sort files in the
 *                                       list viewer by name and date.
 *                                       Altered the labels for the display_by_name
 *                                       and display_by_date radio buttons and altered
 *                                       their layout.
 * 02/04/11      #408       Greg Hull    add flag to save the reference time.    
 * 08/01/11      #450       Greg Hull    Change spf name combo to a Text widget
 * 08/01/11      #450       Greg Hull    Use SpfsManager with new NcPathManager 
 * 	                                     Localization for USER-level save/delete.
 * 11/03/11      #???       B. Hebbard   Add "Save Source Timestamp As:" Constant / Latest  * 
 * 05/02/12      #585       S. Gurung    Commented out unwanted options ("Sort Alphabetically" and "Sort By Date")  
 *                                       
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class SaveRbdDialog extends Dialog { 
   
    private Shell shell;
    private String dlgTitle = "Save Resource Bundle Display";
    
    private Button display_by_name = null;
    private Button display_by_date = null;

    private Combo spf_group_combo = null;
    private Text  spf_name_txt = null;
    private Combo rbd_name_combo = null;
    
    private Button save_time_as_constant = null;
    private Button save_time_as_latest = null;
    
    private Button save_ref_time_btn = null;
    
    private ListViewer spf_name_lviewer = null;
    
    private Button save_btn = null;
    
	private String seldSpfGroup = null;
	private String seldSpfName = null;
    private String seldRbdName = null;
    
    // true if entering a new SPF or rbd name
    private boolean newRbd = false;
    private Boolean saveRbdOkd = false;

    private boolean saveTimeAsConstant;
	private boolean saveRefTime;

    public String getSeldSpfGroup() {
		return seldSpfGroup;
	}

	public String getSeldSpfName() {
		return seldSpfName;
	}

	public String getSeldRbdName() {
		return seldRbdName;
	}

    public boolean getSaveTimeAsConstant() {
		return saveTimeAsConstant;
	}

    public boolean getSaveRefTime() {
		return saveRefTime;
	}

    public SaveRbdDialog( Shell parShell, String spf_group, String spf_name, String rbd_name, boolean refTime, boolean saveTimeAsConstant )  {
    	super(parShell);
    	seldSpfGroup = spf_group;
    	seldSpfName = spf_name;
    	seldRbdName = rbd_name;    	
    	saveRefTime = refTime;
    	this.saveTimeAsConstant = saveTimeAsConstant;
    }
      
	public Object open( ) {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	
    	shell = new Shell( parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS );
    	shell.setText(dlgTitle);
    	shell.setSize( 540, 500 ); // pack later

    	shell.setLayout( new FormLayout() );

        FormData fd = new FormData( );

        /*Composite display_by_grp = new Composite( shell, SWT.SHADOW_NONE );
        display_by_grp.setLayout( new FormLayout() );
        fd = new FormData();
        fd.top = new FormAttachment( 0, 0 );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment( 100, -10 );
        display_by_grp.setLayoutData( fd );    	
        
        display_by_name = new Button( display_by_grp, SWT.RADIO );
        display_by_name.setText("Sort Alphabetically");
        fd = new FormData();
        fd.top = new FormAttachment( 0, 15 );
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
      //  fd.bottom  = new FormAttachment( 100, -10 );        

        display_by_date.setLayoutData( fd );    	
        
        
        display_by_date.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       		    spf_name_lviewer.refresh(true);
       		 
       		}
        });*/
        
        spf_group_combo = new Combo( shell, SWT.DROP_DOWN );
        fd = new FormData();
        fd.top = new FormAttachment( 5, 10 );
        fd.left  = new FormAttachment( 9, 10 );
        fd.right  = new FormAttachment( 85, 0 );
        /*fd.top = new FormAttachment( display_by_grp, 40, SWT.BOTTOM );
        fd.left  = new FormAttachment( display_by_grp, 20, SWT.LEFT );
        fd.right  = new FormAttachment( display_by_grp, -20, SWT.RIGHT );*/
        spf_group_combo.setLayoutData( fd );    	
        
        Label spf_grp_lbl = new Label( shell, SWT.NONE);
        spf_grp_lbl.setText("SPF Group");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spf_group_combo, -3, SWT.TOP );
        fd.left  = new FormAttachment( spf_group_combo, 0, SWT.LEFT );
        spf_grp_lbl.setLayoutData( fd );

        Group spf_name_grp = new Group( shell, SWT.SHADOW_NONE );
        spf_name_grp.setText("Available SPFs ");
        spf_name_grp.setLayout( new FormLayout() );
        fd = new FormData(260,300);
        fd.top = new FormAttachment( spf_group_combo, 20, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment( 100, -10 );
        spf_name_grp.setLayoutData( fd );    	

        spf_name_lviewer = new ListViewer(spf_name_grp, 
        		SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL| SWT.H_SCROLL);
        fd = new FormData();
//        fd.height = 250;
        fd.top = new FormAttachment( 0, 5 );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right  = new FormAttachment( 100, -10 );
        fd.bottom = new FormAttachment( 100, -65 );
        spf_name_lviewer.getList().setLayoutData( fd );    	

        spf_name_txt = new Text( spf_name_grp, SWT.SINGLE | SWT.BORDER );
        fd = new FormData();
        fd.top = new FormAttachment( spf_name_lviewer.getList(), 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.LEFT );  
        fd.right  = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.RIGHT );  
        spf_name_txt.setLayoutData( fd );
        
        Label spf_name_lbl = new Label( spf_name_grp, SWT.NONE);
        spf_name_lbl.setText("SPF Name");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spf_name_txt, -3, SWT.TOP );
        fd.left  = new FormAttachment( spf_name_txt, 0, SWT.LEFT );
        spf_name_lbl.setLayoutData( fd );

        rbd_name_combo = new Combo( shell, SWT.DROP_DOWN );
        fd = new FormData();
        fd.top = new FormAttachment( spf_name_grp, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( spf_name_grp, 0, SWT.LEFT );  
        fd.right  = new FormAttachment( spf_name_grp, 0, SWT.RIGHT );  
        rbd_name_combo.setLayoutData( fd );

        Label rbd_name_lbl = new Label(shell, SWT.NONE);
        rbd_name_lbl.setText("RBD Name");
        fd = new FormData();
        fd.bottom  = new FormAttachment( rbd_name_combo, -3, SWT.TOP );
        fd.left  = new FormAttachment( rbd_name_combo, 0, SWT.LEFT );
        rbd_name_lbl.setLayoutData( fd );

        Label sep0 = new Label( shell, SWT.SEPARATOR | SWT.HORIZONTAL );
        fd = new FormData();
        fd.top  = new FormAttachment( rbd_name_combo, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right  = new FormAttachment( 100,-5 );
//        fd.bottom = new FormAttachment( 100, -50 );
        sep0.setLayoutData( fd );

        Composite save_time_as_grp = new Composite( shell, SWT.SHADOW_NONE );
        save_time_as_grp.setLayout( new FormLayout() );
        fd = new FormData();
        fd.top = new FormAttachment( sep0, 10, SWT.BOTTOM );
        fd.left = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 100, -10 );
        save_time_as_grp.setLayoutData( fd );
        
        save_time_as_constant = new Button( save_time_as_grp, SWT.RADIO );
        save_time_as_constant.setText("Constant");
        fd = new FormData();
        fd.top = new FormAttachment( sep0, 19, SWT.BOTTOM );
        fd.left = new FormAttachment( save_time_as_grp, 10);
        save_time_as_constant.setLayoutData( fd );
        save_time_as_constant.setSelection( saveTimeAsConstant );
        save_time_as_constant.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveTimeAsConstant = save_time_as_constant.getSelection();
       		}
        });
     
        save_time_as_latest = new Button( save_time_as_grp, SWT.RADIO );
        save_time_as_latest.setText("Latest");
        fd = new FormData();
        fd.top = new FormAttachment( sep0, 19, SWT.BOTTOM );
        fd.left = new FormAttachment( save_time_as_constant, 10, SWT.RIGHT );
      //  fd.bottom  = new FormAttachment( 100, -10 );
        save_time_as_latest.setLayoutData( fd );
        save_time_as_latest.setSelection( !saveTimeAsConstant );
        save_time_as_latest.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveTimeAsConstant = !save_time_as_latest.getSelection();
       		 
       		}
        });
        
        Label save_time_as_lbl = new Label( save_time_as_grp, SWT.NONE);
        save_time_as_lbl.setText("Save Source Timestamp As:");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( save_time_as_constant, -3, SWT.TOP );
        fd.left  = new FormAttachment( save_time_as_constant, -3, SWT.LEFT );
        save_time_as_lbl.setLayoutData( fd );

        Label sep1 = new Label( shell, SWT.SEPARATOR | SWT.HORIZONTAL );
        fd = new FormData();
        fd.top  = new FormAttachment( save_time_as_grp, 12, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right  = new FormAttachment( 100,-5 );
//        fd.bottom = new FormAttachment( 100, -50 );
        sep1.setLayoutData( fd );
        
        save_ref_time_btn = new Button( shell, SWT.CHECK );
        fd = new FormData();
        save_ref_time_btn.setText("Save Reference Time");
        fd.top = new FormAttachment( sep1, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( rbd_name_combo, 0, SWT.LEFT );
        save_ref_time_btn.setLayoutData( fd );

        save_ref_time_btn.setSelection( saveRefTime );
        
        save_ref_time_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveRefTime = save_ref_time_btn.getSelection();
       		}
        });

//        Label save_reftime_lbl = new Label(shell, SWT.NONE);
//        rbd_name_lbl.setText("Save Reference Time");
//        fd = new FormData();
//        fd.bottom  = new FormAttachment( save_ref_time_btn, -3, SWT.TOP );
//        fd.left  = new FormAttachment( save_ref_time_btn, 0, SWT.LEFT );
//        save_reftime_lbl.setLayoutData( fd );
        
        Label sep2 = new Label( shell, SWT.SEPARATOR | SWT.HORIZONTAL );
        fd = new FormData();
        fd.top  = new FormAttachment( save_ref_time_btn, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0,5 );
        fd.right  = new FormAttachment( 100,-5 );
        fd.bottom = new FormAttachment( 100, -60 );
        sep2.setLayoutData( fd );

        Button can_btn = new Button( shell, SWT.PUSH );
        fd = new FormData();
        can_btn.setText(" Cancel ");
        fd.bottom = new FormAttachment( 100, -10 );
        fd.right  = new FormAttachment( 100, -20 );
        can_btn.setLayoutData( fd );

        can_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveRbdOkd = false;
    			shell.dispose();
       		}
        });
        
        save_btn = new Button( shell, SWT.PUSH );
        fd = new FormData();
        save_btn.setText("  Save RBD  ");
        fd.bottom = new FormAttachment( 100, -10 );
        fd.right  = new FormAttachment( can_btn, -20, SWT.LEFT );
        save_btn.setLayoutData( fd );

        save_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       	    	seldSpfGroup = spf_group_combo.getText();
       	    	seldSpfName = spf_name_txt.getText();
       	    	    	
       	    	seldRbdName = rbd_name_combo.getText();
       	    	
       	    	if( seldRbdName == null || seldRbdName.isEmpty() ) {
       	    		System.out.println("RBD name is not selected.");
       	    		return;
       	    	}

       	    	if( !newRbd ) {
       	    		MessageDialog confirmDlg = new MessageDialog( 
       	    				NcDisplayMngr.getCaveShell(), 
       	    				"Create RBD", null, 
       	    				"RBD " +seldRbdName+" already exists in this SPF.\n\n"+
       	    				"Do you want to overwrite it?",
       	    				MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
       	    		confirmDlg.open();

       	    		if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
       	    			return;
       	    		}
       	    	}
       	    	
       	    	saveRbdOkd = true;
       	    	
       	    	shell.dispose();
       		}
        });

    	
    	initWidgets();
    	
    	shell.setLocation( parent.getLocation().x+100, parent.getLocation().y+100);
    	shell.setMinimumSize(100, 100);

    	shell.pack();
    	shell.open();

    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}

    	return (Boolean)saveRbdOkd;
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
    	
    	spf_group_combo.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
    			save_btn.setEnabled( 
    					!rbd_name_combo.getText().isEmpty() &&
    					!spf_name_txt.getText().isEmpty() && 
    					!spf_group_combo.getText().isEmpty() ); 
    			seldSpfGroup = spf_group_combo.getText();
    			spf_name_lviewer.setInput(seldSpfGroup);
    			newRbd = true;
			}
    	});

    	spf_name_txt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
    			save_btn.setEnabled( 
    					!rbd_name_combo.getText().isEmpty() &&
    					!spf_name_txt.getText().isEmpty() && 
    					!spf_group_combo.getText().isEmpty() );
    			
    			// if entering a new SPF name, don't show 
    			// anything selected in the list
    			spf_name_lviewer.getList().deselectAll();
    			
    			seldSpfName = spf_name_txt.getText();
    			newRbd = true;
			}
    	});

    	rbd_name_combo.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
    			save_btn.setEnabled( 
    					!rbd_name_combo.getText().isEmpty() &&
    					!spf_name_txt.getText().isEmpty() && 
    					!spf_group_combo.getText().isEmpty() );

    			newRbd = true;
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
        
        spf_name_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged( SelectionChangedEvent event ) {
            	StructuredSelection seldSpfs = (StructuredSelection)spf_name_lviewer.getSelection();  

            	setSeldSpfName( (String)seldSpfs.getFirstElement() );
            }
        });

        spf_group_combo.setItems( SpfsManager.getInstance().getAvailSPFGroups() );
        
        
    	rbd_name_combo.addSelectionListener(new SelectionListener() {
   			public void widgetSelected(SelectionEvent e) {
   				newRbd = false;
   			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				newRbd = false;
			} 
   		});

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
        
        if( seldSpfName != null ) {
        	setSeldSpfName( seldSpfName );
        	spf_name_txt.setText( seldSpfName );
        }

        if( seldRbdName != null ) {
        	rbd_name_combo.setText( seldRbdName );
        }
    }

    
	private void setSeldSpfGroup(String spfGroup) {
		newRbd = false;

		seldSpfGroup = spfGroup;

		spf_name_lviewer.setInput( seldSpfGroup );    

    	spf_name_txt.clearSelection();
//    	String spfNames[] = SpfsManager.getInstance().getSpfNamesForGroup( spf_group_combo.getText() );

//    	spf_name_txt.setText( saveSpfName );    	
    	
//    	setSeldSpfName();
    }
    
	private void setSeldSpfName( String spfName ) {
		newRbd = false;

		seldSpfName = spfName;

		spf_name_txt.setText( seldSpfName );
			
		updateRbdNames( seldSpfGroup, seldSpfName );
    }
    
    public void updateRbdNames( String spfGroup, String spfName ) {

    	String saveRbdName = rbd_name_combo.getText();
    	
    	rbd_name_combo.setItems( SpfsManager.getInstance().getRbdNamesForSPF( spfGroup, spfName ) );
    	
    	rbd_name_combo.setText( saveRbdName );    	
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }    
}

