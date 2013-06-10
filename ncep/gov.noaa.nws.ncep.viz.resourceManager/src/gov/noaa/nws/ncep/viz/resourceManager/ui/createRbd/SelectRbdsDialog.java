package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.resourceManager.ui.loadRbd.RbdViewComposite;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;


/**
 *  Dialog displayed from RBD Mngr window when the 'Import RBD...' is selected. This lets the user 
 *  choose which RBD or which active editor to modify.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/17/09      #139       Greg Hull    Initial Creation.
 * 09/22/09      #169       Greg Hull    Handle Display Panes
 * 11/11/09      #180       Greg Hull    Select from SPF
 * 02/04/10      #226       Greg Hull    Import RBDs or single Panes  
 * 08/04/11      #450       Greg Hull    SpfsManager
 * 07/19/2012    #568       Greg Hull    Use new RbdViewComposite
 * 07/21/2012    #568       Greg Hull    Changed name to SelectRbdsDialog and allowed
 *                                       multi-select and select from Displays
 * 02/22/2013    #972       Greg Hull    work with AbstractRBD
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class SelectRbdsDialog extends Dialog { 
   
    private Shell shell;
    private String dlgTitle = null;

    private Boolean oked =  new Boolean(false);

    // if this is true then the user will have the option 
    // of selecting RBDs from Displays
    private boolean selectFromDisplays = false;

    // if true then the user may select more than one RBD
    //
    private boolean multiSelectEnabled = false;

    // if true then just allow the user to select one pane from 
    // one display and if false then the user will select displays
    private boolean importSinglePane = false;
    
    // the select Rbds section
    private Group      selFromGrp = null;
    private Button     selFromSpfsBtn = null;
    private Button     selFromDisplaysBtn = null;    
    private Label      spf_grp_lbl = null;
    private Label      spf_name_lbl = null;

    private Combo      spfGroupCombo = null;
    private Combo      spfNameCombo = null;
    private ListViewer rbdLviewer = null;
    private RbdViewComposite rscViewer = null;

    private Button     selectAllBtn = null;
    
    private Combo pane_combo = null;
    private Label pane_lbl = null;
    
    private Button ok_btn = null;
    
    private ArrayList<AbstractRBD<?>> seldRbdsList = null;
            
    // might be nice one day to save the group and spf name 
    // so that we can init the SaveRBD dialog with these values??
	private String seldSpfGroup = null;
    private String seldSpfName  = null;
    
    public SelectRbdsDialog( Shell parShell,   String titleStr, 
    				boolean selDisplays, boolean multiSel, boolean singlePane )  {
    	super(parShell);

    	dlgTitle = titleStr;//(importSinglePane ? "Import Pane" : "Import Display" );
    	selectFromDisplays = selDisplays;
    	multiSelectEnabled = multiSel;
    	importSinglePane = singlePane;
    	
    	seldRbdsList = new ArrayList<AbstractRBD<?>>();    	
    }
      
    public Boolean open( ) {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	
    	shell = new Shell( parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS );
    	shell.setText(dlgTitle);
    	shell.setSize( 600, 450 ); // pack later

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	
    	shell.setLayout(mainLayout);

    	createDialog( shell );
    	
    	initWidgets();
    	
    	shell.setLocation( parent.getLocation().x+100, parent.getLocation().y+100);
    	shell.setMinimumSize(400, 300);

    	shell.pack();
    	shell.open();

    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}
//    	if( oked ) { // 
//    		if( currRbdSel != null ) {
//    			currRbdSel.resolveLatestCycleTimes();
//    		}
//    	}
    	return oked;// ? currRbdSel : null );
    }
    
    public AbstractRBD<?> getSelectedRBD() {
    	// should have checked the status from open() before calling this.
    	if( seldRbdsList.isEmpty() ) {
    		return null;
    	}
    	else {
    		return seldRbdsList.get(0); 
    	}
    }
    
	// should have checked the status from open() before calling this.
    public ArrayList<AbstractRBD<?>> getSelectedRBDs() {
    	return seldRbdsList;
    }

    public void createDialog( Composite parent ) {
        Composite top_form = parent; 
        top_form.setLayout( new FormLayout() );

        FormData fd = new FormData( );
        
        Composite sel_rbds_grp = new Composite( top_form, SWT.NONE );
        sel_rbds_grp.setLayout( new FormLayout() );
        
        fd = new FormData(650,400);
        fd.top = new FormAttachment( 0, 5 );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right = new FormAttachment( 100, -5 );
        fd.bottom  = new FormAttachment( 100, -50 );
        sel_rbds_grp.setLayoutData( fd );

        selFromGrp = new Group( sel_rbds_grp, SWT.SHADOW_NONE );
        selFromGrp.setText("Select RBDs ");
        fd = new FormData();
        fd.top = new FormAttachment( 0, 20 );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 30, -7 );
        selFromGrp.setLayoutData( fd );

        selFromGrp.setLayout( new GridLayout( 1, false ) );

        selFromSpfsBtn = new Button( selFromGrp, SWT.RADIO );
        selFromSpfsBtn.setText( "From SPFs" );

        selFromDisplaysBtn = new Button( selFromGrp, SWT.RADIO );
        selFromDisplaysBtn.setText( "From Displays" );

        selFromGrp.setVisible( selectFromDisplays );
        
        spfGroupCombo = new Combo( sel_rbds_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
        fd = new FormData();
        
        // if the Select From section is visible then put this under it, if not
        // then put it at the top.
        if( selectFromDisplays ) {
        	fd.top = new FormAttachment( selFromGrp, 55, SWT.BOTTOM );
        }
        else {
            fd.top = new FormAttachment( 0, 30 );
        }
        
        fd.left  = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 30, -7 );
        spfGroupCombo.setLayoutData( fd );
    	
        spf_grp_lbl = new Label( sel_rbds_grp, SWT.NONE);
        spf_grp_lbl.setText("SPF Group");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spfGroupCombo, -3, SWT.TOP );
        fd.left  = new FormAttachment( spfGroupCombo, 0, SWT.LEFT );
        spf_grp_lbl.setLayoutData( fd );
        
        spfNameCombo = new Combo( sel_rbds_grp, SWT.READ_ONLY | SWT.DROP_DOWN );

        fd = new FormData();
        fd.top = new FormAttachment( spfGroupCombo, 40, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 25, 0 );

        spfNameCombo.setLayoutData( fd );

        spf_name_lbl = new Label(sel_rbds_grp, SWT.NONE);
        spf_name_lbl.setText("SPF Name");
        fd = new FormData();
        fd.bottom = new FormAttachment( spfNameCombo, -3, SWT.TOP );
        fd.left   = new FormAttachment( spfNameCombo, 0, SWT.LEFT );
        spf_name_lbl.setLayoutData( fd );
         
        int listStyleBits = SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL;
        
        listStyleBits |= ( multiSelectEnabled ? SWT.MULTI : SWT.SINGLE );
 
        rbdLviewer = new ListViewer(sel_rbds_grp, listStyleBits );
      	fd = new FormData();
        fd.top = new FormAttachment( 0, 30 );
        fd.left  = new FormAttachment( 30, 7 );
        fd.bottom = new FormAttachment( 100, -20 );
        fd.right = new FormAttachment( 65, 0 ); 
        rbdLviewer.getList().setLayoutData( fd );

        Label rbd_lbl = new Label( sel_rbds_grp, SWT.NONE);
        rbd_lbl.setText("RBDs");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( rbdLviewer.getList(), -3, SWT.TOP );
        fd.left  = new FormAttachment( rbdLviewer.getList(), 0, SWT.LEFT );
        rbd_lbl.setLayoutData( fd );

        
        selectAllBtn = new Button( sel_rbds_grp, SWT.PUSH );
        selectAllBtn.setText( "  Select All  " );
       	fd = new FormData();
        fd.bottom  = new FormAttachment( rbdLviewer.getList(), -5, SWT.BOTTOM );
        fd.right  = new FormAttachment( rbdLviewer.getList(), -20, SWT.LEFT );
        
        selectAllBtn.setLayoutData( fd );
        
        selectAllBtn.setVisible( multiSelectEnabled );
        
        pane_combo = new Combo( sel_rbds_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
        fd = new FormData();
        fd.width = 60;
        fd.top = new FormAttachment( rbdLviewer.getList(), 0, SWT.TOP );
        fd.left  = new FormAttachment( rbdLviewer.getList(), 70, SWT.RIGHT );
        pane_combo.setLayoutData( fd );
    	
        pane_lbl = new Label( sel_rbds_grp, SWT.NONE);
        pane_lbl.setText("Pane Number");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( pane_combo, -3, SWT.TOP );
        fd.left  = new FormAttachment( pane_combo, 0, SWT.LEFT );
        pane_lbl.setLayoutData( fd );
        
        // if importing just a single pane then we need to be able to select
        // which pane from the RBD we want, so enable the pane_combo 
        //
        pane_combo.setVisible( importSinglePane );
        pane_lbl.setVisible( importSinglePane );

        rscViewer = new RbdViewComposite( sel_rbds_grp );
        
      	if( importSinglePane ) {
      		rscViewer.viewSelectedPane();
      	}
      	
        fd = new FormData();
      	if( importSinglePane ) {
      		fd.top = new FormAttachment( pane_combo, 40-20, SWT.BOTTOM );
      		fd.left  = new FormAttachment( rbdLviewer.getList(), 15, SWT.RIGHT );
      		fd.bottom = new FormAttachment( rbdLviewer.getList(), 0, SWT.BOTTOM );
      		fd.right = new FormAttachment( 100, -10 );
      	}
      	else {
            fd.top = new FormAttachment( rbdLviewer.getList(), -20, SWT.TOP );
      		fd.left  = new FormAttachment( rbdLviewer.getList(), 15, SWT.RIGHT );
      		fd.right = new FormAttachment( 100, -10 );
      		fd.bottom = new FormAttachment( rbdLviewer.getList(), 0, SWT.BOTTOM );
     	}
        rscViewer.setLayoutData( fd );
      	        
        Label sep = new Label( sel_rbds_grp, SWT.SEPARATOR | SWT.HORIZONTAL );
       	fd = new FormData();
        fd.top  = new FormAttachment( rbdLviewer.getList(), 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right  = new FormAttachment( 100, -5 );
        
        sep.setLayoutData( fd );

               
        Button can_btn = new Button( top_form, SWT.PUSH );
        fd = new FormData();
        fd.width = 80;
        can_btn.setText(" Cancel ");
        fd.bottom = new FormAttachment( 100, -10 );
        fd.right  = new FormAttachment( 100, -20 );
        can_btn.setLayoutData( fd );

        can_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			seldRbdsList.clear();
       			shell.dispose();
       		}
        });
        
        ok_btn = new Button( top_form, SWT.PUSH );
        fd = new FormData();
        fd.width = 80;
        ok_btn.setText("   OK   ");
        fd.bottom = new FormAttachment( 100, -10 );
        fd.right  = new FormAttachment( can_btn, -20, SWT.LEFT );
        ok_btn.setLayoutData( fd );

        ok_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			ok();
       		}
        });
    }
    
    private void initWidgets() {
    	
    	selFromDisplaysBtn.addSelectionListener(new SelectionAdapter() {		
    		public void widgetSelected(SelectionEvent e) {
    			if( selFromDisplaysBtn.getSelection() ) {
    				selectFromChanged();
    			}
    		}
    	});

    	selFromSpfsBtn.addSelectionListener(new SelectionAdapter() {		
    		public void widgetSelected(SelectionEvent e) {
    			if( selFromSpfsBtn.getSelection() ) {
    				selectFromChanged();
    			}
    		}
    	});
    	
    	spfGroupCombo.addSelectionListener(new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				setSeldSpfGroup( spfGroupCombo.getText() );
   			} 
   		});

        spfNameCombo.addSelectionListener( new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
            	setSeldSpfName( spfNameCombo.getText() );
            }
        });
        
    	selectAllBtn.addSelectionListener(new SelectionAdapter() {		
    		public void widgetSelected(SelectionEvent e) {
    			rbdLviewer.getList().selectAll();
    			setSelectedRBDs();
    		}
    	});

        // 
        rbdLviewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
		 		ArrayList<AbstractRBD<?>> rbdBndls = (ArrayList<AbstractRBD<?>>)inputElement;		 		
				return rbdBndls.toArray();										
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });          

        rbdLviewer.setLabelProvider( new LabelProvider() {
	    	public String getText( Object element ) {
	    		if( element instanceof AbstractRBD<?> ) {
	    			return ((AbstractRBD<?>)element).getRbdName();
	    		}
	    		else  return "Error: bad RBD element";
	    	}
        });
        
       	rbdLviewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
            	setSelectedRBDs();
            } 
       	});     
       	
       	rbdLviewer.getList().addListener( SWT.MouseDoubleClick, new Listener() {
   			public void handleEvent(Event event) {
            	setSelectedRBDs();
            	ok();
   			}
   		});
       	
       	pane_combo.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				NcPaneID seldPaneId = NcPaneID.parsePaneId( pane_combo.getText() );
   				
   	    		if( seldRbdsList.size() == 1 ) {
   	    			// Note that if selecting panes is allowed, multi-select is not
   	    			// and so there will only be one RBD in the list
   	    			seldRbdsList.get(0).setSelectedPaneId( seldPaneId );
   	   	    		rscViewer.viewRbd( seldRbdsList.get(0) );			
   	    		}
   	    		else {
   	    			rscViewer.viewRbd( null );
   	    		}
   			}
       	});

       	if( selectFromDisplays ) {
       		selFromDisplaysBtn.setSelection( true );
    	}	
    	else {
    		selFromSpfsBtn.setSelection( true );
    	}
       	
       	selectFromChanged();
       	
    }

    private void selectFromChanged( ) {

    	if( selFromDisplaysBtn.getSelection() ) {
    		
    		spfGroupCombo.setEnabled( false );
    		spf_grp_lbl.setEnabled( false );
    		spf_name_lbl.setEnabled( false );
    		spfNameCombo.setEnabled( false );

    		rbdLviewer.setInput( getRbdsFromAllDisplays() );
    		
    		if( multiSelectEnabled ) {
    			rbdLviewer.getList().selectAll();
    		}
    		else {
    			rbdLviewer.getList().select(0);
    		}

    		setSelectedRBDs();

    		rbdLviewer.refresh( true );
    	}
    	else if( selFromSpfsBtn.getSelection() ) {
    		
    		spfGroupCombo.setEnabled( true );
    		spf_grp_lbl.setEnabled( true );
    		spf_name_lbl.setEnabled( true );
    		spfNameCombo.setEnabled( true );

    		spfGroupCombo.setEnabled( true );
    		spfNameCombo.setEnabled( true );
    		
            spfGroupCombo.setItems( SpfsManager.getInstance().getAvailSPFGroups() );
            
            if( spfGroupCombo.getItemCount() == 0 ) {
            	spfGroupCombo.add("None Available");
            	spfGroupCombo.select(0);
                spfGroupCombo.setEnabled(false);
            }
            else {
            	if( seldSpfGroup != null && !seldSpfGroup.isEmpty() ) {
                	for( int g=0 ; g<spfGroupCombo.getItemCount() ; g++ ) {
                		if( seldSpfGroup.equals( spfGroupCombo.getItem(g) ) ) {
                			spfGroupCombo.select(g);
                    		setSeldSpfGroup( spfGroupCombo.getText() );        			
                		}
                	}
                }
                
            	if( spfGroupCombo.getItemCount() > 0 &&
            		spfGroupCombo.getSelectionIndex() == -1 ) {
            		
                	spfGroupCombo.select(0);
            		setSeldSpfGroup( spfGroupCombo.getText() );        			
                }
            }

    		if( spfNameCombo.getItemCount() > 0 ) {
    			setSeldSpfName( spfNameCombo.getText() );
    		}
    		else {
    			spfGroupCombo.setEnabled( false );
    			spfNameCombo.setEnabled( false );
    		}
    	}
    }    

    private void setSeldSpfGroup( String newSpfGroup ) {
    	seldSpfGroup = newSpfGroup;
    	
    	spfNameCombo.setItems( SpfsManager.getInstance().getSpfNamesForGroup( seldSpfGroup ) );

		if( spfNameCombo.getItemCount() == 0 ) {
			setSeldSpfName( null );
		}
		else {
			spfNameCombo.select( 0 );

			setSeldSpfName( spfNameCombo.getText() );
		}    	
    }
    
    private void setSeldSpfName( String newSpfName ) {    	
    	seldSpfName = newSpfName;
    	
    	if( seldSpfName == null || seldSpfName.isEmpty() ) {
    		rbdLviewer.setInput( null );
	    	rbdLviewer.refresh();
			setSelectedRBDs();
    		return;
    	}
    	 
    	List<AbstractRBD<?>> rbdBndls;
    	
		try {
			rbdBndls = SpfsManager.getInstance().getRbdsFromSpf(
					seldSpfGroup, seldSpfName, 
					false ); // don't resolve Latest Cycle Times
 		
			rbdLviewer.setInput( rbdBndls );
			rbdLviewer.refresh();

			rbdLviewer.getList().select(0);

			setSelectedRBDs();
			
		} catch (VizException e) {
		}
    }
    
    private void  setSelectedRBDs() {
    	StructuredSelection sel_rbds = (StructuredSelection)rbdLviewer.getSelection();               
    	if( sel_rbds.isEmpty() ) {
    		seldRbdsList.clear();
        	
    		pane_combo.add("N/A");
        	pane_combo.select(0);
    		pane_combo.setEnabled(false);
    		pane_lbl.setEnabled(false);

    		rscViewer.viewRbd( null );
    	}
    	else {
   			Iterator sel_iter = sel_rbds.iterator();
   			seldRbdsList.clear();
   			
   			while( sel_iter.hasNext() ) {
   				seldRbdsList.add( (AbstractRBD<?>)sel_iter.next() );
   			}

   			// set the pane combo items and preselect 
   			pane_combo.removeAll();
   			
   			if( seldRbdsList.size() == 1 ) {

   				AbstractRBD<?> seldRbd = seldRbdsList.get(0);

   				for( int paneIndx=0 ; paneIndx<seldRbd.getPaneLayout().getNumberOfPanes() ; paneIndx++ ) {
//   					for( int c=0 ; c<seldRbd.getPaneLayout().getColumns() ; c++ ) {
   						pane_combo.add( 
   								seldRbd.getPaneLayout().createPaneId(paneIndx).toString() );
//   					}
   				}

   				if( pane_combo.getItemCount() == 0 ) {
   					pane_combo.add("N/A");
   				}
   				pane_combo.select(0);
   				pane_combo.setEnabled( pane_combo.getItemCount()>1 );
   				pane_lbl.setEnabled( pane_combo.getItemCount()>1 );

   				rscViewer.viewRbd( seldRbd );
   			}
   			else { // can view more than one at a time.
   				pane_combo.setEnabled( false );
   				
   				rscViewer.viewRbd( null );
   			}
    	}
    	
    	ok_btn.setEnabled( !seldRbdsList.isEmpty() );
    }     
    
    
	private ArrayList<AbstractRBD<?>> getRbdsFromAllDisplays() {
		
		List<AbstractEditor> allNcDisplays = NcDisplayMngr.getAllNcDisplays();
		ArrayList<AbstractRBD<?>> rbdsFromDisplays = new ArrayList<AbstractRBD<?>>();
		
		// get AbstractRBD<?> from selected display
		for( AbstractEditor ncDisplay : allNcDisplays ) {
			try {
				AbstractRBD<?> rbdFromDisplay = AbstractRBD.createRbdFromEditor( ncDisplay );

				rbdsFromDisplays.add( AbstractRBD.clone( rbdFromDisplay ) );
				
			} catch (VizException e) {
				MessageDialog errDlg = new MessageDialog( 
						shell, "Error", null, 
						"Error getting Rbd from "+ NcEditorUtil.getDisplayName(ncDisplay)+".\n" +
							e.getMessage(),
						MessageDialog.ERROR, new String[]{"OK"}, 0);
				errDlg.open();
			}
		}	
		
		return rbdsFromDisplays;		
	}
	
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
        
    private void ok() {
    	oked = true;
    	shell.dispose();
    }

}

