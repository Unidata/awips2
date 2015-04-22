package gov.noaa.nws.ncep.viz.resourceManager.ui.manageSpf;

import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.SelectRbdsDialog;
import gov.noaa.nws.ncep.viz.resourceManager.ui.loadRbd.RbdViewComposite;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;

import java.io.File;
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
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.UiPlugin;

import static java.lang.System.out;

/**
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
 * 04/30/12       #585       S. Gurung   Save RBDs in the order in which they are displayed;
 * 										 Removed unwanted options ("Sort Alphabetically" and "Sort By Date")  
 * 06/25/12       #568       G. Hull     Changed name to Manage RBDs. 
 * 06/26/12       #568       G. Hull     Reworked to add Select Rbd section, Delete, Edit, ReOrder functionality
 * 02/22/2013     #972       G. Hull     AbstractRBD
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class ManageSpfControl extends Composite {

    private Shell shell;
    
    private SashForm sash_form = null;
    private Group mngRbdsGrp = null;  
    private Group unusedSectionGrp = null;
    
    private Group   actionGroup     = null;
    private Button  modifyActionBtn = null;
    private Button  createActionBtn = null;
    private Button  deleteActionBtn = null;
    private Button  currActionBtn = null; // set to one of the above
        
    private ListViewer seldRbdsLviewer = null;
    private RbdViewComposite rscLviewer = null;

    private Button addRbdsBtn = null;
    private Button removeRbdsBtn = null; // remove from the list
    private Button renameRbdBtn = null;
    
    private Button moveUpBtn = null;
    private Button moveDownBtn = null;
    
    private String seldSpfGroup = "";
	private String seldSpfName  = "";
    private String prevSeldSpfGroup = "";
    private String prevSeldSpfName = "";

    // These will be editable and visible when in create mode
    //
    private Composite  selSpfComp = null;
    private Combo 	   createSpfGroupCombo = null;
    private Text   	   createSpfNameTxt = null;

    // if Modifying or Deleting then these will be visible
    //
    private Combo  modifySpfGroupCombo = null;
    private Combo  modifySpfNameCombo = null;
    
    private Combo  currSpfGroupCombo = null;
    private Widget currSpfNameWidget = null;

    private Label  spfLocationLbl = null;
    
    private Button saveRefTimeBtn = null;
    
    // Modify, Create, or Delete depending on the action selected
    private Button spfActionBtn = null;
    // this will become visible only when deleteSpf is selected and
    // when there are no spfs to delete.
    private Button deleteSpfGroupBtn = null;
    
    private List<AbstractRBD<?>> seldRbdsList = null; // RBDs to 

    private Point initDlgSize = new Point( 750, 860 );

	public ManageSpfControl(Composite parent) {
		super(parent, SWT.NONE);
		
		shell = parent.getShell(); 
		
		seldRbdsList = new ArrayList<AbstractRBD<?>>();

        Composite top_comp = this;        
        top_comp.setLayout( new GridLayout(1,true) );
        
        sash_form = new SashForm( top_comp, SWT.VERTICAL );
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        
        sash_form.setLayoutData( gd );
        sash_form.setSashWidth(10);
        
        mngRbdsGrp = new Group( sash_form, SWT.SHADOW_NONE );
        mngRbdsGrp.setText( "Manage RBDs" );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        
        mngRbdsGrp.setLayoutData( gd );

        mngRbdsGrp.setLayout( new FormLayout() );


        createManageRbdsGroup();
        
        unusedSectionGrp = new Group( sash_form, SWT.SHADOW_NONE );
        //unusedSectionGrp.setText( "Select RBDs" );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;

        unusedSectionGrp.setLayoutData( gd );

        unusedSectionGrp.setLayout( new FormLayout() );

        // createUnusedwidgets();

        sash_form.setWeights( new int[] { 3, 1 } );

        createViewersAndListeners();
        
    	initWidgets();
	}

	
	private void createManageRbdsGroup() {
		
		actionGroup = new Group( mngRbdsGrp, SWT.SHADOW_NONE );
		actionGroup.setText( "Action" );
		
		FormData fd = new FormData();
		fd.top = new FormAttachment( 0, 20 );
		fd.left = new FormAttachment( 0, 15 ); 
		fd.right  = new FormAttachment( 30, -7 );
		actionGroup.setLayoutData( fd );

		actionGroup.setLayout( new FormLayout() );

		modifyActionBtn = new Button( actionGroup, SWT.RADIO );
		modifyActionBtn.setText( " Modify SPF " );
        
       	fd = new FormData();
        fd.top = new FormAttachment( 0, 15 );
        fd.left  = new FormAttachment( 0, 25 );
        fd.right  = new FormAttachment( 100, -15 );
        modifyActionBtn.setLayoutData( fd );
		
		createActionBtn = new Button( actionGroup, SWT.RADIO );
		createActionBtn.setText( " Create SPF " );
        
       	fd = new FormData();
        fd.top  = new FormAttachment( modifyActionBtn, 15, SWT.BOTTOM );
        fd.left = new FormAttachment( modifyActionBtn, 0, SWT.LEFT );
        fd.right  = new FormAttachment( 100, -15 );
        createActionBtn.setLayoutData( fd );
		
		deleteActionBtn = new Button( actionGroup, SWT.RADIO );
		deleteActionBtn.setText( " Delete SPF " );
        
       	fd = new FormData();
        fd.top = new FormAttachment( createActionBtn, 20, SWT.BOTTOM );
        fd.left = new FormAttachment( modifyActionBtn, 0, SWT.LEFT );
        fd.right  = new FormAttachment( 100, -15 );
        fd.bottom  = new FormAttachment( 100, -15 );
        deleteActionBtn.setLayoutData( fd );

        
		selSpfComp = new Composite( mngRbdsGrp, SWT.SHADOW_NONE );
		
		fd = new FormData();
        fd.top = new FormAttachment( actionGroup, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( actionGroup, 0, SWT.LEFT );
        selSpfComp.setLayoutData( fd );

        selSpfComp.setLayout( new FormLayout() );
        
        createSpfGroupCombo = new Combo( selSpfComp, SWT.DROP_DOWN );
        fd = new FormData();
        fd.top = new FormAttachment( 0, 20 );
        fd.left  = new FormAttachment( 0, 0 );
        fd.right  = new FormAttachment( 100, 0 );
        createSpfGroupCombo.setLayoutData( fd );    	

        modifySpfGroupCombo = new Combo( selSpfComp, SWT.READ_ONLY | SWT.DROP_DOWN );
        modifySpfGroupCombo.setLayoutData( fd );
        
        
        Label spf_grp_lbl = new Label( selSpfComp, SWT.NONE);
        spf_grp_lbl.setText("SPF Group");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( createSpfGroupCombo, -3, SWT.TOP );
        fd.left  = new FormAttachment( createSpfGroupCombo, 0, SWT.LEFT );
        spf_grp_lbl.setLayoutData( fd );

        
        createSpfNameTxt = new Text( selSpfComp, SWT.SINGLE | SWT.BORDER );
        fd = new FormData();
        fd.top = new FormAttachment( createSpfGroupCombo, 35, SWT.BOTTOM );
        fd.left  = new FormAttachment( createSpfGroupCombo, 0, SWT.LEFT );
        fd.right  = new FormAttachment( createSpfGroupCombo, 0, SWT.RIGHT );
        createSpfNameTxt.setLayoutData( fd );    	

        modifySpfNameCombo = new Combo( selSpfComp, SWT.READ_ONLY | SWT.DROP_DOWN );
        modifySpfNameCombo.setLayoutData( fd );
        
        
        Label spf_name_lbl = new Label( selSpfComp, SWT.NONE);
        spf_name_lbl.setText("SPF Name");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( createSpfNameTxt, -3, SWT.TOP );
        fd.left  = new FormAttachment( createSpfNameTxt, 0, SWT.LEFT );
        spf_name_lbl.setLayoutData( fd );

        spfLocationLbl = new Label( selSpfComp, SWT.NONE);
        spfLocationLbl.setText("Localization=");
       	fd = new FormData();
        fd.top  = new FormAttachment( createSpfNameTxt, 8, SWT.BOTTOM );
        fd.left  = new FormAttachment( createSpfNameTxt, 0, SWT.LEFT );
        spfLocationLbl.setLayoutData( fd );

        spfActionBtn = new Button( selSpfComp, SWT.PUSH );        
        
       	fd = new FormData(100, 30);
        fd.top = new FormAttachment( createSpfNameTxt, 60, SWT.BOTTOM );
        fd.left  = new FormAttachment( 50, -50 );
        spfActionBtn.setLayoutData( fd );

        // this will become visible (on top of the Delete SPF button) 
        // only when deleteSpf is selected and when there are no spfs to delete.
        deleteSpfGroupBtn = new Button( selSpfComp, SWT.PUSH );        
        deleteSpfGroupBtn.setText( " Delete SPF Group " );
       	fd = new FormData(140, 30);
        fd.top = new FormAttachment( spfActionBtn, 0, SWT.TOP );
        fd.left  = new FormAttachment( spfActionBtn, -20, SWT.LEFT );
        deleteSpfGroupBtn.setLayoutData( fd );
        deleteSpfGroupBtn.setVisible( false );
        
        
        saveRefTimeBtn = new Button( selSpfComp, SWT.CHECK );
        fd = new FormData();
        saveRefTimeBtn.setText("Save Reference Time");
        fd.top = new FormAttachment( spfActionBtn, 20, SWT.BOTTOM );
        fd.left  = new FormAttachment( createSpfNameTxt, 0, SWT.LEFT );
        fd.bottom  = new FormAttachment( 100, -20 );
        saveRefTimeBtn.setLayoutData( fd );

        saveRefTimeBtn.setSelection( false );

        
		seldRbdsLviewer = new ListViewer( mngRbdsGrp, 
				SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
		fd = new FormData();
		fd.top = new FormAttachment( 0, 30 );
		fd.left  = new FormAttachment( 30, 7 );
		fd.bottom = new FormAttachment( 75, 0 );
		fd.right = new FormAttachment( 66, -7 ); 
		seldRbdsLviewer.getList().setLayoutData( fd );

		Label rbd_lbl = new Label( mngRbdsGrp, SWT.NONE);
		rbd_lbl.setText("RBDs");
		fd = new FormData();
		fd.bottom  = new FormAttachment( seldRbdsLviewer.getList(), -3, SWT.TOP );
		fd.left  = new FormAttachment( seldRbdsLviewer.getList(), 0, SWT.LEFT );
		rbd_lbl.setLayoutData( fd );        

		
		rscLviewer = new RbdViewComposite( mngRbdsGrp );

		fd = new FormData();
		fd.top = new FormAttachment( seldRbdsLviewer.getList(), -20, SWT.TOP );
		fd.left = new FormAttachment( 66, 7 ); 
		fd.right = new FormAttachment( 100, -10 );
		fd.bottom = new FormAttachment( seldRbdsLviewer.getList(), -130, SWT.BOTTOM );
		rscLviewer.setLayoutData( fd );

		
		addRbdsBtn = new Button( mngRbdsGrp, SWT.PUSH );
		addRbdsBtn.setText( "Add RBDs..." );
        
       	fd = new FormData(110, 30);
        fd.top = new FormAttachment( seldRbdsLviewer.getList(), 20, SWT.BOTTOM );
        fd.left  = new FormAttachment( seldRbdsLviewer.getList(), 0, SWT.LEFT );
        addRbdsBtn.setLayoutData( fd );

        renameRbdBtn = new Button( mngRbdsGrp, SWT.PUSH );
        renameRbdBtn.setText( "Rename RBD..." );
        
       	fd = new FormData(110, 30);
        fd.top = new FormAttachment( addRbdsBtn, 0, SWT.TOP );
        fd.left  = new FormAttachment( addRbdsBtn, 20, SWT.RIGHT );
        renameRbdBtn.setLayoutData( fd );


		removeRbdsBtn = new Button( mngRbdsGrp, SWT.PUSH );
		removeRbdsBtn.setText( "Remove RBD" );
        
       	fd = new FormData(110, 30);
        fd.top = new FormAttachment( addRbdsBtn, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( addRbdsBtn, 0, SWT.LEFT );
        removeRbdsBtn.setLayoutData( fd );
       
		moveDownBtn = new Button( mngRbdsGrp, SWT.PUSH );
		moveDownBtn.setText( "Move Down" );
        
       	fd = new FormData(90, 30);
        fd.bottom = new FormAttachment( seldRbdsLviewer.getList(), 0, SWT.BOTTOM );
        fd.left  = new FormAttachment( seldRbdsLviewer.getList(), 20, SWT.RIGHT );
        moveDownBtn.setLayoutData( fd );

		moveUpBtn = new Button( mngRbdsGrp, SWT.PUSH );
		moveUpBtn.setText( "Move Up" );
        
       	fd = new FormData(90, 30);
        fd.bottom = new FormAttachment( moveDownBtn, -13, SWT.TOP );
        fd.left  = new FormAttachment( moveDownBtn, 0, SWT.LEFT );
        moveUpBtn.setLayoutData( fd );		
	}
	
	private void createViewersAndListeners() {
		
		SelectionAdapter actionBtnListener =  new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
   				Button btnWid = (Button) e.widget;
   				
   				if( btnWid.getSelection() &&
   					currActionBtn != btnWid ) {
   					setCurrentAction( btnWid );
   				}
   			}
   		};
   		
		modifyActionBtn.addSelectionListener( actionBtnListener );    

		createActionBtn.addSelectionListener( actionBtnListener );

		deleteActionBtn.addSelectionListener( actionBtnListener );
				
		createSpfGroupCombo.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				setSeldSpfGroup( createSpfGroupCombo.getText() );
			}
    	});

		modifySpfGroupCombo.addSelectionListener( new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
   				setSeldSpfGroup( modifySpfGroupCombo.getText() );
   			}
   		});  
		
		createSpfNameTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				seldSpfName = createSpfNameTxt.getText();
				spfActionBtn.setEnabled( !seldSpfGroup.isEmpty() && !seldSpfName.isEmpty() );
			}
    	});
        
        modifySpfNameCombo.addSelectionListener( new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
   				
   				setSeldSpfName( modifySpfNameCombo.getText() );   				
			}
   		});
                    
        // pop up the Select RBDs dialog
       	addRbdsBtn.addSelectionListener(new SelectionAdapter() {		
			public void widgetSelected(SelectionEvent e) {
   				
   	    		SelectRbdsDialog selRbdsDlg = 
   	    			new SelectRbdsDialog( shell, "Select RBDs", 
   	    					true, true, false );
   	    			
   	    		if( !selRbdsDlg.open() ) {
   	    			return;
   	    		}
   	    		
   	    		ArrayList<AbstractRBD<?>> rbdsToAdd = selRbdsDlg.getSelectedRBDs();

   	    		for( AbstractRBD<?> rbd : rbdsToAdd ) {
   	    			rbd.resolveLatestCycleTimes();    		

   		    		try {
   		    			AbstractRBD<?> newRbd = AbstractRBD.clone( rbd );
   		    			
   		    			// if in create mode, this will be 
   		    			newRbd.setLocalizationFile( null );
   		    			
						seldRbdsList.add( newRbd );
						
					} catch (VizException e1) {
						out.println("???Error Cloning rbd ");
					}
   		    	}
   		    	seldRbdsLviewer.setInput( seldRbdsList );
   		    	seldRbdsLviewer.refresh(true);
   			}
   		});


       	// ignore the imput and return the managedRbdsList
        seldRbdsLviewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				if( currActionBtn == createActionBtn && seldRbdsList.isEmpty() ) {
					spfActionBtn.setEnabled( false );
				}
				else {
					spfActionBtn.setEnabled( true );
				}
				
				seldRbdsLviewer.getList().deselectAll();
				
				rscLviewer.viewRbd( null );
				
				return seldRbdsList.toArray();										
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });          

        seldRbdsLviewer.setLabelProvider( new LabelProvider() {
	    	public String getText( Object element ) {
	    		if( element instanceof AbstractRBD<?> ) {
	    			AbstractRBD<?> rbd = (AbstractRBD<?>)element;
	    			if( rbd.isEdited() ) {	    				
		    			return rbd.getRbdName() + "(E)";
	    			}
	    			else {
	    				return rbd.getRbdName();
	    			}
	    		}
	    		else  return "Error: bad RBD element";
	    	}
        });
        
        seldRbdsLviewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				rbdVierwSelectionChanged( event );
			}
        });     

        removeRbdsBtn.addSelectionListener(new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
   				int seldIndxs[] = seldRbdsLviewer.getList().getSelectionIndices();
   		    	
   				if( seldIndxs.length == 0 ) {
   		    		return;
   		    	}
   		
   				// remove the selected RBDs from the list of seld rbds
   				//
   				for( int i=seldIndxs.length-1 ; i >= 0 ; i-- ) {
   	   				// sanity check (since the button should be disabled if not User Level),
   	   				// 
   					if( currActionBtn == modifyActionBtn ) {	
   						LocalizationFile lFile = seldRbdsList.get(seldIndxs[i]).getLocalizationFile(); 
   						
   						if( lFile != null &&
   							lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
   							
   							MessageDialog errDlg = new MessageDialog( 
   									shell, "Error", null, 
   									"You do not have permissions to delete a "+
   									lFile.getContext().getLocalizationLevel().toString() + " Level Rbd.",
   									MessageDialog.ERROR, new String[]{"OK"}, 0);
   							errDlg.open();
   							continue;
   						}
   	   				}
   					
   					seldRbdsList.remove( seldIndxs[i] );
   				}

   				seldRbdsLviewer.refresh();
   			}
    	});
        
        renameRbdBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		renameRbd();
        	}
		});
        
        
        moveUpBtn.addSelectionListener(new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
			    moveRbdsUp();
   			}
    	});

        moveDownBtn.addSelectionListener(new SelectionAdapter() {		
   			public void widgetSelected(SelectionEvent e) {
   				moveRbdsDown();
   			}
    	});

        spfActionBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       	    	if( currActionBtn == modifyActionBtn ) {
       	    		saveSpf();
       	    	}
       	    	else if( currActionBtn == createActionBtn ) {
       	    		createSpf();
       	    	}
       	    	else if( currActionBtn == deleteActionBtn ) {
       	    		deleteSpf();
       	    	}
       		}
        });
        
        deleteSpfGroupBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			if( currActionBtn == deleteActionBtn ) { // sanity check
       	    		deleteSpfGroup();
       	    	}
       		}
        });

        saveRefTimeBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       		}
        });

        
        seldRbdsLviewer.getList().addMouseListener(new MouseListener() {
   			
        	@Override
			public void mouseDown(MouseEvent e) {
        		
				if (e.button == 3) {
					Menu menu = new Menu(shell, SWT.POP_UP);
				    MenuItem item1 = new MenuItem(menu, SWT.PUSH);
				    item1.setText("Move Up");
				    item1.addListener(SWT.Selection, new Listener() {
						@Override
						public void handleEvent(Event event) {
						    moveRbdsUp();
						}				    	
				    });
				    
				    MenuItem item2 = new MenuItem(menu, SWT.PUSH);
				    item2.setText("Move Down");
				    item2.addListener(SWT.Selection, new Listener() {
						@Override
						public void handleEvent(Event event) {
							moveRbdsDown();
						}				    	
				    });
				    
				    MenuItem item3 = new MenuItem(menu, SWT.PUSH);
				    item3.setText("Rename...");
				    item3.addListener(SWT.Selection, new Listener() {
						@Override
						public void handleEvent(Event event) {
							renameRbd();
						}				    	
				    });

//				    MenuItem item4 = new MenuItem(menu, SWT.PUSH);
//				    item4.setText("Remove RBD...");
//				    item4.addListener(SWT.Selection, new Listener() {
//						@Override
//						public void handleEvent(Event event) {
//							renameRbd();
//						}				    	
//				    });

				    seldRbdsLviewer.getList().setMenu( menu );
				}
			}

			@Override
			public void mouseUp(MouseEvent e) { }

			@Override
			public void mouseDoubleClick(MouseEvent e) {}
   		});

	}
	
    private void initWidgets() {
    	
        // Initialize to the Modify Action
    	modifyActionBtn.setSelection( true );
        
        setCurrentAction( modifyActionBtn );       	
    }
    
    private void setCurrentAction( Button currBtn ) {
    	currActionBtn = currBtn;

    	boolean createSpfSeld = (currActionBtn == createActionBtn);
    	boolean deleteSpfSeld = (currActionBtn == deleteActionBtn);
    	
    	currSpfGroupCombo = ( createSpfSeld ? createSpfGroupCombo : modifySpfGroupCombo );
    	currSpfNameWidget = ( createSpfSeld ? createSpfNameTxt : modifySpfNameCombo );
    	
    	createSpfGroupCombo.setVisible( createSpfSeld );
    	createSpfNameTxt.setVisible( createSpfSeld );
    	
    	modifySpfGroupCombo.setVisible( !createSpfSeld );
    	modifySpfNameCombo.setVisible( !createSpfSeld );

        spfActionBtn.setText( 
            	(currActionBtn == modifyActionBtn ? "Save SPF" : 
            		(currActionBtn == createActionBtn ? "Create SPF" : 
            			(currActionBtn == deleteActionBtn ? "Delete SPF" : "" ))));

        spfActionBtn.setVisible( true );
        spfActionBtn.setEnabled( !createSpfSeld );
        deleteSpfGroupBtn.setVisible( false );
        
        saveRefTimeBtn.setVisible( !deleteSpfSeld );
        addRbdsBtn.setEnabled( !deleteSpfSeld );
        removeRbdsBtn.setEnabled( !deleteSpfSeld );
        renameRbdBtn.setEnabled( !deleteSpfSeld );
        moveUpBtn.setEnabled( !deleteSpfSeld );
        moveDownBtn.setEnabled( !deleteSpfSeld );
        
        // Make the rbd list appear read-only if deleting the SPF
        seldRbdsLviewer.getList().setBackground( (deleteSpfSeld ? 
        		modifySpfNameCombo.getParent().getBackground() : createSpfNameTxt.getBackground() ));
                

        currSpfGroupCombo.setItems( SpfsManager.getInstance().getAvailSPFGroups() );

        // if the user has pre selected a group then select it
        //
        if( createSpfSeld ) {
        	currSpfGroupCombo.setText( "" );
    		seldRbdsList.clear();
    		seldRbdsLviewer.setInput( seldRbdsList );
    		seldRbdsLviewer.refresh();
	    	rscLviewer.viewRbd( null );
	    	rscLviewer.refresh();
    		setSeldSpfGroup( currSpfGroupCombo.getText() );
        }
        else if( seldSpfGroup != null && !seldSpfGroup.isEmpty() ) {
        	int g=0;
        	for( g=0 ; g<currSpfGroupCombo.getItemCount() ; g++ ) {
        		if( seldSpfGroup.equals( currSpfGroupCombo.getItem(g) ) ) {
        			currSpfGroupCombo.select(g);
        			seldSpfGroup = ""; // force it to change
            		setSeldSpfGroup( currSpfGroupCombo.getText() );   
            		break;
        		}
        	}
        	if( g == currSpfGroupCombo.getItemCount() ) {
        		setSeldSpfGroup( "" );
        	}
        }
        else if( currSpfGroupCombo.getItemCount() > 0 ) {
        	currSpfGroupCombo.select(0);
    		setSeldSpfGroup( currSpfGroupCombo.getText() );        			
        }
        else {
        	currSpfGroupCombo.setEnabled( false );
        	currSpfGroupCombo.setEnabled( false );
        }
        
    }
    
	private void setSeldSpfGroup(String spfGroup) {

		// don't reset it if it hasn't changed
		if( seldSpfGroup.equals( spfGroup ) ) {
			return;
		}
		
		prevSeldSpfGroup = seldSpfGroup;
		
		seldSpfGroup = spfGroup;
		
		if( currSpfNameWidget instanceof Combo ) {

			((Combo)currSpfNameWidget).setItems( SpfsManager.getInstance().getSpfNamesForGroup( seldSpfGroup ) );

			if( ((Combo)currSpfNameWidget).getItemCount() == 0 ) {
				setSeldSpfName( null );
			}
			else {
				((Combo)currSpfNameWidget).select( 0 );

				setSeldSpfName( ((Combo)currSpfNameWidget).getText() );
			}
		}
		else if( currSpfNameWidget instanceof Text ) {
			((Text)currSpfNameWidget).setText("");
			setSeldSpfName( ((Text)currSpfNameWidget).getText() );
		}		
    }
    
	private void setSeldSpfName( String spfName ) {
//		if( seldSpfName.equals( spfName ) ) {
//			return;
//		}
		
		prevSeldSpfName = seldSpfName;
		
		seldSpfName = spfName;
		
    	if( seldSpfName == null || seldSpfName.isEmpty() ) {
    		if( currActionBtn != createActionBtn ) {
    			seldRbdsList.clear();
    			seldRbdsLviewer.setInput( seldRbdsList );
    			seldRbdsLviewer.refresh();
    			rscLviewer.viewRbd( null );
    			rscLviewer.refresh();

    			removeRbdsBtn.setEnabled( false );
    			renameRbdBtn.setEnabled( false );

    			// if delete is selected and there are no spfs in the selected group
    			// then we will allow the user to delete the spf group.
    			//
    			if( currActionBtn == deleteActionBtn ) {
    				spfActionBtn.setVisible( false );
    				deleteSpfGroupBtn.setVisible( true );
    			}

    			spfLocationLbl.setText("");
    		}

	    	return;
    	}
    	
		spfActionBtn.setVisible( true );
    	deleteSpfGroupBtn.setVisible( false );
     	
    	Boolean isUserLevel = false;
    	
		try {
			seldRbdsList = SpfsManager.getInstance().getRbdsFromSpf(
								seldSpfGroup, seldSpfName, false ); // don't resolve Latest Cycle Times

			isUserLevel = SpfsManager.getInstance().isUserLevelSpf( seldSpfGroup, seldSpfName );
			
			if( isUserLevel ) {
				
			}
			LocalizationContext cntxt = SpfsManager.getInstance().getSpfContext(seldSpfGroup, seldSpfName);
			spfLocationLbl.setText( "Localization="+cntxt.getLocalizationLevel().toString()+":"+
													cntxt.getContextName() );			
		} catch (VizException e) {
			System.out.println("Error getting Rbds from SPF: "+e.getMessage() );
			seldRbdsList.clear();
		}

		seldRbdsLviewer.setInput( seldRbdsList );
		seldRbdsLviewer.refresh();

		if( !seldRbdsList.isEmpty() ) {
			StructuredSelection rbdsel = new StructuredSelection( seldRbdsList.get(0) );                      	    	

			seldRbdsLviewer.setSelection( rbdsel );

			if( currActionBtn == deleteActionBtn ||
					currActionBtn == modifyActionBtn ) {

				spfActionBtn.setEnabled( isUserLevel );       				
			}
		}
		else {
			renameRbdBtn.setEnabled( false );
			removeRbdsBtn.setEnabled( false );
			moveDownBtn.setEnabled( false );
			moveUpBtn.setEnabled( false );
		}
    }
	
	private void rbdVierwSelectionChanged( SelectionChangedEvent event ) {
		StructuredSelection sel_rbds = (StructuredSelection)event.getSelection();                      	    	
		rscLviewer.viewRbd( sel_rbds.size() == 1 ? 
						    (AbstractRBD<?>)sel_rbds.getFirstElement() : null );			       	    	
		rscLviewer.refresh();

		Iterator sel_iter = sel_rbds.iterator();

		Boolean isUserLevelSpf;
		
		if( currActionBtn == createActionBtn ) {
			isUserLevelSpf = true;
		}
		else {				
			isUserLevelSpf = SpfsManager.getInstance().isUserLevelSpf( seldSpfGroup, seldSpfName );
		}
			
		// if deleting then disable all RBD buttons
		//
		if( currActionBtn == deleteActionBtn ) {
		
			spfActionBtn.setEnabled( isUserLevelSpf );       				

			addRbdsBtn.setEnabled( false );
			
			moveDownBtn.setEnabled( false );
			moveUpBtn.setEnabled( false );
			
			removeRbdsBtn.setEnabled( false );
			renameRbdBtn.setEnabled( false );
		}       	
		else if( currActionBtn == createActionBtn ) {

			spfActionBtn.setEnabled( 
					!sel_rbds.isEmpty() && 
					!seldSpfGroup.isEmpty() && 
					!seldSpfName.isEmpty() );

			addRbdsBtn.setEnabled( true );

			moveDownBtn.setEnabled( true );
			moveUpBtn.setEnabled( true );

			removeRbdsBtn.setEnabled( sel_rbds.size() > 0 );   		    	
			renameRbdBtn.setEnabled( sel_rbds.size() == 1 );
		}       				
		// If modifying an SPF, check to see if the rbd is in a
		// non-user level 
		else if( currActionBtn == modifyActionBtn ) {

			spfActionBtn.setEnabled( isUserLevelSpf );       				

			addRbdsBtn.setEnabled( isUserLevelSpf );

			moveDownBtn.setEnabled( isUserLevelSpf );
			moveUpBtn.setEnabled( isUserLevelSpf );

			removeRbdsBtn.setEnabled( isUserLevelSpf && sel_rbds.size() > 0 );   		    	
			renameRbdBtn.setEnabled( isUserLevelSpf && sel_rbds.size() == 1 );
		}
	} 

    
    private void moveRbdsUp() {
		int seldIndxs[] = seldRbdsLviewer.getList().getSelectionIndices();
    	
		if( seldIndxs.length == 0 ) {
    		return;
    	}

		for( int i=0 ; i<seldIndxs.length ; i++ ) {
			if( seldIndxs[i] == i ) {
				continue;
			}
			AbstractRBD<?> rbdSel = seldRbdsList.get( seldIndxs[i] );

			if( rbdSel != null ) {
				seldRbdsList.remove( seldIndxs[i] );
				seldRbdsList.add( seldIndxs[i] - 1, rbdSel );	    		
			}			
		}

		seldRbdsLviewer.refresh();
    }
    
    private void moveRbdsDown() {
		int seldIndxs[] = seldRbdsLviewer.getList().getSelectionIndices();
    	
		if( seldIndxs.length == 0 ) {
    		return;
    	}

		for( int i=seldIndxs.length-1 ; i>=0 ; i-- ) {
			if( seldIndxs[i] == seldRbdsList.size()-seldIndxs.length+i ) {
				continue;
			}
			AbstractRBD<?> rbdSel = seldRbdsList.get( seldIndxs[i] );

			if( rbdSel != null ) {
				seldRbdsList.remove( seldIndxs[i] );
				seldRbdsList.add( seldIndxs[i] + 1, rbdSel );	    		
			}			
		}

		seldRbdsLviewer.refresh();
    }
    

    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

    private void renameRbd() {
    	StructuredSelection rbdsel = (StructuredSelection)seldRbdsLviewer.getSelection();                      	    	

    	if( rbdsel.isEmpty() ) {
    		return;
    	}
    	AbstractRBD<?> selRbd = (AbstractRBD<?>)rbdsel.getFirstElement();

    	// sanity check (since the button should be disabled if not User Level),
    	// 
    	if( currActionBtn == modifyActionBtn ) {
    		LocalizationFile lFile = selRbd.getLocalizationFile(); 

    		if( lFile != null &&
    				lFile.getContext().getLocalizationLevel() != LocalizationLevel.USER ) {
    			MessageDialog errDlg = new MessageDialog( 
    					shell, "Error", null, 
    					"You do not have permissions to rename a "+
    					lFile.getContext().getLocalizationLevel().toString() + " Level Rbd.",
    					MessageDialog.ERROR, new String[]{"OK"}, 0);
    			errDlg.open();
    			return;
    		}
    	}

    	RenameRbdDialog newSpfFileNameDialog = new RenameRbdDialog( shell, selRbd ); 

    	newSpfFileNameDialog.open();

    	seldRbdsLviewer.refresh( true );
    }

    // Modify an existing SPF. 
    //
    private void saveSpf() {

    	try {
    		if( seldRbdsList.isEmpty() ) {
    			throw new VizException( "No RBDs are selected" );
    		}    	
    		else if( seldSpfGroup.isEmpty() || seldSpfName.isEmpty() ) {
    			throw new VizException( "Select an SPF Name and Group." );
    		}
    		else { // check for duplicate names
    			for( AbstractRBD<?> rbd1 : seldRbdsList ) {
    				for( AbstractRBD<?> rbd2 : seldRbdsList ) {
    					if( rbd1 != rbd2 ) {
    						if( rbd1.getRbdName().equals( rbd2.getRbdName() ) ) {
    							throw new VizException("There are duplicate RBD Names in the SPF\n"+
    									"Rename one of the RBDs named "+ rbd1.getRbdName() );
    						}
    					}
    				}
    			}
    		}
    		
    		// loop thru the Rbds and set the sequence number 
    		// TODO: This should be changed later to save the ordering in a 
    		// separate SPF file. One reason is that simply adding an rbd to a SITE level file
    		// will now require that all/most of the SITE level files will need to be overridden
    		// just because their sequence number changes.
    		//
    		for( int seqNum=1 ; seqNum <= seldRbdsList.size() ; seqNum++ ) {

    			seldRbdsList.get( seqNum-1).setRbdSequence( seqNum );
    		}
    
    		SpfsManager.getInstance().saveSpf( seldSpfGroup, seldSpfName, seldRbdsList, 
    				saveRefTimeBtn.getSelection(), false );    			
    		
    		MessageBox mb = new MessageBox( shell, SWT.OK );         								
    		mb.setText( "SPF Saved" );
    		mb.setMessage( "\nSPF "+ seldSpfGroup+File.separator+seldSpfName+
    						" has been saved.");
    		mb.open();
    	}
    	catch( VizException e ) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Error", null, 
					"Error Saving SPF "+ seldSpfGroup+File.separator+seldSpfName+":\n\n"+
							e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
    	}    	
    }
    
    private void createSpf() {
    	try {
			SpfsManager.getInstance().createSpf( seldSpfGroup, seldSpfName, seldRbdsList,
										saveRefTimeBtn.getSelection(), false );
			
    		MessageBox mb = new MessageBox( shell, SWT.OK );         								
    		mb.setText( "SPF Created" );
    		mb.setMessage( "\nSPF "+ seldSpfGroup+File.separator+seldSpfName+
    						" has been created.");
    		mb.open();
    		
    		// reset the gui after creating the SPF
    		currActionBtn.setSelection( false );
    		modifyActionBtn.setSelection( true );
    		
    		setCurrentAction( modifyActionBtn );

		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Error", null, 
					"Error Creating Spf "+ seldSpfGroup+File.separator+seldSpfName+":\n\n"+
							e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
		}
    }

    private void deleteSpf() {
    	try {    		
			SpfsManager.getInstance().deleteSpf( seldSpfGroup, seldSpfName );
			
    		MessageBox mb = new MessageBox( shell, SWT.OK );         								
    		mb.setText( "SPF Deleted" );
    		mb.setMessage( "\nSPF "+ seldSpfGroup+File.separator+seldSpfName+
    						" has been deleted.");
    		mb.open();
    		
    		currActionBtn.setSelection( false );
    		modifyActionBtn.setSelection( true );
    		setCurrentAction( modifyActionBtn );

		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Info", null, 
					"Error Deleting Spf "+ seldSpfName+":\n"+
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
		}

		setSeldSpfGroup( seldSpfGroup );
    }
    
    private void deleteSpfGroup() {
		try {
			SpfsManager.getInstance().deleteSpfGroup( seldSpfGroup );
			
			currActionBtn.setSelection( false );
			modifyActionBtn.setSelection( true );
			setCurrentAction( modifyActionBtn );

		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Info", null, 
					"Error Deleting Spf Group "+ seldSpfGroup+":\n"+
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
    		return;
		}
    }
    
    public void updateDialog() {
    	shell.setSize( initDlgSize );
    	String saveSpfGroup = seldSpfGroup;
    	String saveSpfName  = seldSpfName;
    	
    	currSpfGroupCombo.setItems( SpfsManager.getInstance().getAvailSPFGroups() );

        // if the user has pre selected a group then select it
        //
        if( saveSpfGroup != null && !saveSpfGroup.isEmpty() ) {
        	for( int g=0 ; g<currSpfGroupCombo.getItemCount() ; g++ ) {
        		if( saveSpfGroup.equals( currSpfGroupCombo.getItem(g) ) ) {
        			currSpfGroupCombo.select(g);
        			seldSpfName = saveSpfName;
            		setSeldSpfGroup( currSpfGroupCombo.getText() );        			
        		}
        	}
        }
        else if( currSpfGroupCombo.getItemCount() > 0 ) {
        	currSpfGroupCombo.select(0);
    		setSeldSpfGroup( currSpfGroupCombo.getText() );        			
        }
        else {
        	currSpfGroupCombo.setEnabled( false );
        	currSpfGroupCombo.setEnabled( false );
        }
    }
}
