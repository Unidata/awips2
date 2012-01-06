package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.NmapResourceUtils;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Vector;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;


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
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class ImportRbdDialog extends Dialog { 
   
    private Shell shell;
    private String dlgTitle = null;

    private Boolean oked =  new Boolean(false);

    // if true then just allow the user to select one pane from 
    // one display and if false then the user will select displays
    private boolean importSinglePane = false;

    private Combo      spf_group_combo = null;
    private ListViewer spf_name_lviewer = null;
    private ListViewer rbd_lviewer = null;
    private ListViewer rsc_lviewer = null;
    
    private Combo pane_combo = null;
    private Label pane_lbl = null;
    
    private Button ok_btn = null;
    
    private RbdBundle currRbdSel = null;
            
    // might be nice one day to save the group and spf name 
    // so that we can init the SaveRBD dialog with these values??
	private String seldSpfGroup = null;
    private String seldSpfName  = null;
    private String seldRbdName  = null;
    private PaneID seldPaneId = new PaneID(0,0);
   // private PaneID seldPaneID = null;
    
    public ImportRbdDialog( Shell parShell, boolean singlePane )  {
    	super(parShell);
    	importSinglePane = singlePane;
    	dlgTitle = (importSinglePane ? "Import Pane" : "Import Display" );
    	
 //   	seldRbdBndls = new ArrayList<RbdBundle>();    	
    }
      
    public Object open( ) {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	
    	shell = new Shell( parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS );
    	shell.setText(dlgTitle);
    	shell.setSize( 540, 500 ); // pack later

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

    	return (oked ? currRbdSel : null );
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

        spf_group_combo = new Combo( sel_rbds_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
        fd = new FormData();
        fd.top = new FormAttachment( 0, 35 );
        fd.left  = new FormAttachment( 0, 20 );
        fd.right = new FormAttachment( 25, -20 );
        spf_group_combo.setLayoutData( fd );
    	
        Label spf_grp_lbl = new Label( sel_rbds_grp, SWT.NONE);
        spf_grp_lbl.setText("SPF Group");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( spf_group_combo, -3, SWT.TOP );
        fd.left  = new FormAttachment( spf_group_combo, 0, SWT.LEFT );
        spf_grp_lbl.setLayoutData( fd );
        
        spf_name_lviewer = new ListViewer(sel_rbds_grp, SWT.SINGLE | SWT.BORDER | 
        					SWT.V_SCROLL|SWT.H_SCROLL);
        fd = new FormData();
        fd.top = new FormAttachment( spf_group_combo, 40, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 10 );
        fd.right = new FormAttachment( 25, 0 );
        fd.bottom = new FormAttachment( 100, -20 );
  
        spf_name_lviewer.getList().setLayoutData( fd );
        
        Label spf_name_lbl = new Label(sel_rbds_grp, SWT.NONE);
        spf_name_lbl.setText("SPF Name");
        fd = new FormData();
        fd.bottom = new FormAttachment( spf_name_lviewer.getList(), -3, SWT.TOP );
        fd.left   = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.LEFT );
        spf_name_lbl.setLayoutData( fd );
         
        rbd_lviewer = new ListViewer(sel_rbds_grp, 
        		   SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
      	fd = new FormData();
        fd.top = new FormAttachment( spf_group_combo, 0, SWT.TOP );
        fd.left  = new FormAttachment( spf_name_lviewer.getList(), 15, SWT.RIGHT );
        fd.bottom = new FormAttachment( spf_name_lviewer.getList(), 0, SWT.BOTTOM );
        fd.right = new FormAttachment( 55, 0 ); 
        rbd_lviewer.getList().setLayoutData( fd );

        Label rbd_lbl = new Label( sel_rbds_grp, SWT.NONE);
        rbd_lbl.setText("RBDs");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( rbd_lviewer.getList(), -3, SWT.TOP );
        fd.left  = new FormAttachment( rbd_lviewer.getList(), 0, SWT.LEFT );
        rbd_lbl.setLayoutData( fd );

        pane_combo = new Combo( sel_rbds_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
        fd = new FormData();
        fd.width = 60;
        fd.top = new FormAttachment( rbd_lviewer.getList(), 0, SWT.TOP );
        fd.left  = new FormAttachment( rbd_lviewer.getList(), 70, SWT.RIGHT );
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

        rsc_lviewer = new ListViewer(sel_rbds_grp, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
      	
        fd = new FormData();
      	if( importSinglePane ) {
      		fd.top = new FormAttachment( pane_combo, 40, SWT.BOTTOM );
      		fd.left  = new FormAttachment( rbd_lviewer.getList(), 15, SWT.RIGHT );
      		fd.bottom = new FormAttachment( rbd_lviewer.getList(), 0, SWT.BOTTOM );
      		fd.right = new FormAttachment( 100, -10 );
      	}
      	else {
            fd.top = new FormAttachment( rbd_lviewer.getList(), 0, SWT.TOP );
      		fd.left  = new FormAttachment( rbd_lviewer.getList(), 15, SWT.RIGHT );
      		fd.right = new FormAttachment( 100, -10 );
      		fd.bottom = new FormAttachment( rbd_lviewer.getList(), 0, SWT.BOTTOM );
     	}
        rsc_lviewer.getList().setLayoutData( fd );
        
        // do this as an indication that the list is view-only
        rsc_lviewer.getList().setBackground( sel_rbds_grp.getBackground() );

        Label rsc_lbl = new Label( sel_rbds_grp, SWT.NONE);
        rsc_lbl.setText("Resources && Overlays");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( rsc_lviewer.getList(), -3, SWT.TOP );
        fd.left  = new FormAttachment( rsc_lviewer.getList(), 0, SWT.LEFT );
        rsc_lbl.setLayoutData( fd );

        Label sep = new Label( sel_rbds_grp, SWT.SEPARATOR | SWT.HORIZONTAL );
       	fd = new FormData();
        fd.top  = new FormAttachment( spf_name_lviewer.getList(), 15, SWT.BOTTOM );
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
       			currRbdSel = null;
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
    	spf_group_combo.addSelectionListener(new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				setSeldSpfGroup();
   			} 
   		});
        
    	// TODO : can change this to be a CP that calls NmapResourceUtils.getSpfNamesForGroup()
        spf_name_lviewer.setContentProvider( NmapCommon.createSubDirContentProvider() );     
        spf_name_lviewer.setLabelProvider( NmapCommon.createFileLabelProvider( ) );

        spf_name_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged( SelectionChangedEvent event ) {
            	setSeldSpfName();
            }
        });
        
        // 
        rbd_lviewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
		 		ArrayList<RbdBundle> rbdBndls = (ArrayList<RbdBundle>)inputElement;		 		
				return rbdBndls.toArray();										
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });          

        rbd_lviewer.setLabelProvider( new LabelProvider() {
	    	public String getText( Object element ) {
	    		if( element instanceof RbdBundle ) {
	    			return ((RbdBundle)element).getRbdName();
	    		}
	    		else  return "Error: bad RBD element";
	    	}
        });
        
       	rbd_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
            	setSelectedRBDs();
            } 
       	});     
       	
       	rbd_lviewer.getList().addListener( SWT.MouseDoubleClick, new Listener() {
   			public void handleEvent(Event event) {
            	setSelectedRBDs();
            	ok();
   			}
   		});
       	
       	pane_combo.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldPaneId = PaneID.parsePaneId( pane_combo.getText() );
   				
   	    		if( currRbdSel != null ) {
   	    			currRbdSel.setSelectedPaneId( seldPaneId );
   	    		}
   	    		rsc_lviewer.setInput( currRbdSel );			
   			}
       	});

        rsc_lviewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				RbdBundle selRbd = (RbdBundle)inputElement;
				if( selRbd == null ) {
					return new String[0];
				}
				
				ArrayList<String> rscNames = new ArrayList<String>();
				boolean isMultiPane = (selRbd.getPaneLayout().getNumberOfPanes() > 1 );
			
				// loop thru all of the resources in all of the panes and create the
				// list of items for the viewer. These will depend on whether we are
				// importing a single pane (in which case only the selected pane's 
				// resources are displayed) and whole RBDs (in which case all pane's
				// resources are displayed) For multi-pane RBDs the format will include
				// the name of the pane.
				for( int r=0 ; r<selRbd.getPaneLayout().getRows() ; r++ ) {
					for( int c=0 ; c<selRbd.getPaneLayout().getColumns() ; c++ ) {
						PaneID paneId = new PaneID(r,c)	;
						
						NCMapRenderableDisplay disp = selRbd.getDisplayPane(paneId);
						AbstractDescriptor mapDescr = (AbstractDescriptor)disp.getDescriptor();

						boolean showPane = (!importSinglePane || 
										    (importSinglePane && (paneId.compare( selRbd.getSelectedPaneId() ) == 0)));
					
						// show this pane only if its a single pane or if this is the 
						if( showPane ) {
					
							if( isMultiPane ) {
								rscNames.add( "Pane ("+ paneId.toString() +") : "+disp.getPredefinedAreaName() );
							}
							else { 
								rscNames.add( "Single Pane : "+disp.getPredefinedAreaName() );
							}
							
							for( ResourcePair rp : mapDescr.getResourceList() ) {
								if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
									INatlCntrsResourceData ncRsc = 
										(INatlCntrsResourceData)rp.getResourceData();
									String rscName = ncRsc.getResourceName().toString();

									if( ncRsc.getIsEdited() ) {
										rscName = rscName + "(E)";
									}
	//								if( ncRsc instanceof AbstractNatlCntrsRequestableResourceData &&
	//										((AbstractNatlCntrsRequestableResourceData)ncRsc).getIsDominant() ) {
	//									qualRscName = qualRscName + "(D)";
	//								}

									if( isMultiPane ) {
										rscName = "   " + rscName;
									}

									rscNames.add( rscName );
								}
							}
						}
					}
				}
		    	return rscNames.toArray();
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });          

        // make this View-only
        rsc_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
       			rsc_lviewer.getList().deselectAll();
            } 
       	});     
        rsc_lviewer.addDoubleClickListener( new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
       			rsc_lviewer.getList().deselectAll();				
			}
   		});

       	
        spf_group_combo.setItems( NmapResourceUtils.getAvailSPFGroups() );
        if( spf_group_combo.getItemCount() == 0 ) {
        	spf_group_combo.add("None Available");
        	spf_group_combo.select(0);
            spf_group_combo.setEnabled(false);
        }
        else {
        	spf_group_combo.select(0);
        	setSeldSpfGroup();
        }
    }

    private void setSeldSpfGroup() {
    	seldSpfGroup = spf_group_combo.getText();
    	File spf_group_dir = new File( NmapResourceUtils.getSpfGroupsDir(), seldSpfGroup );
//    	new File(LocalizationManager.getInstance().getFilename("spfGroupsDir") + File.separator + seldSpfGroup );	
    	spf_name_lviewer.setInput( spf_group_dir );
    	spf_name_lviewer.getList().select(0);
    	
    	setSeldSpfName();
    }
    
    private void setSeldSpfName() {
    	StructuredSelection sel_group = (StructuredSelection)spf_name_lviewer.getSelection();  
    	File seldSpfDir = (File)sel_group.getFirstElement();
    	if( seldSpfDir == null || !seldSpfDir.isDirectory() ) {
    		rbd_lviewer.setInput( null );
	    	rbd_lviewer.refresh();
			setSelectedRBDs();
    		return;
    	}
    	
    	seldSpfName = seldSpfDir.getName();
    	
    	File rbdFiles[] = seldSpfDir.listFiles( new FileFilter() {
			@Override
			public boolean accept(File f) {
    			return (!f.isDirectory() && f.getName().endsWith(".xml") ? true : false );
			}
    	});

    	Arrays.sort( rbdFiles );
    	Vector<String> maps_vect = new Vector<String>();

    	ArrayList<RbdBundle> rbdBndls = new ArrayList<RbdBundle>();
 		
 		for( File rbdFile : rbdFiles ) {
 			String rbdFilename = rbdFile.getAbsolutePath(); 
 			try {
 				Object rbdObj = SerializationUtil.jaxbUnmarshalFromXmlFile( 
 														rbdFilename );
				if( !(rbdObj instanceof RbdBundle ) ) {
					System.out.println("??RBD file "+ rbdFilename+"is an " +
							rbdObj.getClass().getName() );
					continue;
				}
				
				// set the timeMatcher's dominant resource. The name of the resource is set but we
				// need to set the actual object.
				RbdBundle rbdBndl = (RbdBundle) rbdObj;
				if( !rbdBndl.initTimeline() ) {
				//	System.out.println("???timeMatcher has dominant rsc which is not in the bundle file???");
				}
				
				rbdBndls.add( rbdBndl );
				
			} catch (SerializationException e) {
				System.out.println("JaxB error unmarshalling rbdfile:"+rbdFilename );
			} 		
 		}
 		
 		rbd_lviewer.setInput( rbdBndls );
 		rbd_lviewer.refresh();

 		rbd_lviewer.getList().select(0);

 		setSelectedRBDs();
    }
    
    private void  setSelectedRBDs() {
    	StructuredSelection sel_rbds = (StructuredSelection)rbd_lviewer.getSelection();               
    	if( sel_rbds.isEmpty() ) {
    		currRbdSel = null;
        	seldRbdName = null;
        	
    		pane_combo.add("N/A");
        	pane_combo.select(0);
        	seldPaneId = new PaneID(0,0);
    		pane_combo.setEnabled(false);
    		pane_lbl.setEnabled(false);
    		rsc_lviewer.setInput( null );
    		rsc_lviewer.refresh();
    	}
    	else {
    		currRbdSel = (RbdBundle) sel_rbds.getFirstElement();
        	
    		seldPaneId = new PaneID(0,0);
        	currRbdSel.setSelectedPaneId( seldPaneId );
    		seldRbdName = currRbdSel.getRbdName();

    		// set the pane combo items and preselect 
    		//AbstractRenderableDisplay panes[] =	currRbdSel.getDisplays();

//    		int paneCount = panes.length;
    		pane_combo.removeAll();

    		for( int r=0 ; r<currRbdSel.getPaneLayout().getRows() ; r++ ) {
    			for( int c=0 ; c<currRbdSel.getPaneLayout().getColumns() ; c++ ) {
    				pane_combo.add( new PaneID(r,c).toString() );
    			}
    		}
    		
    		if( pane_combo.getItemCount() == 0 ) {
    			pane_combo.add("N/A");
    		}
    		pane_combo.select(0);
    		pane_combo.setEnabled( pane_combo.getItemCount()>1 );
    		pane_lbl.setEnabled( pane_combo.getItemCount()>1 );
    		
    		rsc_lviewer.setInput( currRbdSel );			
    	}
    	
    	ok_btn.setEnabled( currRbdSel != null );
    }     
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
        
    private void ok() {
    	oked = true;
    	shell.dispose();
    }

}

