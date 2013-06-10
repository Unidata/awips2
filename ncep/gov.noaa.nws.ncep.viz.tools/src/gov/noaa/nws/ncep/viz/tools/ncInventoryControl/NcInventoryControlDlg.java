package gov.noaa.nws.ncep.viz.tools.ncInventoryControl;

import static java.lang.System.out;

import gov.noaa.nws.ncep.edex.common.ncinventory.ManageNcInventoryMsg;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryDefinition;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.ejb.criteria.expression.function.AggregationFunction.SUM;

import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcGridInventory;

// currently just let the user re-init the ncinventory

// TODO : add following capability :
//   1) query ncInventories. 
//   2) tree-view of inventory
public class NcInventoryControlDlg extends Dialog {
	Shell shell=null;
	
	private Button listInvNames = null;
	
	private Button listRscDefns = null;

	private ListViewer invLViewer = null;

    private Button dumpInvBtn = null;
    
    private Button invSummaryBtn = null;

    private Button reinitInvBtn = null;

    private Button invEnabledBtn = null;

    private Button deleteInvBtn = null;

    private Button deleteAllInvsBtn = null;

    private Button createAllInvsBtn = null;

    private Button genEdexInvDefnsBtn = null;

//    private ResourceDefinition ncGridRscDefn = null;
       
	public NcInventoryControlDlg( Shell sh ) {
		super(sh);
		
    	shell = new Shell( SWT.SHELL_TRIM | SWT.MODELESS );
    	shell.setText( "NcInventory Manager" );

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	shell.setLayout(mainLayout);

    	Composite topForm = new Composite(shell, SWT.NONE);
    	GridData gd = new GridData( GridData.FILL_BOTH);
//    	gd.grabExcessHorizontalSpace = true;
//    	gd.grabExcessVerticalSpace = true;
    	topForm.setLayoutData( gd );
    	
    	topForm.setLayout(new FormLayout());
    	topForm.setSize(500, 160);
    	
    	Composite listTypeComp = new Composite(topForm, SWT.NONE);
    	FormData fd = new FormData();
//    	fd.height = 100;
    	fd.top = new FormAttachment( 0, 15 );
    	fd.left = new FormAttachment( 0, 10 );
    	fd.right = new FormAttachment( 100, -10 );
    	listTypeComp.setLayoutData( fd );

    	GridLayout gridLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	listTypeComp.setLayout(mainLayout);

    	listInvNames = new Button( listTypeComp, SWT.RADIO );
    	listInvNames.setText("List By Inventory Names");
    	listInvNames.setSelection( true );

    	listRscDefns = new Button( listTypeComp, SWT.RADIO );
    	listRscDefns.setText("List By Resource Definition");
    	listRscDefns.setSelection( false );
    	    	
    	invLViewer = new ListViewer( topForm, 
    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData(100, 450);
    	fd.top = new FormAttachment( listTypeComp, 30, SWT.BOTTOM );
    	fd.left = new FormAttachment( 0, 10 );
    	fd.right = new FormAttachment( 100, -10 );
    	// This allows a resize to change the size of the lists.
    	fd.bottom = new FormAttachment( 100, -195 ); 
    	invLViewer.getList().setLayoutData( fd );

    	Label lbl = new Label(topForm, SWT.NONE);
    	lbl.setText("Inventories:");
    	fd = new FormData();
    	fd.left = new FormAttachment( invLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( invLViewer.getList(), -3, SWT.TOP );
    	lbl.setLayoutData( fd );

    	
//		final ResourceDefinition ncGridRscDefn = new ResourceDefinition();
//		ncGridRscDefn.setResourceDefnName( NcGridInventory.ncGridInventoryName );
//		ncGridRscDefn.setInventoryEnabled( false );

    	// elements are the Inventory Names
    	//
    	invLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				if( listInvNames.getSelection() ) {
					try {
						NcInventoryRequestMsg dirRequest = NcInventoryRequestMsg.makeDirectoryRequest();

						Object rslts = ThriftClient.sendRequest( dirRequest );

//						out.println("inv names request returned "+rslts.getClass().getCanonicalName() );

//						if( !(rslts instanceof String[]) ) {
//							out.println("Inventory Names Request Error: expecting String[] return." );
//							throw new VizException( rslts.toString() );
//						}
//						String[] invNamesList = (String[])rslts;

						if( rslts instanceof String ) {
							throw new VizException( rslts.toString() );
						}
						else if( !(rslts instanceof ArrayList<?>) ) {
							out.println("Inventory Directory Directory Error: expecting NcInventoryDefinition[] return." );
							throw new VizException( "Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>." );
						}
						else if( ((ArrayList<?>)rslts).isEmpty() ) {
							out.println("Inventory Directory Request Error: No Inventories initialized.???" );
//							throw new VizException( "Inventory Directory Request Error: No Inventories initialized." );
						}
						if( !(((ArrayList<?>)rslts).get(0) instanceof NcInventoryDefinition) ) {
							throw new VizException( "Inventory Directory Request Error: expecting ArrayList<NcInventoryDefinition>." );
						}
						
						// used to set the inventory initialized flag
						ArrayList<NcInventoryDefinition> invDefnsList = (ArrayList<NcInventoryDefinition>)rslts;
						
						String[] invNamesList = new String[ invDefnsList.size()];
						for( int i=0 ; i<invNamesList.length ; i++ ) {
							invNamesList[i] = invDefnsList.get(i).getInventoryName();
						}
						Arrays.sort( invNamesList );
						
						return invNamesList;
					} 
					catch ( VizException vizex ) {
						MessageDialog errDlg = new MessageDialog( 
								shell, "Error", null, 
								"Error retrieving list of Inventories.\n"+vizex.getMessage(),
								MessageDialog.ERROR, new String[]{"OK"}, 0);
						errDlg.open();
						return null;
					}
				}
				else if ( listRscDefns.getSelection() ) {

					List<ResourceDefinition> rscDefnsList=new ArrayList<ResourceDefinition>();
					try {
						for( ResourceDefinition rd : 
							ResourceDefnsMngr.getInstance().getAllResourceDefinitions() ) {
							if( rd.usesInventory() ) {
								rscDefnsList.add( rd );
							}						
						}			

						// A dummy placeholder for the NcGridInventory
						//
//						rscDefnsList.add( ncGridRscDefn );

						ResourceDefinition[] rscDefnsArray = 
							rscDefnsList.toArray( new ResourceDefinition[0] );
						Arrays.sort( rscDefnsArray );

						return rscDefnsArray;
//						Object invNamesList[] = new Object[ rscDefnsList.size() ];
//
//						for( int r=0 ; r< rscDefnsList.size() ; r++ ) { 
//							ResourceDefinition rscDefn = rscDefnsList.get( r );
//							String invName = rscDefn.getResourceDefnName();
////								LocalizationManager.getInstance().getCurrentUser()+
////								":"+rscDefn.getResourceDefnName();
//							invNamesList[r] = (Object)invName;
//						}
//						return invNamesList;

//						return rscDefnsArray;

					} catch (VizException e) {
						return null;
					} 					
				}
				return null;
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});

    	invLViewer.setLabelProvider( new LabelProvider() {
   			public String getText( Object element ) {
   				if( element instanceof ResourceDefinition ) {
   					ResourceDefinition seldRscDefn=(ResourceDefinition)element;
   					return seldRscDefn.getResourceDefnName()+
   					(seldRscDefn.getInventoryEnabled() ? "(E)" : "");
   				}
   				else if( element instanceof String ){
   					return (String)element;
   				}
   				else return "???";
   			}
		});
    	
    	invLViewer.setInput( listInvNames.getSelection() ); // 
    	invLViewer.refresh( true );
    	
		invEnabledBtn = new Button( topForm, SWT.CHECK );
		invEnabledBtn.setText( "Rsc Defn Inventory Enabled");
//		invEnabledBtn.setEnabled( false );
		// not applicable anymore
		invEnabledBtn.setEnabled( listRscDefns.getSelection() );
		
    	fd = new FormData();
    	fd.left = new FormAttachment( invLViewer.getList(), 0, SWT.LEFT );
    	fd.top  = new FormAttachment( invLViewer.getList(), 20, SWT.BOTTOM );
    	invEnabledBtn.setLayoutData(fd);

    	dumpInvBtn = new Button( topForm, SWT.NONE);
    	dumpInvBtn.setText("  Dump  ");

    	fd = new FormData();
    	fd.left = new FormAttachment( invLViewer.getList(), 10, SWT.LEFT );
    	fd.top  = new FormAttachment( invEnabledBtn, 10, SWT.BOTTOM );
    	dumpInvBtn.setLayoutData(fd);
        
    	//
    	reinitInvBtn = new Button( topForm, SWT.NONE);
    	reinitInvBtn.setText("  Re-Init  ");

    	fd = new FormData();
    	fd.left = new FormAttachment( dumpInvBtn, 30, SWT.RIGHT );
    	fd.top  = new FormAttachment( dumpInvBtn, 0, SWT.TOP );    	
    	reinitInvBtn.setLayoutData(fd);
        
		deleteInvBtn = new Button( topForm, SWT.NONE );
		deleteInvBtn.setText( " Delete ");
//		deleteInvBtn.setEnabled( false );
		
    	fd = new FormData();
    	fd.left = new FormAttachment( dumpInvBtn, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( dumpInvBtn, 15, SWT.BOTTOM );
    	deleteInvBtn.setLayoutData(fd);

    	invSummaryBtn = new Button( topForm, SWT.NONE );
    	invSummaryBtn.setText( " Summary ");
		
    	fd = new FormData();
    	fd.left = new FormAttachment( reinitInvBtn, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( reinitInvBtn, 15, SWT.BOTTOM );
    	invSummaryBtn.setLayoutData(fd);

    	genEdexInvDefnsBtn = new Button( topForm, SWT.NONE );
    	genEdexInvDefnsBtn.setText( " Generate Edex IDs... ");
		
    	fd = new FormData();
    	fd.left = new FormAttachment( reinitInvBtn, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( reinitInvBtn, 15, SWT.BOTTOM );
    	genEdexInvDefnsBtn.setLayoutData(fd);

    	
    	Button defineInvBtn = new Button( topForm, SWT.None );
    	defineInvBtn.setText( " Define... ");
		
    	fd = new FormData();
    	fd.left = new FormAttachment( deleteInvBtn, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( deleteInvBtn, 15, SWT.BOTTOM );
    	defineInvBtn.setLayoutData(fd);
    	
    	invLViewer.addSelectionChangedListener( new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();
            	
            	if( seld_elem == null ) {
            		//out.println("sel changed and is null");
        			dumpInvBtn.setEnabled( false );
            		invEnabledBtn.setEnabled( false );
        			reinitInvBtn.setEnabled( false );
        			deleteInvBtn.setEnabled( false );
        			invSummaryBtn.setEnabled( false );
        			genEdexInvDefnsBtn.setEnabled( false );
            	}
            	else if( listInvNames.getSelection() ) {
        			dumpInvBtn.setEnabled( true );
            		invEnabledBtn.setEnabled( true );
        			reinitInvBtn.setEnabled( true );
        			
        			deleteInvBtn.setEnabled( true );
        			invSummaryBtn.setEnabled( true );
        			genEdexInvDefnsBtn.setEnabled( true );
        			reinitInvBtn.setText( " Re-Init " );
            	}
            	else {
            		ResourceDefinition rd = (ResourceDefinition)seld_elem.getFirstElement();
            		
            		if( rd != null ) {        
            			Boolean initialized = rd.isInventoryInitialized();
//            				( rd.getResourceDefnName().equals(
//            						NcGridInventory.ncGridInventoryName ) ? 
//            								NcGridInventory.getInstance().isInitialized() : 
//            									rd.isInventoryInitialized() );

            			invEnabledBtn.setEnabled( initialized );
            			
            			invEnabledBtn.setSelection( rd.getInventoryEnabled() );
            			
            			reinitInvBtn.setEnabled( true );
            			reinitInvBtn.setText(
            					(initialized ? " Re-Init " : " Create " ) );
            			deleteInvBtn.setEnabled( initialized );
            			invSummaryBtn.setEnabled( initialized );
            			dumpInvBtn.setEnabled( initialized );
            		}
            		else {
            			dumpInvBtn.setEnabled( false );
                		invEnabledBtn.setEnabled( false );
            			reinitInvBtn.setEnabled( false );
            			deleteInvBtn.setEnabled( false );
            			invSummaryBtn.setEnabled( false );
            			genEdexInvDefnsBtn.setEnabled( false );
            		}
            	}
			}
    	});

    	listInvNames.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                invLViewer.refresh( true );
                invEnabledBtn.setEnabled( false );
            }
    	});

    	listRscDefns.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                invLViewer.refresh( true );
                invEnabledBtn.setEnabled( true );            }
    	});

    	invEnabledBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	
            	if( listInvNames.getSelection() ) {
            		return;
            	}
            	
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();            	
            	ResourceDefinition rscDefn = (ResourceDefinition)seld_elem.getFirstElement();       

        		try {
        			if( rscDefn == null ) {
        				throw new VizException("No inventory is selected");
        			}

        			Boolean enabled = invEnabledBtn.getSelection();
        			Boolean initialized = rscDefn.isInventoryInitialized();
//        				( rscDefn.getResourceDefnName().equals(
//        						NcGridInventory.ncGridInventoryName ) ? 
//        								NcGridInventory.getInstance().isInitialized() : 
//        									rscDefn.isInventoryInitialized() );
        	
        			if( enabled && !initialized ) { 
        				throw new VizException("Inventory has not been initialized");
        			}

        			rscDefn.setInventoryEnabled( enabled );
        			if( enabled ) {
        				rscDefn.enableInventoryUse();
        			}
        			else {
        				rscDefn.disableInventoryUse();
        			}
        			
//        			if( rscDefn.getResourceDefnName().equals( 
//        					NcGridInventory.ncGridInventoryName ) ) {
//        				NcGridInventory.getInstance().
//        			}
//        			dumpInvBtn.setEnabled( enabled );
//        			deleteInvBtn.setEnabled( enabled );
//        			reinitInvBtn.setText( (enabled ? " Re-Init " : " Create ") );
        			
        			invLViewer.refresh( true );
        			
//        			MessageDialog msgDlg = new MessageDialog( 
//        					shell, "Info", null, "Inventory "+rscDefn.getResourceDefnName()+" has been "+
//        						(enabled ? "Enabled." : "Disabled"),
//        					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
//        			msgDlg.open();

        		} catch (VizException e) {
        			MessageDialog errDlg = new MessageDialog( 
        					shell, "Error", null, 
        					"Error:\n\n"+e.getMessage(),
        					MessageDialog.ERROR, new String[]{"OK"}, 0);
        			errDlg.open();
        		}            		
            }
        });
    	
    	dumpInvBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	String invName;
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();            	

        		try {
        			if( listInvNames.getSelection() ) {
        				invName = (String)seld_elem.getFirstElement();
        			}
        			else {
        				ResourceDefinition rscDefn = (ResourceDefinition)seld_elem.getFirstElement();       

        				if( rscDefn == null ) {
        					throw new VizException("No inventory is selected");
        				}
        				else if( !rscDefn.usesInventory() || 
        						 !rscDefn.getInventoryEnabled() ) {
        					throw new VizException("Inventory is not Enabled");
        				}
        		    	else if( !rscDefn.isInventoryInitialized() ) {
        		    		throw new VizException("Inventory Not Initialized.");
        		    	}

        				Boolean initialized = rscDefn.isInventoryInitialized();
//        					( rscDefn.getResourceDefnName().equals(
//        							NcGridInventory.ncGridInventoryName ) ? 
//        									NcGridInventory.getInstance().isInitialized() : 
//        										rscDefn.isInventoryInitialized() );

        				if( !initialized ) {
        					throw new VizException("Inventory has not been initialized");
        				}
//        				else if( rscDefn.getResourceDefnName().equals( NcGridInventory.ncGridInventoryName ) ) {
//
//        					String dumpResponseStr = 
//        						NcGridInventory.getInstance().dumpNcGribInventory();
//
//        					MessageDialog msgDlg = new MessageDialog( 
//        							shell, "Grid Inventory Dump", null, dumpResponseStr,
//        							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
//        					msgDlg.open();	
//        				}
        				invName = rscDefn.createNcInventoryDefinition().getInventoryName();//
        				    //LocalizationManager.getInstance().getCurrentUser() + ":" + rscDefn.getResourceDefnName();        					
        					//String dumpResponseStr = rscDefn.dumpInventory();        			
        			}
        			
					String dumpResponseStr = dumpInventory( invName );
					
					MessageDialog msgDlg = new MessageDialog( 
							shell, "Inventory Dump", null, dumpResponseStr,
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
					msgDlg.open();

        		} catch (VizException e) {
        			MessageDialog errDlg = new MessageDialog( 
        					shell, "Error", null, 
        					"Error:\n\n"+e.getMessage(),
        					MessageDialog.ERROR, new String[]{"OK"}, 0);
        			errDlg.open();
        		}            		
            }
        });

    	reinitInvBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();
          
            	Iterator sel_iter = seld_elem.iterator();

            	while( sel_iter.hasNext() ) {
            		try {
            			String inventoryName;
            			if( listInvNames.getSelection() ) {
            				inventoryName = (String)sel_iter.next();

            				initInventory( inventoryName );
                			
                			MessageDialog msgDlg = new MessageDialog( 
                					shell, "Info", null, 
                					"NcInventory "+inventoryName + 
                					" has been re-initialized.\n", 
                					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
                			msgDlg.open();

            			}
            			else {
            				ResourceDefinition rscDefn = (ResourceDefinition)sel_iter.next();
            				
            				// if this has not been created yet, create the inventory.
            				if( !rscDefn.isInventoryInitialized() ) {
            					
            					createInventory( rscDefn.createNcInventoryDefinition() );

            					// in this case the alias is the name created by our
            					// resource Defn since we just created it.
            					rscDefn.setInventoryAlias(
            							rscDefn.createNcInventoryDefinition().getInventoryName() );
            					
            					rscDefn.enableInventoryUse();
            					
            					MessageDialog msgDlg = new MessageDialog( 
                    					shell, "Info", null, 
                    					"NcInventory has been created and initialized for "+
                    					"RscDefn "+ rscDefn.getResourceDefnName()+ ".\n", 
                    					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
                    			msgDlg.open();                    		
            				}
            				else {
                				inventoryName = rscDefn.createNcInventoryDefinition().getInventoryName();    
                    			initInventory( inventoryName );
                    			
            					MessageDialog msgDlg = new MessageDialog( 
                    					shell, "Info", null, 
                    					"NcInventory for RscDefn "+ rscDefn.getResourceDefnName()+ " has been re-initialized.\n", 
                    					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
                    			msgDlg.open();                    		
            				}
            			}
            			
            			invLViewer.refresh( true );
            			reinitInvBtn.setText( " Re-Init ");

            		} catch (VizException e) {
            			MessageDialog errDlg = new MessageDialog( 
            					shell, "Error", null, 
            					"Error:\n\n"+e.getMessage(),
            					MessageDialog.ERROR, new String[]{"OK"}, 0);
            			errDlg.open();
            		}
            	}            	
            }
        });

    	invSummaryBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();
          
            	Iterator sel_iter = seld_elem.iterator();

            	while( sel_iter.hasNext() ) {
        			String inventoryName;
        			if( listInvNames.getSelection() ) {
        				inventoryName = (String)sel_iter.next();
        			}
        			else {
        				ResourceDefinition rscDefn = (ResourceDefinition)sel_iter.next();
//        				if( rscDefn.isInventoryInitialized() ) {
        				try {
							inventoryName = rscDefn.createNcInventoryDefinition().getInventoryName();
						} catch (VizException e) {
							out.println("Error getting invName? ");
							return; 
						}        					
        			}

            		getInventorySummary( inventoryName );							            		
            	}            	
            }
        });
    	
    	defineInvBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	NcInventoryDefineDlg dfnDlg = new NcInventoryDefineDlg( shell );
            	dfnDlg.open();            	
            }
    	});
    	
    	deleteInvBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();
          
            	Iterator sel_iter = seld_elem.iterator();

            	while( sel_iter.hasNext() ) {
        			String inventoryName;
        			if( listInvNames.getSelection() ) {
        				inventoryName = (String)sel_iter.next();
        			}
        			else {
        				ResourceDefinition rscDefn = (ResourceDefinition)sel_iter.next();
        				try {
							inventoryName = rscDefn.createNcInventoryDefinition().getInventoryName();
        					//LocalizationManager.getInstance().getCurrentUser() + ":" + rscDefn.getResourceDefnName();
						} catch (VizException e) {
							// TODO Auto-generated catch block
							out.println("error getting invName from RD???");
							return;							
						}         					
        			}

        			try {
						deleteInventory( inventoryName );
					
						MessageDialog msgDlg = new MessageDialog( 
								shell, "Info", null, 
								"NcInventory "+ inventoryName + 
								" has been deleted.\n", 
								MessageDialog.INFORMATION, new String[]{"OK"}, 0);
						msgDlg.open();
					} catch ( VizException e ) {
	        			MessageDialog errDlg = new MessageDialog( 
	        					shell, "Error", null, 
	        					"Error:\n\n"+e.getMessage(),
	        					MessageDialog.ERROR, new String[]{"OK"}, 0);
	        			errDlg.open();
					}

                   	invLViewer.refresh( true );
            	}            	
            }
        });

    	genEdexInvDefnsBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	
            	StructuredSelection seld_elem = (StructuredSelection)invLViewer.getSelection();
          
            	Iterator sel_iter = seld_elem.iterator();

            	while( sel_iter.hasNext() ) {
            		  {
            			String inventoryName;
            			if( listInvNames.getSelection() ) {
            				inventoryName = (String)sel_iter.next();

//            				initInventory( inventoryName );
            			}
            		}
            	}
            }
    	});
    	
    	createAllInvsBtn = new Button( topForm, SWT.NONE);
    	createAllInvsBtn.setText(" Re-Init All ");
    	createAllInvsBtn.setVisible( false );
    	
    	fd = new FormData();
    	fd.left = new FormAttachment( invLViewer.getList(), 0, SWT.LEFT );
    	fd.top  = new FormAttachment( reinitInvBtn, 20, SWT.BOTTOM );    	
    	createAllInvsBtn.setLayoutData(fd);
        
//    	createAllInvsBtn.addSelectionListener(new SelectionAdapter() {
//            public void widgetSelected(SelectionEvent event) {
//            	for( int i=0 ; i<invLViewer.getList().getItemCount() ; i++ ) {
//            		ResourceDefinition rscDefn = (ResourceDefinition)invLViewer.getElementAt(i);         
//        
//            		if( rscDefn == null ) {
//            			out.println("No inventory is selected???");
//            		}
//            		else if( !rscDefn.getInventoryEnabled() ) {
//            			rscDefn.setInventoryEnabled( true );
//            		}
//            	}
//                      	 	    	            	
//            	try {
//            		ResourceDefnsMngr.getInstance().initializeInventory( true );
//
//            		MessageDialog msgDlg = new MessageDialog( 
//                			shell, "Info", null, 
//                			"Began NcInventory initializing .\n" + 
//                			"This will take a few seconds.",
//                			MessageDialog.INFORMATION, new String[]{"OK"}, 0);
//                   	msgDlg.open();
//                }
//            	catch (VizException vizex ) { 
//                	MessageDialog msgDlg = new MessageDialog( 
//                			shell, "Error", null, 
//                			"Error initializing NcInventory.\n",
//                			MessageDialog.ERROR, new String[]{"OK"}, 0);
//                   	msgDlg.open();            		
//            	}
//
//            }
//        });

    	Button closeBtn = new Button( topForm, SWT.NONE);
        closeBtn.setText("  Close  ");

        fd = new FormData();
    	fd.right = new FormAttachment( 100, -10 );
    	fd.top  = new FormAttachment( genEdexInvDefnsBtn, 15, SWT.BOTTOM );
        closeBtn.setLayoutData(fd);
        
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
	}
	
	public void open() {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();

		invLViewer.refresh( true );

    	shell.setMinimumSize(260, 160);
        shell.pack(true);
    	shell.open();
   
    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}		
	}
	
	public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }	
	
	public void initInventory( String inventoryName ) throws VizException {
    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	ManageNcInventoryMsg initMsg = ManageNcInventoryMsg.makeReinitDirective();
    	
    	initMsg.setInventoryName( inventoryName );
    	//delMsg.deleteForAllEdexes( true );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( initMsg );

		out.println("inv request returned "+rslts.getClass().getCanonicalName() );
		
		if( !(rslts instanceof String) ) {
			out.println("Inventory Init Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		long t02 = System.currentTimeMillis();

		out.println("Inventory R-Inited for "+ inventoryName+ 
				" took "+ (t02-t01)+ "msecs " );
	}
	
	public void createInventory( NcInventoryDefinition invDefn ) throws VizException {
    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	ManageNcInventoryMsg createInvMsg = ManageNcInventoryMsg.makeCreateDirective();
    	
    	createInvMsg.setInventoryDefinition( invDefn );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( createInvMsg );

		out.println("inv request returned "+rslts.getClass().getCanonicalName() );
		
		if( !(rslts instanceof String) ) {
			out.println("Error Creating Inventory: "+rslts.toString() );
			throw new VizException("Error Creating Inventory: "+rslts.toString() );
		}

		long t02 = System.currentTimeMillis();

		out.println("Inventory Created for "+ invDefn.getInventoryName() + 
				" took "+ (t02-t01)+ "msecs " );
	}

	// 
    public String deleteInventory( String inventoryName ) throws VizException {
    	
    	if( inventoryName.equals( NcGridInventory.ncGridInventoryName ) ) {
			throw new VizException( "Can't delete the NcGridInventory." );
    	}
    	
    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	ManageNcInventoryMsg delMsg = ManageNcInventoryMsg.makeDeleteDirective();
    	
    	delMsg.setInventoryName( inventoryName );
    	//delMsg.deleteForAllEdexes( true );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( delMsg );

		out.println("inv request returned "+rslts.getClass().getCanonicalName() );
		
		if( !(rslts instanceof String) ) {
			out.println("Inventory Delete Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		long t02 = System.currentTimeMillis();

		out.println("Inventory Dump for "+ inventoryName+ 
				" took "+ (t02-t01)+ "msecs " );
		return (String)rslts;
    }

    // return a summary of the dump...
    public String dumpInventory( String inventoryName ) throws VizException {
    
    	// Create an NcInventoryRequest script to get query the ncInventory for the types.
    	// 
    	NcInventoryRequestMsg reqMsg = NcInventoryRequestMsg.makeDumpRequest();
//		String inventoryName = LocalizationManager.getInstance().getCurrentUser() + ":" +
//								getResourceDefnName();
		reqMsg.setInventoryName( inventoryName );
		
		reqMsg.setRequestedParam( "" ); 
//				( inventoryName.equals( NcGridInventory.ncGridInventoryName  ) ? ""
//					: "dataTime" ) );
//		reqMsg.setDumpToFile( true );
//		reqMsg.setReqConstraintsMap(  );
		
		Object rslts;

		long t01 = System.currentTimeMillis();

		rslts = ThriftClient.sendRequest( reqMsg );

//		out.println("inv request returned "+rslts.getClass().getCanonicalName() );
		
		if( rslts == null ) {
			throw new VizException("Inventory "+ inventoryName +" is empty." );
		}
		else if( !(rslts instanceof String) ) {
//			out.println("Inventory Dump Error: expecting String return." );
			throw new VizException("Inventory Dump Request Error: String response expecting instead of "+
					rslts.getClass().getName() );
		}

		long t02 = System.currentTimeMillis();

		out.println("Inventory Dump for "+ inventoryName + 
				" took "+ (t02-t01)+ "msecs " );
		return (String)rslts;
    }

    public void getInventorySummary( String inventoryName ) {

    	NcInventoryRequestMsg sumMsg = NcInventoryRequestMsg.makeSummaryRequest();
    	
    	sumMsg.setInventoryName( inventoryName );
    	//delMsg.deleteForAllEdexes( true );
		
		Object summaryMsg;

		try {
			summaryMsg = ThriftClient.sendRequest( sumMsg );

			if( !(summaryMsg instanceof String) ) {
				out.println("Inventory Summary Error: expecting String return." );
				throw new VizException( summaryMsg.toString() );
			}

			MessageDialog msgDlg = new MessageDialog( 
					shell, "Summary Info", null, 
					(String)summaryMsg, 
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
		   	msgDlg.open();

		} catch (VizException e) {
			e.printStackTrace();
		}
    }
}
