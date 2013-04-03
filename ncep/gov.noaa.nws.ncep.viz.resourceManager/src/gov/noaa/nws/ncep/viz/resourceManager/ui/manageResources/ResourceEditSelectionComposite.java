package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.AttributeSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 * .
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/03/10      #273        Greg Hull   
 * 03/01/11      #408        Greg Hull   remove Forecast/Observed
 * 07/25/11      #450        Greg Hull   use NcPathManager for Localization
 * 03/14/12      #606        Greg Hull   get types/sub-types from ncInventory
 * 06/06/2012     #816       Greg Hull   Alphabetize lists. Change content of listViewer to ResourceDefinitions
 * 12/13/2012    #957        Greg Hull   Show the localization level of files 
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ResourceEditSelectionComposite extends Composite {
	
	private ResourceDefnsMngr rscDefnsMngr;
	
	private ResourceName seldResourceName = null;
	
	private String prevSeldCat = "";
	
	// a map to store the previous selections for each category.
	private static HashMap<String,ResourceName> prevCatSeldRscNames;
	
    private Composite sel_rsc_comp = null;

    private Label rscTypeLbl = null;
    private Label rscTypeGroupLbl = null;

    private ListViewer rscCatLViewer = null;
    private ListViewer rscTypeLViewer = null;
    private ListViewer rscGroupLViewer = null;
    private ListViewer rscAttrSetLViewer = null;
    
    private Label  rscTypeLocLbl  = null;
    private Button copyRscTypeBtn = null;
    private Button editRscTypeBtn = null;
    private Button removeRscTypeBtn = null;    // TODO : not implemented
    
    private Label  rscGroupLocLbl  = null;
    private Button copyRscGroupBtn = null;
    private Button editRscGroupBtn = null;
    private Button removeRscGroupBtn = null;
    
    private Label  attrSetLocLbl  = null;
    private Button copyRscAttrSetBtn = null;
    private Button editRscAttrSetBtn = null;
    private Button removeRscAttrSetBtn = null;

    
    private EditResourceAction activeAction = EditResourceAction.NULL_ACTION;
	
    private final static int rscListViewerHeight = 140;
	
    
    enum EditResourceAction {
    	NULL_ACTION,
    	COPY_RESOURCE_TYPE, EDIT_RESOURCE_TYPE,    REMOVE_RESOURCE_TYPE,
    	COPY_RESOURCE_GROUP,  EDIT_RESOURCE_GROUP,   REMOVE_RESOURCE_GROUP,
    	COPY_RESOURCE_ATTR_SET, EDIT_RESOURCE_ATTR_SET, REMOVE_RESOURCE_ATTR_SET
    }
        
    private HashMap<EditResourceAction, Button> editButtonMap = new HashMap<EditResourceAction,Button>();
    
    public interface IEditResourceListener {
    	public void editResourceAction( ResourceName rscName, EditResourceAction action );
    }
    
    private IEditResourceListener rscActionListener;

    SelectionAdapter editActionBtnSelectionListener = new SelectionAdapter() {
		@Override
    	public void widgetSelected( SelectionEvent ev ) {
			EditResourceAction seldAction = (EditResourceAction)ev.widget.getData();
			
			// if the use clicks the same/active button then make sure the toggle is 
			// selected and leave is as is. 
			if( seldAction == activeAction ) {
				((Button)ev.widget).setSelection( true );
				//	return;
			}
			
			if( activeAction != EditResourceAction.NULL_ACTION ) {
				editButtonMap.get( activeAction ).setSelection( false );
			}

			activeAction = (EditResourceAction)ev.widget.getData();
			
			if( rscActionListener != null ) {
				rscActionListener.editResourceAction( seldResourceName, activeAction );
			}
		}    	
    };

	
    public ResourceEditSelectionComposite( Composite parent, IEditResourceListener editActionListener )   throws VizException {
        super(parent, SWT.SHADOW_NONE );
        
        rscDefnsMngr = ResourceDefnsMngr.getInstance();
        
        rscActionListener = editActionListener;
        
        seldResourceName = new ResourceName();
        
        if( prevCatSeldRscNames == null ) {
            prevCatSeldRscNames = new HashMap<String,ResourceName>();        	        
        }
        
    	sel_rsc_comp = this;
    	
		FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 0 );
    	fd.left = new FormAttachment( 0, 0 );
    	fd.right = new FormAttachment( 100, 0 );
    	fd.bottom = new FormAttachment( 100, 0 );
    	sel_rsc_comp.setLayoutData(fd);

    	sel_rsc_comp.setLayout( new FormLayout() );
                    	   		                
        createSelectResourceGroup();

        // set up the content providers for the ListViewers
        setContentProviders();
        addSelectionListeners();

        initWidgets();        
    }
    
    // create all the widgets in the Resource Selection (top) section of the sashForm.  
    // 
    private void createSelectResourceGroup() {
    	
    	rscCatLViewer = new ListViewer( sel_rsc_comp, 
    			             SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	FormData fd = new FormData();//100, rscListViewerHeight);
    	fd.height = rscListViewerHeight;
    	fd.top = new FormAttachment( 0, 35 );
    	fd.left = new FormAttachment( 0, 10 );
    	fd.bottom = new FormAttachment( 100, -130 );
    	fd.right = new FormAttachment( 25, -3 );
    	rscCatLViewer.getList().setLayoutData( fd );

    	Label rscCatLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscCatLbl.setText("Category");
    	fd = new FormData();
    	fd.left = new FormAttachment( rscCatLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscCatLViewer.getList(), -5, SWT.TOP );
    	rscCatLbl.setLayoutData( fd );

        
    	// first create the lists and then attach the label to the top of them
        rscTypeLViewer = new ListViewer( sel_rsc_comp, 
        		                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();//150, rscListViewerHeight);
    	fd.top = new FormAttachment( rscCatLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( 25, 3 );//rscCatLViewer.getList(), 10, SWT.RIGHT );
    	fd.bottom = new FormAttachment( rscCatLViewer.getList(), 0, SWT.BOTTOM );
    	fd.right = new FormAttachment( 50, -3 );
    	rscTypeLViewer.getList().setLayoutData( fd );

        rscTypeLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscTypeLbl.setText("Resource Type");
    	fd = new FormData();
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscTypeLViewer.getList(), -5, SWT.TOP );
    	
    	rscTypeLbl.setLayoutData( fd );
    	
    	rscTypeLocLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscTypeLocLbl.setText("Loc");
    	fd = new FormData();
    	fd.top = new FormAttachment( rscTypeLViewer.getList(), 6, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.LEFT );
    	fd.right = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.RIGHT );
    	rscTypeLocLbl.setLayoutData( fd );
    	
        copyRscTypeBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
        copyRscTypeBtn.setText("Copy ...");
        fd = new FormData(100,25);
    	fd.top = new FormAttachment( rscTypeLocLbl, 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), -50, SWT.CENTER );
    	copyRscTypeBtn.setLayoutData( fd );
        
    	editRscTypeBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
        editRscTypeBtn.setText("Edit ...");
        fd = new FormData(100,25);
        fd.top = new FormAttachment( copyRscTypeBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( copyRscTypeBtn, -50, SWT.CENTER );
    	editRscTypeBtn.setLayoutData( fd );

    	removeRscTypeBtn = new Button( sel_rsc_comp, SWT.PUSH );
    	removeRscTypeBtn.setText("Remove");
        fd = new FormData(100,25);
        fd.top = new FormAttachment( editRscTypeBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( editRscTypeBtn, -50, SWT.CENTER );
    	removeRscTypeBtn.setLayoutData( fd );
    	//removeRscTypeBtn.setEnabled( false ); // TODO : not implemented
            	
    	
    	// first create the lists and then attach the label to the top of them
        rscGroupLViewer = new ListViewer( sel_rsc_comp, 
        		                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData(); //150, rscListViewerHeight);
    	fd.top = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( 50, 3 );//rscTypeLViewer.getList(), 10, SWT.RIGHT );
    	fd.bottom = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.BOTTOM );
    	fd.right = new FormAttachment( 75, -3);
    	rscGroupLViewer.getList().setLayoutData( fd );

        rscTypeGroupLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscTypeGroupLbl.setText("Group"); // changed later depending on type
    	fd = new FormData();
    	fd.left = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscGroupLViewer.getList(), -5, SWT.TOP );
    	fd.right = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.RIGHT );
    	rscTypeGroupLbl.setLayoutData( fd );
       	
    	rscGroupLocLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscGroupLocLbl.setText("Loc");
    	fd = new FormData();
    	fd.top = new FormAttachment( rscGroupLViewer.getList(), 6, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.LEFT );
    	fd.right = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.RIGHT );
    	rscGroupLocLbl.setLayoutData( fd );
    	
        copyRscGroupBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
        copyRscGroupBtn.setText("Copy ...");
        fd = new FormData(100,25);
    	fd.top = new FormAttachment( rscGroupLocLbl, 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscGroupLViewer.getList(), -50, SWT.CENTER );
    	copyRscGroupBtn.setLayoutData( fd );
        
    	editRscGroupBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
    	editRscGroupBtn.setText("Edit ...");
        fd = new FormData(100,25);
//    	fd.top = new FormAttachment( rscGroupLViewer.getList(), 7, SWT.BOTTOM );
//    	fd.left = new FormAttachment( rscGroupLViewer.getList(), -50, SWT.CENTER );
        fd.top = new FormAttachment( copyRscGroupBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( copyRscGroupBtn, -50, SWT.CENTER );
    	editRscGroupBtn.setLayoutData( fd );

    	removeRscGroupBtn = new Button( sel_rsc_comp, SWT.PUSH );
    	removeRscGroupBtn.setText(" Remove ");
        fd = new FormData(100,25);
        fd.top = new FormAttachment( editRscGroupBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( editRscGroupBtn, -50, SWT.CENTER );
    	removeRscGroupBtn.setLayoutData( fd );
        

    	
        rscAttrSetLViewer = new ListViewer( sel_rsc_comp, 
                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();//150, rscListViewerHeight);
        fd.top = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.TOP );
        fd.left = new FormAttachment( 75, 3 ); //rscGroupLViewer.getList(), 7, SWT.RIGHT );
        fd.right = new FormAttachment( 100, -10 );
        fd.bottom = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.BOTTOM );
        rscAttrSetLViewer.getList().setLayoutData( fd );

        Label rscAttrsLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscAttrsLbl.setText("Attributes");
        fd = new FormData();
        fd.left = new FormAttachment( rscAttrSetLViewer.getList(), 0, SWT.LEFT );
        fd.bottom = new FormAttachment( rscAttrSetLViewer.getList(), -5, SWT.TOP );
        rscAttrsLbl.setLayoutData( fd );
        
    	attrSetLocLbl = new Label(sel_rsc_comp, SWT.NONE);
    	attrSetLocLbl.setText("Loc");
    	fd = new FormData();
    	fd.top = new FormAttachment( rscAttrSetLViewer.getList(), 6, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscAttrSetLViewer.getList(), 0, SWT.LEFT );
    	fd.right = new FormAttachment( rscAttrSetLViewer.getList(), 0, SWT.RIGHT );
    	attrSetLocLbl.setLayoutData( fd );

        copyRscAttrSetBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
        copyRscAttrSetBtn.setText("Copy ...");
        fd = new FormData(100,25);
    	fd.top = new FormAttachment( attrSetLocLbl, 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscAttrSetLViewer.getList(), -50, SWT.CENTER );
    	copyRscAttrSetBtn.setLayoutData( fd );
        
    	editRscAttrSetBtn = new Button( sel_rsc_comp, SWT.TOGGLE );
    	editRscAttrSetBtn.setText("Edit ...");
        fd = new FormData(100,25);
//    	fd.top = new FormAttachment( rscAttrSetLViewer.getList(), 7, SWT.BOTTOM );
//    	fd.left = new FormAttachment( rscAttrSetLViewer.getList(), -50, SWT.CENTER );
        fd.top = new FormAttachment( copyRscAttrSetBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( copyRscAttrSetBtn, -50, SWT.CENTER );
    	editRscAttrSetBtn.setLayoutData( fd );

    	removeRscAttrSetBtn = new Button( sel_rsc_comp, SWT.PUSH );
    	removeRscAttrSetBtn.setText(" Remove ");
        fd = new FormData(100,25);
        fd.top = new FormAttachment( editRscAttrSetBtn, 7, SWT.BOTTOM );
    	fd.left = new FormAttachment( editRscAttrSetBtn, -50, SWT.CENTER );
    	removeRscAttrSetBtn.setLayoutData( fd );      
    }

    private void setContentProviders() {
    	
    	// input is the rscDefnsMngr and output is a list of categories based
    	// on the forecast flag
   		rscCatLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				return rscDefnsMngr.getResourceCategories( true ); // include disabled defns
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});
   		
   		// The Elements of the ListViewer are ResourceDefinitions.
   		//
    	rscTypeLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				if( seldResourceName.getRscCategory().isEmpty() ) {
					rscTypeLbl.setText("");
					return new ResourceDefinition[]{};
				}
				else if( seldResourceName.getRscCategory().equals("PGEN" ) ) {
					rscTypeLbl.setText("PGEN");
				}
				else {
					rscTypeLbl.setText(seldResourceName.getRscCategory()+" Resources");
				}
				
				try {
					List<ResourceDefinition> rscTypes = 
						rscDefnsMngr.getResourceDefnsForCategory( 
							seldResourceName.getRscCategory(), null, 
							false, true ); // no generated types and all disabled types
					
					return rscTypes.toArray(new ResourceDefinition[0]); 					
				}
				catch ( VizException e ) {
		        	MessageDialog errDlg = new MessageDialog( 
		        			NmapUiUtils.getCaveShell(), 
		        			"Error", null, 
		        			"Error getting Resource Types\n"+ e.getMessage(),
		        			MessageDialog.ERROR, new String[]{"OK"}, 0);
		        	errDlg.open();
		        	return new String[]{};
				}						
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});
    	
    	rscTypeLViewer.setLabelProvider(  new LabelProvider() {
	    	public String getText( Object element ) {
	    		ResourceDefinition rd = (ResourceDefinition)element;
	    		if( rd == null ) {
	    			return "error";
	    		}
	    		LocalizationLevel lLvl = rd.getLocalizationFile().getContext().getLocalizationLevel();
	    		if( lLvl == LocalizationLevel.BASE ) {
	    			return rd.getResourceDefnName();
	    		}
	    		else {
	    			return rd.getResourceDefnName()+" ("+lLvl.name().charAt(0)+")";
	    		}
	    	}
        });
    	
    	rscTypeLViewer.setComparator( new ViewerComparator() {
    		// TODO : implement this if we want to group definitions according to 
    		// some meaningful category....
    	    public int category(Object element) {
    	    	ResourceDefinition rd = (ResourceDefinition)element;
    	    	return ( rd.isForecast() ? 1 : 0 ); 
    	        //return super.category(element);
    	    }

            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
            	//super.compare(viewer, e1, e2);
            	int catComp = category(e1) - category(e2);            	 
            	return ( catComp != 0 ? catComp :  
            				rscDefnsMngr.getDefaultRscDefnComparator().compare(
            						(ResourceDefinition)e1, (ResourceDefinition)e2 ) ); 
            }
    	});
    	
   		// The Elements of the ListViewer are AttrSetGroups.
   		//
    	rscGroupLViewer.setContentProvider(  new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				//String rscType = (String)inputElement;
				String rscType = seldResourceName.getRscType();
				
				if( !rscType.isEmpty() ) {
					
					// if this resource uses attrSetGroups then get get the list of 
					// groups. (PGEN uses groups but we will list the subTypes (products) and not the group)
					if( rscDefnsMngr.doesResourceUseAttrSetGroups( rscType ) ) {

						// for PGEN there is only 1 'default' attrSetGroup 
//						if( seldResourceName.isPgenResource() ) {
//							rscTypeGroupLbl.setText("PGEN Attribute Group");
////							return new String[]{"PGEN"};
//							rscDefnsMngr.getAttrSetGroupsForResource(rscType)
//						}

//						List<String> rscAttrSetsList = rscDefnsMngr.getAttrSetGroupNamesForResource( rscType );

						List<AttrSetGroup> rscAttrSetGroupsList = 
							    rscDefnsMngr.getAttrSetGroupsForResource( rscType );

						if( rscAttrSetGroupsList != null &&
								!rscAttrSetGroupsList.isEmpty() ) {
							if( rscType.length() < 8 ) {
								rscTypeGroupLbl.setText( rscType+" Attribute Groups " );
							}
							else { 
								rscTypeGroupLbl.setText( rscType+" Attr Groups " );
							}

							return rscAttrSetGroupsList.toArray();
						}
					}
					else {			
						rscTypeGroupLbl.setText("N/A");
						return new String[0];
//						try {
//							String[] rscGroups = rscDefnsMngr.getResourceSubTypes( rscType );
//
//							rscTypeGroupLbl.setText( rscType+" Sub-Types");
//
//							if( rscGroups != null && rscGroups.length != 0 ) {
//								return rscGroups;//.toArray();
//							}
//						}
//						catch ( VizException e ) {
//				        	MessageDialog errDlg = new MessageDialog( 
//				        			NmapUiUtils.getCaveShell(), 
//				        			"Error", null, 
//				        			"Error getting sub-types\n"+ e.getMessage(),
//				        			MessageDialog.ERROR, new String[]{"OK"}, 0);
//				        	errDlg.open();
//						}						
					}
				}
				else {
					rscTypeGroupLbl.setText("");
				}				

				return new String[]{};
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});    

    	// enable sorting 
    	rscGroupLViewer.setComparator( new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
            	return super.compare(viewer, e1, e2);
            }
    	});

    	rscGroupLViewer.setLabelProvider( new LabelProvider() { 	   
    		public String getText( Object element ) {
    			AttrSetGroup asg = (AttrSetGroup)element;
    			if( asg == null ) {
    				return "error";
    			}
    			else {
    				LocalizationLevel lLvl = 
    					asg.getLocalizationFile().getContext().getLocalizationLevel();
    	
    				if( lLvl == LocalizationLevel.BASE ) {
    					return asg.getAttrSetGroupName();
    				}
    				else {
    					return asg.getAttrSetGroupName() + " ("+lLvl.name().charAt(0)+")";
    				}
    			}
    		}
    	});

   		// The Elements of the ListViewer are AttributeSets.
   		//
    	rscAttrSetLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				// if an attrSetGroup is selected, return the attrSets in the group
				if( !seldResourceName.getRscType().isEmpty() ) {
					List<AttributeSet> attrSetsList = rscDefnsMngr.getAttrSetsForResource( seldResourceName, false );

					return attrSetsList.toArray( new AttributeSet[0] );
				}
				return new String[]{};
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
    	});
    			
    	rscAttrSetLViewer.setComparator( new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
            	AttributeSet a1 = (AttributeSet) e1;
            	AttributeSet a2 = (AttributeSet) e2;
            	
            	if( a1.getName().equals("default") ||
            		a1.getName().equals("standard")	) {
            		return -1;
            	}
            	else if( a2.getName().equals("default") ||
                		 a2.getName().equals("standard")	) {
                	return 1;
                }
            	else {
            		// super calls getText which can trigger a bunch of inventory queries in some cases
            		return (a1.getName().compareTo( a2.getName() ) ); //super.compare(viewer, e1, e2);
            	}
            }
    	});

        rscAttrSetLViewer.setLabelProvider( new LabelProvider() {
	    	public String getText( Object element ) {
	    		AttributeSet attrSet = (AttributeSet)element;
	    		
	    		if( attrSet == null ) {
	    			return "error";
	    		}
				LocalizationLevel lLvl = attrSet.getLocalizationLevel();
				
				if( lLvl == LocalizationLevel.BASE ) {
					return attrSet.getName();
	    		}
	    		else {
					return attrSet.getName() + " ("+lLvl.name().charAt(0)+")";
	    		}

	    	}
        });
   	}
   	

    // add all of the listeners for widgets on this dialog
    private void addSelectionListeners() {

        copyRscTypeBtn.setData( EditResourceAction.COPY_RESOURCE_TYPE );
    	copyRscTypeBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.COPY_RESOURCE_TYPE, copyRscTypeBtn);
    	
    	editRscTypeBtn.setData( EditResourceAction.EDIT_RESOURCE_TYPE );
    	editRscTypeBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.EDIT_RESOURCE_TYPE, editRscTypeBtn);
    	
    	removeRscTypeBtn.setData( EditResourceAction.REMOVE_RESOURCE_TYPE );
    	removeRscTypeBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.REMOVE_RESOURCE_TYPE, removeRscTypeBtn);

        copyRscGroupBtn.setData( EditResourceAction.COPY_RESOURCE_GROUP );
        copyRscGroupBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.COPY_RESOURCE_GROUP, copyRscGroupBtn);

    	editRscGroupBtn.setData( EditResourceAction.EDIT_RESOURCE_GROUP );
    	editRscGroupBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.EDIT_RESOURCE_GROUP, editRscGroupBtn);
    	
    	removeRscGroupBtn.setData( EditResourceAction.REMOVE_RESOURCE_GROUP );
    	removeRscGroupBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.REMOVE_RESOURCE_GROUP, removeRscGroupBtn);

        copyRscAttrSetBtn.setData( EditResourceAction.COPY_RESOURCE_ATTR_SET );
        copyRscAttrSetBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.COPY_RESOURCE_ATTR_SET, copyRscAttrSetBtn);

    	editRscAttrSetBtn.setData( EditResourceAction.EDIT_RESOURCE_ATTR_SET );
    	editRscAttrSetBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.EDIT_RESOURCE_ATTR_SET, editRscAttrSetBtn);
    	
    	removeRscAttrSetBtn.setData( EditResourceAction.REMOVE_RESOURCE_ATTR_SET );
    	removeRscAttrSetBtn.addSelectionListener( editActionBtnSelectionListener );
    	editButtonMap.put( EditResourceAction.REMOVE_RESOURCE_ATTR_SET, removeRscAttrSetBtn);


    	rscCatLViewer.addSelectionChangedListener( new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();            	
            	String seldCat = (String)seld_elem.getFirstElement();            	
            	
            	// get the previously selected resource for this category

        		seldResourceName = new ResourceName( );
        		seldResourceName.setRscCategory( seldCat );
            	
        		// if a resource was previously selected for this category, select it
        		if( prevCatSeldRscNames.containsKey( seldCat ) ) {
        			seldResourceName = prevCatSeldRscNames.get( seldCat );
        		}

            	updateResourceTypes();
			}
    	});

       	rscTypeLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();               
            	
            	seldResourceName.setRscType( ((ResourceDefinition)seld_elem.getFirstElement()).getResourceDefnName() );
            	seldResourceName.setRscGroup( "" );
            	seldResourceName.setRscAttrSetName( "" );

            	updateResourceGroups();
            }
        } );       	      

       	rscGroupLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();               
            	AttrSetGroup asg =  (AttrSetGroup)seld_elem.getFirstElement();
            	
            	seldResourceName.setRscGroup( 
            			(asg == null ? null  : asg.getAttrSetGroupName() ) );
            	seldResourceName.setRscAttrSetName( "" );

            	updateResourceAttrSets();
            }
        });       	      

       	rscAttrSetLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection(); 
            	
            	seldResourceName.setRscAttrSetName( ((AttributeSet)seld_elem.getFirstElement()).getName() );

            	updateSelectedResource();
            }
        });       	      

       	
//       	// a double click will close the dialog
//       	rscAttrSetLViewer.getList().addListener(SWT.MouseDoubleClick, new Listener() {
//			public void handleEvent(Event event) {
//				selectResource( true );
//			}
//       	});
   	}
   	
    // allow calling with null as a 'refresh' of the currently selected rscName
    // This is used for updating the ListViewers when the localization level changes.
    public void updateResourceSelections( ResourceName newSeldRscName ) {
    	if( newSeldRscName != null ) {
    	seldResourceName = newSeldRscName;
    	}
    	
//    	if( fcstCatSelected ) {
//    		prevFcstCatSeldRscNames.put( seldResourceName.getRscCategory(), seldResourceName );
//    	}
//    	else {
//    		prevObsvdCatSeldRscNames.put( seldResourceName.getRscCategory(), seldResourceName );
//    	}
    	
    	if( seldResourceName != null ) {
    	updateResourceCategories();
    }
    }
    
   	// set the initial values of the widgets. 
   	//     
   	private void initWidgets() {
 
   		prevSeldCat = seldResourceName.getRscCategory();
			
   		seldResourceName = new ResourceName();

		updateResourceCategories( );
	}
	
	// set the cat list based on the fcst flag and then 
	// use seldResourceName to select the category 
	private void updateResourceCategories( ) {
		
		// update the cat list
		rscCatLViewer.setInput( rscDefnsMngr );
		rscCatLViewer.refresh( true );
		
		rscCatLViewer.getList().deselectAll();

		// 
		if( !seldResourceName.getRscCategory().isEmpty() ) {
			for( int itmIndx=0 ; 
					 itmIndx < rscCatLViewer.getList().getItemCount() ; itmIndx++ )  {
				
				if( rscCatLViewer.getList().getItem(itmIndx).equals( 
						seldResourceName.getRscCategory() ) ) {
					rscCatLViewer.getList().select( itmIndx );
					break;
				}
			}
			
			if( rscCatLViewer.getList().getSelectionCount() == 0 ) {
				seldResourceName = new ResourceName();
			}
		}

		// if no cat is selected or it is not found for some reason, select the first 
		if( seldResourceName.getRscCategory().isEmpty() && 
			rscCatLViewer.getList().getItemCount() > 0 ) {

			rscCatLViewer.getList().select(0);
			StructuredSelection seld_elem = (StructuredSelection)rscCatLViewer.getSelection();
			
			seldResourceName = new ResourceName( );
			seldResourceName.setRscCategory( (String)seld_elem.getFirstElement() );
		}

		updateResourceTypes();		
	}

	// refresh the types list based on the type in the seldResourceName 
	// use seldResourceName to select the type 
	private void updateResourceTypes() {
		
		rscTypeLViewer.setInput( rscDefnsMngr );
		rscTypeLViewer.refresh( true );
		
		rscTypeLViewer.getList().deselectAll();

		// 
		if( !seldResourceName.getRscType().isEmpty() ) {
			for( int itmIndx=0 ; 
					 itmIndx < rscTypeLViewer.getList().getItemCount() ; itmIndx++ )  {
				
				ResourceDefinition rd = (ResourceDefinition)rscTypeLViewer.getElementAt(itmIndx);
				
				if( rd.getResourceDefnName().equals( 
						                       seldResourceName.getRscType() ) ) {
					rscTypeLViewer.getList().select( itmIndx );
					break;
				}
			}
			
			if( rscTypeLViewer.getList().getSelectionCount() == 0 ) {
				seldResourceName.setRscType("");
				seldResourceName.setRscGroup("");
				seldResourceName.setRscAttrSetName(""); 
			}
		}

		// if no type is selected or it is not found for some reason, select the first 
		if( seldResourceName.getRscType().isEmpty() &&
					rscTypeLViewer.getList().getItemCount() > 0 ) {

			rscTypeLViewer.getList().select(0);
			StructuredSelection seld_elem = (StructuredSelection)rscTypeLViewer.getSelection();
			
			seldResourceName.setRscType( ((ResourceDefinition)seld_elem.getFirstElement()).getResourceDefnName() );
			seldResourceName.setRscGroup("");
			seldResourceName.setRscAttrSetName(""); 
		}
		
		// enable/disable the Edit/Delete buttons based on whether a type is selected.
		//
		if( seldResourceName.getRscType().isEmpty() ) {
			copyRscTypeBtn.setEnabled( false );
			editRscTypeBtn.setEnabled( false );
			removeRscTypeBtn.setEnabled( false );
		}
		else {
			copyRscTypeBtn.setEnabled( true );
			editRscTypeBtn.setEnabled( true );
			
			removeRscTypeBtn.setEnabled( false );			
			rscTypeLocLbl.setText("");
		}

		updateResourceGroups();
	}

	private void updateResourceGroups() {
		rscGroupLViewer.setInput( rscDefnsMngr );
		rscGroupLViewer.refresh( true );
		
		// if there are no groups 
		if( rscGroupLViewer.getList().getItemCount() == 0 ) {
			if( !seldResourceName.getRscGroup().isEmpty() ) {
				// ????
				seldResourceName.setRscGroup("");
				seldResourceName.setRscAttrSetName("");				
			}
		}
		else { // there are items in the groups list
			  // if a group has been selected then select it in the list, otherwise
			 // select the first in the list and update the seldResourceName
			//
			rscGroupLViewer.getList().deselectAll();

			// 
			if( !seldResourceName.getRscGroup().isEmpty() ) {
				for( int itmIndx=0 ; 
						 itmIndx < rscGroupLViewer.getList().getItemCount() ; itmIndx++ )  {

					AttrSetGroup asg = (AttrSetGroup)rscGroupLViewer.getElementAt( itmIndx ); 
					
					if( asg.getAttrSetGroupName().equals( 
							seldResourceName.getRscGroup() ) ) {
						rscGroupLViewer.getList().select( itmIndx );
						break;
					}
				}

				if( rscGroupLViewer.getList().getSelectionCount() == 0 ) {
					seldResourceName.setRscGroup("");
					seldResourceName.setRscAttrSetName(""); 
				}
			}

			// if no type is selected or it is not found for some reason, select the first 
			if( seldResourceName.getRscGroup().isEmpty() &&
					rscGroupLViewer.getList().getItemCount() > 0 ) {

				rscGroupLViewer.getList().select(0);
				StructuredSelection seld_elem = (StructuredSelection)rscGroupLViewer.getSelection();

				seldResourceName.setRscGroup( 
						((AttrSetGroup)seld_elem.getFirstElement()).getAttrSetGroupName() );
				seldResourceName.setRscAttrSetName(""); 
			}
		}
		
		ResourceDefinition rscDefn = rscDefnsMngr.getResourceDefinition( seldResourceName );
		
		// enable/disable the Edit/Delete buttons based on whether a group is selected and whether
		// 
		// TODO : if there is a USER group superceding a BASE, SITE, or DESK group then
		// change the name of the Remove button to 'Revert'
		
		if( seldResourceName.getRscGroup().isEmpty() ||
			!rscDefn.applyAttrSetGroups() ) {
			
			copyRscGroupBtn.setEnabled( false );
			editRscGroupBtn.setEnabled( false );
			removeRscGroupBtn.setEnabled( false );			
			rscGroupLocLbl.setText("");
		}
		// Also, PGEN groups are the PGEN files which can't be edited here.
		else if( seldResourceName.getRscCategory().equals("PGEN") ) {
			copyRscGroupBtn.setEnabled( false );
			editRscGroupBtn.setEnabled( false );
			removeRscGroupBtn.setEnabled( false );			
			rscGroupLocLbl.setText("");
		}
		else {
			copyRscGroupBtn.setEnabled( true );
			editRscGroupBtn.setEnabled( true );
			removeRscTypeBtn.setEnabled( false );

			AttrSetGroup asg = rscDefnsMngr.getAttrSetGroupForResource( seldResourceName );
			
			if( asg != null ) {
				LocalizationLevel lLvl = asg.getLocalizationFile().getContext().getLocalizationLevel();

				removeRscTypeBtn.setEnabled( (lLvl == LocalizationLevel.USER) );	
			}
		}
		
//		// Can't remove the last group. Must be in the BASE 
//		if( rscGroupLViewer.getList().getItemCount() == 1 ) {
//			removeRscGroupBtn.setEnabled( false );
//		}
		
		rscTypeGroupLbl.setVisible( !seldResourceName.getRscGroup().isEmpty() );

		LocalizationContext locCxt = rscDefn.getLocalizationFile().getContext();
		
		removeRscTypeBtn.setEnabled( (locCxt.getLocalizationLevel() == LocalizationLevel.USER) );	

		String locStr = "Unknown";
		
		if( locCxt.getLocalizationLevel() == LocalizationLevel.BASE ) {
			locStr = "Location=BASE";
		}
		else {
			locStr = locCxt.getLocalizationLevel().name()+"="+locCxt.getContextName(); 
		}
		
		rscTypeLocLbl.setText( locStr );

		updateResourceAttrSets();
	}

	private void updateResourceAttrSets() {
		
		// if there is a group set it is from the BASE or SITE level then don't let the user 
		// delete it.
		if( !seldResourceName.getRscGroup().isEmpty() ) {		
			AttrSetGroup asg = rscDefnsMngr.getAttrSetGroupForResource( seldResourceName );

			// PGEN just has 1 default/hardcoded group that the user can't edit
			if( asg == null || asg.getAttrSetGroupName().equals("PGEN") ) {
				removeRscGroupBtn.setEnabled( false );
				rscGroupLocLbl.setText("");
			}
			else {
				String locStr = "Unknown";
				LocalizationContext locCxt = asg.getLocalizationFile().getContext();
				
				if( locCxt.getLocalizationLevel() == LocalizationLevel.BASE ) {

					locStr = "Location=BASE";
				removeRscGroupBtn.setEnabled( false );
			}
				else {
					locStr = locCxt.getLocalizationLevel().name()+"="+locCxt.getContextName(); 
					removeRscGroupBtn.setEnabled( true );					
				}
				
				rscGroupLocLbl.setText( locStr );
			}
		}
		
		rscAttrSetLViewer.setInput( rscDefnsMngr );
		rscAttrSetLViewer.refresh( true );
		
		rscAttrSetLViewer.getList().deselectAll();

		// 
		if( !seldResourceName.getRscAttrSetName().isEmpty() ) {
			for( int itmIndx=0 ; 
					 itmIndx < rscAttrSetLViewer.getList().getItemCount() ; itmIndx++ )  {
				
				AttributeSet as = (AttributeSet)rscAttrSetLViewer.getElementAt( itmIndx );
				
				if( as.getName().equals( seldResourceName.getRscAttrSetName() ) ) {
					rscAttrSetLViewer.getList().select( itmIndx );
					break;
				}
			}
			
			if( rscAttrSetLViewer.getList().getSelectionCount() == 0 ) {
				seldResourceName.setRscAttrSetName(""); 
			}
		}

		// if no attr set is selected or it is not found for some reason, select the first 
		if( seldResourceName.getRscAttrSetName().isEmpty() &&
				rscAttrSetLViewer.getList().getItemCount() > 0 ) {

			rscAttrSetLViewer.getList().select(0);
			StructuredSelection seld_elem = (StructuredSelection)rscAttrSetLViewer.getSelection();
			
			seldResourceName.setRscAttrSetName( 
					((AttributeSet)seld_elem.getFirstElement()).getName() );
		}		
		
		updateSelectedResource( );
	}
   	
   	// when an attrSetName is selected and a valid resource name is ready for selection 
   	// 
	public void updateSelectedResource( ) {
		// enable/disable the Edit/Delete buttons based on whether an attr set is selected.
		//
		if( seldResourceName.getRscAttrSetName().isEmpty() ) {
			copyRscAttrSetBtn.setEnabled( false );
			editRscAttrSetBtn.setEnabled( false );
			removeRscAttrSetBtn.setEnabled( false );
		}
		else {
			copyRscAttrSetBtn.setEnabled( true );
			editRscAttrSetBtn.setEnabled( true );

			// The user can not edit or delete a BASE, SITE or DESK level file.
			AttributeSet selAttrSet = rscDefnsMngr.getAttrSet( seldResourceName );

			if( selAttrSet == null ) {
				removeRscAttrSetBtn.setEnabled( false );
			}
			else {							

				String locStr = "Unknown";
				LocalizationContext locCxt = selAttrSet.getFile().getContext();
				
				if( locCxt.getLocalizationLevel() == LocalizationLevel.BASE ) {

					locStr = "Location=BASE";
				removeRscAttrSetBtn.setEnabled( false );
			}
				else {
					locStr = locCxt.getLocalizationLevel().name()+"="+locCxt.getContextName();
					removeRscAttrSetBtn.setEnabled( true );
				}
				
				attrSetLocLbl.setText( locStr );
			}
		}
	}


	public void cancelEditAction( ) {
		if( activeAction != EditResourceAction.NULL_ACTION ) {
			editButtonMap.get( activeAction ).setSelection( false );
			activeAction = EditResourceAction.NULL_ACTION;
		}
	}

    // code for the Listeners for the Add Resource button and the double Click on the list
	public void selectResource( boolean done ) {
	}
	
//	public void addResourceSelectionListener( IResourceSelectedListener lstnr ) {
//		rscSelListeners.add( lstnr );
//	}
	
}