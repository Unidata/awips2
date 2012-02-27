package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;


/**
 * Data Selection dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/26/10 	 #226	     Greg Hull	 Broke out from RscBndlDefnDialog
 * 04/05/10      #226        Greg Hull   Add back PGEN selection
 * 06/18/10      #273        Greg Hull   Rework for new ResourceCnfgMngr
 * 09/13/10      #307        Greg Hull   implement cycle times.
 * 09/28/10      #307        Greg Hull   save the fcst/observed mode when re-initing
 * 10/01/10      #298        B. Hebbard  handle MOS resources in updateCycleTimes()
 * 10/20/10                  X. Guo      Rename getCycleTimeStringFromDataTime to getTimeStringFromDataTime
 * 10/20/10		 #277		 M. Li		 get model name for ensemble
 * 11/18/10	      277		 M. Li		 get correct cycle for ensemble
 * 11/29/10				  	mgamazaychikov	Changed updateCycleTime method for GEMPAK data source
 * 02/28/11      #408        Greg Hull   Replace Forecast/Observed with Filter combo
 * 04/18/11                  Greg Hull   caller sets name of the 'select' button
 * 06/07/11       #445       Xilin Guo   Data Manager Performance Improvements
 * 09/20/2011				mgamazaychikov	Made changes associated with removal of DatatypeTable class
 * 12/22/2011     #578       Greg Hull   Ensemble selection
 * 01/06/2012                S. Gurung   Add/display cycle times at 00Z only for nctaf
 * 01/10/2012                S. Gurung   Changed resource parameter name plugin to pluginName in updateCycleTimes()
 *
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class ResourceSelectionControl extends Composite {
	
	private ResourceDefnsMngr rscDefnsMngr;
	
	private Combo  filterCombo = null;
	private ResourceName seldResourceName = null;

	private String seldFilterStr = "";	
	
	private static String prevSeldCat = "";

	// a map to store the previous selections for each category.
	private static HashMap<String,ResourceName> prevCatSeldRscNames;
	
	private Boolean isPgenMode = false;

	// this list must stay in sync with the cycleTimeCombo.
	private ArrayList<DataTime> cycleTimes = new ArrayList<DataTime>();
		
    private Composite sel_rsc_comp = null;

    private Text   seldRscNameTxt = null;
    private Label  cycleTimeLbl = null;
    private Combo  cycleTimeCombo = null;
    
    // For now only one of these will be visible but we may want to allow both later 
    // (and remove the Modify button from the Create RBD tab)
    private Button addResourceBtn = null;
    private Button replaceResourceBtn = null;

    private Boolean replaceBtnVisible;
	private Boolean replaceBtnEnabled;


    private Button addToAllPanesBtn = null;

    private Label rscTypeLbl = null;
    private Label rscTypeGroupLbl = null;

    private ListViewer rscCatLViewer = null;
    private ListViewer rscTypeLViewer = null;
    private ListViewer rscGroupLViewer = null;
    private ListViewer rscAttrSetLViewer = null;
    
	private final static int rscListViewerHeight = 220;
	
    public interface IResourceSelectedListener {
    	public void resourceSelected( ResourceName rscName, boolean replace, boolean addAllPanes, boolean done );
    }
    
    private Set<IResourceSelectedListener> rscSelListeners = new HashSet<IResourceSelectedListener>();

    public ResourceSelectionControl( Composite parent, 
    		Boolean replaceVisible,
    		Boolean replaceEnabled,
    		ResourceName initRscName, 
    		Boolean multiPane )   throws VizException {
        super(parent, SWT.SHADOW_NONE );
        
        rscDefnsMngr = ResourceDefnsMngr.getInstance();
        
        replaceBtnVisible =replaceVisible;
    	replaceBtnEnabled = replaceEnabled;
        
        if( seldResourceName == null ) {
        	seldResourceName = new ResourceName();
        } 
        else {
        	seldResourceName = initRscName;
        }
        
        if( prevCatSeldRscNames == null ) {
            prevCatSeldRscNames = new HashMap<String,ResourceName>();        	        
        }
        
    	sel_rsc_comp = this;
    		
    	GridData gd = new GridData();
    	gd.grabExcessHorizontalSpace = true;
    	gd.grabExcessVerticalSpace = true;
    	gd.horizontalAlignment = SWT.FILL;
    	gd.verticalAlignment = SWT.FILL;
    	sel_rsc_comp.setLayoutData( gd );
    	
    	sel_rsc_comp.setLayout( new FormLayout() );
                    	   		                
        createSelectResourceGroup( multiPane );

        // set up the content providers for the ListViewers
        setContentProviders();
        addSelectionListeners();

        initWidgets();        
    }


    public IStructuredContentProvider createPgenFileContentProvider() {
    	return NmapCommon.createFileContentProvider( new String[]{".xml"} );
    }


    // create all the widgets in the Resource Selection (top) section of the sashForm.  
    // 
    private void createSelectResourceGroup( Boolean multiPane ) {
    	
    	rscCatLViewer = new ListViewer( sel_rsc_comp, 
    			             SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	FormData fd = new FormData(100, rscListViewerHeight);
    	fd.top = new FormAttachment( 0, 75 );
    	fd.left = new FormAttachment( 0, 10 );
    	
    	// This allows a resize to change the size of the lists.
    	fd.bottom = new FormAttachment( 100, -125 );
    	rscCatLViewer.getList().setLayoutData( fd );

    	Label rscCatLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscCatLbl.setText("Category");
    	fd = new FormData();
    	fd.left = new FormAttachment( rscCatLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscCatLViewer.getList(), -3, SWT.TOP );
    	rscCatLbl.setLayoutData( fd );

        
    	// first create the lists and then attach the label to the top of them
        rscTypeLViewer = new ListViewer( sel_rsc_comp, 
        		                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData(150, rscListViewerHeight);
    	fd.top = new FormAttachment( rscCatLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( rscCatLViewer.getList(), 10, SWT.RIGHT );
    	fd.bottom = new FormAttachment( rscCatLViewer.getList(), 0, SWT.BOTTOM );
    	rscTypeLViewer.getList().setLayoutData( fd );

        rscTypeLbl = new Label(sel_rsc_comp, SWT.NONE);
    	rscTypeLbl.setText("Resource Type");
    	fd = new FormData();
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscTypeLViewer.getList(), -3, SWT.TOP );
    	
    	rscTypeLbl.setLayoutData( fd );
    	
    	
    	filterCombo = new Combo( sel_rsc_comp, SWT.DROP_DOWN | SWT.READ_ONLY );
    	fd = new FormData();
    	fd.width = 130;
    	fd.bottom = new FormAttachment( rscTypeLViewer.getList(), -30, SWT.TOP );
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.LEFT );
    	filterCombo.setLayoutData( fd );

    	Label filt_lbl = new Label(sel_rsc_comp, SWT.NONE);
    	filt_lbl.setText("Type Filter:");
    	fd = new FormData();
    	fd.left = new FormAttachment( filterCombo, 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( filterCombo, -3, SWT.TOP );
    	filt_lbl.setLayoutData( fd );
    	
    	// first create the lists and then attach the label to the top of them
        rscGroupLViewer = new ListViewer( sel_rsc_comp, 
        		                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData(150,rscListViewerHeight);
    	fd.top = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( rscTypeLViewer.getList(), 10, SWT.RIGHT );
    	fd.bottom = new FormAttachment( rscTypeLViewer.getList(), 0, SWT.BOTTOM );
    	rscGroupLViewer.getList().setLayoutData( fd );

        rscTypeGroupLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscTypeGroupLbl.setText("Resource Group");
    	fd = new FormData();
    	fd.left = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( rscGroupLViewer.getList(), -3, SWT.TOP );
    	rscTypeGroupLbl.setLayoutData( fd );
       	
        rscAttrSetLViewer = new ListViewer( sel_rsc_comp, 
                SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
        fd = new FormData(220,rscListViewerHeight);
        fd.top = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.TOP );
        fd.left = new FormAttachment( rscGroupLViewer.getList(), 10, SWT.RIGHT );
        fd.right = new FormAttachment( 100, -10 );
        fd.bottom = new FormAttachment( rscGroupLViewer.getList(), 0, SWT.BOTTOM );
        rscAttrSetLViewer.getList().setLayoutData( fd );

        Label rscAttrsLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscAttrsLbl.setText("Resource Attributes");
        fd = new FormData();
        fd.left = new FormAttachment( rscAttrSetLViewer.getList(), 0, SWT.LEFT );
        fd.bottom = new FormAttachment( rscAttrSetLViewer.getList(), -3, SWT.TOP );
        rscAttrsLbl.setLayoutData( fd );

       	seldRscNameTxt = new Text( sel_rsc_comp, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );
//    	fd = new FormData(360,20);
    	fd = new FormData();
    	//   	fd.bottom = new FormAttachment( 100, -50 ); // change to addResourceBtn
    	fd.top = new FormAttachment( rscCatLViewer.getList(), 30, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscCatLViewer.getList(), 0, SWT.LEFT );
    	fd.right = new FormAttachment( 75, 0 );
    	seldRscNameTxt.setLayoutData( fd );

       	Label seld_rsc_name_lbl = new Label( sel_rsc_comp, SWT.None );
       	seld_rsc_name_lbl.setText("Selected Resource Name");
       	fd = new FormData();
    	fd.left = new FormAttachment( seldRscNameTxt, 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( seldRscNameTxt, -3, SWT.TOP );
    	seld_rsc_name_lbl.setLayoutData( fd );

    	addResourceBtn = new Button( sel_rsc_comp, SWT.None );
    	
    	fd = new FormData();
    	
    	if( replaceBtnVisible ) {
        	fd.top  = new FormAttachment( seldRscNameTxt, 20, SWT.BOTTOM );
        	fd.right = new FormAttachment( 50, -20 );
    	}
    	else {
        	fd.top  = new FormAttachment( seldRscNameTxt, 20, SWT.BOTTOM );
        	fd.left = new FormAttachment( 50,  20 );    		
    	}
//    	fd.left = new FormAttachment( seldRscNameTxt, 75, SWT.RIGHT );
//    	fd.bottom  = new FormAttachment( 100, -10 );
      	addResourceBtn.setLayoutData( fd );
    	addResourceBtn.setText( "  Add Resource " ); // Add To RBD
    	
    	replaceResourceBtn = new Button( sel_rsc_comp, SWT.None );
    	fd = new FormData();
    	fd.left = new FormAttachment( 50, 20 );
    	fd.top  = new FormAttachment( addResourceBtn, 0, SWT.TOP );
    	replaceResourceBtn.setLayoutData( fd );
    	replaceResourceBtn.setText( " Replace Resource " ); // ie Modify 

    	// both for now unless we change it to be one or the other
//    	addResourceBtn.setVisible(  !replaceBtnVisible );
    	replaceResourceBtn.setVisible( replaceBtnVisible );
    	
    	addToAllPanesBtn = new Button( sel_rsc_comp, SWT.CHECK );
    	fd = new FormData();
    	fd.left = new FormAttachment( seldRscNameTxt, 40, SWT.RIGHT );
    	fd.top  = new FormAttachment( replaceResourceBtn, 0, SWT.TOP );
    	addToAllPanesBtn.setLayoutData( fd );
    	addToAllPanesBtn.setText( "Add To All Panes" ); 

    	addToAllPanesBtn.setVisible( multiPane );
    	
    	// allow the user to enter any previous datatime
    	cycleTimeCombo = new Combo( sel_rsc_comp, SWT.READ_ONLY );
    	fd = new FormData();
//    	fd.left = new FormAttachment( addResourceBtn, 30, SWT.RIGHT );
    	fd.left = new FormAttachment( 80, 0 );
    	fd.right = new FormAttachment( 100, -20 );
    	//fd.bottom  = new FormAttachment( 100, -10 );
    	fd.top  = new FormAttachment( seldRscNameTxt, 0, SWT.TOP );
    	
    	cycleTimeCombo.setLayoutData( fd );
    	
       	cycleTimeLbl = new Label( sel_rsc_comp, SWT.None );
       	cycleTimeLbl.setText("Cycle Time");
       	fd = new FormData();
    	fd.left = new FormAttachment( cycleTimeCombo, 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( cycleTimeCombo, -3, SWT.TOP );
    	cycleTimeLbl.setLayoutData( fd );
    }

    private void setContentProviders() {
    	
    	// input is the rscDefnsMngr and output is a list of categories based
    	// on the forecast flag
   		rscCatLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				return rscDefnsMngr.getResourceCategories();
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});
   		
   		// order the Categories according to the 
   		
//    	rscCatLViewer.setComparator( new ViewerComparator() {
//            @Override
//            public int compare(Viewer viewer, Object e1, Object e2) {
//            	getAvailResourceCategories 
//            	if( ((String)e1).equals("") ) {
//            		return -1;
//            	}
//            	else {
//            		return super.compare(viewer, e1, e2);
//            	}
//            }
//    	});
   		
   		
    	rscTypeLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				//String rscCat = (String)inputElement;				
				if( !seldResourceName.getRscCategory().isEmpty() ) {
					List<String> rscTypes = rscDefnsMngr.getResourceTypesForCategory( 
							seldResourceName.getRscCategory(), seldFilterStr, true );
					return rscTypes.toArray();
				}
				return new String[]{};
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});

//    	rscTypeLViewer.setLabelProvider( NmapCommon.createFileLabelProvider() );
    	
    	rscGroupLViewer.setContentProvider(  new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				String rscType = seldResourceName.getRscType();

			if( !rscType.isEmpty() ) {
					// if this resource uses attrSetGroups then get get the list of 
					// groups. (PGEN uses groups but we will list the subTypes (products)
					// and not the single PGEN attr set group)
					if( rscDefnsMngr.doesResourceUseAttrSetGroups( rscType ) &&
							!seldResourceName.isPgenResource() ) {
					 
						List<String> rscAttrSetsList = rscDefnsMngr.getAttrSetGroupNamesForResource( rscType );

						if( rscAttrSetsList != null &&
								!rscAttrSetsList.isEmpty() ) {
							return rscAttrSetsList.toArray();
						}
					}
					else {					
						String[] rscGroups = rscDefnsMngr.getResourceSubTypes( rscType );

						if( rscGroups != null && rscGroups.length != 0 ) {
							return rscGroups;//.toArray();
						}
					}
				}
				return new String[]{};
			}

			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }    			
   		});    

//    	rscGroupLViewer.setLabelProvider( NmapCommon.createFileLabelProvider() );
    	
    	rscAttrSetLViewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				
				// if there is a group selected then 
//				if( !seldResourceName.getRscGroup().isEmpty() ) {
//					if( rscDefnsMngr.getAttrSetsForResource( seldResourceName ))
//				}
				
				// if an attrSetGroup is selected, return the attrSets in the group
				if( !seldResourceName.getRscType().isEmpty() ) {
					return rscDefnsMngr.getAttrSetsForResource( seldResourceName,
                                                    true );
				}
//				}
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
            	if( ((String)e1).equals("default") ||
            		((String)e1).equals("standard")	) {
            		return -1;
            	}
            	else if( ((String)e2).equals("default") ||
                		 ((String)e2).equals("standard")	) {
                	return 1;
                }
            	else {
            		return super.compare(viewer, e1, e2);
            	}
            }
    	});

        rscAttrSetLViewer.setLabelProvider( new LabelProvider() {
	    	public String getText( Object element ) {
	    		String attrSetName = (String)element;
	    		if( attrSetName.endsWith(".attr") ) {
	    			return attrSetName.substring(0, attrSetName.length()-5);
	    		}
	    		else {
	    			return attrSetName;
	    		}
	    	}
        });
    
   	}
   	    		
    // add all of the listeners for widgets on this dialog
    private void addSelectionListeners() {

    	rscCatLViewer.addSelectionChangedListener( new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();            	
            	String seldCat = (String)seld_elem.getFirstElement();            	
            	
            	// get the previously selected resource for this category

        		seldResourceName = new ResourceName( ); 
        		seldResourceName.setRscCategory( seldCat );
            	
        		prevSeldCat = seldCat;
        		
        		// if a resource was previously selected for this category, select it
        		// 
        		if( prevCatSeldRscNames.containsKey( seldCat ) ) {
        			seldResourceName = prevCatSeldRscNames.get( seldCat );
        		}
        		
        		updateResourceFilters();
        		
            	updateResourceTypes();
			}
    	});

    	filterCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		String filtStr = null; // init to no filter
        		
        		if( filterCombo.getSelectionIndex() == 0 ) { // "All" 
        			filtStr = "";
        		}
        		else {
        			filtStr = filterCombo.getText();            	
        		}

        		if( filtStr.equals( seldFilterStr ) ) {
        			return;
        		}
    			seldFilterStr = filtStr;

    			updateResourceTypes();
        	}
       	});
    	

       	rscTypeLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();               
            	String seld_rsc_type = (String)seld_elem.getFirstElement();
            	
            	seldResourceName.setRscType( (String)seld_elem.getFirstElement() );
            	seldResourceName.setRscGroup( "" );
            	seldResourceName.setRscAttrSetName( "" );
            	seldResourceName.setCycleTime( null );
           	            	
            	updateCycleTimes();
            	
            	updateResourceGroups();
            }
        } );       	      

       	rscGroupLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection();               
            	seldResourceName.setRscGroup( (String)seld_elem.getFirstElement() );
            	seldResourceName.setRscAttrSetName( "" );

            	updateResourceAttrSets();
            }
        });       	      

       	rscAttrSetLViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
            	StructuredSelection seld_elem = (StructuredSelection) event.getSelection(); 
            	
            	seldResourceName.setRscAttrSetName( (String)seld_elem.getFirstElement() );
            	
            	updateSelectedResource();
            }
        });       	      


       	// get the selected rsc and add to the list.
   		// ignoring the cycle time for now.
   		//
       	addResourceBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		selectResource( false, false );
        	}
       	});
       	
       	// TODO : do we want replace to pop down the dialog? 
       	replaceResourceBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		selectResource( true, false );
        	}
       	});

       	// a double click will add the resource and close the dialog
       	rscAttrSetLViewer.getList().addListener(SWT.MouseDoubleClick, new Listener() {
			public void handleEvent(Event event) {
				if( addResourceBtn.isVisible() ) {
					selectResource( false, true );
				}
				else {
					selectResource( true, true );
				}
			}
       	});

       	cycleTimeCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
//            	seldResourceName.setRscAttrSetName( (String)seld_elem.getFirstElement() );

            	updateSelectedResource();
        	}
       	});
   	}
   	
   	// set the initial values of the widgets. (base this on previously selected values??)
   	// 
   	private void initWidgets() {
 
   		filterCombo.setItems( new String[] {"All"} );
   		filterCombo.select( 0 );
   		 
   		seldFilterStr = "";

		// update the cat list
		rscCatLViewer.setInput( rscDefnsMngr );
		rscCatLViewer.refresh();
		rscCatLViewer.getList().deselectAll();
   		
		// 
		addToAllPanesBtn.setSelection( false );
		
		// if 
		if( prevSeldCat.isEmpty() ) {
   			// if no cat is selected, select the first
			if( rscCatLViewer.getList().getItemCount() > 0 ) { 
				rscCatLViewer.getList().select(0);
				StructuredSelection seld_elem = (StructuredSelection)rscCatLViewer.getSelection();
				seldResourceName.setRscCategory( (String)seld_elem.getFirstElement() );
			}
		}
		else { // if a resource was previously selected for this category, select it
    		  // 
			seldResourceName.setRscCategory( prevSeldCat );    			

			if( prevCatSeldRscNames.containsKey( prevSeldCat ) ) {
    			seldResourceName = prevCatSeldRscNames.get( prevSeldCat );
    		}
		}
  			   			
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


		updateResourceFilters( );
		
		updateResourceTypes();

//   		filterResourceTypes( seldFilterStr );
   	}

   	// 
	public void filterResourceTypes( String filterStr ) {
		
		// if the value hasn't changed then do nothing
//		if(  seldFilterStr.equals( filterStr ) ) {
//			return;
//		}
		// get the previously selected resourceName
			
//		}
//		else { // was obs, now fcst
//			if( seldResourceName.isValid() ) {
//				prevSeldObsCat = seldResourceName.getRscCategory();
//			}
//
//			if( prevSeldFcstCat.isEmpty() ) {
//				seldResourceName = new ResourceName();
//			}
//			else if( prevFcstCatSeldRscNames.containsKey( prevSeldFcstCat )){
//				seldResourceName = prevFcstCatSeldRscNames.get( prevSeldFcstCat );
//			}
//		}

//		cycleTimeLbl.setVisible( fcstCatSelected );
//		cycleTimeCombo.setVisible( fcstCatSelected );
		
	}
	

	// get a list of all the possible filter labels from all of the resources 
	// in this category
	private void updateResourceFilters(  ) {
		String seldCat = seldResourceName.getRscCategory();
		
		// TODO : add code to save the prev seld filter for each cat
		List<String> filtLabels = rscDefnsMngr.getFilterLabelsForResourceCategory( seldCat );
		filtLabels.add(0, "All");
		filterCombo.setItems( filtLabels.toArray( new String[0] ) );
   		filterCombo.select( 0 );
		seldFilterStr = "";
	}

	// refresh the types list based on the type in the seldResourceName 
	// use seldResourceName to select the type 
	private void updateResourceTypes() {
		
		rscTypeLViewer.setInput( rscDefnsMngr );
		rscTypeLViewer.refresh();
		
		rscTypeLViewer.getList().deselectAll();

		// 
		if( !seldResourceName.getRscType().isEmpty() ) {
			for( int itmIndx=0 ; 
					 itmIndx < rscTypeLViewer.getList().getItemCount() ; itmIndx++ )  {
				
				if( rscTypeLViewer.getList().getItem(itmIndx).equals( 
						                       seldResourceName.getRscType() ) ) {
					rscTypeLViewer.getList().select( itmIndx );
					break;
				}
			}
			
			if( rscTypeLViewer.getList().getSelectionCount() == 0 ) {
				seldResourceName.setRscType("");
				seldResourceName.setRscGroup("");
				seldResourceName.setRscAttrSetName(""); 
            	seldResourceName.setCycleTime( null );
			}
		}

		// if no type is selected or it is not found for some reason, select the first 
		if( seldResourceName.getRscType().isEmpty() &&
			rscTypeLViewer.getList().getItemCount() > 0 ) {

			rscTypeLViewer.getList().select(0);
			StructuredSelection seld_elem = (StructuredSelection)rscTypeLViewer.getSelection();
			
			seldResourceName.setRscType( (String)seld_elem.getFirstElement() );
			seldResourceName.setRscGroup("");
			seldResourceName.setRscAttrSetName(""); 
        	seldResourceName.setCycleTime( null );
		}
		
		updateCycleTimes();

		updateResourceGroups();
	}

	private void updateResourceGroups() {
		rscGroupLViewer.setInput( rscDefnsMngr );
		rscGroupLViewer.refresh();
		
		// if there are no groups 
		if( rscGroupLViewer.getList().getItemCount() == 0 ) {
			if( !seldResourceName.getRscGroup().isEmpty() ) {
				// ????
				seldResourceName.setRscGroup("");
				seldResourceName.setRscAttrSetName("");
            	seldResourceName.setCycleTime( null );
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

					if( rscGroupLViewer.getList().getItem(itmIndx).equals( 
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

				seldResourceName.setRscGroup( (String)seld_elem.getFirstElement() );
				seldResourceName.setRscAttrSetName(""); 
			}
		}
		
		updateResourceAttrSets();
	}

	private void updateResourceAttrSets() {
		rscAttrSetLViewer.setInput( rscDefnsMngr );
		rscAttrSetLViewer.refresh();
		
		rscAttrSetLViewer.getList().deselectAll();

		// 
		if( !seldResourceName.getRscAttrSetName().isEmpty() ) {
			for( int itmIndx=0 ; 
					 itmIndx < rscAttrSetLViewer.getList().getItemCount() ; itmIndx++ )  {
				
				if( rscAttrSetLViewer.getList().getItem(itmIndx).equals( 
						                          seldResourceName.getRscAttrSetName() ) ) {
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
			
			seldResourceName.setRscAttrSetName( (String)seld_elem.getFirstElement() );
		}		
		
		updateSelectedResource();
	}
   	
   	// when an attrSetName is selected and a valid resource name is ready for selection 
   	// 
	public void updateSelectedResource( ) {
		// enable/disable the Add Resource Button 
		// and set the name of the Resource 
		if( seldResourceName.isValid() ) {
			
			addResourceBtn.setEnabled( true );	
			//if( replaceEnabled ) {
			replaceResourceBtn.setEnabled( replaceBtnEnabled );	

			cycleTimeLbl.setEnabled( true );
			cycleTimeCombo.setEnabled( true );

			//
			ResourceDefinition rscDefn = rscDefnsMngr.getResourceDefinition( 
					seldResourceName.getRscType() );
			
			if( rscDefn.isForecast() ) {
				
				int seldCycleTimeIndx = cycleTimeCombo.getSelectionIndex();	// Cycle for Ensemble

//				if (rscDefn.getRscImplementation().equals( "EnsembleFcstGridContours" )) {
//					HashMap<String,String> rscParams = rscDefn.getResourceParameters();
//					ModelListInfo modelListInfo = new ModelListInfo(rscParams.get("GDFILE"));
//					String cycleStr = modelListInfo.getModelList().get(0).getCycle();
//					
//					boolean hasCycle = false;
//					if (cycleStr != null)  {
//						NumberFormat nf = NumberFormat.getInstance();
//						nf.setMinimumIntegerDigits(2);
//						nf.setMinimumFractionDigits(0);
//						nf.setMaximumFractionDigits(2);
//						nf.setMaximumIntegerDigits(2);
//
//						for (DataTime dt: cycleTimes) {
//							Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
//							cal.setTime(dt.getRefTime());
//							String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));
//							if (cycleStr.contains(hh)) {
//								seldResourceName.setCycleTime(dt);
//								hasCycle = true;
//
//								break;
//							}
//						}
//					} 
//
//					// TODO : Allow the user to select 'LATEST' specifically
//					if (!hasCycle) {
//						seldResourceName.setCycleTimeLatest();
//					}
//				
//				}	
//				else {
					// TODO : Allow the user to select 'LATEST' specifically
					if( seldCycleTimeIndx == -1 ) {  
						seldResourceName.setCycleTimeLatest();
					}
					else if( seldCycleTimeIndx < cycleTimes.size() ) { 
						seldResourceName.setCycleTime( 
								cycleTimes.get( seldCycleTimeIndx ) );
					}
					else { // shoulndn't happen
						seldResourceName.setCycleTimeLatest();
					}
				}
//			}

			// For now, don't let the user select 'Latest'
			if( seldResourceName.isLatestCycleTime() ) {
				
				addResourceBtn.setEnabled( false );	
				replaceResourceBtn.setEnabled( false );	
				seldRscNameTxt.setText( "" );				
			}
			else {			
				seldRscNameTxt.setText( seldResourceName.toString() );
			}
		}
		else {
			cycleTimeLbl.setEnabled( false );
			cycleTimeCombo.setEnabled( false );

			addResourceBtn.setEnabled( false );	
			replaceResourceBtn.setEnabled( false );	

			seldRscNameTxt.setText( "" );
		}			

		prevCatSeldRscNames.put( seldResourceName.getRscCategory(), seldResourceName );
	}


    // code for the Listeners for the Add Resource button and the double Click on the list
	public void selectResource( boolean replaceRsc, boolean done ) {

		boolean addToAllPanes = ( addToAllPanesBtn.isVisible() && addToAllPanesBtn.getSelection() );
		if( seldResourceName.isValid() ) {
			for( IResourceSelectedListener lstnr : rscSelListeners ) {
				lstnr.resourceSelected( seldResourceName, replaceRsc, addToAllPanes, done );
			}
		}
		else { } // sanity check failed
	}
	
	public ResourceName getCurrentlySelectedResource( ) {
		return seldResourceName;
	}
	
	public void addResourceSelectionListener( IResourceSelectedListener lstnr ) {
		rscSelListeners.add( lstnr );
	}
	
	public boolean isPgenMode() {
		return isPgenMode;
	}

	public void setPgenMode(boolean pgenMode) {
		if( pgenMode != isPgenMode ) {
			isPgenMode = pgenMode;
			
			rscTypeLViewer.setInput(null);
			rscGroupLViewer.setInput(null);
			
		    if( isPgenMode ) {
		    	rscTypeLbl.setText("PGEN Location");
		    	rscTypeGroupLbl.setText("PGEN File");
		    	
		    	rscGroupLViewer.setContentProvider( createPgenFileContentProvider() );    
//		    	rscGroupLViewer.setLabelProvider( NmapCommon.createFileLabelProvider(new String[]{".xml"}) );
		    }
		    else {
		    	rscTypeLbl.setText("Resource Type");
		    	rscTypeGroupLbl.setText("Resource Group");

		    	rscGroupLViewer.setContentProvider( NmapCommon.createSubDirContentProvider() );    
		    }
		}
	}


	// TODO: add a way to let the user specifically choose the "LATEST" cycle time.
	// Currently the user cannot select a forecast resource without selecting an 
	// available cycle time.
    //
	public void updateCycleTimes() {
        Boolean isGempakData = false;
        
		ResourceDefinition rscDefn = rscDefnsMngr.getResourceDefinition( seldResourceName );

		if( rscDefn == null ) {    	
			cycleTimeLbl.setEnabled( false );
			cycleTimeCombo.setEnabled( false );
        	return;
        }
        else {
			cycleTimeLbl.setEnabled( true );
			cycleTimeCombo.setEnabled( true );
			cycleTimeLbl.setVisible( rscDefn.isForecast() );
			cycleTimeCombo.setVisible( rscDefn.isForecast() );
	
        	if( !rscDefn.isForecast() ) {        
        		return;
        	}
        }

    	HashMap<String,RequestConstraint> constraintMap = new HashMap<String,RequestConstraint>();
        // the parameters associated with the Resource Type and not the group or attributes.
    	
		// TODO : Temporary code ..... It would be nice to use the resourceData objects to 
    	// get the query parameters but they haven't been instantiated yet...
    	//
        try {
            HashMap<String,String> rscParams = 
    			rscDefnsMngr.getAllResourceParameters( seldResourceName );

            if( rscDefn.getRscImplementation().equals( "ModelFcstGridContours" ) ||
        		rscDefn.getRscImplementation().equals( "EnsembleFcstGridContours" )) {

            	if( !rscParams.containsKey("pluginName") ) {
            		System.out.println("Error: grid .prm file must specify a pluginName of either: "+
            				"ncgrib or "+ GempakGrid.gempakPluginName );
            		return;
            	}

            	String pluginName = rscParams.get("pluginName");

            	// would like to use the constant in NcGridData but E dependency again.
            	if( pluginName.equals( GempakGrid.gempakPluginName ) ) {
            		isGempakData = true;
            	} else if( pluginName.equals("ncgrib")) {
            		
            		constraintMap.put("pluginName", new RequestConstraint("ncgrib",
            				ConstraintType.EQUALS));
            		
            		if (rscDefn.getRscImplementation().equals( "EnsembleFcstGridContours")) {
            			// ModelListInfo modelListInfo = new ModelListInfo(rscParams.get("GDFILE"));
            			// String modelName = modelListInfo.getModelList().get(0)
            			//                   .getModelName().toUpperCase();
            			
            			// 	get the primary model stored as a Resource parameter 
            			// (the commented out code below will get the modelname stored 
            			// in the attributeset)            			
            			String availModels = rscParams.get("availableModels");
            			if( availModels == null ) {
            				System.out.println("Error: no model names set for ensembleComponentModels");
            				throw new VizException("Error: no model names set for ensembleComponentModels");
            			}
            			
            			// get the 'primary' model which is the first in the list.
            			String[] modelNames = availModels.split(";");
            			String modelName = modelNames[0];
            			
            			// if the primary model has ensemble members. 
            			if( modelName.indexOf(":" ) != -1 ) {
            				modelName = modelName.split(":")[0];
            			}
            			
//                    	// we can't get the if the attributeSet has not been selected yet.
//            			// 
//            			if( seldResourceName.getRscAttrSetName().isEmpty() ) {
//            				cycleTimeCombo.removeAll();
//            				cycleTimes.clear();
//            				return;
//            			}
//            			
//            			String ensCompWeights = rscParams.get("ENS_COMPONENT_WEIGHTS");
//            			
//            			if( ensCompWeights == null ) {
//            				System.out.println("Error: no model names set for ensembleComponentWeights");
//            				throw new VizException("Error: no model names set for ensembleComponentWeights");
//            			}
//            			// TODO : move the code below out of this class....
//            			//EnsembleComponentData.getPrimaryModel( ensCompWeights );            				
//            			String[] modelWeights = ensCompWeights.split(",");
//            			String modelName = modelWeights[0].trim();
//            			int indx1 = ( modelName.indexOf("%") != -1 ?
//            					      modelName.indexOf("%") : modelName.indexOf("{") );
//            			int indx2 = ( modelName.indexOf("|") != -1  ?
//            						  modelName.indexOf("|") : modelName.indexOf("}") );
//            			
//            			modelName = modelName.substring( indx1+1,            					
//            						(indx2 != -1 ? indx2 : modelName.length() ) );
            			
            			constraintMap.put("modelInfo.modelName",
            					new RequestConstraint(modelName, ConstraintType.EQUALS));            				

            		} else {
            			constraintMap.put("modelInfo.modelName",
            					new RequestConstraint(rscParams.get("GDFILE"),
            							ConstraintType.EQUALS));
            			
            			if( rscParams.containsKey("eventName") ) {
            				constraintMap.put("eventName",
                					new RequestConstraint( rscParams.get("eventName"),
                							ConstraintType.LIKE ) );
            			}
            		}
            	}
            } else if (rscDefn.getRscImplementation().equals("IDFT")) {
            	constraintMap.put("pluginName", new RequestConstraint("idft",
            			ConstraintType.EQUALS));
            } else if (rscDefn.getRscImplementation().equals("PlotData")) {
//            	rscParams = rscDefnsMngr
//            	         .getResourceParametersForType(seldResourceName);
            	constraintMap.put("pluginName", new RequestConstraint(rscParams
            			.get("pluginName"), ConstraintType.EQUALS));
//            	if( rscParams.containsKey("reportType") ) { // bufrmos doesn't have a reportType
//            		constraintMap.put( "reportType",
//            				new RequestConstraint(rscParams.get("reportType"),
//            						ConstraintType.IN));
//            	}
            } else {
            	System.out.println("Cant get cycle times for unknown fcst rsc:"
            			+ seldResourceName.getRscType());
            	return;
            }

            if (isGempakData) {
            	/*
            	 * For a GEMPAK dataSource get gridCycleTimes from the dataLocation
            	 */
            	cycleTimeCombo.removeAll();
            	cycleTimes.clear();
            	try {
            		String dataLocation = null;
            		try {
            			dataLocation = GempakGrid.getGempakGridPath(
            					rscParams.get("GDFILE"));
            		} catch (VizException e) {
            			throw new VizException(e);
            		}
            		String[] gridCycleTimes = GempakGrid.getGridCycleTimes(dataLocation);
            		for (String gct : gridCycleTimes) {
            			String gct2DataTimeFormat = "20" + gct.substring(0, 2)
            			+ "-" + gct.substring(2, 4) + "-"
            			+ gct.substring(4, 6) + " " + gct.substring(7, 9)
            			+ ":" + gct.substring(9, 11) + ":00.0 ";
            			cycleTimes.add(0, new DataTime(gct2DataTimeFormat));
            			cycleTimeCombo.add(gct, 0);
            		}
            		if (gridCycleTimes.length > 0) {
            			cycleTimeCombo.select(0);
            		}
            	} catch (VizException e) {
            		System.out.println("Error querying cycle times: "
            				+ e.getMessage().split(":")[1]);
            	}
            } 
            else {
            	LayerProperty property = new LayerProperty();
            	property.setDesiredProduct(ResourceType.PLAN_VIEW);

            	property.setEntryQueryParameters(constraintMap);

            	DataTime[] availableTimes = property.getEntryTimes();

            	// ArrayList<String> cycleTimes = new ArrayList<String>();

            	cycleTimeCombo.removeAll();
            	cycleTimes.clear();

            	for (DataTime dt : availableTimes) {
            		DataTime refTime = new DataTime(dt.getRefTime());

            		if (!cycleTimes.contains(refTime)) {
            			//cycleTimes.add(0, refTime);

            			NumberFormat nf = NumberFormat.getInstance();
            			nf.setMinimumIntegerDigits(2);
            			nf.setMinimumFractionDigits(0);
            			nf.setMaximumFractionDigits(2);
            			nf.setMaximumIntegerDigits(2);

            			Calendar cal = Calendar.getInstance(TimeZone
            					.getTimeZone("GMT"));
            			cal.setTime(dt.getRefTime());
            			int yy = cal.get(Calendar.YEAR) % 100;
            			String yyStr = nf.format(yy);// (yy<2000 ? yy-1900 :
            			// yy-2000 )
            			String mon = nf.format(cal.get(Calendar.MONTH) + 1);
            			String dd = nf.format(cal.get(Calendar.DAY_OF_MONTH));
            			String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));
            			String min = nf.format(cal.get(Calendar.MINUTE));

            			String cycleTimeStr = String.format("%s%s%s/%s%s",
            					yyStr, mon, dd, hh, min);

            			if (rscParams.get("pluginName") != null && rscParams.get("pluginName").equals("nctaf")) {
            				// for nctaf, only add cycle times at 00Z
            				if (cycleTimeStr.endsWith("0000")) {
            					cycleTimes.add(0, refTime);
            					cycleTimeCombo.add(NmapCommon
            							.getTimeStringFromDataTime(dt, "_"), 0);  
            				}
            			}
            			else {
            				cycleTimes.add(0, refTime);
        					cycleTimeCombo.add(NmapCommon
            						.getTimeStringFromDataTime(dt, "_"), 0);            				
            			}
            		}
            	}

            	if (!cycleTimes.isEmpty()) {
            		cycleTimeCombo.select(0);
            	}
            }
        } catch (VizException ve ) {
        	MessageDialog errDlg = new MessageDialog( 
        			NmapUiUtils.getCaveShell(), 
        			"Error", null, 
        			"Error Requesting Cycle Times:"+ve.getMessage(),
        			MessageDialog.ERROR, new String[]{"OK"}, 0);
        	errDlg.open();
        	return;
        }

        return;
	}
	
    public void setMultiPaneEnabled( Boolean multPaneEnable ) {
    	addToAllPanesBtn.setVisible( multPaneEnable );
    }
    
    public void setReplaceEnabled( Boolean rplEnbld ) {
    	replaceBtnEnabled = rplEnbld;

    	if( !isDisposed() ) {
        	updateSelectedResource();    		
    	}
    	//replaceResourceBtn.setEnabled( replaceEnabled );
    }
    
    public ResourceName getPrevSelectedResource() {
    	return seldResourceName;
    }
}