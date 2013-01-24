package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resourceManager.timeline.TimelineControl;
import gov.noaa.nws.ncep.viz.resourceManager.timeline.TimelineControl.IDominantResourceChangedListener;

import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionControl.IResourceSelectedListener;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.EditResourceAttrsAction;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.geotools.coverage.grid.ImageGeometry;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiPlugin;
import gov.noaa.nws.ncep.viz.customprojection.GempakProjectionValuesUtil;

/**
 * Data Selection dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/26/10		  #226		 Greg Hull	 Broke out and refactored from ResourceMngrDialog
 * 04/27/10       #245       Greg Hull   Added Apply Button
 * 06/13/10       #273       Greg Hull   RscBndlTemplate->ResourceSelection, use ResourceName
 * 07/14/10       #273       Greg Hull   remove Select Overlay list (now in ResourceSelection)
 * 07/21/10       #273       Greg Hull   add un-implemented Up/Down/OnOff buttons
 * 08/18/10       #273       Greg Hull   implement Clear RBD button
 * 08/23/10       #303       Greg Hull   changes from workshop
 * 09/01/10       #307       Greg Hull   implement auto update
 * 01/25/11                  Greg Hull   fix autoImport of current Display, autoImport
 *                                       of reprojected SAT area, 
 * 02/11/11       #408       Greg Hull   Move Clear to Clear Pane; Add Load&Close btn                                 
 * 02/22/11       #408       Greg Hull   allow for use by EditRbdDialog
 * 06/07/11       #445       Xilin Guo   Data Manager Performance Improvements
 * 07/11/11                  Greg Hull   Rm code supporting 'Save All to SPF' capability.
 * 08/20/11       #450       Greg Hull   Use new SpfsManager
 * 10/22/11       #467       Greg Hull   Add Modify button
 * 11/03/11       #???       B. Hebbard  Add "Save Source Timestamp As:" Constant / Latest 
 * 02/15/2012     627        Archana      Updated the call to addRbd() to accept 
 *                                      a NCMapEditor object as one of the arguments
 * 04/26/2012     #766       Quan Zhou   Modified rscSelDlg listener for double click w. existing rsc--close the dlg.
 * 04/03/2012     #765       S. Gurung   Modified method importRBD to change the display when a RBD is imported
 * 05/17/2012     #791       Quan Zhou   Added getDefaultRbdRsc() to get name and rsc from original defaultRbd.xml
 * 										 Modified LoadRBD to check if default editor is empty, then replace it.
 * 										 findCloseEmptyEdotor() is ready but not used now.
 * 06/18/2012     #624       Greg Hull   set size correctly when initially importing mult-pane
 * 06/18/2012     #713       Greg Hull   clone the RbdBundl when importing
 * 06/20/2012     #647       Greg Hull   dont call selectDominantResource() after importRbd.
 * 06/20/2012                S. Gurung   Fix for TTR# 539 (Auto-update checkbox gets reset to OFF)
 * 06/21/2012     #646       Greg Hull   import full PredefinedArea instead of just the name.
 * 06/28/2012     #824       Greg Hull   update the importRbdCombo in the ActivateListener.
 * 08/01/2012     #836       Greg Hull   check for paneLayout when using empty editor
 * 08/02/2012     #568       Greg Hull   Clear Rbd -> Reset Rbd. get the Default RBD
 * 11/16/2012     #630       Greg Hull   Allow selection of resource-defined areas.
 * 12/02/2012     #630       Greg Hull   add zoomLevel combo
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class CreateRbdControl extends Composite {
	
	private ResourceSelectionDialog rscSelDlg = null;

	private RscBundleDisplayMngr rbdMngr;
	private Shell shell;
	
    private SashForm sash_form = null;
    private Group rbd_grp = null;
    
    private Text rbd_name_txt = null;
    private Label rbd_name_lbl = null;
    private Button sel_rsc_btn = null;
    
    private Button multi_pane_tog = null;
    private Button auto_update_btn = null;
    private Button geo_sync_panes = null;

    private Group seld_rscs_grp = null;

    private ListViewer seld_rscs_lviewer = null; 	

    private Button replace_rsc_btn = null;
    private Button edit_rsc_btn = null;
    private Button del_rsc_btn = null;
    private Button disable_rsc_btn = null;
    
    private Group geo_area_grp = null;
    private Combo geo_area_combo = null;
    
    private Composite geo_area_info_comp = null; // only one of these visible at a time
    private Composite rsc_area_opts_comp = null; // depending on if a satellite area is selected
    
    private Text  proj_info_txt = null;   // view-only projection and map center info
    private Text  map_center_txt = null;   // view-only projection and map center info
    private Button fit_to_screen_btn = null;    
    private Button size_of_image_btn = null;
    
    private Button custom_area_btn = null;
      
    private Group  pane_layout_grp = null;
	private final int maxPaneRows = 6;
	private final int maxPaneCols = 6;

    private Button pane_sel_btns[][] = new Button[maxPaneRows][maxPaneCols];;
    private Button import_pane_btn = null;
    private Button load_pane_btn = null;
    private Button clr_pane_btn = null;
    
    private Label import_lbl = null;
    private Combo import_rbd_combo = null;
    private Button load_rbd_btn = null;
    private Button load_and_close_btn = null;
    private Button save_rbd_btn = null;    
    private Button clear_rbd_btn = null;
    private Button cancel_edit_btn = null; // when part of the 'Edit Rbd' dialog these will
    private Button ok_edit_btn = null;     // replace the Clear, Save, and Load buttons

    private RbdBundle editedRbd = null;    // set on OK when this is an 'Edit Rbd' dialog

    // used to initialize the Save Dialog
    private String savedSpfGroup = null;
    private String savedSpfName  = null;
    
    private Point initDlgSize = new Point( 750, 860 );
    private int   singlePaneDlgWidth = 750;
    private int   multiPaneDlgWidth = 950;
    
    private TimelineControl timelineControl = null;
    
    private final String ImportFromSPF = "From SPF...";
    
//    private final String[] StandardZoomLevels = {"1", "1.5","2","3","5","7.5","10","15","20","30"};
    
    private static List<String> defaultRscList = new ArrayList<String>();
    private static boolean emptyEditorRemoved = false;
    	        
    private static Map<String, String> gempakProjMap = 
    				GempakProjectionValuesUtil.initializeProjectionNameMap();
    
    // the rbdMngr will be used to set the gui so it should either be initialized/cleared 
    // or set with the initial RBD.
    public CreateRbdControl(Composite parent, RscBundleDisplayMngr mngr )   throws VizException {
        super(parent, SWT.NONE);
        shell = parent.getShell();

        rbdMngr = mngr;
//        rbdMngr.init();
        
        rscSelDlg = new ResourceSelectionDialog( shell );
        
        Composite top_comp = this;        
        top_comp.setLayout( new GridLayout(1,true) );
        
//        top_comp.setSize( 400, 400 );
        
        sash_form = new SashForm( top_comp, SWT.VERTICAL );
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        
        sash_form.setLayoutData( gd );
        sash_form.setSashWidth(10);
                
        rbd_grp = new Group( sash_form, SWT.SHADOW_NONE );
        rbd_grp.setText( "Resource Bundle Display" );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;

        rbd_grp.setLayoutData( gd );

        rbd_grp.setLayout( new FormLayout() );
        
        createRBDGroup();

        Group timeline_grp = new Group( sash_form, SWT.SHADOW_NONE );
        timeline_grp.setText( "Select Timeline" );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        timeline_grp.setLayoutData( gd );

        timeline_grp.setLayout( new GridLayout() );

        timelineControl = new TimelineControl( timeline_grp );
        
        timelineControl.addDominantResourceChangedListener( new IDominantResourceChangedListener() {
			@Override
			public void dominantResourceChanged(
					AbstractNatlCntrsRequestableResourceData newDomRsc ) {
				if( newDomRsc == null ) {
					auto_update_btn.setSelection( rbdMngr.isAutoUpdate() );
					auto_update_btn.setEnabled( false );
				}
				else if( newDomRsc.isAutoUpdateable() ){
					auto_update_btn.setEnabled( true );
					//auto_update_btn.setSelection( rbdMngr.isAutoUpdate() );
					auto_update_btn.setSelection( true );
				}
				else {
					auto_update_btn.setSelection(false);
					auto_update_btn.setEnabled( false );
				}
			}
        });
        
        timelineControl.setTimeMatcher( new NCTimeMatcher( ) );
        
        Composite loadSaveComp = new Composite( top_comp, SWT.NONE );
        gd = new GridData();
        gd.minimumHeight = 40;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;

        loadSaveComp.setLayoutData( gd );

        loadSaveComp.setLayout( new FormLayout() );

    	clear_rbd_btn = new Button( loadSaveComp, SWT.PUSH );
    	clear_rbd_btn.setText(" Reset To Default ");
    	FormData fd = new FormData();
    	fd.width = 130;
    	fd.top = new FormAttachment( 0, 7 );
    	fd.left = new FormAttachment( 17, -65 );
    	clear_rbd_btn.setLayoutData( fd );		

    	save_rbd_btn = new Button( loadSaveComp, SWT.PUSH );
   		save_rbd_btn.setText(" Save RBD ");
    	fd = new FormData();
    	fd.width = 100;
    	fd.top = new FormAttachment( 0, 7 );
    	fd.left = new FormAttachment( 40, -50 );
    	save_rbd_btn.setLayoutData( fd );		

    	load_rbd_btn = new Button( loadSaveComp, SWT.PUSH );
    	load_rbd_btn.setText("Load RBD");
    	fd = new FormData();
    	fd.width = 100;
    	fd.top = new FormAttachment( 0, 7 );
//    	fd.bottom = new FormAttachment( 100, -7 );
    	fd.left = new FormAttachment( 63, -50 );
    	load_rbd_btn.setLayoutData( fd );

    	load_and_close_btn = new Button( loadSaveComp, SWT.PUSH );
    	load_and_close_btn.setText("Load And Close");
    	fd = new FormData();
    	fd.width = 120;
    	fd.top = new FormAttachment( 0, 7 );
//    	fd.bottom = new FormAttachment( 100, -7 );
    	fd.left = new FormAttachment( 83, -50 );
    	load_and_close_btn.setLayoutData( fd );

    	cancel_edit_btn = new Button( loadSaveComp, SWT.PUSH );
    	cancel_edit_btn.setText(" Cancel ");
    	fd = new FormData();
    	fd.width = 80;
    	fd.top = new FormAttachment( 0, 7 );
//    	fd.bottom = new FormAttachment( 100, -7 );
    	fd.right = new FormAttachment( 45, 0 );
    	cancel_edit_btn.setLayoutData( fd );

    	ok_edit_btn = new Button( loadSaveComp, SWT.PUSH );
    	ok_edit_btn.setText("   Ok   ");
    	fd = new FormData();
    	fd.width = 80;
    	fd.top = new FormAttachment( 0, 7 );
//    	fd.bottom = new FormAttachment( 100, -7 );
    	fd.left = new FormAttachment( 55, 0 );
    	ok_edit_btn.setLayoutData( fd );

    	cancel_edit_btn.setVisible( false );  // only visible if configureForEditRbd is called
    	ok_edit_btn.setVisible( false );
    	
        sash_form.setWeights( new int[] { 50, 35 } );
        
        // set up the content providers for the ListViewers
        setContentProviders();
        addSelectionListeners();
        
        initWidgets();
    }

    // create all the widgets in the Resource Bundle Definition (bottom) section of the sashForm.  
    //  
    private void createRBDGroup() {
    	
    	import_rbd_combo = new Combo( rbd_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
    	FormData form_data = new FormData(195,20);
    	form_data.left = new FormAttachment( 0, 20 );
    	form_data.top  = new FormAttachment( 0, 40 );
    	import_rbd_combo.setLayoutData( form_data );
    	import_rbd_combo.setEnabled(true);    	

        import_lbl = new Label( rbd_grp, SWT.None );
        import_lbl.setText("Import");
        form_data = new FormData();
     	form_data.left = new FormAttachment( import_rbd_combo, 0, SWT.LEFT );
    	form_data.bottom = new FormAttachment( import_rbd_combo, -3, SWT.TOP );
    	import_lbl.setLayoutData( form_data );


    	rbd_name_txt = new Text( rbd_grp, SWT.SINGLE | SWT.BORDER );
    	form_data = new FormData(175,20);
    	form_data.left = new FormAttachment( import_rbd_combo, 25, SWT.RIGHT );
    	form_data.top  = new FormAttachment( import_rbd_combo, 0, SWT.TOP );
    	rbd_name_txt.setLayoutData( form_data );

        rbd_name_lbl = new Label( rbd_grp, SWT.None );
        rbd_name_lbl.setText("RBD Name");
        form_data = new FormData();
        form_data.width = 180;
     	form_data.left = new FormAttachment( rbd_name_txt, 0, SWT.LEFT );
    	form_data.bottom = new FormAttachment( rbd_name_txt, -3, SWT.TOP );
    	rbd_name_lbl.setLayoutData( form_data );
    	
    	multi_pane_tog = new Button( rbd_grp, SWT.CHECK );
    	multi_pane_tog.setText("Multi-Pane Display");
    	form_data = new FormData();
    	form_data.top  = new FormAttachment( rbd_name_txt, -30, SWT.TOP );
    	form_data.left = new FormAttachment( rbd_name_txt, 35, SWT.RIGHT );
    	multi_pane_tog.setLayoutData( form_data );

        auto_update_btn = new Button( rbd_grp, SWT.CHECK );
        form_data = new FormData();
        auto_update_btn.setText("Auto Update");
        form_data.top = new FormAttachment( multi_pane_tog, 10, SWT.BOTTOM );
        form_data.left  = new FormAttachment( multi_pane_tog, 0, SWT.LEFT );
        auto_update_btn.setLayoutData( form_data );
        auto_update_btn.setEnabled(false);
        
        geo_sync_panes = new Button( rbd_grp, SWT.CHECK );
        form_data = new FormData();
        geo_sync_panes.setText("Geo-Sync Panes");
        form_data.top = new FormAttachment( auto_update_btn, 10, SWT.BOTTOM );
        form_data.left  = new FormAttachment( auto_update_btn, 0, SWT.LEFT );
        geo_sync_panes.setLayoutData( form_data );
    	
    	createAreaGroup();
    	
    	// create all the widgets used to show and edit the Selected Resources
    	seld_rscs_grp = createSeldRscsGroup( );

    	createPaneLayoutGroup();    	
    }
    
    private void createAreaGroup( ) {
    	geo_area_grp = new Group( rbd_grp, SWT.SHADOW_NONE );
    	geo_area_grp.setText("Area" );
    	geo_area_grp.setLayout( new FormLayout() );
    	FormData form_data = new FormData();
    	form_data.top  = new FormAttachment( rbd_name_txt, 25, SWT.BOTTOM );
    	// form_data.bottom  = new FormAttachment( 100, -60 ); // if offset for room for the Load and Save buttons
    	form_data.bottom  = new FormAttachment( 100, -10 );
    	form_data.left = new FormAttachment( 0, 10 );
    	form_data.right = new FormAttachment( 0, 180 );

    	geo_area_grp.setLayoutData( form_data );

    	geo_area_combo = new Combo( geo_area_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 10 );
    	form_data.top  = new FormAttachment( 0, 15 );
    	form_data.right = new FormAttachment( 100, -10 );
    	geo_area_combo.setLayoutData( form_data );
    	
    	// 2 Composites. 1 for when a predefined area is selected which will show the
    	// projection and map center. And 1 for when a satellite resource is selecte which
    	// will let the user select either FitToScreen or SizeOfImage
    	//
    	geo_area_info_comp = new Composite( geo_area_grp, SWT.NONE );
    	geo_area_info_comp.setLayout( new FormLayout() );
    	rsc_area_opts_comp = new Composite( geo_area_grp, SWT.NONE );
    	rsc_area_opts_comp.setLayout( new GridLayout(1,true) );
    	
    	geo_area_info_comp.setVisible( true );
    	rsc_area_opts_comp.setVisible( false );
    	
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 10 );
    	form_data.top  = new FormAttachment( geo_area_combo, 15, SWT.BOTTOM );
    	form_data.right = new FormAttachment( 100, -10 );

    	// both overlap each other since only one visible at a time
    	geo_area_info_comp.setLayoutData( form_data );
    	rsc_area_opts_comp.setLayoutData( form_data );
    	
    	fit_to_screen_btn = new Button( rsc_area_opts_comp, SWT.RADIO );
    	fit_to_screen_btn.setText( "Fit To Screen");
    	// grid data layout not needed
//    	form_data = new FormData();
//    	form_data.left = new FormAttachment( rsc_area_opts_comp, 0, SWT.LEFT );
//    	form_data.top  = new FormAttachment( rsc_area_opts_comp, 5, SWT.BOTTOM );
//    	form_data.right = new FormAttachment( rsc_area_opts_comp, 0, SWT.RIGHT );
//    	fit_to_screen_btn.setLayoutData( form_data );

    	size_of_image_btn = new Button( rsc_area_opts_comp, SWT.RADIO );
    	size_of_image_btn.setText("Size Of Image");

    	fit_to_screen_btn.setSelection( true ); // radio behaviour
    	size_of_image_btn.setSelection( false );
    	

        Label proj_lbl = new Label( geo_area_info_comp, SWT.None );
        proj_lbl.setText("Projection");
        form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 0 );
    	form_data.top  = new FormAttachment( 0, 0 );
    	form_data.right = new FormAttachment( 100, 0 );
    	proj_lbl.setLayoutData( form_data );

        proj_info_txt = new Text( geo_area_info_comp, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 0 );
    	form_data.top  = new FormAttachment( proj_lbl, 2, SWT.BOTTOM );
    	form_data.right = new FormAttachment( 100, 0 );
    	proj_info_txt.setLayoutData( form_data );
    	proj_info_txt.setText("");
    	proj_info_txt.setBackground(rbd_grp.getBackground() ); // indicate Read-only

        Label map_center_lbl = new Label( geo_area_info_comp, SWT.None );
        map_center_lbl.setText("Map Center");
        form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 0 );
    	form_data.top  = new FormAttachment( proj_info_txt, 15, SWT.BOTTOM );
    	form_data.right = new FormAttachment( 100, 0 );
    	map_center_lbl.setLayoutData( form_data );
        
        map_center_txt = new Text( geo_area_info_comp, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 0 );
    	form_data.top  = new FormAttachment( map_center_lbl, 2, SWT.BOTTOM );
    	form_data.right = new FormAttachment( 100, 0 );
    	map_center_txt.setLayoutData( form_data );
    	map_center_txt.setText(" ");
    	map_center_txt.setBackground(rbd_grp.getBackground() ); // indicate Read-only

    	// TODO : move this to be a Tool from main menu to create and name predefined areas
    	// and move this button to be an option under the predefined areas list
    	//
        custom_area_btn = new Button( geo_area_grp, SWT.PUSH );
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 40 );
    	form_data.right = new FormAttachment( 100, -40 );
    	form_data.bottom  = new FormAttachment( 100, -15 );

    	custom_area_btn.setLayoutData( form_data );
    	custom_area_btn.setText(" Custom ... ");
    	custom_area_btn.setEnabled(false); // not implemented

    }
    // create the Selected Resources List, the Edit, Delete and Clear buttons
    //
   	private Group createSeldRscsGroup( ) {
   		Group seld_rscs_grp = new Group( rbd_grp, SWT.SHADOW_NONE );
   		seld_rscs_grp.setText("Selected Resources" );
   		seld_rscs_grp.setLayout( new FormLayout() );
    	FormData form_data = new FormData();
    	
//    	form_data.left = new FormAttachment( 30, 2 );
		form_data.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
    	form_data.top  = new FormAttachment( geo_area_grp, 0, SWT.TOP );
    	form_data.right = new FormAttachment( 100, -10 );
    	form_data.bottom = new FormAttachment( geo_area_grp, 0, SWT.BOTTOM );
    	seld_rscs_grp.setLayoutData( form_data );

   		// This is multi-select to make Deleting resources easier.
   		seld_rscs_lviewer = new ListViewer( seld_rscs_grp,
   				     SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	form_data = new FormData();
    	form_data.top = new FormAttachment( 0, 5 );
    	form_data.left = new FormAttachment( 0, 5 );
    	form_data.right = new FormAttachment( 100, -95);//-110 ); //80, 0 );
    	form_data.bottom = new FormAttachment( 100, -47 );
    	seld_rscs_lviewer.getList().setLayoutData( form_data );

    	// 
    	edit_rsc_btn = new Button( seld_rscs_grp, SWT.PUSH ); 
   		edit_rsc_btn.setText(" Edit ...");
    	form_data = new FormData();
		form_data.width = 90;
		form_data.bottom = new FormAttachment( 100, -10 );

//		if( enableReplaceBtnFromCreateRbd ) {
//			form_data.left = new FormAttachment( 42, -45 );
//		}
//    	else {
        	form_data.left = new FormAttachment( 40, 20 );
//    	}
    	    	
		edit_rsc_btn.setLayoutData( form_data );
    	edit_rsc_btn.setEnabled(false);


    	sel_rsc_btn = new Button( seld_rscs_grp, SWT.PUSH );
    	sel_rsc_btn.setText(" New ... ");
    	form_data = new FormData();
    	form_data.width = 90;
    	form_data.bottom = new FormAttachment( 100, -10 );
//		if( enableReplaceBtnFromCreateRbd ) {
//			form_data.right = new FormAttachment( edit_rsc_btn, -30, SWT.LEFT );
//		}
//		else {
	    	form_data.right = new FormAttachment( 40, -20 );
//		}
    	sel_rsc_btn.setLayoutData( form_data );
 
    	replace_rsc_btn = new Button( seld_rscs_grp, SWT.PUSH ); 
    	replace_rsc_btn.setText(" Replace ...");
    	form_data = new FormData();
    	form_data.width = 90;
    	form_data.bottom = new FormAttachment( 100, -10 );
    	form_data.left = new FormAttachment( edit_rsc_btn, 30, SWT.RIGHT );
    	replace_rsc_btn.setLayoutData( form_data );
    	replace_rsc_btn.setEnabled(false);

    	
    	replace_rsc_btn.setVisible( false ); //enableReplaceBtnFromCreateRbd );

    	
   		del_rsc_btn = new Button( seld_rscs_grp, SWT.PUSH ); 
   		del_rsc_btn.setText("Remove");
    	form_data = new FormData();    	
    	form_data.width = 75;
    	form_data.top = new FormAttachment( 10, -10 );
    	form_data.right = new FormAttachment( 100, -10 );
    	del_rsc_btn.setLayoutData( form_data );
    	del_rsc_btn.setEnabled(false);

    	disable_rsc_btn = new Button( seld_rscs_grp, SWT.TOGGLE );
    	disable_rsc_btn.setText("Turn Off");
    	form_data = new FormData();
    	form_data.width = 75;
    	form_data.right = new FormAttachment( 100, -10 );
    	form_data.top = new FormAttachment( 30, -10 );
    	disable_rsc_btn.setLayoutData( form_data );

    	
    	Button move_down_btn = new Button( seld_rscs_grp, SWT.ARROW | SWT.DOWN );
     	move_down_btn.setToolTipText("Move Down");
    	form_data = new FormData();
    	form_data.width = 35;
    	form_data.top = new FormAttachment( 50, -10 );
    	form_data.right = new FormAttachment( 100, -10 );
    	move_down_btn.setLayoutData( form_data );
    	move_down_btn.setEnabled(false);

    	Button move_up_btn = new Button( seld_rscs_grp, SWT.ARROW | SWT.UP );
     	move_up_btn.setToolTipText("Move Up");
    	form_data = new FormData();
    	form_data.width = 35;
    	form_data.top = new FormAttachment( move_down_btn, 0, SWT.TOP );
    	form_data.left  = new FormAttachment( disable_rsc_btn, 0, SWT.LEFT);
    	move_up_btn.setLayoutData( form_data );
    	move_up_btn.setEnabled(false);

    	Button edit_span_btn = new Button( seld_rscs_grp, SWT.PUSH );
    	edit_span_btn.setText(" Bin ... ");
    	form_data = new FormData();
    	form_data.width = 75;
    	form_data.top = new FormAttachment( 70, -10 );
    	form_data.right = new FormAttachment( 100, -10 );
    	edit_span_btn.setLayoutData( form_data );
    	edit_span_btn.setEnabled(false);


//    	seld_rscs_grp.pack(true);

    	return seld_rscs_grp;    	
   	}

   	private void createPaneLayoutGroup() {
   		pane_layout_grp = new Group( rbd_grp, SWT.SHADOW_NONE );
   		pane_layout_grp.setText("Pane Layout" );
   		pane_layout_grp.setLayout( new FormLayout() );
    	FormData fd = new FormData();
    	fd.left = new FormAttachment( seld_rscs_grp, 10, SWT.RIGHT );
    	fd.top  = new FormAttachment( 0, 3);
    	fd.right = new FormAttachment( 100, -10 );
    	fd.bottom = new FormAttachment( 100, -15 );
    	pane_layout_grp.setLayoutData( fd );
    
    	Composite num_rows_cols_comp = new Composite( pane_layout_grp, SWT.NONE );
    	GridLayout gl = new GridLayout(maxPaneCols, false);
    //	gl.horizontalSpacing = 4;
    	
    	num_rows_cols_comp.setLayout( gl );

    	fd = new FormData();
    	fd.left = new FormAttachment( 0, 80 );
    	fd.top  = new FormAttachment( 0, 3 );
    	fd.right = new FormAttachment( 100, -10 );
    	num_rows_cols_comp.setLayoutData( fd );

    	Button num_rows_btns[] = new Button[maxPaneRows];
    	Button num_cols_btns[] = new Button[maxPaneCols];
    	
    	for( int r=0 ; r<maxPaneRows ; r++ ) {
    		num_rows_btns[r] = new Button( num_rows_cols_comp, SWT.PUSH );
    		num_rows_btns[r].setText( Integer.toString(r+1) );
    		num_rows_btns[r].setSize(20, 20);
    		num_rows_btns[r].setData( new Integer(r+1));
    		num_rows_btns[r].addSelectionListener( new SelectionAdapter() {
       			public void widgetSelected(SelectionEvent e) {
       				selectPane(
       						rbdMngr.setPaneLayout(
       								new PaneLayout( (Integer)e.widget.getData(), 
       										        rbdMngr.getPaneLayout().getColumns() ) ) );
       				updatePaneLayout();
       			}
    		});
    	}
    	
    	GridData gd = new GridData();
    	gd.widthHint = 50;
    	gd.grabExcessHorizontalSpace = true;

    	for( int c=0 ; c<maxPaneCols ; c++ ) {    		
    		num_cols_btns[c] = new Button( num_rows_cols_comp, SWT.PUSH );
    		num_cols_btns[c].setText( Integer.toString(c+1) );
    		num_cols_btns[c].setData( new Integer(c+1));
    		
    		num_cols_btns[c].addSelectionListener( new SelectionAdapter() {
       			public void widgetSelected(SelectionEvent e) {
       				selectPane(
       						rbdMngr.setPaneLayout(
       								new PaneLayout(rbdMngr.getPaneLayout().getRows(), 
       										(Integer)e.widget.getData() ) ) );
   				updatePaneLayout();
        	}			
        });        
    	}

    	Label num_rows_lbl = new Label( pane_layout_grp, SWT.NONE );
    	num_rows_lbl.setText( "Rows:");
    	fd = new FormData();
    	fd.right = new FormAttachment( num_rows_cols_comp, -5, SWT.LEFT );
    	fd.top  = new FormAttachment( num_rows_cols_comp, 10, SWT.TOP );
    	num_rows_lbl.setLayoutData( fd );
    	
    	Label num_cols_lbl = new Label( pane_layout_grp, SWT.NONE );
    	num_cols_lbl.setText( "Columns:");
    	fd = new FormData();
    	fd.right = new FormAttachment( num_rows_cols_comp, -5, SWT.LEFT );
    	fd.top  = new FormAttachment( num_rows_lbl, 15, SWT.BOTTOM );
    	num_cols_lbl.setLayoutData( fd );

    	Label sel_pane_lbl = new Label( pane_layout_grp, SWT.NONE );
    	sel_pane_lbl.setText("Select Pane" );
    	fd = new FormData();
    	fd.left = new FormAttachment( 0, 5 );
    	fd.top  = new FormAttachment( num_rows_cols_comp, 8, SWT.BOTTOM );
    	sel_pane_lbl.setLayoutData( fd );

    	Label sep = new Label( pane_layout_grp, SWT.SEPARATOR | SWT.HORIZONTAL );
    	fd = new FormData();
    	fd.left = new FormAttachment( sel_pane_lbl, 5, SWT.RIGHT );
    	fd.right = new FormAttachment( 100, 0 );
    	fd.top  = new FormAttachment( num_rows_cols_comp, 11, SWT.BOTTOM );
    	sep.setLayoutData( fd );

    	Composite pane_sel_comp = new Composite( pane_layout_grp, SWT.NONE );
    	pane_sel_comp.setLayout( new GridLayout(maxPaneCols, true));

    	fd = new FormData();
    	fd.left = new FormAttachment( 0, 25 );
    	fd.top  = new FormAttachment( sep, 12 );
    	fd.bottom = new FormAttachment( 100, -45 ); 
    	fd.right = new FormAttachment( 100, -15 );
    	pane_sel_comp.setLayoutData( fd );

    	
    	for( int r=0 ; r<maxPaneRows ; r++ ) { 
    		for( int c=0 ; c<maxPaneCols ; c++ ) {    
    			pane_sel_btns[r][c] = new Button( pane_sel_comp, SWT.TOGGLE );
    			pane_sel_btns[r][c].setText( 
    				Integer.toString(r+1)+ ","+Integer.toString(c+1) );
    			pane_sel_btns[r][c].setData( new PaneID(r,c) );
    			pane_sel_btns[r][c].addSelectionListener( new SelectionAdapter() {
           			public void widgetSelected(SelectionEvent e) {
           				PaneID seldPane = (PaneID)e.widget.getData();
           				selectPane(seldPane);
           			}
    			});
    	    	pane_sel_btns[r][c].setSelection( (r==0 && c==0) );
    		}
    	}
    	
    	import_pane_btn = new Button( pane_layout_grp, SWT.PUSH );
    	fd = new FormData();
    	fd.top = new FormAttachment( pane_sel_comp, 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( 0, 10 );
    	import_pane_btn.setLayoutData( fd );
    	import_pane_btn.setText(" Import... " );
    	import_pane_btn.setEnabled(true);    	
    	
    	load_pane_btn = new Button( pane_layout_grp, SWT.PUSH );
    	fd = new FormData();
    	fd.top = new FormAttachment( import_pane_btn, 0, SWT.TOP );
    	fd.left = new FormAttachment( 50, -38 );
    	load_pane_btn.setLayoutData( fd );
    	load_pane_btn.setText("  Re-Load  " );

   		clr_pane_btn = new Button( pane_layout_grp, SWT.PUSH ); 
   		clr_pane_btn.setText("   Clear   ");
    	fd = new FormData();
//    	fd.width = 75;
    	fd.top = new FormAttachment( import_pane_btn, 0, SWT.TOP );
    	fd.right = new FormAttachment( 100, -10 );
    	clr_pane_btn.setLayoutData( fd );


    	pane_layout_grp.setVisible( false );
   	}
   	
   	
   	private void setContentProviders() {
  
   		seld_rscs_lviewer.setContentProvider(new IStructuredContentProvider() { 
   			public void dispose() {}
   			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { }
   			
   			public Object[] getElements(Object inputElement) {
   				return ((ResourceSelection[])inputElement);
   			}
   		});

   		// get the full path of the attr file and then remove .prm extension
   		// and the prefix path up to the cat directory.
   		seld_rscs_lviewer.setLabelProvider(new LabelProvider() {     	
   			public String getText( Object element ) {
   				ResourceSelection rscSel=(ResourceSelection)element;
   				return rscSel.getRscLabel();
   			}
   		});
   		
   		updateSelectedResourcesView( true );
   		
   		//  enable/disable the Edit/Delete/Clear buttons...
   		seld_rscs_lviewer.addSelectionChangedListener(new ISelectionChangedListener() {
   			public void selectionChanged(SelectionChangedEvent event) {   				
   				updateSelectedResourcesView( false );
   			}
   		});

   		seld_rscs_lviewer.getList().addListener( SWT.MouseDoubleClick, new Listener() {
   			public void handleEvent(Event event) {
   				editResourceData();
   			}
   		});

   	}
   	
    // add all of the listeners for widgets on this dialog
   	void addSelectionListeners() {
    	sel_rsc_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   				ResourceSelection rscSel = (ResourceSelection)sel_elems.getFirstElement();
   				
   				if( !rscSelDlg.isOpen()) {
   					ResourceName initRscName = ( rscSel == null ? 
   							rscSelDlg.getPrevSelectedResource() : rscSel.getResourceName() );
   					
   					rscSelDlg.open( true, // Replace button is visible
   							(rscSel != null ? true : false),
   							initRscName,
   					multi_pane_tog.getSelection(),
							SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS);
   				}
   			}
    	});

    	// may be invisible, if implementing the Replace on the Select Resource Dialog
    	replace_rsc_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				if (! rscSelDlg.isOpen()) {
   	       			StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   	       			ResourceSelection rscSel = (ResourceSelection)sel_elems.getFirstElement();

   					rscSelDlg.open( true, // Replace button is visible
   							(rscSel != null ? true : false),
   							rscSel.getResourceName(),
   							multi_pane_tog.getSelection(),
							//SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS);   
   							SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL );
   				}
   			}
    	});
    	
   		rscSelDlg.addResourceSelectionListener( new IResourceSelectedListener() {
   			@Override
   			public void resourceSelected( ResourceName rscName, boolean replace,
   										  boolean addAllPanes, boolean done ) {
   				
   				try {
   					ResourceSelection rbt = ResourceFactory.createResource( rscName );
   					
   					rbdMngr.getAreaProvider();

   					
   					// if replacing existing resources, get the selected resources
   					// (For now just replace the 1st if more than one selected.)
   					// 
   	   				if( replace ) {
   	   					StructuredSelection sel_elems = 
   	   						(StructuredSelection) seld_rscs_lviewer.getSelection();   
   	   					ResourceSelection rscSel = (ResourceSelection)sel_elems.getFirstElement();
   	   				
   	   					if( !rbdMngr.replaceSelectedResource( rscSel, rbt ) ) {   	   			
//   	   						return;
   	   					}
   	   					
   	   					// remove this from the list of available dominant resources.
   	   					if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
   	   						timelineControl.removeAvailDomResource( 
   	   		   					(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
   	   					}
   	   					
   	   					// if replacing a resource which is set to provide the geographic area
   	    		   		// check to see if the current area has been reset to the default because 
   	    		   		// it was not available   	   					
   	    				String seldArea = rbdMngr.getAreaProvider().getProviderName();
   	    				
   	    				if( !seldArea.equals( geo_area_combo.getText() ) ) {
   	    					selectArea( rbdMngr.getAreaProvider() );
   	    				}
   	   				}
   	   				else {
   	   					if( addAllPanes ) {
   	   	   					if( !rbdMngr.addSelectedResourceToAllPanes( rbt ) ) {
   	   	   						if( done ) {
   	   	   							rscSelDlg.close();
   	   	   						}
   	   	   						return;
   	   	   					}
   	   					}
   	   					else if( !rbdMngr.addSelectedResource( rbt ) ) {
   	   						if( done ) {
   	    						rscSelDlg.close();
   	   						}
   	   						return;
   	   					}
   					}

   	   				// add the new resource to the timeline as a possible dominant resource 
   					if( rbt.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
   						timelineControl.addAvailDomResource( 
   								(AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
   					
   					// if there is not a dominant resource selected then select this one
   						if( timelineControl.getDominantResource() == null ) {
   							timelineControl.setDominantResource(
   									(AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
   							//   						timelineControl.selectDominantResource();
//   							(AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
   						}
   					}
   				}
   				catch (VizException e ) {
   					System.out.println( "Error Adding Resource to List: " + e.getMessage() );
					MessageDialog errDlg = new MessageDialog( 
							shell, "Error", null, 
							"Error Creating Resource:"+rscName.toString()+"\n\n"+e.getMessage(),
							MessageDialog.ERROR, new String[]{"OK"}, 0);
					errDlg.open();
   				}
   				
   		   		updateSelectedResourcesView( true );
   				   				
   				if( done ) {
   					rscSelDlg.close();
   				}
   			}
   		});

   		// TODO : if geoSync is enabled then this will need to update the 
   		// area for all of the panes
    	geo_area_combo.addSelectionListener(new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				
   				try {
   				     rbdMngr.setAreaProviderName( geo_area_combo.getText() );

   				     selectArea( rbdMngr.getAreaProvider() );
   				}
   				catch( VizException vizex ) {
   					MessageDialog errDlg = new MessageDialog( 
   							shell, "Error", null, 
   							vizex.getMessage(),
   							MessageDialog.ERROR, new String[]{"OK"}, 0);
   					errDlg.open();
   				}
   			}
   		});

    	geo_area_combo.addListener( SWT.Activate, new Listener() {
			@Override
			public void handleEvent(Event event) {
				updateAvailAreas();
			}    		
    	}); 

    	size_of_image_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				rbdMngr.setZoomLevel(
   						(size_of_image_btn.getSelection() ? // -1.0 : 1.0 ) );
   								ZoomLevelStrings.SizeOfImage.toString() :
   								ZoomLevelStrings.FitToScreen.toString() ) );

   			}
   		});

    	fit_to_screen_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				rbdMngr.setZoomLevel( 
   						(fit_to_screen_btn.getSelection() ? //1.0 : -1.0 ) );
   								ZoomLevelStrings.FitToScreen.toString() :
   								ZoomLevelStrings.SizeOfImage.toString() ) );
   			}
   		});

    	// TODO: if single pane and there are resources selected
    	// in other panes then should we prompt the user on whether
    	// to clear them? We can ignore them if Loading/Saving a 
    	// single pane, but if they reset multi-pane then should the
    	// resources in other panes still be selected. 			
   		multi_pane_tog.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent ev) {
   				rbdMngr.setMultiPane( multi_pane_tog.getSelection() );
   				
   				updateGUIforMultipane( multi_pane_tog.getSelection() );
   				
   				if( multi_pane_tog.getSelection() ) {
   					updatePaneLayout();
   				}
   				else {
   					selectPane( new PaneID(0,0) );
   				}
   				
   				if( rscSelDlg != null && rscSelDlg.isOpen() ) {
   					rscSelDlg.setMultiPaneEnabled(  multi_pane_tog.getSelection() );
   				}
   			}
   		});
   		
   		// if syncing the panes 
   		geo_sync_panes.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent ev) {
   				
   				if( geo_sync_panes.getSelection() ) {
   					
   					MessageDialog confirmDlg = new MessageDialog( shell, 
   						"Confirm Geo-Sync Panes", null, 
   							"This will set the Area for all panes to the currently\n"+
   						"selected Area: "+ rbdMngr.getAreaProvider().getProviderName() + "\n\n"+
   							"Continue?",
   							MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
   					confirmDlg.open();

   					if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
   						geo_sync_panes.setSelection( false );
   						return;
   					}           		
   					
   	   				rbdMngr.syncPanesToArea( );
   				}
   				else {
   					rbdMngr.setGeoSyncPanes( false );
   				}
   			}
   		});

   		custom_area_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent ev) {
//   		        Shell shell = NmapUiUtils.getCaveShell();
//   		        CreateCustomProjectionDialog dlg = CreateCustomProjectionDialog(shell); 
//   		        dlg.open();
   		        
   			}
   		});
   		 	
       	// only 1 should be selected or this button should be greyed out
       	edit_rsc_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			editResourceData();
       		}
       	});
       	
       	del_rsc_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
    			StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
    			Iterator itr = sel_elems.iterator();
    			
    			while( itr.hasNext() ) {
    				
    				removeSelectedResource( (ResourceSelection) itr.next() );
    			}
    			
   		   		updateSelectedResourcesView( true );
   		   		
   		   		// check to see if the current area has been reset to the default because 
   		   		// it was the 
   				String seldArea = rbdMngr.getAreaProvider().getProviderName();

   				if( !seldArea.equals( geo_area_combo.getText() ) ) {   					
   					selectArea( rbdMngr.getAreaProvider() );
   				}
       		}
       	});

       	disable_rsc_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               

//				disable_rsc_btn.setText( !disable_rsc_btn.getSelection() ? 
//						"Turn On" : "Turn Off" );
//       			
       			ResourceSelection rscSel = (ResourceSelection)sel_elems.getFirstElement();
       			
       			if( rscSel != null ) {
       				rscSel.setIsVisible( !disable_rsc_btn.getSelection() );
       			}
       			
       			seld_rscs_lviewer.refresh( true );
       			
   		   		updateSelectedResourcesView( false ); // true
       		}
       	});

       	clr_pane_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			clearSeldResources();
       		}
       	});       	


       	clear_rbd_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			clearRBD();
       		}
       	});

       	
       	load_rbd_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			loadRBD( false );
       		}
       	});

       	load_and_close_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			loadRBD( true );
       		}
       	});

       	load_pane_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			loadPane();
       		}
       	});

       	save_rbd_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveRBD( false );
       		}
       	});
       	
       	import_rbd_combo.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			importRBD( import_rbd_combo.getText() );            	
       		}
       	});
       	    
       	// TODO : Currently we can't detect if the user clicks on the import combo and then clicks on the currently selected
       	// item which means the users can't re-import the current selection (or import another from an spf.) The following may
       	// allow a hackish work around later....  (I tried virtually every listener and these where the only ones to trigger.)
       	// ....update...with new Eclipse this seems to be working; ie. triggering a selection when
       	// combo is clicked on but selection isn't changed.
    	import_rbd_combo.addFocusListener( new FocusListener() {
			@Override
			public void focusGained(FocusEvent e) {
//				System.out.println("focusGained: ");			
			}
			@Override
			public void focusLost(FocusEvent e) {
//				System.out.println("focusLost: " );			
			}
    	});
    	import_rbd_combo.addListener( SWT.MouseDown, new Listener() { // and SWT.MouseUp
			@Override
			public void handleEvent(Event event) {
//				System.out.println("SWT.MouseDown: " );							
			}    		
    	}); 
    	import_rbd_combo.addListener( SWT.Activate, new Listener() {
			@Override
			public void handleEvent(Event event) {
				updateImportCombo();
			}    		
    	}); 
    	import_rbd_combo.addListener( SWT.Deactivate, new Listener() {
			@Override
			public void handleEvent(Event event) {
//				System.out.println("SWT.Deactivate: " );							
			}    		
    	});     	
    	
       	import_pane_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			SelectRbdsDialog impDlg = 
       				 new SelectRbdsDialog( shell, "Import Pane", 
       						 true, false, true );
    			
        		if( !impDlg.open() ) {
        			return;
        		}

        		RbdBundle impRbd = impDlg.getSelectedRBD();

       			if( impRbd != null ) {    			
           			// if any selections have been made then popup a confirmation msg
       				// TODO:add support for determining if only this pane has been modified.
//       				boolean confirm = ( rbdMngr.getSelectedRscs().length > 1 );
//       				if( confirm ) {
//       					MessageDialog confirmDlg = new MessageDialog( 
//       							NmapUiUtils.getCaveShell(), "Confirm", null, 
//       							"Do you want to remove all currently selected Resources?",
//       							MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
//       					confirmDlg.open();
//
//       					if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
//       						return;
//       					}           				
//       				}
       				
       				impRbd.resolveLatestCycleTimes();
       				
       				try {
           			importPane( impRbd, impRbd.getSelectedPaneId() );
       	    	}
       				catch (VizException e) {
       					MessageDialog errDlg = new MessageDialog( 
       							shell, "Error", null, 
       							"Error Importing Rbd, "+impRbd.getRbdName()+".\n"+e.getMessage(),
       							MessageDialog.ERROR, new String[]{"OK"}, 0);
       					errDlg.open();
       				}
       	    	}
       		}
       	});

   	}
   	
   	// import the current editor or initialize the widgets.
   	// 
   	public void initWidgets() {
   		
    	rbd_name_txt.setText("");

    	updateAvailAreas();

//    	for( String szl : StandardZoomLevels ) {
//        	rsc_area_opts_combo.add( szl );    		
//    	}

    	selectArea( rbdMngr.getAreaProvider() );// should be the default area
    	
    	shell.setSize( initDlgSize );

    	updateGUIforMultipane( rbdMngr.isMultiPane() );

    	timelineControl.clearTimeline();

    	defaultRscList = getDefaultRbdRsc();
   	}
   	
   	// if this is called from the Edit Rbd Dialog (from the LoadRbd tab), then 
   	// remove widgets that don't apply. (ie, import, Save, and Load)
   	//
   	public void configureForEditRbd() {
// allow the user to change the name
//   		rbd_name_txt.setEditable( false );
//   		rbd_name_txt.setBackground( rbd_name_txt.getParent().getBackground() );
   		import_lbl.setVisible( false );
   		import_rbd_combo.setVisible( false );
   		clear_rbd_btn.setVisible( false );
   		save_rbd_btn.setVisible( false );
   		load_pane_btn.setVisible( false );
   		load_rbd_btn.setVisible( false );
   		load_and_close_btn.setVisible( false );
   		
   		cancel_edit_btn.setVisible( true );
   		ok_edit_btn.setVisible( true );
   		
   		cancel_edit_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {       			
       			editedRbd = null;
       			shell.dispose();
       		}
       	});

   		// dispose and leave the edited RBD in 
   		ok_edit_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {       
       			createEditedRbd();
       			shell.dispose();
       		}
       	});
   		
    	FormData fd = (FormData) rbd_name_txt.getLayoutData();
    	fd.left = new FormAttachment( 20, 0 );
    	rbd_name_txt.setLayoutData( fd );

   		timelineControl.getParent().setVisible( false );
        sash_form.setWeights( new int[] { 10, 1 } );
        shell.setSize( shell.getSize().x-100,  350 );
        shell.pack(true);
   	}
   	
   	private void updateImportCombo() {
    	// check for possible new Displays that may be imported.
    	String seldImport = import_rbd_combo.getText();
    	
   		import_rbd_combo.removeAll();

   		for( String displayName : NmapUiUtils.getNatlCntrsDisplayNames(true) ) {
   			import_rbd_combo.add( displayName );
   			
   			// if this was selected before, select it again
   	   		if( seldImport == null || seldImport.isEmpty() ||
   	   			seldImport.equals( displayName ) ) {
   	   			import_rbd_combo.select( import_rbd_combo.getItemCount()-1 );
   	   		}
   		}

   		// if the previous selection wasn't found then select 'from SPF'
   		import_rbd_combo.add( ImportFromSPF );
   		
   		if( import_rbd_combo.getSelectionIndex() == -1 ) {
   	   		import_rbd_combo.select( import_rbd_combo.getItemCount()-1 );   			
   		}
   	}
   	
   	// set the area and update the proj/center field
   	// the
   	public void selectArea( PredefinedArea area ) {   		
		
   		geo_area_combo.deselectAll();
		geo_area_info_comp.setVisible( false );
		rsc_area_opts_comp.setVisible( false );

   		// find the area in the available list of areas in the combo menu
   		//
		for( int i=0 ; i<geo_area_combo.getItemCount() ; i++ ) {
		
			String areaStr = geo_area_combo.getItem(i);
			
			if( areaStr.equals( rbdMngr.getAreaProvider().getProviderName() ) ) {
			
				geo_area_combo.select(i);
		
				// ? anything different if imported from a display? 
				if( area.getAreaSource() == AreaSource.PREDEFINED_AREA || 
					area.getAreaSource() == AreaSource.DISPLAY_AREA ) {

					geo_area_info_comp.setVisible( true );
					
					String projStr = rbdMngr.getAreaProvider().getGridGeometry().
										getCoordinateReferenceSystem().getName().toString();
					
					proj_info_txt.setText( projStr );
					proj_info_txt.setToolTipText( projStr );
					
					// use the GEMPAK name if possible.
					for( String gemProj : gempakProjMap.keySet() ) {
						
						if( gempakProjMap.get( gemProj ).equals( projStr ) ) {
							proj_info_txt.setText( gemProj.toUpperCase() );
						}
					}

					Integer lat = (int)(area.getMapCenter()[1] *1000.0);
					Integer lon = (int)(area.getMapCenter()[0] *1000.0);

					map_center_txt.setText(   
							Double.toString((double)lat/1000.0) +"/" + Double.toString((double)lon/1000.0) );
				}
				else if( area.getAreaSource() == AreaSource.RESOURCE_DEFINED ) {

					rsc_area_opts_comp.setVisible( true );

					if( area.getZoomLevel().equals( ZoomLevelStrings.FitToScreen.toString() ) ) {
						fit_to_screen_btn.setSelection( true );
						size_of_image_btn.setSelection( false );						
					}
					else if( area.getZoomLevel().equals( ZoomLevelStrings.SizeOfImage.toString() ) ) {
						fit_to_screen_btn.setSelection( false );
						size_of_image_btn.setSelection( true );
					}
					else {
						/// ????
						area.setZoomLevel( "1.0" );
						fit_to_screen_btn.setSelection( true );
						size_of_image_btn.setSelection( false );
					}
				}

				break;
			}
		}

		if( geo_area_combo.getSelectionIndex() == -1 ) {
//   			System.out.println("rbd Area not found in avail list: adding to the list");
//   			
			try {
				rbdMngr.setAreaProviderName( NmapCommon.getDefaultMap() );
				geo_area_combo.add( NmapCommon.getDefaultMap() );
				selectArea( rbdMngr.getAreaProvider() );
			} catch (VizException e) {
			}
   		}   	   		
   	}
   	
   	// get all the currently available selections for the display area
   	// and add them to the combo.
   	public void updateAvailAreas() {
		String seldArea = geo_area_combo.getText();
		geo_area_combo.removeAll();
		
		for( IGridGeometryProvider availArea : rbdMngr.getAvailAreaProviders() ) {
			
			geo_area_combo.add( availArea.getProviderName() );				

			if( availArea.getProviderName().equals( seldArea ) ) {
				geo_area_combo.select( geo_area_combo.getItemCount()-1 );
			}
		}
   	}
   	
    // called when the user switches to this tab in the ResourceManagerDialog or when 
   	// the EditRbd Dialog initializes
    //
    public void updateDialog() {
    	
    	updateImportCombo();    	
   		
   		// If the gui has not been set with the current rbdMngr then do it now.
    		updateGUI();

        	// updateGUI triggers the spinner which ends up calling rbdMngr.setPaneLayout(),
        	// so we need to reset this here.
        	rbdMngr.setRbdModified( false ); 
    	}
    
   	// set widgets based on rbdMngr   	
   	public void updateGUI( ) { 
   		rbd_name_txt.setText( rbdMngr.getRbdName() );
   		
   		rbd_name_txt.setSelection(0, rbdMngr.getRbdName().length() );
   		rbd_name_txt.setFocus();

   		import_rbd_combo.deselectAll();
   		
   		for( int i=0 ; i<import_rbd_combo.getItemCount() ; i++ ) {
   			String importRbdName = import_rbd_combo.getItems()[i];
   			importRbdName = NmapUiUtils.getNcDisplayNameWithoutID(importRbdName);
   			if( importRbdName.equals( rbdMngr.getRbdName() ) ) {
   				import_rbd_combo.select( i );   		
   			}
   		}

   		if( import_rbd_combo.getSelectionIndex() == -1 ) {
   			import_rbd_combo.select( import_rbd_combo.getItemCount()-1 );
   		}
   		
   		// 
   		updateAvailAreas();
   		
   		selectArea( rbdMngr.getAreaProvider() );
    	
    	geo_sync_panes.setSelection( rbdMngr.isGeoSyncPanes() );
    	multi_pane_tog.setSelection( rbdMngr.isMultiPane() );
    	
    	updateGUIforMultipane( rbdMngr.isMultiPane() );    	
    	
    	int numRows = rbdMngr.getPaneLayout().getRows();
    	int numCols = rbdMngr.getPaneLayout().getColumns();
    	
    	// set the pane sel buttons based on the combos
    	// also sets the rbdMngr's layout
    	if( rbdMngr.isMultiPane() ) {
    		updatePaneLayout();
    	}
    	
    	selectPane( rbdMngr.getSelectedPaneId() );
    	
    	timelineControl.clearTimeline();
    	
    	// set the list of available resources for the timeline
    	for( int r=0 ; r<numRows ; r++ ) {
    		for( int c=0 ; c<numCols ; c++ ) {
    			for( ResourceSelection rscSel : rbdMngr.getRscsForPane( new PaneID(r,c) ) ) {
    				if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {     		    		
    					timelineControl.addAvailDomResource( 
    						(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
    				}
    			}
    		}
    	}

    	NCTimeMatcher timeMatcher = rbdMngr.getInitialTimeMatcher();

    	timelineControl.setTimeMatcher( timeMatcher );
    	
    	if( timeMatcher.getDominantResource() != null &&
    		timeMatcher.getDominantResource().isAutoUpdateable() ){
			auto_update_btn.setEnabled( true );
			auto_update_btn.setSelection( rbdMngr.isAutoUpdate() );
		}
		else {
			auto_update_btn.setSelection(false);
			auto_update_btn.setEnabled( false );
		}
   	}

   	// TODO : we could have the pane buttons indicate whether 
   	// there are resources selected for them by changing the foreground color to
   	// yellow or green...
   	//
   	private void updatePaneLayout( ) {

   		int colCnt = rbdMngr.getPaneLayout().getColumns();
	   	int rowCnt = rbdMngr.getPaneLayout().getRows();
   		
   	   	for( int r=0 ; r<maxPaneRows ; r++ ) {
   	   		for( int c=0 ; c<maxPaneCols ; c++ ) {
   	   		   pane_sel_btns[r][c].setVisible( r<rowCnt && c<colCnt );   	   		   
   	   		}   			
   		}  		
   	}
   	
   	// update the GUI with the selections (area/list of rscs) for the selected pane
   	private void selectPane( PaneID seldPane ) {
   		
   		rbdMngr.setSelectedPaneId( seldPane );
   		
   		if( rbdMngr.isMultiPane() ) {
   			seld_rscs_grp.setText("Selected Resources for Pane "+
    				Integer.toString(seldPane.getRow()+1)+ ","+
    				Integer.toString(seldPane.getColumn()+1) );
   		}
   		else {
   			seld_rscs_grp.setText("Selected Resources" );
   		}

   		// implement radio behaviour
    	for( int r=0 ; r<maxPaneRows ; r++ ) {    
    		for( int c=0 ; c<maxPaneCols ; c++ ) { 
    			pane_sel_btns[r][c].setSelection( 
    					(r==seldPane.getRow() && c==seldPane.getColumn()) );
    		}
    	}
    	
    	// set the selected area
//    	geo_area_combo.setText( rbdMngr.getAreaProvider().getProviderName() );
    	selectArea( rbdMngr.getAreaProvider() );
    	
    	updateSelectedResourcesView( true );
   	}

   	public void removeSelectedResource( ResourceSelection rscSel ) {
   		rbdMngr.removeSelectedResource( rscSel );

   		// remove this from the list of available dominant resources.
   		if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
   			timelineControl.removeAvailDomResource( 
   					(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
   		}
   	}
   	
	// Listener callback for the Edit Resource button and dbl click on the list
	public void editResourceData() {
		StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();         
		ResourceSelection rscSel = (ResourceSelection)sel_elems.getFirstElement();

		if( rscSel == null ) {
			System.out.println("sanity check: no resource is selected");
			return;
		}
		INatlCntrsResourceData rscData = rscSel.getResourceData();

		if( rscData  == null ) {
			System.out.println("sanity check: seld resource is not a INatlCntrsResource");
			return;
		}
		EditResourceAttrsAction editAction = new EditResourceAttrsAction();
		if( editAction.PopupEditAttrsDialog( shell, (INatlCntrsResourceData)rscData, false ) ) {
			rbdMngr.setRbdModified( true );
		}

		seld_rscs_lviewer.refresh( true ); // display modified (ie edited*) name
	}
	
	public void	clearSeldResources() {
		// remove the requestable resources from the timeline
		//
		for( ResourceSelection rbt : rbdMngr.getSelectedRscs() ) {
			if( rbt.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {
				timelineControl.removeAvailDomResource( 
						(AbstractNatlCntrsRequestableResourceData) rbt.getResourceData() );
			}
		}
		
		rbdMngr.removeAllSelectedResources( );
		
		// TODO : should this reset the rbd to not modified if single pane? No since
		// the num panes and/or the area could have changed. clearRbd will reset the 
		// modified flag
		
	   	updateSelectedResourcesView( true );
	}
	
	// reset all panes. This includes 'hidden' panes which may have resources 
	// selected but are hidden because the user has changed to a smaller layout.
	//
	public void clearRBD() {
		// TODO : ? reset the predefined area to the default???
		
		boolean    saveMultiPane = rbdMngr.isMultiPane();
		PaneLayout savePaneLayout = rbdMngr.getPaneLayout();
		PaneID     saveSeldPane   = rbdMngr.getSelectedPaneId();
		
		if( rbdMngr.isMultiPane() ) {
			MessageDialog confirmDlg = new MessageDialog( 
					shell, "Confirm", null, 
					"The will remove all resources selections\n"+
					"from all panes in this RBD. \n\n"+
					"Are you sure you want to do this?",
					MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
			confirmDlg.open();

			if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
				return;
			}
		}
		
		try {
			RbdBundle dfltRbd = RbdBundle.getDefaultRBD();
			
			rbdMngr.initFromRbdBundle( dfltRbd );
    	
		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Error", null, 
					e.getMessage(), MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();

			rbdMngr.init();	    	
		}
    	
		updateGUI();
	}
	
   	// reset the ListViewer's input and update all of the buttons
   	// TODO; get the currently selected resources and reselect them.
   	private void updateSelectedResourcesView( boolean updateList ) {	
   	
   		if( updateList ) {
   			
   			StructuredSelection orig_sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   			List<ResourceSelection> origSeldRscsList = 
   				               (List<ResourceSelection>) orig_sel_elems.toList();
   	
   			seld_rscs_lviewer.setInput( rbdMngr.getSelectedRscs() );
   			seld_rscs_lviewer.refresh( true );

   			List<ResourceSelection> newSeldRscsList = new ArrayList<ResourceSelection>(); 

   			// create a new list of selected elements 
   			for( ResourceSelection rscSel : origSeldRscsList ) {
   				for( int r=0 ; r<seld_rscs_lviewer.getList().getItemCount() ; r++ ) {
   					if( rscSel == seld_rscs_lviewer.getElementAt( r ) ) {
   						newSeldRscsList.add( rscSel );
   						break;
   					}
   				}
   			}
   			seld_rscs_lviewer.setSelection( new StructuredSelection( newSeldRscsList.toArray() ), true );
   		}
   		
   		int numSeldRscs = seld_rscs_lviewer.getList().getSelectionCount();
   		int numRscs = seld_rscs_lviewer.getList().getItemCount();

   		
   		// the Clear button is enabled if there are more than 1 resources in the list.
   		clr_pane_btn.setEnabled( numRscs > 1 );

		// the edit button is enabled iff there is only one selected resource.
		edit_rsc_btn.setEnabled( numSeldRscs == 1 );
		
		// the delete button is enabled if there is at least one resource selected and it
		// is not the default overlay.
   		StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   		List<ResourceSelection> seldRscsList = 
   			                           (List<ResourceSelection>) sel_elems.toList();
   		
   		if( numSeldRscs != 1 ) {
   			del_rsc_btn.setEnabled( false );

   			// TODO : we could allow for the user to replace multiple resources. Or to select
   			// the resource to replace after the dialog is up...
   			//
   			replace_rsc_btn.setEnabled( false );
	   		rscSelDlg.setReplaceEnabled( false );

   			disable_rsc_btn.setEnabled( false );
   			disable_rsc_btn.setSelection( false );
   			disable_rsc_btn.setText( "Turn Off");
   		}
   		else {
   			ResourceSelection rscSel = seldRscsList.get(0);
   			
   			// Can't delete, replace or turn off the base overlay.
   			if( rscSel.getResourceName().equals( rbdMngr.getBaseOverlay().getResourceName() ) ) {
   	   			del_rsc_btn.setEnabled( false );
   	   			
   	   			replace_rsc_btn.setEnabled( false );
   	   			rscSelDlg.setReplaceEnabled( false );

   	   			disable_rsc_btn.setEnabled( false );
   	   			disable_rsc_btn.setSelection( false );
   	   			disable_rsc_btn.setText( "Turn Off");
   			}
   			else {
   	   			del_rsc_btn.setEnabled( true );
   	   			replace_rsc_btn.setEnabled( true );   	   		
   	   			rscSelDlg.setReplaceEnabled( true );
   	   			
   	   			disable_rsc_btn.setEnabled( true );
   			
   	   			if( rscSel.isVisible() ) {
   	   	   			disable_rsc_btn.setSelection( false );
   	   	   			disable_rsc_btn.setText( "Turn Off");   	   				
   	   			}
   	   			else {
   	   	   			disable_rsc_btn.setSelection( true );
   	   	   			disable_rsc_btn.setText( "Turn On");
   	   			}
   			}   			
   		}
   		// TODO : we could allow for multiple selections to be replaced.
   		//
//   		if( rscSelDlg != null && rscSelDlg.isOpen() ) {
//
//   			rscSelDlg.setReplaceEnabled( enableReplace );
//   		}
   	}
	
	// This will load the currently configured RBD (all panes) into
	// a new editor or the active editor if the name matches the current
	// name of the RBD
    public void loadRBD( boolean close ) {

    	String rbdName = rbd_name_txt.getText().trim();

    	if( rbdName == null || rbdName.isEmpty() ) {
    		rbdName = "Preview";
    	}

    	// Since rbdLoader uses the same resources in the rbdBundle to load in the editor,
    	// we will need to make a copy here so that future edits are not immediately reflected in
    	// the loaded display. The easiest way to do this is to marshal and then unmarshal the rbd.
    	try {
    		
    		rbdMngr.setGeoSyncPanes( geo_sync_panes.getSelection() );
    		rbdMngr.setAutoUpdate( auto_update_btn.getSelection() );
//    		rbdMngr.setAutoUpdate( auto_update_btn.getEnabled() );
    		RbdBundle rbdBndl = rbdMngr.createRbdBundle( rbdName,
    				                                     timelineControl.getTimeMatcher() );
    		
    		rbdBndl = RbdBundle.clone( rbdBndl );
    		rbdBndl.resolveDominantResource();
    
    		ResourceBndlLoader rbdLoader = null;
    		rbdLoader = new ResourceBndlLoader("RBD Previewer");

    		// Get the Display Editor to use.
    		// First look for a preview editor and if not found then check if the 
    		// active editor name matches that of the current rbd name (Note: don't
    		// look for an editor matching the rbd name because it is possible to
    		// have the same named display for a different RBD.)
    		// If the active editor doesn't match then load into a new display editor
    		NCMapEditor editor=null;
    		
    		if( rbdName.equals("Preview") ) {
    			editor = (NCMapEditor) NmapUiUtils.findDisplayByName( rbdName );
    		}
    		// else if the rbd was imported from a display and the name was not changed 
    		//    get this display
    		if( editor == null ) {
    			String importDisplayName = NmapUiUtils.getNcDisplayNameWithoutID(import_rbd_combo.getText());

    			if( importDisplayName.equals( rbdName ) ) {
    				// get by ID since the rbd name doesn't have to be unique
        			editor = NmapUiUtils.findDisplayByID( importDisplayName );    				
    			}    			
    		}
    		
    		// else if the active editor matches this rbdName, use the active editor
    		// Don't attempt to find an editor based just on the display name since 
    		// they may not be unique without the display id.
    		if( editor == null ) {    			
    			editor = NmapUiUtils.getActiveNatlCntrsEditor();

    			String activeEditorName = NmapUiUtils.getNcDisplayNameWithoutID(
    					                           editor.getDisplayName() );
    	
    			if( !activeEditorName.equals( rbdName ) ) {
    				editor = null;
    			}
    		}

    		// if they changed the layout then we should be able to modify the layout of the current
    		/// display but for now just punt and get a new editor.
    		if( editor != null &&
    				editor.getPaneLayout().compare( rbdBndl.getPaneLayout() ) != 0 ) {
    			System.out.println("Creating new Editor because the paneLayout differs.");
    			editor = null;
    		}

    		if( editor == null ) {//&& !emptyEditorRemoved) {
    			String defaultName = defaultRscList.get(0);
    			
				if( !defaultName.equals( rbdName ) && 
					!emptyEditorRemoved && 
					rbdBndl.getPaneLayout().getNumberOfPanes() == 1 &&
					NmapUiUtils.findEmptyEditor(defaultRscList)) {
					emptyEditorRemoved = true;
					editor = (NCMapEditor)NmapUiUtils.findDisplayByName(defaultName);
					String name = editor.getDisplayName();
					editor.setDisplayName(name.substring(0, name.indexOf("-")+1) + rbdName);
				}
				else {
					editor = NmapUiUtils.createNatlCntrsEditor( rbdName, rbdBndl.getPaneLayout() );
				}
    		}
//    		else {
//    			editor.setDisplayName( NmapUiUtils.createNatlCntrsDisplayName( rbdName ) );
//    		}

    		if( editor == null ) {
    			throw new VizException("Unable to create a display to load the RBD.");
    		}

			NmapUiUtils.bringToTop(editor);
    		
    		editor.setApplicationName(rbdName); 
    		
        	rbdLoader.addRBD( rbdBndl, editor );
    		
        	VizApp.runSync( rbdLoader );
        	
        	// They aren't going to like this if there is an error loading....
        	if( close ) {
        		shell.dispose();
        	}
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
    
    
//    public static NCMapEditor findCloseEmptyEditor() {
////    	NCMapEditor defaultEditor = (NCMapEditor) NmapUiUtils.findDisplayByName( "Welcome" );
//    	
//    	IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
//		for (IWorkbenchWindow window : windows) {
//			IWorkbenchPage pages[] = window.getPages();
//			for (IWorkbenchPage page : pages) {
//				IEditorReference[] refs = page.getEditorReferences();
//				for (IEditorReference r : refs) {
//					String name = r.getName();
//					String s1 = name.substring(0, name.indexOf("-"));
//					String s2 = name.substring(name.indexOf("-")+1, name.length());
//					
//					NCMapEditor defaultEditor = (NCMapEditor) (r.getEditor(false));
//					
//					if (defaultEditor != null && name != null) {
//						if (defaultEditor.getDescriptor() != null) 	{
//							ResourceList rl = defaultEditor.getDescriptor().getResourceList();
//							
//							if (rl !=null && s2.equalsIgnoreCase(defaultRscList.get(0)) && s1.equalsIgnoreCase("1")) {
//								boolean flag = true;
//								for( ResourcePair rp : rl ) {
//					    			if( rp != null ) {
//					    				String s = rp.getResource().getResourceData().getClass().getName();
//					    				
//					    				if (defaultRscList.contains(s)) {
//											continue;	
//					    				}
//					    				else {
//					    					flag = false;
//					    					break;
//					    				}
//									}																				
//					    		}
//								
//								if (flag == true) {
//									page.closeEditor(defaultEditor, false);
////									//updateEditorNum();
//									return defaultEditor;
//								}
//							}
//						}
//					}
//				}
//			}
//		}	
//		
//		return null;
//    }
    	
    // replace this later when there is a way for the user to set up there 
    // preferred default RBD.
//    public RbdBundle getDefaultRBD( ) throws VizException {
//    	
//    	File rbdFile = NcPathManager.getInstance().getStaticFile( 
//		         NcPathConstants.DFLT_RBD );
//    	
//
//    	try {
//    		RbdBundle dfltRbd = RbdBundle.unmarshalRBD( rbdFile, null );
//            
//            // shouldn't need this but just in case the user creates a default with
//            // real resources in it
//    		dfltRbd.resolveLatestCycleTimes();
//    		
//    		return dfltRbd;
//    	}
//    	catch ( Exception ve ) {
//    		throw new VizException( "Error getting default RBD: " + ve.getMessage());
//    	}  
//    }
    
    /*
     *  get Rbd name and resources from defaultRbd.xml
     */
    public static List<String> getDefaultRbdRsc() {
    	
    	List<String> list = new ArrayList<String>();
    	
    	try {
    		RbdBundle rbd = RbdBundle.getDefaultRBD();
    		if (rbd != null)
    			list.add(rbd.getRbdName() );
    		
    			ResourceList rscList = (rbd.getDisplays())[0].getDescriptor().getResourceList();
    			//ResourceList rscList1 = (rbd.getDisplays())[0].getDescriptor().getRenderableDisplay().getDescriptor().getResourceList();
    		
    		for( ResourcePair rp : rscList ) {
//    			try {
    				if( rp != null && rp.getResourceData() != null) {
    					// there are 2 type of res
//    					AbstractNatlCntrsResourceData resourceData = (AbstractNatlCntrsResourceData)(rp.getResourceData());
//    					String s = resourceData.getResourceName().toString();
    					String s = rp.getResourceData().getClass().getName();
    					list.add(s);
    				}
//    			}
//    			catch (Exception e) {
//    				String s = rp.getResourceData().getClass().getName();	    				
//    				list.add(s);    				
//    			}
    		}
    	}
    	catch ( Exception ve ) {
    		System.out.println("Could not load Rbd: " + ve.getMessage());
    		
    	}  
    	return list;
    }
    
    // After Loading an RBD the user may 're-load' a modified Pane. Currently the number of panes
    // has to be the same as previously displayed.
    public void loadPane() {
    	String rbdName = rbd_name_txt.getText();

    	if( rbdName == null || rbdName.isEmpty() ) {
    		rbdName = "Preview";
    	}

    	// Since rbdLoader uses the same resources in the rbdBundle to load in the editor,
    	// we will need to make a copy here so that future edits are not immediately reflected in
    	// the loaded display. The easiest way to do this is to marshal and then unmarshall the rbd.
    	try {
    		rbdMngr.setGeoSyncPanes( geo_sync_panes.getSelection() );
    		rbdMngr.setAutoUpdate( auto_update_btn.getSelection() );
   
    		// TODO : check timeline compatibility with other panes...
    		
    		RbdBundle rbdBndl = rbdMngr.createRbdBundle( rbdName,
    				                                     timelineControl.getTimeMatcher() );
    		rbdBndl = RbdBundle.clone( rbdBndl );
    
    		ResourceBndlLoader rbdLoader = null;
    		rbdLoader = new ResourceBndlLoader("RBD Previewer");

    		rbdLoader.setLoadSelectedPaneOnly();
    		
    		// Get the Display Editor to use. If the active editor doesn't 
    		// match the name of the RBD then prompt the user.
    		NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();

    		// Check that the selected pane is within the layout of the Display
    		if( editor == null ) {
    			throw new VizException("Error retrieving the Active Display??");
    		}
    		
    		String activeEditorName = NmapUiUtils.getNcDisplayNameWithoutID(
    				editor.getDisplayName() );   	
    		
    		if( !activeEditorName.equals( rbdName ) ) {
    			MessageDialog confirmDlg = new MessageDialog( 
    					shell, "Confirm Load Pane", null, 
    					"You will first need to Load the RBD before\n"+
    					"re-loading a pane. \n\n"+
    					"Do you want to load the currently defined RBD?",
    					MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
    			confirmDlg.open();

    			if( confirmDlg.getReturnCode() == MessageDialog.OK ) {
    				loadRBD( false );
    			}
    			return;    			          
    		}

    		// TODO : We could make this smarter by adjusting the display...
    		//
    		if( !editor.getPaneLayout().equals( rbdBndl.getPaneLayout() ) ) {
    			MessageDialog msgDlg = new MessageDialog( 
    					shell, "Load Pane", null, 
    					"The pane layout of the display doesn't match the currently selected\n"+
    					"RBD pane layout. You will first need to Load the RBD before\n"+
    					"changing the number of panes.",
    					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
    			msgDlg.open();
         		
    			return;
//    			throw new VizException("Error: The Active Display doesn't have enough Panes"+
//    					" for the selected Pane: "+rbdBndl.getSelectedPaneId().toString() );				
			}
    		

//    		rbdBndl.setNcEditor( editor );

    		rbdLoader.addRBD( rbdBndl, editor );
    		

        	VizApp.runSync( rbdLoader );
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
    
    public void importRBD( String seldDisplayName ) {
    	RbdBundle impRbd;
    	if( seldDisplayName.equals( ImportFromSPF ) ) {

    		SelectRbdsDialog impDlg = 
    			new SelectRbdsDialog( shell, "Import RBD", 
    					false, false, false );

    		if( !impDlg.open() ) {
    			return;
    		}
    		
    		impRbd = impDlg.getSelectedRBD();

    		impRbd.resolveLatestCycleTimes();    		
    	}
    	else {
    		// get RbdBundle from selected display
    		NCMapEditor seldEditor = NmapUiUtils.findDisplayByID( seldDisplayName );	
    		if( seldEditor == null ) {
    			System.out.println("Unable to load Display :"+seldDisplayName );
    			return;
    		}
    		
    		impRbd = new RbdBundle();

			try {
    		impRbd.initFromEditor(seldEditor);  
    			
				impRbd = RbdBundle.clone( impRbd );
			} catch (VizException e) {
				MessageDialog errDlg = new MessageDialog( 
						shell, "Error", null, 
						"Error Importing Rbd from Display, "+seldDisplayName+".\n"+e.getMessage(),
						MessageDialog.ERROR, new String[]{"OK"}, 0);
				errDlg.open();
			}
    		
    		NmapUiUtils.bringToTop(seldEditor);
    	}

    	//boolean confirm = ( rbdMngr.getSelectedRscs().length > 1 ) || rbdMngr.isMultiPane();

    	// if any selections have been made then popup a confirmation msg
    	if( rbdMngr.isRbdModified() ) { //confirm ) {
    		MessageDialog confirmDlg = new MessageDialog( 
    				shell, "Confirm", null, 
    				"This RBD has been modified.\n\n"+
    				"Do you want to coninue the import and clear the current RBD selections?",
    				MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
    		confirmDlg.open();

    		if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
    			return;
    		}           				
    	}

    	try {
    	rbdMngr.initFromRbdBundle( impRbd );
    	}
    	catch( VizException e ) {
    		rbdMngr.init();
			MessageDialog errDlg = new MessageDialog( 
					shell, "Error", null, 
					"Error Importing RBD:"+impRbd.getRbdName()+"\n\n"+e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
    	}

    	updateGUI();

    	// updateGUI triggers the spinner which ends up calling rbdMngr.setPaneLayout(),
    	// so we need to reset this here.
    	rbdMngr.setRbdModified( false ); 

		//    	timelineControl.setTimeMatcher( impRbd.getTimeMatcher() );    	
    }
    
    // import just the given pane in the rbdBndl into the dialog's 
    // currently selected pane. 
    // Note: paneID is not the currently selected pane id when
    // we are importing just a single pane.
    public void	importPane( RbdBundle rbdBndl, PaneID paneId  ) throws VizException {

		clearSeldResources();

		rbdMngr.setPaneData( rbdBndl.getDisplayPane(paneId) );
        			
		for( ResourceSelection rscSel : rbdMngr.getRscsForPane( rbdMngr.getSelectedPaneId() ) ) {
			if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {     		    		
				timelineControl.addAvailDomResource( 
					(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
    		}
    	}
    	
//    	
    	timelineControl.setTimeMatcher( rbdBndl.getTimeMatcher() );
    	
    	// TODO : if geo sync is set and if this geogArea is different than the
    	// current one then prompt the user what to do.
    	// 
//    	if( geo_sync_panes.getSelection() ) {
    	//
//    	}
//    	else {
//    		// TODO : check for non-predefined areas and handle them appropriately
//    		//
//    		PredefinedArea origArea = rbdBndl.getDisplayPane(paneId).getInitialArea();
//    		
//    		if( origArea == null ) {
//    			System.out.println("RbdBundle: Area is not set in pane???");
//    		}
//    		else {    		
//    			NCMapRenderableDisplay display = rbdBndl.getDisplayPane(paneId);
//
//    			PredefinedArea pArea = display.getCurrentArea();
//
//    			rbdMngr.setAreaProviderName(  pArea.getAreaName() );
//
//    			// selectPane( rbdBndl.getSelectedPaneId() );
//    	    	// set the selected area
//    	    	geo_area_combo.setText( rbdMngr.getAreaProvider().getProviderName() );
//    		}
//    	}
    	
    	selectArea( rbdMngr.getAreaProvider() );
    	
    	updateSelectedResourcesView( true );
    }    
    
    public void updateGUIforMultipane( boolean isMultiPane ) {    	
    	FormData fd = new FormData();

		geo_sync_panes.setVisible( isMultiPane );

		pane_layout_grp.setVisible( isMultiPane );
    	
    	if( isMultiPane ) {
    		fd.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
//        	fd.left = new FormAttachment( 30, 2 );
    		fd.top  = new FormAttachment( geo_area_grp, 0, SWT.TOP );
    		fd.bottom = new FormAttachment( geo_area_grp, 0, SWT.BOTTOM );
    		fd.right = new FormAttachment( 100, -300 );
    		seld_rscs_grp.setLayoutData( fd );

    		shell.setSize( new Point( multiPaneDlgWidth, shell.getSize().y ) );
    	}
    	else {
        	fd.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
//        	fd.left = new FormAttachment( 30, 2 );

        	fd.top  = new FormAttachment( geo_area_grp, 0, SWT.TOP );
        	fd.right = new FormAttachment( 100, -10 );
        	fd.bottom = new FormAttachment( geo_area_grp, 0, SWT.BOTTOM );
        	seld_rscs_grp.setLayoutData( fd );

        	shell.setSize( new Point( singlePaneDlgWidth, shell.getSize().y ) );
    	}
    }


    public void saveRBD( boolean new_pane ) {
    	try {    		
    		NCTimeMatcher timeMatcher = timelineControl.getTimeMatcher();
    		boolean saveRefTime = !timeMatcher.isCurrentRefTime();
    		boolean saveTimeAsConstant = false;  //TODO -- how should this be set???

    		// get the filename to save to.
    		SaveRbdDialog saveDlg = new SaveRbdDialog( shell,
    				savedSpfGroup, savedSpfName, rbd_name_txt.getText(), saveRefTime, saveTimeAsConstant ); 

    		if( (Boolean)saveDlg.open() == false ) {
    			return; 
    		}

    		savedSpfGroup = saveDlg.getSeldSpfGroup();
    		savedSpfName  = saveDlg.getSeldSpfName();

    		saveRefTime = saveDlg.getSaveRefTime();
    		saveTimeAsConstant = saveDlg.getSaveTimeAsConstant();

    		// Set the name to the name that was actually used 
    		// to save the RBD.
    		// TODO : we could store a list of the RBDNames and load these
    		// as items in the combo.
    		rbd_name_txt.setText( saveDlg.getSeldRbdName() );

    		rbdMngr.setGeoSyncPanes( geo_sync_panes.getSelection() );
    		rbdMngr.setAutoUpdate( auto_update_btn.getSelection() );

    		RbdBundle rbdBndl = rbdMngr.createRbdBundle( saveDlg.getSeldRbdName(), timeMatcher );

    		SpfsManager.getInstance().saveRbdToSpf( savedSpfGroup, savedSpfName, rbdBndl, saveRefTime, saveTimeAsConstant );

    		VizApp.runSync(new Runnable() {
    			public void run() {
    				String msg = null;
    				msg = new String("Resource Bundle Display "+
    						rbd_name_txt.getText() + " Saved to SPF "+
    						savedSpfGroup + File.separator+ savedSpfName+".");
    				MessageBox mb = new MessageBox( shell, SWT.OK );         								
    				mb.setText( "RBD Saved" );
    				mb.setMessage( msg );
    				mb.open();

    				rbdMngr.setRbdModified( false );
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
    
    // if Editing then save the current rbd to an RbdBundle 
    private void createEditedRbd() {
    	
    	try {    		
			NCTimeMatcher timeMatcher = timelineControl.getTimeMatcher();
    		
			if( !rbd_name_txt.getText().isEmpty() ) {
				rbdMngr.setRbdName( rbd_name_txt.getText() );
			}
			
    		rbdMngr.setGeoSyncPanes( geo_sync_panes.getSelection() );
    		rbdMngr.setAutoUpdate( auto_update_btn.getSelection() );
			
			editedRbd = rbdMngr.createRbdBundle( rbdMngr.getRbdName(), timeMatcher );
			
			editedRbd.setIsEdited( rbdMngr.isRbdModified() );
			    		    		
    	}
    	catch( VizException e ) {
    		editedRbd = null;
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
    
    public RbdBundle getEditedRbd () {
		return editedRbd;
    }
}