package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import gov.noaa.nws.ncep.viz.common.area.AreaMenus.AreaMenuItem;
import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider.ZoomLevelStrings;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.customprojection.GempakProjectionValuesUtil;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resourceManager.timeline.TimelineControl;
import gov.noaa.nws.ncep.viz.resourceManager.timeline.TimelineControl.IDominantResourceChangedListener;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionControl.IResourceSelectedListener;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.EditResourceAttrsAction;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor.LayoutData;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 01/24/2013     #972       Greg Hull   add NcDisplayType to support NonMap and solar based RBDs
 * 05/24/2013     #862       Greg Hull   get areas from menus file and change combo to a cascading menu
 * 06/03/2013     #1001      Greg Hull   allow multiple Remove/TurnOff of resources
 * 10/22/2013     #1043      Greg Hull   setSelectedResource() if rsc sel dlg is already up.
 * 11/25/2013     #1079      Greg Hull   adjust size/font of area toolbar based on the text 
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
    
    private Combo  disp_type_combo = null;
    private Label  disp_type_lbl = null;
    
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
    
   	private ToolItem areaTItm;
    private AreaMenuItem seldAreaMenuItem = null;
    private Font areaFont = null; // dispose if new Font created
    
    private Group geo_area_grp = null;
    private MenuManager areaMenuMngr = null;
    
    private Composite geo_area_info_comp = null; // only one of these visible at a time
    private Composite rsc_area_opts_comp = null; // depending on if a satellite area is selected
    
    private Text  proj_info_txt = null;   // view-only projection and map center info
    private Text  map_center_txt = null;   // view-only projection and map center info
    private Button fit_to_screen_btn = null;    
    private Button size_of_image_btn = null;
    
    private Button custom_area_btn = null;
      
    private Group  pane_layout_grp = null;

    private Button pane_sel_btns[][] = null;
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

    private AbstractRBD<?> editedRbd = null;    // set on OK when this is an 'Edit Rbd' dialog

    // used to initialize the Save Dialog
    private String savedSpfGroup = null;
    private String savedSpfName  = null;
    
    private Point initDlgSize = new Point( 750, 860 );
    private int   singlePaneDlgWidth = 750;
    private int   multiPaneDlgWidth = 950;
    
    private TimelineControl timelineControl = null;
    
    private final String ImportFromSPF = "From SPF...";
//    private final String[] StandardZoomLevels = {"1", "1.5","2","3","5","7.5","10","15","20","30"};
      
    private static Map<String, String> gempakProjMap = 
    				GempakProjectionValuesUtil.initializeProjectionNameMap();
    
    // the rbdMngr will be used to set the gui so it should either be initialized/cleared 
    // or set with the initial RBD.
    public CreateRbdControl(Composite parent, RscBundleDisplayMngr mngr )   throws VizException {
        super(parent, SWT.NONE);
        shell = parent.getShell();

        rbdMngr = mngr;
                
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
    	FormData form_data = new FormData();//195,20);
    	form_data.left = new FormAttachment( 0, 15 );
    	form_data.top  = new FormAttachment( 0, 30 );
    	form_data.right= new FormAttachment( 24, 0 );
    	import_rbd_combo.setLayoutData( form_data );
    	import_rbd_combo.setEnabled(true);    	

        import_lbl = new Label( rbd_grp, SWT.None );
        import_lbl.setText("Import");
        form_data = new FormData();
     	form_data.left = new FormAttachment( import_rbd_combo, 0, SWT.LEFT );
    	form_data.bottom = new FormAttachment( import_rbd_combo, -3, SWT.TOP );
    	import_lbl.setLayoutData( form_data );


    	rbd_name_txt = new Text( rbd_grp, SWT.SINGLE | SWT.BORDER );
    	form_data = new FormData(200,20);
    	form_data.left = new FormAttachment( import_rbd_combo, 25, SWT.RIGHT );
    	form_data.top  = new FormAttachment( import_rbd_combo,  0, SWT.TOP );
    	rbd_name_txt.setLayoutData( form_data );

        rbd_name_lbl = new Label( rbd_grp, SWT.None );
        rbd_name_lbl.setText("RBD Name");
        form_data = new FormData();
        form_data.width = 180;
     	form_data.left = new FormAttachment( rbd_name_txt, 0, SWT.LEFT );
    	form_data.bottom = new FormAttachment( rbd_name_txt, -3, SWT.TOP );
    	rbd_name_lbl.setLayoutData( form_data );
    	
    	
    	disp_type_combo = new Combo( rbd_grp, SWT.DROP_DOWN | SWT.READ_ONLY );
    	form_data = new FormData(120,20);
    	form_data.left = new FormAttachment( import_rbd_combo, 0, SWT.LEFT );
    	form_data.top  = new FormAttachment( import_rbd_combo, 45, SWT.BOTTOM );
    	disp_type_combo.setLayoutData( form_data );
    	disp_type_combo.setEnabled(true);    	
    	
    	disp_type_combo.setItems( new String[] { 
    			NcDisplayType.NMAP_DISPLAY.getName(),
    			NcDisplayType.NTRANS_DISPLAY.getName(),
    			NcDisplayType.SOLAR_DISPLAY.getName()} );

    	
        disp_type_lbl = new Label( rbd_grp, SWT.None );
        disp_type_lbl.setText("RBD Type");
        form_data = new FormData();
     	form_data.left = new FormAttachment( disp_type_combo, 0, SWT.LEFT );
    	form_data.bottom = new FormAttachment( disp_type_combo, -3, SWT.TOP );
    	disp_type_lbl.setLayoutData( form_data );

    	
    	multi_pane_tog = new Button( rbd_grp, SWT.CHECK );
    	multi_pane_tog.setText("Multi-Pane Display");
    	form_data = new FormData();
    	form_data.top  = new FormAttachment( rbd_name_txt, -10, SWT.TOP );
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
    	form_data.top  = new FormAttachment( disp_type_combo, 25, SWT.BOTTOM );
    	// form_data.bottom  = new FormAttachment( 100, -60 ); // if offset for room for the Load and Save buttons
    	form_data.bottom = new FormAttachment( 100, -10 );
    	form_data.left   = new FormAttachment( 0, 10 );
    	form_data.right  = new FormAttachment( 24, 0 );
 
    	geo_area_grp.setLayoutData( form_data );

        ToolBar areaTBar = new ToolBar(geo_area_grp, SWT.SHADOW_OUT|SWT.HORIZONTAL|SWT.RIGHT|SWT.WRAP);
    	form_data = new FormData();
    	form_data.left = new FormAttachment( 0, 10 );
    	form_data.top  = new FormAttachment( 0, 15 );
    	form_data.right = new FormAttachment( 100, -10 );
    	form_data.height = 30;
    	areaTBar.setLayoutData( form_data );
    	
    	this.addDisposeListener( new DisposeListener() {			
			@Override
			public void widgetDisposed(DisposeEvent e) {
				if( areaFont != null ) {
					areaFont.dispose();
				}
			}
		});
    	
    	areaTItm = new ToolItem(areaTBar, SWT.DROP_DOWN);    	
    	areaMenuMngr = new MenuManager("CreateRbdControl");
    	areaMenuMngr.setRemoveAllWhenShown( true );
    	final Menu areaCtxMenu = areaMenuMngr.createContextMenu( shell );
    	
    	areaCtxMenu.setVisible( false );
    	geo_area_grp.setMenu( areaCtxMenu );
    	
    	areaMenuMngr.addMenuListener( new IMenuListener() {
			@Override
			public void menuAboutToShow(IMenuManager amngr) {				
				createAvailAreaMenuItems( amngr );
			}
		});
    	
    	// Main toolbar button clicked:  show the areas popup menu at 
    	// the location of the toolbar so it appears like a combo 
    	// dropdown. This will also trigger the menu manager to create 
    	// the menu items for the available areas. 
    	areaTItm.addListener(SWT.Selection, new Listener() {
        	public void handleEvent(Event event) {
        		ToolItem ti = ((ToolItem)event.widget);
        		Rectangle bounds = ti.getBounds();
        		Point point = ((ToolBar)ti.getParent()).toDisplay(bounds.x, bounds.y + bounds.height);
        		
        		areaCtxMenu.setLocation(point);
        		areaCtxMenu.setVisible(true);
        	}
        });
    	
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
    	form_data.top  = new FormAttachment( areaTBar, 15, SWT.BOTTOM );
    	form_data.right = new FormAttachment( 100, -10 );

    	// both overlap each other since only one visible at a time
    	geo_area_info_comp.setLayoutData( form_data );
    	
    	form_data.top  = new FormAttachment( areaTBar, 30, SWT.BOTTOM );
    	rsc_area_opts_comp.setLayoutData( form_data );
    	
    	fit_to_screen_btn = new Button( rsc_area_opts_comp, SWT.RADIO );
    	fit_to_screen_btn.setText( "Fit To Screen");

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
    	custom_area_btn.setVisible( false );
    }
    
    // create the Selected Resources List, the Edit, Delete and Clear buttons
    //
   	private Group createSeldRscsGroup( ) {
   		Group seld_rscs_grp = new Group( rbd_grp, SWT.SHADOW_NONE );
   		seld_rscs_grp.setText("Selected Resources" );
   		seld_rscs_grp.setLayout( new FormLayout() );
    	FormData form_data = new FormData();
    	
    	// NOTE : This is reset in updateGUIforMultipane()
//    	form_data.left = new FormAttachment( 30, 2 );
//    	form_data.top  = new FormAttachment( rbd_name_txt, 25, SWT.BOTTOM );
    	form_data.top  = new FormAttachment( auto_update_btn, 15, SWT.BOTTOM );
		form_data.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
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
    	GridLayout gl = new GridLayout(rbdMngr.getMaxPaneLayout().getColumns(), false);
    //	gl.horizontalSpacing = 4;
    	
    	num_rows_cols_comp.setLayout( gl );

    	fd = new FormData();
    	fd.left = new FormAttachment( 0, 80 );
    	fd.top  = new FormAttachment( 0, 3 );
    	fd.right = new FormAttachment( 100, -10 );
    	num_rows_cols_comp.setLayoutData( fd );

    	Button num_rows_btns[] = new Button[rbdMngr.getMaxPaneLayout().getRows()];
    	Button num_cols_btns[] = new Button[rbdMngr.getMaxPaneLayout().getColumns()];
    	
    	for( int r=0 ; r<rbdMngr.getMaxPaneLayout().getRows() ; r++ ) {
    		num_rows_btns[r] = new Button( num_rows_cols_comp, SWT.PUSH );
    		num_rows_btns[r].setText( Integer.toString(r+1) );
    		num_rows_btns[r].setSize(20, 20);
    		num_rows_btns[r].setData( new Integer(r+1));
    		num_rows_btns[r].addSelectionListener( new SelectionAdapter() {
       			public void widgetSelected(SelectionEvent e) {
       				selectPane(
       						rbdMngr.setPaneLayout(
       								new NcPaneLayout( (Integer)e.widget.getData(), 
       										((NcPaneLayout)rbdMngr.getPaneLayout()).getColumns() ) ) );
       				updatePaneLayout();
       			}
    		});
    	}
    	
    	GridData gd = new GridData();
    	gd.widthHint = 50;
    	gd.grabExcessHorizontalSpace = true;

    	for( int c=0 ; c<rbdMngr.getMaxPaneLayout().getColumns() ; c++ ) {    		
    		num_cols_btns[c] = new Button( num_rows_cols_comp, SWT.PUSH );
    		num_cols_btns[c].setText( Integer.toString(c+1) );
    		num_cols_btns[c].setData( new Integer(c+1));
    		
    		num_cols_btns[c].addSelectionListener( new SelectionAdapter() {
       			public void widgetSelected(SelectionEvent e) {
       				selectPane(
       						rbdMngr.setPaneLayout(
       								new NcPaneLayout( 
       										((NcPaneLayout)rbdMngr.getPaneLayout()).getRows(), 
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
    	pane_sel_comp.setLayout( new GridLayout(rbdMngr.getMaxPaneLayout().getColumns(), true));

    	fd = new FormData();
    	fd.left = new FormAttachment( 0, 25 );
    	fd.top  = new FormAttachment( sep, 12 );
    	fd.bottom = new FormAttachment( 100, -45 ); 
    	fd.right = new FormAttachment( 100, -15 );
    	pane_sel_comp.setLayoutData( fd );

    	pane_sel_btns = new Button[rbdMngr.getMaxPaneLayout().getRows()]
    	                          [rbdMngr.getMaxPaneLayout().getColumns()];
    	
    	int numPanes = rbdMngr.getMaxPaneLayout().getNumberOfPanes();
    	for( int p=0 ; p<numPanes ; p++ ) {
    		NcPaneID pid = (NcPaneID)rbdMngr.getMaxPaneLayout().createPaneId( p );
    		int r = pid.getRow();
    		int c = pid.getColumn();
  
    		pane_sel_btns[r][c] = new Button( pane_sel_comp, SWT.TOGGLE );
    		pane_sel_btns[r][c].setText( pid.toString() ); 

    		pane_sel_btns[r][c].setData( pid );
    		pane_sel_btns[r][c].addSelectionListener( new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				NcPaneID seldPane = (NcPaneID)e.widget.getData();
    				selectPane(seldPane);
    			}
    		});
    		pane_sel_btns[r][c].setSelection( (r==0 && c==0) );
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
   		
   		disp_type_combo.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				NcDisplayType selDispType = NcDisplayType.getDisplayType( disp_type_combo.getText() );   				
   				if( rbdMngr.getRbdType() != selDispType ) {
   					if( rbdMngr.isRbdModified() ) {
   			    		MessageDialog confirmDlg = new MessageDialog( 
   			    				shell, "Confirm", null, 
   			    				"This RBD has been modified.\n\n"+
   			    				"Do you want to clear the current RBD selections?",
   			    				MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
   			    		confirmDlg.open();

   			    		if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
   			    			return;
   			    		}           				   			    		
   					}
   					
   					try {
   	   					rbdMngr.setRbdType( selDispType );

   	   					AbstractRBD<?> dfltRbd = AbstractRBD.getDefaultRBD( rbdMngr.getRbdType() );
   						
   						rbdMngr.initFromRbdBundle( dfltRbd );
   						
   					} catch (VizException ve) {
   						MessageDialog errDlg = new MessageDialog( 
   								shell, "Error", null, 
   								ve.getMessage(), MessageDialog.ERROR, new String[]{"OK"}, 0);
   						errDlg.open();
   						
   						rbdMngr.init( rbdMngr.getRbdType() );
   					}
   					updateGUI();
   				}				
   			}
   		});
   		
    	sel_rsc_btn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   		   		List<ResourceSelection> seldRscsList = (List<ResourceSelection>) sel_elems.toList();
   		   		int numSeldRscs = seld_rscs_lviewer.getList().getSelectionCount();

   		   		Boolean isBaseLevelRscSeld = false;
   		   		// the initially selected resource is the first non-baselevel rsc
   		   		ResourceName initRscName = null;

   		   		for( ResourceSelection rscSel : seldRscsList ) {
   		   			isBaseLevelRscSeld |= rscSel.isBaseLevelResource();
   		   			if( initRscName == null && !rscSel.isBaseLevelResource() ) {
   		   				initRscName = rscSel.getResourceName();
   		   			}
   		   		}
   		   		
   		   		// the replace button is enabled if there is only 1 resource selected and
   		   		// it is not the base resource
   				if( rscSelDlg.isOpen() ) {
   					if( initRscName != null ) {
   						rscSelDlg.setSelectedResource( initRscName );
   					}
   				}
   				else {   					
   					rscSelDlg.open( true, // Replace button is visible
   							(numSeldRscs == 1 && !isBaseLevelRscSeld), // replace enabled
   							initRscName,
   							multi_pane_tog.getSelection(),
   							rbdMngr.getRbdType(),
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
   							rbdMngr.getRbdType(),
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
   	   				
   					rbdMngr.getSelectedArea();

   					
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
//   	    				String seldArea = rbdMngr.getSelectedArea().getProviderName();
//   	    				
//   	    				if( !seldAreaMenuItem.getAreaName().equals( seldArea ) ) {
   	    				updateAreaGUI( ); 
//   	    				}
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
   					selectPane( new NcPaneID() ); // 0,0
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
   						"selected Area: "+ rbdMngr.getSelectedArea().getProviderName() + "\n\n"+
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
//   		        Shell shell = NcDisplayMngr.getCaveShell();
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
    			
    			// note: the base may be selected if there are multi selected with others.
    			while( itr.hasNext() ) {
    				ResourceSelection rscSel = (ResourceSelection)itr.next();
    				if( !rscSel.isBaseLevelResource() ) {
    					removeSelectedResource( rscSel );
    				}
    			}
    			
   		   		updateSelectedResourcesView( true );
   		   		
   		   		// check to see if the current area has been reset to the default because 
   		   		// it was the 
//   				String seldArea = rbdMngr.getSelectedArea().getProviderName();
//
//   				if( seldAreaMenuItem.getAreaName().equals( seldArea) ) {
   					updateAreaGUI( );
//   				}
       		}
       	});

       	disable_rsc_btn.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
    			Iterator itr = sel_elems.iterator();
    			
    			while( itr.hasNext() ) {
    				ResourceSelection rscSel = (ResourceSelection)itr.next();
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

        		AbstractRBD<?> impRbd = impDlg.getSelectedRBD();

       			if( impRbd != null ) {    			
           			// if any selections have been made then popup a confirmation msg
       				// TODO:add support for determining if only this pane has been modified.
//       				boolean confirm = ( rbdMngr.getSelectedRscs().length > 1 );
//       				if( confirm ) {
//       					MessageDialog confirmDlg = new MessageDialog( 
//       							NcDisplayMngr.getCaveShell(), "Confirm", null, 
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
    	
    	updateAreaGUI( );// should be the default area
    	    	
    	shell.setSize( initDlgSize );

    	updateGUIforMultipane( rbdMngr.isMultiPane() );

    	timelineControl.clearTimeline();
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
   		NcDisplayName seldImport = NcDisplayName.parseNcDisplayNameString( import_rbd_combo.getText() );
    	
   		import_rbd_combo.removeAll();
   		for( AbstractEditor ncDisplay : NcDisplayMngr.getAllNcDisplays() ) { //rbdMngr.getRbdType() ) ) {
   			
   			NcDisplayName displayName = NcEditorUtil.getDisplayName( ncDisplay );
   			import_rbd_combo.add( displayName.toString() );
   			
   			// if this was selected before, select it again
   	   		if( seldImport == null || //seldImport.isEmpty() ||
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
   	
   	// Note: if the text set for the ToolItem doesn't fit on the size of the item then
	// it will become blank and unselectable. Need to make sure this doesn't happen so
	// create a multi-line text string for the tool item and make sure it is wide and high
	// enough to hold the string.
	//
   	public void setAreaTextOnMenuItem( AreaName areaName ) {
		seldAreaMenuItem = new AreaMenuItem( areaName );

		// based on the starting width of the dialog and current attachments, the 
		// ToolItem has a width of 136. This will display up to 13 characters.
		//
		Point toolBarSize = areaTItm.getParent().getSize(); // current width and height
  
		if( toolBarSize.x == 0 ) { // gui not initialized yet 
			return;
		}

		int maxChars = toolBarSize.x * 13 / 136;
		int fontSize = 10; // normal font 
		boolean truncated = false;
		String menuText = seldAreaMenuItem.getMenuName(); // string that will be used to set the menuItem
		
		// if greater than 13 then we will have to figure out how to make it fit
		//
		if( menuText.length() > maxChars ) {
			// if its close then just change the font size
			//
			if( menuText.length() <= maxChars+1 ) {
				fontSize = 8;
			}
			else if( menuText.length() <= maxChars+2 ) {
				fontSize = 7;
			}
			else if( menuText.length() <= maxChars+3 ) {
				fontSize = 7;
			}
			// if this is a Display-defined area then just truncate it since the 
			// display id should be enough to let the user know what the area is
			else if( areaName.getSource() == AreaSource.DISPLAY_AREA ) {
				fontSize = 8;
				menuText = menuText.substring( 0, maxChars+3 );
				truncated = true;
			}
			// 
			else if( areaName.getSource().isImagedBased() ) {
				// if a Mcidas or Gini satellite then the name is the satellite name and area or sector name
				// separated with '/'
				// in this case we can leave off the satelliteName
				int sepIndx = menuText.indexOf( File.separator );
				if( sepIndx == -1 ) {
					System.out.println("Expecting '/' in satellite defined area???? ");
					menuText = menuText.substring( 0, maxChars );
					truncated = true;
				}
				else {
//				    StringBuffer menuName = new StringBuffer( menuText );	
//					menuName.insert( sepIndx+1, "\n" );
					String satName = menuText.substring(0,sepIndx);
					String area = menuText.substring(sepIndx+1);

					// if the areaName is close then change the font size
					if( area.length() > maxChars ) {
						if( area.length() <= maxChars+1 ) {
							fontSize = 8;
						}
						else if( area.length() <= maxChars+2 ) {
							fontSize = 7;
						}
						else if( area.length() <= maxChars+3 ) {
							fontSize = 7;
						}
						else { // else have to truncate
							fontSize = 7;
							area = area.substring(0,maxChars+3);
							truncated = true;
						}
					}
					if( satName.length() > maxChars+10-fontSize ) {
						satName = satName.substring(0,maxChars+10-fontSize );
						truncated = true;
					}
					
					menuText = satName + "\n" + area;
				} 
			}
			else {
				fontSize = 8;
				menuText = menuText.substring( 0, maxChars+1 );
				truncated = true;
			}
		}		
		
		// change the font and set the text.
		// (don't dispose the original font)
		Font curFont = areaTItm.getParent().getFont();		
		FontData[] fd = curFont.getFontData();

		if( fd[0].getHeight() != fontSize ) {
			fd[0].setHeight( fontSize );
			Device dev = curFont.getDevice();
			
			if( areaFont != null ) {
				areaFont.dispose();
			}
			areaFont = new Font( dev, fd );
			areaTItm.getParent().setFont( areaFont );
		}
		
		int tbHght = ( menuText.indexOf("\n") > 0 ? 47 : 30 );
		
		if( tbHght != toolBarSize.y ) {			
			toolBarSize.y = ( menuText.indexOf("\n") > 0 ? 47 : 30 );
			// without this the size will revert back when the dialog is resized for multi-pane
			FormData formData = (FormData)areaTItm.getParent().getLayoutData();
			formData.height = tbHght;
			areaTItm.getParent().getLayoutData();
			areaTItm.getParent().getParent().layout( true );
//			areaTItm.getParent().setSize( toolBarSize ); // don't need this anymore with changing the layout
		}		
		areaTItm.setText( menuText );
		
		// if truncated then show the menuname in the tooltips		
		areaTItm.setToolTipText( truncated ? seldAreaMenuItem.getMenuName() : "" );
		
   	}
   	
   	// set the area and update the proj/center field
   	// the
   	public void updateAreaGUI() {  		
		
		PredefinedArea area = rbdMngr.getSelectedArea();
		
		setAreaTextOnMenuItem( new AreaName( area.getSource(), area.getAreaName() ) );
		
		geo_area_info_comp.setVisible( false );
		rsc_area_opts_comp.setVisible( false );
			
		if( area.getSource().isImagedBased() ) {

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
		else {
			geo_area_info_comp.setVisible( true );
			
			String projStr = rbdMngr.getSelectedArea().getGridGeometry().
								getCoordinateReferenceSystem().getName().toString();
			
			proj_info_txt.setText( projStr );
			proj_info_txt.setToolTipText( projStr );
			
			// use the GEMPAK name if possible.
			for( String gemProj : gempakProjMap.keySet() ) {
				
				if( gempakProjMap.get( gemProj ).equals( projStr ) ) {
					proj_info_txt.setText( gemProj.toUpperCase() );
					break;
				}
			}

			if( area.getMapCenter() != null ) {
				Integer lat = (int)(area.getMapCenter()[1] *1000.0);
				Integer lon = (int)(area.getMapCenter()[0] *1000.0);
	
				map_center_txt.setText(   
						Double.toString((double)lat/1000.0) +"/" + Double.toString((double)lon/1000.0) );
			}
			else {
				map_center_txt.setText( "N/A" );
			}
		}
		
//		if( seldAreaMenuItem == null ) {
//			try {
//				rbdMngr.setAreaProviderName( 
//						new AreaName( AreaSource.PREDEFINED_AREA, 
//								      rbdMngr.getRbdType().getDefaultMap() ));
//				updateAreaGUI();
//			} catch (VizException e) {
//			}
//   		}   	   		
   	}
   
   	private class SelectAreaAction extends Action {
   		private AreaMenuItem ami;
   		
   		public SelectAreaAction( AreaMenuItem a ) {
   			super( a.getMenuName() );
   			ami = a;
   		}
   		
   	    @Override
   	    public void run() {
   	    	try {
				rbdMngr.setSelectedAreaName( 
						new AreaName( AreaSource.getAreaSource( ami.getSource() ), 
															    ami.getAreaName() ) );
				updateAreaGUI();
			} 
   	    	catch (VizException e) {
   				MessageDialog errDlg = new MessageDialog( 
   					     NcDisplayMngr.getCaveShell(), "Error", null, 
   					     		e.getMessage(), MessageDialog.ERROR, new String[]{"OK"}, 0);
   				errDlg.open();
			}
   	    }
   	}
//   	private void setSeldAreaMenuItem( AreaMenuItem ami ) {
//   		areaTItm.setText(ami.getAreaName());
//   		
//   		seldAreaMenuItem = ami;
//   		
//   	}
   	
   	public void createAvailAreaMenuItems( IMenuManager aMenuMngr ) {
   	// a map from the sub-menu name to a list of menu item 
   		List<List<AreaMenuItem>> availMenuItems = rbdMngr.getAvailAreaMenuItems();

   		for( List<AreaMenuItem> amiList : availMenuItems ) {
   			if( amiList == null || amiList.isEmpty() ) {
   				continue;
   			}
   			
   			// all the submenu name in the list should be the same.
   			String subMenuName = amiList.get(0).getSubMenuName();
   			IMenuManager menuMngrToAddTo = aMenuMngr;
   			
   			if( subMenuName != null && !subMenuName.isEmpty() ) {
   				IMenuManager subMenu  = new MenuManager( subMenuName,
   						areaMenuMngr.getId() + "." + subMenuName);
   				aMenuMngr.add( subMenu );
   				menuMngrToAddTo = subMenu;
   			}
    		
    		for( AreaMenuItem ami : amiList ) {
    			menuMngrToAddTo.add( new SelectAreaAction( ami ) );
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

   		// update the display type combo
   		for( int i=0 ; i<disp_type_combo.getItemCount() ; i++ ) {
   			NcDisplayType dType = NcDisplayType.getDisplayType( disp_type_combo.getItems()[i] );
   			if( dType.equals( rbdMngr.getRbdType() ) ) {
   				disp_type_combo.select( i );   		
   			}
   		}

   		if( disp_type_combo.getSelectionIndex() == -1 ) {
   			disp_type_combo.select( 0 );
   		}
   				
   		rbd_name_txt.setText( rbdMngr.getRbdName() );
   		
   		rbd_name_txt.setSelection(0, rbdMngr.getRbdName().length() );
   		rbd_name_txt.setFocus();

   		import_rbd_combo.deselectAll();
   		
   		for( int i=0 ; i<import_rbd_combo.getItemCount() ; i++ ) {
   			String importRbdName = import_rbd_combo.getItems()[i];
   			importRbdName = NcDisplayName.parseNcDisplayNameString( importRbdName ).getName();
   			if( importRbdName.equals( rbdMngr.getRbdName() ) ) {
   				import_rbd_combo.select( i );   		
   			}
   		}

   		if( import_rbd_combo.getSelectionIndex() == -1 ) {
   			import_rbd_combo.select( import_rbd_combo.getItemCount()-1 );
   		}
   		
   		   		
   		updateAreaGUI( );
   		    	
    	geo_sync_panes.setSelection( rbdMngr.isGeoSyncPanes() );
    	multi_pane_tog.setSelection( rbdMngr.isMultiPane() );
    	
    	updateGUIforMultipane( rbdMngr.isMultiPane() );    	
    	
    	// set the pane sel buttons based on the combos
    	// also sets the rbdMngr's layout
    	if( rbdMngr.isMultiPane() ) {
    		updatePaneLayout();
    	}
    	
    	selectPane( rbdMngr.getSelectedPaneId() );
    	
    	timelineControl.clearTimeline();
    	
    	INcPaneLayout paneLayout = rbdMngr.getPaneLayout();
    	
    	// set the list of available resources for the timeline
    	for( int paneIndx=0 ; paneIndx<paneLayout.getNumberOfPanes() ; paneIndx++ ) {
    		for( ResourceSelection rscSel : 
    			rbdMngr.getRscsForPane( (NcPaneID) paneLayout.createPaneId(paneIndx) ) ) {

    			if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {     		    		
    				timelineControl.addAvailDomResource( 
    						(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
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
   		
   		int colCnt = ((NcPaneLayout)rbdMngr.getPaneLayout()).getColumns();
	   	int rowCnt = ((NcPaneLayout)rbdMngr.getPaneLayout()).getRows();
   		
   	   	for( int r=0 ; r<rbdMngr.getMaxPaneLayout().getRows() ; r++ ) {
   	   		for( int c=0 ; c<rbdMngr.getMaxPaneLayout().getColumns() ; c++ ) {
   	   			pane_sel_btns[r][c].setVisible( r<rowCnt && c<colCnt );   	   		   
   	   		}   			
   		}  		
   	}
   	
   	// update the GUI with the selections (area/list of rscs) for the selected pane
   	private void selectPane( INcPaneID pane ) {
   		
   		NcPaneID seldPane = (NcPaneID)pane;
   		
   		rbdMngr.setSelectedPaneId( seldPane );
   		
   		
   		if( rbdMngr.isMultiPane() ) {
   			seld_rscs_grp.setText("Selected Resources for Pane "+seldPane.toString() );
   		}
   		else {
   			seld_rscs_grp.setText("Selected Resources" );
   		}

   		// implement radio behaviour
    	for( int r=0 ; r<rbdMngr.getMaxPaneLayout().getRows() ; r++ ) {    
    		for( int c=0 ; c<rbdMngr.getMaxPaneLayout().getColumns() ; c++ ) { 
    			pane_sel_btns[r][c].setSelection( 
    					(r==seldPane.getRow() && c==seldPane.getColumn()) );
    		}
    	}
    	
    	updateAreaGUI( );

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
		INcPaneLayout savePaneLayout = rbdMngr.getPaneLayout();
		INcPaneID     saveSeldPane   = rbdMngr.getSelectedPaneId();
		
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

		NcDisplayType curDispType = rbdMngr.getRbdType();
			
		try {
			AbstractRBD<?> dfltRbd = NcMapRBD.getDefaultRBD( curDispType );
			
			rbdMngr.initFromRbdBundle( dfltRbd );
			
		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( 
					shell, "Error", null, 
					e.getMessage(), MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
			
			rbdMngr.init( curDispType );  	
		}
		
		updateGUI();
	}
	
   	// reset the ListViewer's input and update all of the buttons
   	// TODO; get the currently selected resources and reselect them.
   	private void updateSelectedResourcesView( boolean updateList ) {	
   	
   		if( updateList ) {
   			
   			StructuredSelection orig_sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   			List<ResourceSelection> origSeldRscsList = 
   				               (List<ResourceSelection>)orig_sel_elems.toList();
   	
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
		
   		StructuredSelection sel_elems = (StructuredSelection) seld_rscs_lviewer.getSelection();               
   		List<ResourceSelection> seldRscsList = (List<ResourceSelection>) sel_elems.toList();

   		// Can't delete, replace or turn off the base overlay.
   		//
   		Boolean isBaseLevelRscSeld = false;
   		Boolean allRscsAreVisible = true;
   		
   		for( ResourceSelection rscSel : seldRscsList ) {
   			isBaseLevelRscSeld |= rscSel.isBaseLevelResource();
   		
   			allRscsAreVisible &= rscSel.isVisible();
   		}
   		
   		// the replace button is enabled if there is only 1 resource selected and
   		// it is not the base resource
   		rscSelDlg.setReplaceEnabled( (numSeldRscs == 1 && !isBaseLevelRscSeld) );
   		
		// the delete button is only disabled if there is one the one base resource selected.
   		//
   		del_rsc_btn.setEnabled( numSeldRscs > 1 || (numSeldRscs == 1 && !isBaseLevelRscSeld) );

   		// the disable_rsc_btn is always enabled.
   		disable_rsc_btn.setEnabled( (numSeldRscs > 0) );
   		
   		//but the state is  
  		if( allRscsAreVisible ) {
  			disable_rsc_btn.setSelection( false );
  			disable_rsc_btn.setText( "Turn Off");   	   				
  		}
  		else {
  			disable_rsc_btn.setSelection( true );
  			disable_rsc_btn.setText( "Turn On");
  		}		
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
    		
    		AbstractRBD<?> rbdBndl = rbdMngr.createRbdBundle( rbdName,
    				                                     timelineControl.getTimeMatcher() );
    		
    		rbdBndl = AbstractRBD.clone( rbdBndl );
    		rbdBndl.resolveDominantResource();
    		    
    		ResourceBndlLoader rbdLoader = null;
    		rbdLoader = new ResourceBndlLoader("RBD Previewer");

    		// TODO : Allow the user to define preferences such as 
    		// whether to prompt when re-loading into an existing editor,
    		// whether to look for an empty editor first before re-loading an exising one...
    		//
    		// Determine which Display Editor to use.
    		//
    		// 1-check if the name of the active editor is the same as the RBD and
    		//   if so, re-load into this editor.
    		// 2-if the RBD was imported then look specifically for this display ID.
    		// 3-if no rbd name was given it will be called "Preview" and look for an
    		//   existing "Preview" editor.
    		// 4-
    		// First look for a preview editor and if not found then check if the 
    		// active editor name matches that of the current rbd name (Note: don't
    		// look for an editor matching the rbd name because it is possible to
    		// have the same named display for a different RBD.)
    		// If the active editor doesn't match then load into a new display editor
    		AbstractEditor editor=null;

    		// 1-if the active editor matches this rbdName, use the active editor
    		// Don't attempt to find an editor based just on the display name since 
    		// they may not be unique without the display id.
    		if( editor == null  ) {    			
    			editor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    			if( !NcEditorUtil.getDisplayName( editor ).getName().equals( rbdName ) ) {
    				editor = null;
    			}
        		// if they changed the layout then we should be able to modify the layout of the current
        		/// display but for now just punt and get a new editor.
    			else if( NcEditorUtil.getPaneLayout( editor ).compare( rbdBndl.getPaneLayout() ) != 0 ) {
        			System.out.println("Creating new Editor because the paneLayout differs.");
        			editor = null;
        		}
    		}

    		// 2- look for and available editor that has not been modified. 
    		//    This is usually the initial 'default' editor but could be one that has been
    		//    cleared/wiped, or one from the New Display menu.
    		if( editor == null  ) {    			

    			editor = NcDisplayMngr.findUnmodifiedDisplayToLoad( rbdBndl.getDisplayType() );
    			if( editor != null &&
    				NcEditorUtil.getPaneLayout( editor ).compare( rbdBndl.getPaneLayout() ) != 0 ) {
        			System.out.println("Creating new Editor because the paneLayout differs.");
        			editor = null;
        		}
//    			String name = NcEditorUtil.getDisplayName(editor).getName();
//    			NcEditorUtil.setDisplayName(
//    					editor, name.substring(0, name.indexOf("-")+1) + rbdName);
    		}
    		
    		// 3- if the rbd was imported from a display and the name was not changed 
    		//    get this display and attempt to use it.
    		
    		if( editor == null ) {
    			NcDisplayName importDisplayName = 
    				NcDisplayName.parseNcDisplayNameString( import_rbd_combo.getText() );
    			
    			if( importDisplayName.getName().equals( rbdName ) ) {
    				// get by ID since the rbd name doesn't have to be unique
    				editor = NcDisplayMngr.findDisplayByID( 
    						rbdBndl.getDisplayType(), importDisplayName );    				

    				if( editor != null &&
    					NcEditorUtil.getPaneLayout( editor ).compare( rbdBndl.getPaneLayout() ) != 0 ) {
    					editor = null;
    				}
    			}
    		}    			
    		
    		// 4-reuse if it is a Preview editor.
    		//
    		if( editor == null && 
    			rbdName.equals("Preview") ) {
    			editor = NcDisplayMngr.findDisplayByName( rbdBndl.getDisplayType(), rbdName );
    			
    			if( editor != null &&
    				NcEditorUtil.getPaneLayout( editor ).compare( rbdBndl.getPaneLayout() ) != 0 ) {
    				editor = null;
    			}
    		}
    		
    		// 5-Create a new one.
    		//    Note that it is possible for a display name matching the RBD name to not be reused
    		//    if the display is not active. This is because the names may not be unique and so 
    		//    we can't determine which unless it was specifically imported into the Manager and
    		//    even in this case they may not want to re-load it.
    		//
    		if( editor == null ) {			
    			editor = NcDisplayMngr.createNatlCntrsEditor( 
    					rbdBndl.getDisplayType(), rbdName, rbdBndl.getPaneLayout() );
    		}

    		if( editor == null ) {
    			throw new VizException("Unable to create a display to load the RBD.");
    		}
    		
			NcDisplayMngr.bringToTop(editor);
    		
			//
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
    		
    		AbstractRBD<?> rbdBndl = rbdMngr.createRbdBundle( rbdName,
    				                                     timelineControl.getTimeMatcher() );
    		rbdBndl = AbstractRBD.clone( rbdBndl );
    
    		ResourceBndlLoader rbdLoader = null;
    		rbdLoader = new ResourceBndlLoader("RBD Previewer");

    		rbdLoader.setLoadSelectedPaneOnly();
    		
    		// Get the Display Editor to use. If the active editor doesn't 
    		// match the name of the RBD then prompt the user.
    		AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();

    		// Check that the selected pane is within the layout of the Display
    		if( editor == null ) {
    			throw new VizException("Error retrieving the Active Display??");
    		}
    		
    		String activeEditorName = NcEditorUtil.getDisplayName( editor ).getName();
    		    		
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
    		if( !NcEditorUtil.getPaneLayout( editor ).equals( rbdBndl.getPaneLayout() ) ) {
    			MessageDialog msgDlg = new MessageDialog( 
    					shell, "Load Pane", null, 
    					"The pane layout of the display doesn't match the currently selected\n"+
    					"RBD pane layout. You will first need to Load the RBD before\n"+
    					"changing the number of panes.",
    					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
    			msgDlg.open();
         		
    			return;
			}

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
    	AbstractRBD<?> impRbd;
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
    		// get NcMapRBD from selected display
    		AbstractEditor seldEditor = NcDisplayMngr.findDisplayByID( 
    				NcDisplayName.parseNcDisplayNameString( seldDisplayName ));
    		
    		if( seldEditor == null ) {
    			System.out.println("Unable to load Display :"+seldDisplayName.toString() );
    			return;
    		}
    		
			try {
				impRbd = AbstractRBD.createRbdFromEditor( seldEditor );

//    			impRbd.initFromEditor(seldEditor);
    			
				impRbd = AbstractRBD.clone( impRbd );
			} catch (VizException e) {
				MessageDialog errDlg = new MessageDialog( 
						shell, "Error", null, 
						"Error Importing Rbd from Display, "+seldDisplayName.toString()+".\n"+e.getMessage(),
						MessageDialog.ERROR, new String[]{"OK"}, 0);
				errDlg.open();
				return;
			}
    		
    		NcDisplayMngr.bringToTop(seldEditor);
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
    	
    	NcDisplayType curDispType = rbdMngr.getRbdType();
    	
    	try {
    		rbdMngr.initFromRbdBundle( impRbd );
    	}
    	catch( VizException e ) {
    		rbdMngr.init( curDispType );
    		
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
    public void	importPane( AbstractRBD<?> rbdBndl, INcPaneID paneId  ) throws VizException {

    	if( rbdBndl.getDisplayType() != rbdMngr.getRbdType() ) {
    		throw new VizException("Can't import a non-matching display type.");// sanity check
    	}
    	
		clearSeldResources();

		rbdMngr.setPaneData( rbdBndl.getDisplayPane(paneId) );
		
		for( ResourceSelection rscSel : rbdMngr.getRscsForPane( rbdMngr.getSelectedPaneId() ) ) {
			if( rscSel.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData ) {     		    		
				timelineControl.addAvailDomResource( 
					(AbstractNatlCntrsRequestableResourceData) rscSel.getResourceData() );
			}
		}

    	timelineControl.setTimeMatcher( rbdBndl.getTimeMatcher() );
    	
    	// TODO : if geo sync is set and if this geogArea is different than the
    	// current one then pro`mpt the user what to do.
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
//    			System.out.println("NcMapRBD: Area is not set in pane???");
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
    	
    	updateAreaGUI( );
    	
    	updateSelectedResourcesView( true );
    }    
    
    public void updateGUIforMultipane( boolean isMultiPane ) {    	
    	FormData fd = new FormData();

		geo_sync_panes.setVisible( isMultiPane );

		pane_layout_grp.setVisible( isMultiPane );
    	
    	if( isMultiPane ) {
    		fd.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
//        	fd.left = new FormAttachment( 30, 2 );
//    		fd.top  = new FormAttachment( geo_area_grp, 0, SWT.TOP );
        	fd.top  = new FormAttachment( geo_sync_panes, 10, SWT.BOTTOM );
    		fd.bottom = new FormAttachment( geo_area_grp, 0, SWT.BOTTOM );
    		fd.right = new FormAttachment( 100, -300 );
    		seld_rscs_grp.setLayoutData( fd );

    		shell.setSize( new Point( multiPaneDlgWidth, shell.getSize().y ) );
    	}
    	else {
        	fd.left = new FormAttachment( geo_area_grp, 10, SWT.RIGHT );
//        	fd.left = new FormAttachment( 30, 2 );
//        	fd.top  = new FormAttachment( geo_area_grp, 0, SWT.TOP );
        	fd.top  = new FormAttachment( auto_update_btn, 5, SWT.BOTTOM );
        	fd.right = new FormAttachment( 100, -10 );
        	fd.bottom = new FormAttachment( geo_area_grp, 0, SWT.BOTTOM );
        	seld_rscs_grp.setLayoutData( fd );

        	shell.setSize( new Point( singlePaneDlgWidth, shell.getSize().y ) );
    	}
    	
    	// the area name may be truncated based on a shorter toolbar widget
    	// reset it now that it is wider.
		PredefinedArea area = rbdMngr.getSelectedArea();		
		setAreaTextOnMenuItem( new AreaName( area.getSource(), area.getAreaName() ) );
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

    		AbstractRBD<?> rbdBndl = rbdMngr.createRbdBundle( saveDlg.getSeldRbdName(), timeMatcher );

//    		if( !checkAndSavePredefinedAreas() ) {
//    			return;
//    		}
    		
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
    
//		 check to see if any of the selected areas are defined by another display and if so
//		 prompt the user to save them to a file before saving the RBD. (if we don't do this the
//		 area can still be saved but there is a problem of what the areaSource will be in this case. It
//		 could be INITIAL_DISPLAY_AREA but the factory for this area source is currently designed to only
//		 look for loaded displays and not displays that are saved in an RBD which is what the case will 
//		 be if this RBD is imported into the ResourceManager again. There are other possible ways to fix 
//		 this but the most straightforward for now is to just require the user to save the area to a file
//		 if 
//    private Boolean checkAndSavePredefinedAreas( ) {
//
//    	Map<String,AreaName> seldAreas = rbdMngr.getAllSelectedAreaNames();
//    	String confirmMsg = "";
//    	List<String> pids = new ArrayList<String>(seldAreas.keySet());
//    	
//    	for( String pid : pids ) {
//    		if( seldAreas.get( pid ).getSource() != AreaSource.INITIAL_DISPLAY_AREA ) {
//    			seldAreas.remove( pid );
//    		}
//    	}
//    	
//    	if( seldAreas.isEmpty() ) {
//    		return true;
//    	}
//    	
//    	if( !rbdMngr.isMultiPane() ) {
//    		if( seldAreas.)
//	    		MessageDialog confirmDlg = new MessageDialog( 
//		    				shell, "Confirm", null, 
//		    				"This RBD has been modified.\n\n"+
//		    				"Do you want to clear the current RBD selections?",
//		    				MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
//		    		confirmDlg.open();
//
//		    		if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
//		    			return;
//		    		}           				   			    		
//
//    	}
//    	// if geoSynced just check the first 
//    	if( rbdMngr.isGeoSyncPanes() ) {
//    		
//    	}
//    }
                
    // if Editing then save the current rbd to an AbstractRBD<?> 
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
    
    public AbstractRBD<?> getEditedRbd () {
		return editedRbd;
    }    
}