package gov.noaa.nws.ncep.viz.tools.plotModelMngr;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Plot Model Manager dialog.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#         Engineer        Description
 * ------------ ----------      -----------     --------------------------
 * Nov. 2009                            M. Li           initial creation
 * Dec. 2009     217        Greg Hull   Get Categories from resources dir and 
 *                                      read from individual plotModel files.
 * Feb. 2010     226        Greg Hull   NmapCommon -> NmapResourceUtils
 * Mar  2011     425        Greg Hull   categories are now the plotData plugins;   
 *               					    add a Delete and Save As button
 * Jan 2012                 S. Gurung   Changed resource parameter name plugin to pluginName in init().
 * Mar 2012      606        Greg Hull   now have 3 Plot resource Implementations. 
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PlotModelMngrDialog extends Dialog { 
	private Shell shell;
	private Font font;

	private List pluginNameList = null;
	private String seldPlugin = null;

	private List plotModelList = null;

	private Button editPlotModelBtn = null;
	private Button deletePlotModelBtn = null;

//	private PlotModelMngr plotModelMngr = null;

	private HashMap<String,PlotModel> plotModels = null;

	private EditPlotModelDialog  editPlotModelDlg = null;

	// the resource ext point mngr is used to get a list of all the
	// resources that have a plotModel attribute and then we will
	// get the plugin from the resource name
	protected ResourceExtPointMngr rscExtPointMngr = null;

	public PlotModelMngrDialog(Shell parent) {
		super(parent);
//		plotModelMngr = PlotModelMngr.getInstance();
		rscExtPointMngr = ResourceExtPointMngr.getInstance();
	}

	public Object open() {
		Shell parent = getParent();
		Display display = parent.getDisplay();
		shell = new Shell( parent, SWT.SHELL_TRIM | SWT.MODELESS);//SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL);
		shell.setText("Plot Model Manager");

		// Create the main layout for the shell.
		FormLayout mainLayout = new FormLayout();
		shell.setLayout(mainLayout);
		shell.setLocation( parent.getLocation().x+10, parent.getLocation().y+10);

		font = new Font(shell.getDisplay(), "Monospace", 10, SWT.BOLD);

		// create the controls and layouts
		createMainWindow();
		init();

		shell.setMinimumSize(100,300);
		shell.pack();

		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		font.dispose();

		return null;
	}

	/**
	 * closes the Dialog
	 */
	public void close() {
		if ( shell != null ) shell.dispose();
	}

	/**
	 * Create the dialog components.
	 */
	private void createMainWindow() {
		Composite topComp = new Composite(shell, SWT.NONE);
		FormData fd = new FormData();
		fd.top = new FormAttachment(0, 0);
		fd.left = new FormAttachment(0, 0);
		fd.right = new FormAttachment(100, 0);
		fd.bottom = new FormAttachment(100, 0);
		topComp.setLayoutData( fd );

		topComp.setLayout( new FormLayout() );

		pluginNameList = new List(topComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
		fd = new FormData( );
		fd.height = 100;
		fd.width  = 200;
		fd.top = new FormAttachment( 0, 35  );
		fd.left  = new FormAttachment( 0, 15 );
		fd.right  = new FormAttachment( 100, -15 );
		pluginNameList.setLayoutData( fd );

		pluginNameList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				loadPlotModelsList();
			}
		});

		Label catLbl = new Label( topComp, SWT.NONE);
		catLbl.setText("Plot Model Category");
		fd = new FormData();
		fd.left = new FormAttachment( pluginNameList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( pluginNameList, -3, SWT.TOP );
		catLbl.setLayoutData( fd );

		plotModelList = new List(topComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
		fd = new FormData();
		fd.height = 250;
		fd.top = new FormAttachment( pluginNameList, 40, SWT.BOTTOM  );
		fd.left = new FormAttachment( pluginNameList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( 100, -140 );
		fd.bottom = new FormAttachment( 100, -100 );
		fd.right  = new FormAttachment( 100, -15 );
		plotModelList.setLayoutData(fd);

		plotModelList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				if (plotModelList.getSelectionCount() > 0) {
					editPlotModelBtn.setEnabled(true);
					deletePlotModelBtn.setEnabled(false);

					String plotModelName = plotModelList.getSelection()[0];
					
					// if this plotModel is in the USER context
					// then allow the user to delete it.
					PlotModel pm = PlotModelMngr.getInstance().getPlotModel( 
								seldPlugin, plotModelName );
					
					if( pm != null && 
						pm.getLocalizationFile().getContext().getLocalizationLevel() 
						             == LocalizationLevel.USER ) {
						deletePlotModelBtn.setEnabled(true);
					}
				}
			}
		});

		plotModelList.addListener( SWT.MouseDoubleClick, new Listener() {
			public void handleEvent(Event event) {
				editPlotModel();
			}
		});

		Label pmLbl = new Label( topComp, SWT.NONE);
		pmLbl.setText("Plot Models");
		fd = new FormData();
		fd.left = new FormAttachment( plotModelList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( plotModelList, -3, SWT.TOP );
		pmLbl.setLayoutData( fd );

		editPlotModelBtn = new Button(topComp, SWT.PUSH);
		editPlotModelBtn.setText(" Edit...");
		fd = new FormData();
		fd.width = 80;
		fd.left = new FormAttachment( 30, -40 );
		fd.top = new FormAttachment( plotModelList, 10, SWT.BOTTOM );
		editPlotModelBtn.setLayoutData( fd );

		editPlotModelBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				editPlotModel();
			}
		});        


		deletePlotModelBtn = new Button(topComp, SWT.PUSH);
		deletePlotModelBtn.setText(" Delete ");
		fd = new FormData();
		fd.width = 80;
		fd.left = new FormAttachment( 70, -40 );
		fd.top = new FormAttachment( plotModelList, 10, SWT.BOTTOM );
		deletePlotModelBtn.setLayoutData( fd );

		deletePlotModelBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				deletePlotModel();
			}
		});        

		Label sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
		fd = new FormData();
		fd.left = new FormAttachment( 0, 2 );
		fd.right = new FormAttachment( 100, -2 );
		fd.bottom = new FormAttachment( 100, -45 );
		sepLbl.setLayoutData(fd);

		Button closeBtn = new Button(topComp, SWT.PUSH);
		closeBtn.setText(" Close  ");
		fd = new FormData();
		fd.right = new FormAttachment( 100, -20);
		fd.top = new FormAttachment( sepLbl, 10, SWT.BOTTOM);
		//      fd.bottom = new FormAttachment( 100, -5 );
		closeBtn.setLayoutData(fd);

		closeBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				shell.dispose();
			}
		});
	}

	private void editPlotModel() {
		String plotModelName = plotModelList.getSelection()[0];
		if( plotModelName == null ) {
			return; // nothing selected; sanity check
		}

		PlotModel pm = null;

		if( plotModels.containsKey( plotModelName ) ) {
			pm = new PlotModel( plotModels.get( plotModelName ) );
		}
		else {
			pm = new PlotModel();
			pm.setName( plotModelName );
			pm.setPlugin( seldPlugin );
			pm.setSvgTemplate( PlotModelMngr.getInstance().getDefaultSvgTemplate() );
			pm.getAllPlotModelElements(); // create the list of elements
		}

		editPlotModelDlg = new EditPlotModelDialog( shell, pm );
		
		PlotModel newPlotModel = (PlotModel)editPlotModelDlg.open( 
						shell.getLocation().x + shell.getSize().x + 10, 
					      shell.getLocation().y );

		
		if( newPlotModel != null ) {
			
			// create a LocalizationFile 
			try {
				PlotModelMngr.getInstance().savePlotModel( newPlotModel );

				plotModels.put( plotModelName, newPlotModel );

				loadPlotModelsList();
			}
			catch ( VizException ve ) {
	    		MessageDialog errDlg = new MessageDialog( 
	    				NcDisplayMngr.getCaveShell(), 
	    				"Error", null, 
	    				"Error Saving Plot Model "+plotModelName+ ".\n\n"+
	    				ve.getMessage(),
	    				MessageDialog.ERROR, new String[]{"OK"}, 0);
	    		errDlg.open();
			}
		}
	}

	//
	private void deletePlotModel() {
		String plotModelName = plotModelList.getSelection()[0];
		if( plotModelName == null ) {
			return; // nothing selected; sanity check
		}

		// TODO : get a list of all the attribute sets that refer to this plotModel
		// and tell the user to edit attribute sets 
		try {
			ArrayList<String> pltRscAttrSets = 
				ResourceDefnsMngr.getInstance().getAvailAttrSetsForRscImpl("SurfacePlot");
			pltRscAttrSets.addAll( 
					ResourceDefnsMngr.getInstance().getAvailAttrSetsForRscImpl("UpperAirPlot") );
			pltRscAttrSets.addAll( 
					ResourceDefnsMngr.getInstance().getAvailAttrSetsForRscImpl("MosPlot") );
			
			// don't delete BASE/SITE/DESK level plot models
			PlotModel pm = PlotModelMngr.getInstance().getPlotModel( seldPlugin, plotModelName );
			if( pm == null ) {
				throw new VizException();
			}
			
			MessageDialog confirmDlg = new MessageDialog(shell, "Confirm Delete", null, 
					"Are you sure you want to delete the "+plotModelName+"?\n",
					MessageDialog.QUESTION, 
					new String[]{"Yes", "No"}, 0);
			confirmDlg.open();

			if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
				return;
			}

    		PlotModelMngr.getInstance().deletePlotModel( seldPlugin, plotModelName );
    	}
    	catch( VizException e ) {
    		MessageDialog errDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Error", null, 
    				"Error Deleting Plot Model "+plotModelName+ ".\n\n"+
    				e.getMessage(),
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    	}
    	
    	loadPlotModelsList();
	}

	private void init() {        
		pluginNameList.removeAll();

		// the Categories are the plugins for the PlotData implementations.	
		
		// get a list of all the ResourceDefinitions with the "PlotData" resource implementation.
		// the plugin name will be one of the resource parameters.
		//
		try {
			ArrayList<String> plotDataPlugins = new ArrayList<String>();
			
			ArrayList<String> plotRscDefns =
				ResourceDefnsMngr.getInstance().getRscTypesForRscImplementation("SurfacePlot");
			plotRscDefns.addAll( 
					ResourceDefnsMngr.getInstance().getRscTypesForRscImplementation("UpperAirPlot") );
			plotRscDefns.addAll( 
					ResourceDefnsMngr.getInstance().getRscTypesForRscImplementation("MosPlot") );
			
			for( String rscType : plotRscDefns ) {
				ResourceDefinition rscDefn = 
					ResourceDefnsMngr.getInstance().getResourceDefinition( rscType );
				if( rscDefn != null ) {
					String pluginName = rscDefn.getPluginName();
					if( pluginName != null && !plotDataPlugins.contains( pluginName ) ) { 
						plotDataPlugins.add( pluginName );
						pluginNameList.add( pluginName );
					}
				}
			}
		} catch (VizException e) {
			System.out.println("Error getting list of PlotData Plugins:"+e.getMessage() );
		}

		if( pluginNameList.getItemCount() > 0 ) {
			pluginNameList.select(0);
			loadPlotModelsList();
		}

		editPlotModelBtn.setEnabled(false);
		deletePlotModelBtn.setEnabled(false);
	}

	// this will load all of the prm files for all resources currently defined
	// in the selected resource type(category). If there is not a plot model defined
	// with the category and name of the prm file 
	private void loadPlotModelsList() {
		if( pluginNameList.getSelectionCount() > 0) {
			plotModelList.removeAll();

			seldPlugin = pluginNameList.getSelection()[0];
			
			plotModels = PlotModelMngr.getInstance().getPlotModelsByPlugin( 
					seldPlugin  );
			
			for( PlotModel pltMdl : plotModels.values() ) {
				plotModelList.add( pltMdl.getName() );
			}

			editPlotModelBtn.setEnabled(false);
			deletePlotModelBtn.setEnabled(false);
		}
	}

	public boolean isOpen() {
		return shell != null && !shell.isDisposed();
	}
} 