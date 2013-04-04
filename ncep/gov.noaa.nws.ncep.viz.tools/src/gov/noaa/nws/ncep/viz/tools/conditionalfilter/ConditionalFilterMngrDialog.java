package gov.noaa.nws.ncep.viz.tools.conditionalfilter;

import gov.noaa.nws.ncep.viz.common.ui.UserEntryDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
 
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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilterElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilterMngr;

/**
 * Conditional Filter Manager dialog.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#         Engineer        Description
 * ------------ ----------      -----------     --------------------------
 * April 2012   #615             S. Gurung       Initial Creation
 * April 2012   #606             G. Hull         get 3 plot resource implementations.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class ConditionalFilterMngrDialog extends Dialog { 
	
	private Shell shell;
	private Font font;

	private List pluginNameList = null;
	private String seldPlugin = null;

	private List condFilterList = null;

	private Button newCondFilterBtn = null;
	private Button copyCondFilterBtn = null;
	private Button editCondFilterBtn = null;
	private Button deleteCondFilterBtn = null;

	private HashMap<String,ConditionalFilter> condFilters = null;

	private EditConditionalFilterDialog  editConditionalFilterDlg = null;

	// the resource ext point mngr is used to get a list of all the
	// resources that have a conditionalFilterName attribute and then we will
	// get the plugin from the resource name
	protected ResourceExtPointMngr rscExtPointMngr = null;

	public ConditionalFilterMngrDialog(Shell parent) {
		super(parent);
		rscExtPointMngr = ResourceExtPointMngr.getInstance();
	}

	public Object open() {
		Shell parent = getParent();
		Display display = parent.getDisplay();
		shell = new Shell( parent, SWT.SHELL_TRIM | SWT.MODELESS);//SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL);
		shell.setText("Conditional Filter Manager");

		// Create the main layout for the shell.
		FormLayout mainLayout = new FormLayout();
		shell.setLayout(mainLayout);
		shell.setLocation( parent.getLocation().x+10, parent.getLocation().y+10);

		font = new Font(shell.getDisplay(), "Monospace", 10, SWT.BOLD);

		// create the controls and layouts
		createMainWindow();
		init();

		shell.setMinimumSize(150,300);
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
		fd.height = 230;
		fd.width  = 250;
		fd.top = new FormAttachment( 0, 35  );
		fd.left  = new FormAttachment( 0, 15 );
		fd.right  = new FormAttachment( 100, -15 );
		pluginNameList.setLayoutData( fd );

		pluginNameList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				loadConditionalFiltersList();
			}
		});

		Label catLbl = new Label( topComp, SWT.NONE);
		catLbl.setText("Plot Model Category");
		fd = new FormData();
		fd.left = new FormAttachment( pluginNameList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( pluginNameList, -3, SWT.TOP );
		catLbl.setLayoutData( fd );

		condFilterList = new List(topComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
		fd = new FormData();
		fd.height = 250;
		fd.top = new FormAttachment( pluginNameList, 40, SWT.BOTTOM  );
		fd.left = new FormAttachment( pluginNameList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( 100, -140 );
		fd.bottom = new FormAttachment( 100, -100 );
		fd.right  = new FormAttachment( 100, -15 );
		condFilterList.setLayoutData(fd);

		condFilterList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				
				if (condFilterList.getSelectionCount() == 1) {
					copyCondFilterBtn.setEnabled(true);
					editCondFilterBtn.setEnabled(true);

					String condFilterName = condFilterList.getSelection()[0];
					
					if( condFilterName.equals( ConditionalFilterMngr.NullFilterName ) ) {
						editCondFilterBtn.setEnabled(false);						
						copyCondFilterBtn.setEnabled(false);
					}
					else {
						copyCondFilterBtn.setEnabled(true);
						editCondFilterBtn.setEnabled(true);
					}
					
					// if this condFilter is in the USER context
					// then allow the user to delete it.
					ConditionalFilter cf = ConditionalFilterMngr.getInstance().getConditionalFilter( 
								seldPlugin, condFilterName );
					
					if( cf != null && 
						cf.getLocalizationFile().getContext().getLocalizationLevel() 
						             == LocalizationLevel.USER ) {
						deleteCondFilterBtn.setEnabled(true);
					}
					else {
						deleteCondFilterBtn.setEnabled(false);						
					}
				}
				else {
					copyCondFilterBtn.setEnabled(false);
					editCondFilterBtn.setEnabled(false);
					deleteCondFilterBtn.setEnabled(false);						
				}
			}
		});

		condFilterList.addListener( SWT.MouseDoubleClick, new Listener() {
			public void handleEvent(Event event) {
				editConditionalFilter(false);
			}
		});

		Label pmLbl = new Label( topComp, SWT.NONE);
		pmLbl.setText("Conditional Filters");
		fd = new FormData();
		fd.left = new FormAttachment( condFilterList, 0, SWT.LEFT );
		fd.bottom = new FormAttachment( condFilterList, -3, SWT.TOP );
		pmLbl.setLayoutData( fd );

		
		newCondFilterBtn = new Button(topComp, SWT.PUSH);
		newCondFilterBtn.setText(" New... ");
		fd = new FormData();
		fd.width = 60;
		fd.left = new FormAttachment( 20, -30 );
		fd.top = new FormAttachment( condFilterList, 10, SWT.BOTTOM );
		newCondFilterBtn.setLayoutData( fd );

		newCondFilterBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				
				editConditionalFilter(true);    
			}
		});      
				
		copyCondFilterBtn = new Button(topComp, SWT.PUSH);
		copyCondFilterBtn.setText(" Copy... ");
		fd = new FormData();
		fd.width = 60;
		fd.left = new FormAttachment( 40, -30 );
		fd.top = new FormAttachment( condFilterList, 10, SWT.BOTTOM );
		copyCondFilterBtn.setLayoutData( fd );

		copyCondFilterBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				
				String fromCondFilterName = condFilterList.getSelection()[0];
				// pop up a dialog to prompt for the new name
                UserEntryDialog entryDlg = new UserEntryDialog( shell,
                		"Copy", 
                		"Conditional Filter Name:", "CopyOf" + fromCondFilterName);
                String newCondFilterName = entryDlg.open();
                
                if( newCondFilterName == null || // cancel pressed
                	newCondFilterName.isEmpty() ) {
                	return;
                }

                // if this condtionalFilter already exists, display a message
                if( condFilters.containsKey( newCondFilterName )) {

                	MessageDialog infoDlg = new MessageDialog(shell, "Message", null, 
                			"A '"+newCondFilterName+"' Conditional Filter already exists for this plugin.",
                			MessageDialog.INFORMATION, 
                			new String[]{" OK "}, 0);
                	infoDlg.open();

                	return;
                }
//                else if (ConditionalFilterMngr.conditionalFilterFileExists(newCondFilterName)) {
//                	MessageDialog infoDlg = new MessageDialog(shell, "Message", null, 
//                			"A '"+newCondFilterName+"' Conditional Filter already exists for another plugin. Please enter a different name.",
//                			MessageDialog.INFORMATION, 
//                			new String[]{" OK "}, 0);
//                	infoDlg.open();
//
//                	return;
//                }
                
                copyConditionalFilter(fromCondFilterName, newCondFilterName);
			}
		});  
		
		editCondFilterBtn = new Button(topComp, SWT.PUSH);
		editCondFilterBtn.setText(" Edit... ");
		fd = new FormData();
		fd.width = 60;
		fd.left = new FormAttachment( 60, -30 );
		fd.top = new FormAttachment( condFilterList, 10, SWT.BOTTOM );
		editCondFilterBtn.setLayoutData( fd );

		editCondFilterBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				editConditionalFilter(false);
			}
		});        

		deleteCondFilterBtn = new Button(topComp, SWT.PUSH);
		deleteCondFilterBtn.setText(" Delete ");
		fd = new FormData();
		fd.width = 60;
		fd.left = new FormAttachment( 80, -30 );
		fd.top = new FormAttachment( condFilterList, 10, SWT.BOTTOM );
		deleteCondFilterBtn.setLayoutData( fd );

		deleteCondFilterBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				deleteConditionalFilter();
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
		closeBtn.setLayoutData(fd);

		closeBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				shell.dispose();
			}
		});
	}
	
	private void copyConditionalFilter(String fromCondFilterName, String toCondFilterName) {
		if( fromCondFilterName == null || toCondFilterName == null ) {
			return; // nothing selected; sanity check
		}

		ConditionalFilter cf = null;

		if( condFilters.containsKey( fromCondFilterName ) ) {
			cf = new ConditionalFilter( condFilters.get( fromCondFilterName ) );
			cf.setName(toCondFilterName);
			
			// create a LocalizationFile 
			try {
				ConditionalFilterMngr.getInstance().saveConditionalFilter( cf );

				condFilters.put( fromCondFilterName, cf );

				loadConditionalFiltersList();
			}
			catch ( VizException ve ) {
	    		MessageDialog errDlg = new MessageDialog( 
	    				NmapUiUtils.getCaveShell(), 
	    				"Error", null, 
	    				"Error Saving Conditional Filter "+fromCondFilterName+ ".\n\n"+
	    				ve.getMessage(),
	    				MessageDialog.ERROR, new String[]{"OK"}, 0);
	    		errDlg.open();
			}
		}
	}
	
	private void editConditionalFilter(boolean isNew) {
		String condFilterName = "";
		if (!isNew ) {
			condFilterName = condFilterList.getSelection()[0];
			
			if( condFilterName.equals( ConditionalFilterMngr.NullFilterName ) ) {
				MessageDialog errDlg = new MessageDialog( 
	    				NmapUiUtils.getCaveShell(), 
	    				"Error", null, 
	    				"Can't edit the Null Conditional Filter "+ ".\n\n",
	    				MessageDialog.ERROR, new String[]{"OK"}, 0);
	    		errDlg.open();
	    		return;
			}
		}
		
		if( condFilterName == null ) {
			return; // nothing selected; sanity check
		}

		ConditionalFilter cf = null;

		if( !isNew && condFilters.containsKey( condFilterName ) ) {
			cf = new ConditionalFilter( condFilters.get( condFilterName ) );
		}
		else {
			cf = new ConditionalFilter();
			cf.setName( condFilterName );
			cf.setPlugin( seldPlugin );
			cf.setDescription( "" );
			cf.getConditionalFilterElements().add(new ConditionalFilterElement()); // create the list of elements			
		}

		editConditionalFilterDlg = new EditConditionalFilterDialog( shell, cf );
		
		ConditionalFilter newConditionalFilter = (ConditionalFilter)editConditionalFilterDlg.open( 
						shell.getLocation().x + shell.getSize().x/2,  shell.getLocation().y );
		
		if( newConditionalFilter != null ) {
			// create a LocalizationFile 
			try {
				ConditionalFilterMngr.getInstance().saveConditionalFilter( newConditionalFilter );

				condFilters.put( condFilterName, newConditionalFilter );

				loadConditionalFiltersList();
			}
			catch ( VizException ve ) {
	    		MessageDialog errDlg = new MessageDialog( 
	    				NmapUiUtils.getCaveShell(), 
	    				"Error", null, 
	    				"Error Saving Conditional Filter "+condFilterName+ ".\n\n"+
	    				ve.getMessage(),
	    				MessageDialog.ERROR, new String[]{"OK"}, 0);
	    		errDlg.open();
			}
		}
	}

	private void deleteConditionalFilter() {
		String condFilterName = condFilterList.getSelection()[0];
		if( condFilterName == null ) {
			return; // nothing selected; sanity check
		}

		// TODO : get a list of all the attribute sets that refer to this condFilter
		// and tell the user to edit attribute sets 
		try {
			
			// don't delete BASE/SITE/DESK level Conditional Filter
			ConditionalFilter cf = ConditionalFilterMngr.getInstance().getConditionalFilter( seldPlugin, condFilterName );
			if( cf == null ) {
				throw new VizException();
			}
			
			MessageDialog confirmDlg = new MessageDialog(shell, "Confirm Delete", null, 
					"Are you sure you want to delete "+condFilterName+"?\n",
					MessageDialog.QUESTION, 
					new String[]{"Yes", "No"}, 0);
			confirmDlg.open();

			if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
				return;
			}

			ConditionalFilterMngr.getInstance().deleteConditionalFilter( seldPlugin, condFilterName );
    	}
    	catch( VizException e ) {
    		MessageDialog errDlg = new MessageDialog( 
    				NmapUiUtils.getCaveShell(), 
    				"Error", null, 
    				"Error Deleting Conditional Filter "+condFilterName+ ".\n\n"+
    				e.getMessage(),
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    	}
    	
    	loadConditionalFiltersList();
	}

	private void init() {        
		pluginNameList.removeAll();

		// the Categories are the plugins for the PlotModel implementations.	
		
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
					if( !plotDataPlugins.contains( pluginName ) ) { 
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
			loadConditionalFiltersList();
		}

		copyCondFilterBtn.setEnabled(false);
		editCondFilterBtn.setEnabled(false);
		deleteCondFilterBtn.setEnabled(false);
	}

	// this will load all of the conditional filters files for all resources currently defined
	// in the selected resource type(category).
	private void loadConditionalFiltersList() {
		if( pluginNameList.getSelectionCount() > 0) {
			condFilterList.removeAll();

			seldPlugin = pluginNameList.getSelection()[0];
			
			condFilters = ConditionalFilterMngr.getInstance().getConditionalFiltersByPlugin( 
					seldPlugin  );
			
			for( ConditionalFilter cf : condFilters.values() ) {
				condFilterList.add( cf.getName() );
			}

			copyCondFilterBtn.setEnabled(false);
			editCondFilterBtn.setEnabled(false);
			deleteCondFilterBtn.setEnabled(false);
		}
	}

	public boolean isOpen() {
		return shell != null && !shell.isDisposed();
	}
} 