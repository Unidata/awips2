package gov.noaa.nws.ncep.viz.tools.ncInventoryControl;

import static java.lang.System.out;

import gov.noaa.nws.ncep.edex.common.ncinventory.ManageNcInventoryMsg;
import gov.noaa.nws.ncep.edex.common.ncinventory.NcInventoryRequestMsg;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.RscParamsJaxBAdapter;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcGridInventory;

// let the user define new NcInventories for 
public class NcInventoryDefineDlg extends Dialog {
	Shell shell=null;
	
	private Text   invNameTxt = null;
	private Text   baseConstrTxt = null;
	private Text   invParamsTxt = null;
	
	private Button defineInvBtn = null;
           
	public NcInventoryDefineDlg( Shell sh ) {
		super(sh);
		
    	shell = new Shell( SWT.SHELL_TRIM | SWT.MODELESS );
    	shell.setText( "Define New NcInventory" );

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	shell.setLayout(mainLayout);

    	Composite topForm = new Composite(shell, SWT.NONE);
    	GridData gd = new GridData( GridData.FILL_BOTH );
//    	gd.grabExcessHorizontalSpace = true;
//    	gd.grabExcessVerticalSpace = true;
    	topForm.setLayoutData( gd );
    	
    	topForm.setLayout(new FormLayout());
    	topForm.setSize(400, 160);
    	    	
		invNameTxt = new Text( topForm, SWT.SINGLE | SWT.BORDER);
		invNameTxt.setText("NcGrid");
		invNameTxt.setSelection(0, 6);
		invNameTxt.setToolTipText("The Inventory Name. Unique from other inventory names.");
		
    	FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 15 );
    	fd.left = new FormAttachment( 0, 10 );
    	fd.right = new FormAttachment( 100, -10 );
    	invNameTxt.setLayoutData(fd);

    	baseConstrTxt = new Text( topForm, SWT.MULTI | SWT.BORDER );
    	baseConstrTxt.setText("pluginName=ncgrib");
    	baseConstrTxt.setToolTipText("Base Constraints that every entry in the inventory must match.\n(no commas. one constraint per line, pluginName is required");

    	fd = new FormData();
    	fd.left = new FormAttachment( invNameTxt, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( invNameTxt, 10, SWT.BOTTOM );
    	fd.right = new FormAttachment( 100, -10 );
    	fd.bottom  = new FormAttachment( 30, 0 );
    	baseConstrTxt.setLayoutData(fd);
        
    	//
    	invParamsTxt = new Text( topForm, SWT.MULTI | SWT.BORDER );
    	invParamsTxt.setText("pluginName,");
    	invParamsTxt.setToolTipText("The Inventory Parameters.\n"+
    			"A comma separated list of each parameter to be stored in the inventory.\n"+
    			"pluginName and one other parameter is required.");
    	
    	fd = new FormData();
    	fd.left = new FormAttachment( baseConstrTxt, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( baseConstrTxt, 10, SWT.BOTTOM );    	
    	fd.right = new FormAttachment( 100, -10 );
    	fd.bottom  = new FormAttachment( 80, 0 );
    	invParamsTxt.setLayoutData(fd);
        
		defineInvBtn = new Button( topForm, SWT.NONE );
		defineInvBtn.setText( "Define New Inventory");
		
    	fd = new FormData();
    	fd.left = new FormAttachment( invParamsTxt, 0, SWT.LEFT );
    	fd.top  = new FormAttachment( invParamsTxt, 15, SWT.BOTTOM );
    	defineInvBtn.setLayoutData(fd);
    	
    	defineInvBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {            	
            	try {
                	String inventoryName = invNameTxt.getText().trim();

                	// get the list of baseConstraints
            		String baseConstrStr = baseConstrTxt.getText();
            		
            		
            		String invParamNamesStr= invParamsTxt.getText();
            		String invParamNames[] = invParamNamesStr.split(",");
            		
            		if( inventoryName.isEmpty() || baseConstrStr.isEmpty() ||
            			invParamNamesStr.isEmpty() ) {
            			throw new Exception("Inventory Name, baseConstraints or invParams is not set.");
            		}
            		
            		for( int i=0 ; i<invParamNames.length ; i++ ) {
            			invParamNames[i] = invParamNames[i].trim();
            		}
            		
            		HashMap<String, String> baseParmMap = new RscParamsJaxBAdapter().unmarshal( baseConstrStr );
            		HashMap<String, RequestConstraint> baseReqConstrMap= new HashMap<String,RequestConstraint>();
            		
            		for( String parm : baseParmMap.keySet() ) {
            			baseReqConstrMap.put( parm, new RequestConstraint( baseParmMap.get( parm ) ) );
            		}
            		
                	// Create an NcInventoryRequest script to get query the ncInventory for the types.
                	// 
                	ManageNcInventoryMsg defineMsg = ManageNcInventoryMsg.makeCreateDirective();
                	
                	defineMsg.setInventoryName( inventoryName );
            		defineMsg.setBaseConstraints( baseReqConstrMap );
            		defineMsg.setInventoryParamsList( new ArrayList<String>( Arrays.asList( invParamNames ) ) );
            		
            		Object response;

            		long t01 = System.currentTimeMillis();

            		response = ThriftClient.sendRequest( defineMsg );

            		out.println("inv init request returned "+response.getClass().getCanonicalName() );
            		
            		if( !(response instanceof String) ) {
            			out.println("Inventory Init Error: expecting String return." );
            			throw new VizException("Inventory defineMsg Request Error: String response expecting instead of "+
            					response.getClass().getName() );
            		}
            		else if( !response.equals( ManageNcInventoryMsg.CREATE_SUCCESS_RESPONSE ) ) {
            			throw new VizException("Inventory defineMsg Request Error: "+ response.toString() );
            		}

            		long t02 = System.currentTimeMillis();

            		out.println("Inventory Created for "+ inventoryName+ 
            				" took "+ (t02-t01)+ "msecs " );

            		
            		MessageDialog msgDlg = new MessageDialog( 
        					shell, "Info", null, 
        					"NcInventory "+ inventoryName + " has been created.\n", 
        					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
        			msgDlg.open();                    		
			
        		} catch (Exception e) {
        			MessageDialog errDlg = new MessageDialog( 
        					shell, "Error", null, 
        					"Error:\n\n"+e.getMessage(),
        					MessageDialog.ERROR, new String[]{"OK"}, 0);
        			errDlg.open();
        		}            		
            }
        });

    
    	Button closeBtn = new Button( topForm, SWT.NONE);
        closeBtn.setText("  Close  ");

        fd = new FormData();
    	fd.right = new FormAttachment( 100, -10 );
    	fd.top  = new FormAttachment( defineInvBtn, 15, SWT.BOTTOM );
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
}
