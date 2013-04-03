package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpModelSoundingTypeConfigDialog
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 08/20/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGridInventory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpGribModelTypeConfigDialog extends Dialog {
	private static NsharpGribModelTypeConfigDialog thisDialog=null;
	private NsharpConfigStore configStore=null;
	private NsharpConfigManager mgr;
	private List<String> configuredModelTypeList = new ArrayList<String>(); //saved to store
	private org.eclipse.swt.widgets.List availableModelTypeList, selectedMdlTypeList;
	private static Group topGp, pickedMdlTypeListGp;
	private void updateCfgStore(){
		if(configStore != null){
			configStore.getGraphProperty().setGribModelTypeList(configuredModelTypeList);
		}
	}

	public static NsharpGribModelTypeConfigDialog getInstance( Shell parShell){

		if ( thisDialog == null ){
			try {
				thisDialog = new NsharpGribModelTypeConfigDialog( parShell );
				
			} catch (VizException e) {
				e.printStackTrace();
			}

		}

		return thisDialog;

	}

	public static NsharpGribModelTypeConfigDialog getAccess() {
		return thisDialog;
	}

	public NsharpGribModelTypeConfigDialog(Shell parentShell) throws VizException {
		super(parentShell);
		thisDialog = this;
		mgr =NsharpConfigManager.getInstance();
		configStore = mgr.retrieveNsharpConfigStoreFromFs();
		if(configStore != null){
			configuredModelTypeList = configStore.getGraphProperty().getGribModelTypeList();
		}
		if(configStore == null || configuredModelTypeList.isEmpty()){
			configuredModelTypeList = createAvailableModelTypeList(true);
			updateCfgStore();
			mgr.saveConfigStoreToFs(configStore);
		}
		
	}
	private void updateSelectedMdlTypeList(){
		selectedMdlTypeList.removeAll();
		for(String elm: configuredModelTypeList)
			selectedMdlTypeList.add(elm);
	}
	private List<String> createAvailableModelTypeList(boolean returnNeeded){
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( "pluginName", new RequestConstraint("grid") );

    	ArrayList<String> queryRsltsList = 
    		NsharpGridInventory.getInstance().searchInventory( 
    			rcMap, "info.datasetId" );
    	List<String> modelTypeList = new ArrayList<String>(); 
    	if( queryRsltsList != null && !queryRsltsList.isEmpty() ) {
			for(String queryRslt : queryRsltsList ) {
				System.out.println("model name:"+queryRslt );
				String modelName = queryRslt.substring( "grid/".length() );
				if(returnNeeded)
					modelTypeList.add(modelName);
				else
					availableModelTypeList.add(modelName);
			}
		}
    	else
    		modelTypeList = null;
    	return modelTypeList;
	}
	private void createDialogContents(Composite parent){
		topGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topGp.setLayout( new GridLayout( 3, false ) );
		
		Group availableMdlTypeGp = new Group(topGp,SWT.SHADOW_ETCHED_IN);
		availableMdlTypeGp.setText("Available Model Type:");
		availableModelTypeList = new org.eclipse.swt.widgets.List(availableMdlTypeGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		availableModelTypeList.setBounds(availableMdlTypeGp.getBounds().x, availableMdlTypeGp.getBounds().y + NsharpConstants.labelGap , NsharpConstants.listWidth, NsharpConstants.listHeight*32/5 );
		
		createAvailableModelTypeList(false);
		
		Group addDeleteGp = new Group(topGp,SWT.SHADOW_ETCHED_IN);
		addDeleteGp.setLayout( new GridLayout( 1, false ) );
		Button addBtn = new Button(addDeleteGp,  SWT.PUSH );
		addBtn.setText("    Add to Selected List      \u21E8");
		addBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				String selectedMdlType=null;	
				if (availableModelTypeList.getSelectionCount() > 0 ) {
					for(int i=0; i < availableModelTypeList.getSelectionCount(); i++) {
						selectedMdlType = availableModelTypeList.getSelection()[i];
						
						if(configuredModelTypeList.contains(selectedMdlType) == false){
							configuredModelTypeList.add(selectedMdlType);
							//System.out.println("selected sounding file is " + selectedMdlType);
						}
					}	
					updateSelectedMdlTypeList();
					//pickedMdlTypeListGp.layout();
				}
			}          		            	 	
		} );
		Button delBtn = new Button(addDeleteGp, SWT.PUSH);
		delBtn.setText("\u21E6 Delete From Selected List");
		delBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				String selectedMdlType=null;	
				if (selectedMdlTypeList.getSelectionCount() > 0 ) {
					for(int i=0; i < selectedMdlTypeList.getSelectionCount(); i++) {
						selectedMdlType = selectedMdlTypeList.getSelection()[i];
						//System.out.println("selected sounding file is " + selectedMdlType);
						if(configuredModelTypeList.contains(selectedMdlType) == true)
							configuredModelTypeList.remove(selectedMdlType);
					}	
					updateSelectedMdlTypeList();//pickedMdlTypeListGp.layout();
				}
			}          		            	 	
		} );
		 
		pickedMdlTypeListGp = new Group(topGp,SWT.SHADOW_ETCHED_IN);
		pickedMdlTypeListGp.setText("Selected Model Type:");
		selectedMdlTypeList = new org.eclipse.swt.widgets.List(pickedMdlTypeListGp, SWT.BORDER  | SWT.MULTI| SWT.V_SCROLL  );
		selectedMdlTypeList.removeAll();
		for(String elm: configuredModelTypeList)
			selectedMdlTypeList.add(elm);
		selectedMdlTypeList.setBounds(pickedMdlTypeListGp.getBounds().x, pickedMdlTypeListGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight *32/5);
		/*selectedMdlTypeList.addListener ( SWT.Selection, new Listener () {
    		public void handleEvent (Event e) {   			
    		}
    	});*/


	}
	
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		Button saveBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Save",false);
		saveBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				try {
		        	//save to xml
					updateCfgStore();
					mgr.saveConfigStoreToFs(configStore);
					//mb.open();
					
				} catch (VizException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		        
				
			}          		            	 	
		} );  

		Button canBtn = createButton(parent, IDialogConstants.CLOSE_ID,
				IDialogConstants.CLOSE_LABEL, false);
		canBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("close listener is called");
				close();
			}          		            	 	
		} );  
	}

	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Grid Model Type Selection" );
        
    }
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		createDialogContents(top);

		return top;
	}   

	
	@Override
    public int open( ) {
        if ( this.getShell() == null ){
			this.create();
		}
   	    this.getShell().setLocation(this.getShell().getParent().getLocation().x+1100,
   	    		this.getShell().getParent().getLocation().y+200);
   	    return super.open();
    	
    }
	@Override
	public boolean close() {
		return (super.close());
    }
	
}
