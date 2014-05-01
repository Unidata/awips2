package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaneConfigDialog
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 06/28/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpPaneConfigDialog extends Dialog {
	private static NsharpPaneConfigDialog thisDialog=null;
	private NsharpConfigStore configStore=null;
	private NsharpConfigManager mgr;
	private int btnWidth = 300;
	private int btnHeight = 20;
	private int labelGap = 20;
	private int btnGapX = 5;
	private int btnGapY = 5;
	private Combo paneCfgCombo;
	private String paneConfigurationName;
	private MessageBox mb ;
	private void updateCfgStore(){
		if(configStore != null){
			configStore.getGraphProperty().setPaneConfigurationName(paneConfigurationName);
		}
	}

	public static NsharpPaneConfigDialog getInstance( Shell parShell){

		if ( thisDialog == null ){
			try {
				thisDialog = new NsharpPaneConfigDialog( parShell );
				
			} catch (VizException e) {
				e.printStackTrace();
			}

		}

		return thisDialog;

	}

	public static NsharpPaneConfigDialog getAccess() {
		return thisDialog;
	}
	
	public NsharpPaneConfigDialog(Shell parentShell) throws VizException {
		super(parentShell);
		thisDialog = this;
		mgr =NsharpConfigManager.getInstance();
		configStore = mgr.retrieveNsharpConfigStoreFromFs();
		if(configStore != null){
			paneConfigurationName = configStore.getGraphProperty().getPaneConfigurationName();
		}
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  
		mb = new MessageBox(shell, SWT.ICON_WARNING
				| SWT.OK );
		mb.setMessage( "Switching configuration from/to Legacy to/from all other configurations will only take effect after SAVE configuration, CLOSE Nsharp (editor and control view) and RESTART Nsharp!");
		
	}
	private void createDialogContents(Composite parent){
		
		Group  btnGp = new Group(parent, SWT.SHADOW_ETCHED_IN | SWT.NO_RADIO_GROUP);
	    Label paneCfgComboLbl = new Label(btnGp, SWT.BORDER );
	    paneCfgComboLbl.setText("Pane Configuration Selection :");
	    paneCfgComboLbl.setBounds(btnGp.getBounds().x+ btnGapX, btnGp.getBounds().y + labelGap, btnWidth,btnHeight);
	    paneCfgCombo = new Combo(btnGp, SWT.NULL);
	    paneCfgCombo.setBounds(btnGp.getBounds().x+ btnGapX,  paneCfgComboLbl.getBounds().y + paneCfgComboLbl.getBounds().height+ btnGapY,btnWidth,btnHeight);
	    int selectIndex=0;
	    configStore = mgr.retrieveNsharpConfigStoreFromFs();
		if(configStore != null){
			paneConfigurationName = configStore.getGraphProperty().getPaneConfigurationName();
		}
	    for(int i=0; i<NsharpConstants.PANE_CONFIGURATION_NAME.length; i++){
	    	paneCfgCombo.add(NsharpConstants.PANE_CONFIGURATION_NAME[i]);	 
	    	if(paneConfigurationName.equals(NsharpConstants.PANE_CONFIGURATION_NAME[i])){
	    		selectIndex = i;
	    		//break;
	    	}
	    	
	    }
	    paneCfgCombo.select(selectIndex);
	    paneCfgCombo.addSelectionListener(new SelectionListener() {
	    	public void widgetSelected(SelectionEvent e) {
	    		//paneConfigurationName = paneCfgCombo.getItem(paneCfgCombo.getSelectionIndex());
	    		//System.out.println("Selected index: " + paneCfgCombo.getSelectionIndex() + ", selected item: " + paneCfgCombo.getItem(paneCfgCombo.getSelectionIndex()) + ", text content in the text field: " + paneCfgCombo.getText());
	    	}

	    	public void widgetDefaultSelected(SelectionEvent e) {
	    		System.out.println("Default selected index: " + paneCfgCombo.getSelectionIndex() + ", selected item: " + (paneCfgCombo.getSelectionIndex() == -1 ? "<null>" : paneCfgCombo.getItem(paneCfgCombo.getSelectionIndex())) + ", text content in the text field: " + paneCfgCombo.getText());
	    		String text = paneCfgCombo.getText();
	    		if(paneCfgCombo.indexOf(text) < 0) { // Not in the list yet. 
	    			paneCfgCombo.add(text);
	    			// Re-sort
	    			String[] items = paneCfgCombo.getItems();
	    			paneCfgCombo.setItems(items);
	    		}
	    	}
	    });


	}
	
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		// create OK button but using Apply label for applying user entered data
		//Chin note: when "apply" button is selected or Return key is entered, 
		// okPressed() will be called. So, handle event at one place, ie.e at okPressed(). 
		createButton(parent, IDialogConstants.OK_ID,
				"Apply",
				true);
		
		Button saveBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Save",false);
		saveBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				try {
		        	//save to xml
					paneConfigurationName = paneCfgCombo.getItem(paneCfgCombo.getSelectionIndex());
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

	@Override
	public void okPressed() {
		//"Enter" key is pressed, or "Apply" button is pressed.
		//Chin: handle user entered data and apply its changes.
		//System.out.println("CR is pressed");
		String newpaneConfigurationName = paneCfgCombo.getItem(paneCfgCombo.getSelectionIndex());
		//if(newpaneConfigurationName.equals(paneConfigurationName)){
		//	setReturnCode(OK);
		//	return;
		//}
		/*if(newpaneConfigurationName.equals(NsharpConstants.PANE_LEGACY_CFG_STR)){
			mb.open();
		}
		else */{
			paneConfigurationName = newpaneConfigurationName;
			updateCfgStore();
			NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
			if(editor!=null){
				editor.restartEditor(paneConfigurationName);
				/*IRenderableDisplay[] tempDisp  =  editor.getRenderableDisplays();
				editor.dispose();
				EditorInput edInput = new EditorInput(new NCLoopProperties(),
						tempDisp);
				try {
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
					.openEditor(edInput, "gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpEditor");
				} catch (PartInitException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}*/
			}
			else {
				NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
		    	if(paletteWin!=null){
		    		paletteWin.updateSpcGraphBtn(paneConfigurationName);
		    	}
			}
		}
		setReturnCode(OK);
	}
	
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Pane Configuration Selection" );
        
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
		paneCfgCombo.dispose();
		paneCfgCombo=null;
		return (super.close());
    }
	
}
