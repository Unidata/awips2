/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.menu.NsharpLoadDialog
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.menu;


import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpLoadDialog extends Dialog {
	
	private final static int DIALOG_WIDTH = 350;
	private final static int DIALOG_HEIGHT = 620;
	
	protected Composite top;
	private static Composite dialogParent;
	private static NsharpLoadDialog INSTANCE = null;
	private static Shell shell;
	private org.eclipse.swt.widgets.List soundingTypeList;
	public static final String[] soundingTypeStringArray = {
		"Observed Soundings" , "Model Soundings",  "PFC Soundings",  "ACARS Soundings", "Archive Files"
	};
	// define index to loadStringArray
	public static final int OBSER_SND = 0; 
	public static final int MODEL_SND = 1;
	public static final int PFC_SND = 2;
	public static final int ACARS_SND = 3;
	public static final int ARCHIVE = 4;
	private ObservedSoundingDialogContents obsDialog;
	private PfcSoundingDialogContents pfcDialog;
	private ModelSoundingDialogContents mdlDialog;
	private Group  soundingTypeGp, acarsGp;
	private int activeLoadSoundingType;
	private Text text1;
	private MessageBox mb;
	
	
	public ObservedSoundingDialogContents getObsDialog() {
		return obsDialog;
	}
	public PfcSoundingDialogContents getPfcDialog() {
		return pfcDialog;
	}
	public void setAndOpenMb(String msg) {
		if (mb != null) {
			mb.setMessage(msg);
			try {
				mb.open();
			}catch (Exception e) {
				mb = new MessageBox(shell, SWT.ICON_WARNING
						| SWT.OK);
				mb.setMessage(msg);
				mb.open();
				//e.printStackTrace();
			}
		}
	}
	public ModelSoundingDialogContents getMdlDialog() {
		return mdlDialog;
	}
	public void createSndTypeList(Group TopLoadGp) {
		soundingTypeGp =  new Group(TopLoadGp,SWT.SHADOW_ETCHED_IN);
		soundingTypeGp.setText("Sounding Type");
		soundingTypeList = new org.eclipse.swt.widgets.List(soundingTypeGp, SWT.SINGLE |  SWT.V_SCROLL );
        soundingTypeList.setBounds(soundingTypeGp.getBounds().x + NsharpConstants.btnGapX, soundingTypeGp.getBounds().y +NsharpConstants.labelGap, NsharpConstants.listWidth, NsharpConstants.listHeight+30);
		for(String loadStr : soundingTypeStringArray){
			soundingTypeList.add( loadStr );
		}
		//create a selection listener to handle user's selection on list
        soundingTypeList.addListener ( SWT.Selection, new Listener () {
        	private String selectedProduct=null;	
        	public void handleEvent (Event e) {   			
        		if (soundingTypeList.getSelectionCount() > 0 ) {

        			selectedProduct = soundingTypeList.getSelection()[0];
        			NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
        			nsharpMapResource.setPoints(null);
        			NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
        			if(editor!=null)
        				editor.refresh();
        			//shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(); 

        			if(selectedProduct.equals(soundingTypeStringArray[OBSER_SND])){
        				//System.out.println("OBSER_SND enter");
        				
        				if(activeLoadSoundingType != OBSER_SND){
        					cleanupDialog(activeLoadSoundingType); //clean up before resetting activeLoadType
        					activeLoadSoundingType = OBSER_SND;
        					obsDialog.createObsvdDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        					soundingTypeList.setSelection(OBSER_SND);
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[MODEL_SND])){
        				//System.out.println("MODEL_SND enter");
        				if(activeLoadSoundingType != MODEL_SND) {
        					cleanupDialog(activeLoadSoundingType);//clean up before resetting activeLoadType
        					activeLoadSoundingType = MODEL_SND; 
        					mdlDialog.createMdlDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        					soundingTypeList.setSelection(MODEL_SND);
            				
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[PFC_SND])){
        				//System.out.println("PFC_SND enter");
        				if(activeLoadSoundingType != PFC_SND){
        					cleanupDialog(activeLoadSoundingType); //clean up before resetting activeLoadType
        					activeLoadSoundingType = PFC_SND;
        					pfcDialog.createPfcDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        					soundingTypeList.setSelection(PFC_SND);
            				
        				}

        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[ACARS_SND])){
        				//System.out.println("ACARS_SND enter");
        				if(activeLoadSoundingType != ACARS_SND) {
        					cleanupDialog(activeLoadSoundingType);//clean up before resetting activeLoadType
        					activeLoadSoundingType = ACARS_SND; 
        					setShellSize(false);
        					acarsGp = new Group(dialogParent,SWT.SHADOW_ETCHED_IN);
        					acarsGp.setLayout( new GridLayout( 1, false ) );
        					createSndTypeList(acarsGp);
        					text1 = new Text(acarsGp,  SWT.MULTI | SWT.BORDER | SWT.WRAP );
        					text1.setText("Acars Soundings\nis still under\ndevelopment!");
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        					soundingTypeList.setSelection(ACARS_SND);
            				
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[ARCHIVE])){
        				//System.out.println("ARCHIVE enter");
        				
        				if(activeLoadSoundingType != ARCHIVE) {
        					cleanupDialog(activeLoadSoundingType);//clean up before resetting activeLoadType
        					activeLoadSoundingType = ARCHIVE; 
        					NsharpHandleArchiveFile.openArchiveFile(shell);
        					close();
        				}

        			}

        		}
        	}
        });
	}
	public void cleanSndTypeList(){
		if(soundingTypeList!= null){
			soundingTypeList.removeListener(SWT.Selection, soundingTypeList.getListeners(SWT.Selection)[0]);			
			soundingTypeList.dispose();
			soundingTypeList = null;
		}
		if(soundingTypeGp!=null){
			soundingTypeGp.dispose();
			soundingTypeGp=null;
		}
	}
	
	private void cleanSelf(){
		
		if(text1 != null){
			text1.dispose();
			text1 = null;
		}
	}
	
	private void cleanupDialog(int activeLoadType){
		switch (activeLoadType) {
		case OBSER_SND:
			obsDialog.cleanup();
			break;
		case MODEL_SND:
			mdlDialog.cleanup();
			break;
		case PFC_SND:
			pfcDialog.cleanup();
			break;
		case ACARS_SND:
			if(text1 != null){
				text1.dispose();
				text1 = null;
			}
			if(acarsGp!= null){
				acarsGp.dispose();
				acarsGp = null;
			}
			break;
		case ARCHIVE:
			break;
		default:
			break;
		}
		
	}
	
	public void setActiveLoadSoundingType(int activeLoadSoundingType) {
		this.activeLoadSoundingType = activeLoadSoundingType;
	}
	public int getActiveLoadSoundingType() {
		return activeLoadSoundingType;
	}



	static int count = 0;

	public static NsharpLoadDialog getAccess() {
		return INSTANCE;
	}

	public NsharpLoadDialog(Shell parentShell)throws VizException  {
		super(parentShell);
		// set modeless, so mouse button can be used by others
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
		
		//System.out.println("loadDia constructed");
		activeLoadSoundingType = OBSER_SND;
		
	}

	   /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        NsharpLoadDialog.shell = shell;     
        setShellSize(false);
        shell.setText( "Load" );
        mb = new MessageBox(shell, SWT.ICON_WARNING
				| SWT.OK);

		mb.setMessage( "User Input Error!");

    }

	private void createLoadContents(Composite parent) {
		NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
		nsharpMapResource.setPoints(null);
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpSkewTResource rsc = editor.getNsharpSkewTDescriptor().getSkewtResource();
        	rsc.cleanUpRsc();
        	editor.refresh();
        }
		//System.out.println("createLoadContents called");
		//NsharpMapModalTool.setModal();
		//nsharpMapResource.
		dialogParent = parent;
		
		/*
		topLoadGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		topLoadGp.setLayout( new GridLayout( 2, false ) );
		createSndTypeList(topLoadGp);
		
		*/
		//dialogParent.setBounds(x, y, width, height);
		obsDialog =  new ObservedSoundingDialogContents(dialogParent);
		pfcDialog = new PfcSoundingDialogContents(dialogParent);
		mdlDialog = new ModelSoundingDialogContents(dialogParent);
		obsDialog.createObsvdDialogContents();
		//mdlDialog.createMdlDialogContents();
		// set default selection to observed sounding
		soundingTypeList.setSelection(0);
		activeLoadSoundingType = OBSER_SND;
		//soundingTypeList.setSelection(MODEL_SND);
		//activeLoadType = MODEL_SND;
		/*
        //create a selection listener to handle user's selection on list
        soundingTypeList.addListener ( SWT.Selection, new Listener () {
        	private String selectedProduct=null;	
        	public void handleEvent (Event e) {   			
        		if (soundingTypeList.getSelectionCount() > 0 ) {

        			selectedProduct = soundingTypeList.getSelection()[0];

        			shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(); 

        			if(selectedProduct.equals(soundingTypeStringArray[OBSER_SND])){
        				//System.out.println("OBSER_SND enter");
        				if(activeLoadType != OBSER_SND){
        					cleanupDialog(activeLoadType); //clean up before resetting activeLoadType
        					activeLoadType = OBSER_SND;
        					obsDialog.createObsvdDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[MODEL_SND])){
        				//System.out.println("MODEL_SND enter");
        				if(activeLoadType != MODEL_SND) {
        					cleanupDialog(activeLoadType);//clean up before resetting activeLoadType
        					activeLoadType = MODEL_SND; 
        					mdlDialog.createMdlDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[PFC_SND])){
        				//System.out.println("PFC_SND enter");
        				if(activeLoadType != PFC_SND){
        					cleanupDialog(activeLoadType); //clean up before resetting activeLoadType
        					activeLoadType = PFC_SND;
        					pfcDialog.createPfcDialogContents();
        					dialogParent.pack();
        					dialogParent.layout(true);
        					dialogParent.redraw();
        				}

        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[ACARS_SND])){
        				//System.out.println("ACARS_SND enter");
        				if(activeLoadType != ACARS_SND) {
        					cleanupDialog(activeLoadType);//clean up before resetting activeLoadType
        					activeLoadType = ACARS_SND; 
        					//Not supported now!!
        					text1 = new Text(topLoadGp,  SWT.MULTI | SWT.BORDER | SWT.WRAP );
        					text1.setText("Acars Soundings\nis still under\ndevelopment!");
        					topLoadGp.pack();
        					topLoadGp.layout(true);
        					topLoadGp.redraw();
        				}
        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[ARCHIVE])){
        				//System.out.println("ARCHIVE enter");
        				if(activeLoadType != ARCHIVE) {
        					cleanupDialog(activeLoadType);//clean up before resetting activeLoadType
        					activeLoadType = ARCHIVE; 
        					NsharpHandleArchiveFile.openArchiveFile(shell);
        					close();
        				}

        			}

        		}
        	}
        });*/
		
	}
	
	/**
	 * Creates the dialog area
	 */	
	@Override
	public Control createDialogArea(Composite parent) {
		
	        top = (Composite) super.createDialogArea(parent);

	        // Create the main layout for the shell.
	        GridLayout mainLayout = new GridLayout(1, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        createLoadContents(top);

	        return top;
	}   

	
	@Override
    public int open( ) {
        //System.out.println("loadDia opened");
        
        if ( this.getShell() == null ){
			this.create();
		}
   	    this.getShell().setLocation(this.getShell().getParent().getLocation().x+1100,
   	    		this.getShell().getParent().getLocation().y+200);
   	    NsharpMapResource.bringMapEditorToTop();
   	    return super.open();
    	
    }
	@Override
	public boolean close() {
		NsharpMapResource nsharpMapResource = NsharpMapResource.getMapRsc();
		if(nsharpMapResource!=null)
			nsharpMapResource.setPoints(null);
		//System.out.println("loadDia closed");
		cleanSelf();
		cleanupDialog(activeLoadSoundingType);
		INSTANCE = null;
		return (super.close());
    }
	public boolean closeDiaOnly() {
		cleanSelf();
		return (super.close());
	}
	
	//Only use Cancel button but NOT ok button
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		// create Cancel buttons by default, but use close label
		createButton(parent, IDialogConstants.CANCEL_ID,
				IDialogConstants.CLOSE_LABEL, false);
	}

    

   public static NsharpLoadDialog getInstance( Shell parShell){
	   //System.out.println("getInstance called");
		if ( INSTANCE == null ){
			try {
				INSTANCE = new NsharpLoadDialog( parShell );
				//System.out.println("new load dialog INSTANCE created");
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}
		return INSTANCE;
		
	}

   	public void setShellSize(boolean big){
   		if(big == true) {
   			shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
   		}
   		else {
   			shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT-100);
   		}
   	}
   
}
