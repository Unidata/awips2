/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.NsharpLoadDialog
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
package gov.noaa.nws.ncep.ui.nsharp.view;


import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
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
	private final static int DIALOG_HEIGHT = 920;
	
	protected Composite top;
	private static Composite dialogParent;
	private static NsharpLoadDialog INSTANCE = null;
	private static Shell shell;
	private org.eclipse.swt.widgets.List soundingTypeList;
	public static final String[] soundingTypeStringArray = {
		"Observed Soundings" , "Model Soundings",  "PFC Soundings",  "Archive Files","ACARS Soundings"
	};
	// define index to loadStringArray
	public static final int OBSER_SND = 0; 
	public static final int MODEL_SND = 1;
	public static final int PFC_SND = 2;
	public static final int ARCHIVE = 3;
	public static final int ACARS_SND = 4;
	private ObservedSoundingDialogContents obsDialog;
	private PfcSoundingDialogContents pfcDialog;
	private ModelSoundingDialogContents mdlDialog;
	private Group  soundingTypeGp, acarsGp;
	private int activeLoadSoundingType;
	private NcSoundingProfile.ObsSndType activeObsSndType=NcSoundingProfile.ObsSndType.NCUAIR;
	private  ArrayList<String> obsSelectedTimeList = new ArrayList<String>(); 
	private NcSoundingProfile.PfcSndType activePfcSndType=NcSoundingProfile.PfcSndType.NAMSND;
	private List<String> pfcSelectedFileList = new ArrayList<String>(); 
	private List<String> pfcSelectedTimeList = new ArrayList<String>(); 
	private String activeMdlSndMdlType="";
	private List<String> mdlSelectedFileList = new ArrayList<String>(); 
	private List<String> mdlSelectedTimeList = new ArrayList<String>(); 
	private Text text1;
	private MessageBox mb;
	private Cursor waitCursor=null;
	private Font newFont;
	
	public Font getNewFont() {
		return newFont;
	}
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
		soundingTypeGp.setFont(newFont);
		soundingTypeList = new org.eclipse.swt.widgets.List(soundingTypeGp, SWT.SINGLE |  SWT.V_SCROLL );
        soundingTypeList.setBounds(soundingTypeGp.getBounds().x + NsharpConstants.btnGapX, soundingTypeGp.getBounds().y +NsharpConstants.labelGap, NsharpConstants.filelistWidth, NsharpConstants.listHeight);
        soundingTypeList.setFont(newFont);
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
        			NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        			if(editor!=null)
        				editor.refresh();
        			//shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(); 

        			if(selectedProduct.equals(soundingTypeStringArray[OBSER_SND])){
        				//System.out.println("OBSER_SND enter");
        				
        				if(activeLoadSoundingType != OBSER_SND){
        					cleanupDialog(activeLoadSoundingType); //clean up before resetting activeLoadType
        					activeLoadSoundingType = OBSER_SND;
        					obsDialog.createObsvdDialogContents();
        					//shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
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
        			else if(selectedProduct.equals(soundingTypeStringArray[ARCHIVE])){
        				//System.out.println("ARCHIVE enter");
        				
        				if(activeLoadSoundingType != ARCHIVE) {
        					cleanupDialog(activeLoadSoundingType);//clean up before resetting activeLoadType
        					activeLoadSoundingType = ARCHIVE; 
        					NsharpHandleArchiveFile.openArchiveFile(shell);
        					close();
        				}

        			}
        			else if(selectedProduct.equals(soundingTypeStringArray[ACARS_SND])){
        				//System.out.println("ACARS_SND enter");
        				if(activeLoadSoundingType != ACARS_SND) {
        					cleanupDialog(activeLoadSoundingType);//clean up before resetting activeLoadType
        					activeLoadSoundingType = ACARS_SND; 
        					//setShellSize(false);
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

	public List<String> getPfcSelectedFileList() {
		return pfcSelectedFileList;
	}
	public void setPfcSelectedFileList(List<String> pfcSelectedFileList) {
		this.pfcSelectedFileList = pfcSelectedFileList;
	}
	public List<String> getPfcSelectedTimeList() {
		return pfcSelectedTimeList;
	}
	public void setPfcSelectedTimeList(List<String> pfcSelectedTimeList) {
		this.pfcSelectedTimeList = pfcSelectedTimeList;
	}
	public ArrayList<String> getObsSelectedTimeList() {
		return obsSelectedTimeList;
	}
	public void setObsSelectedTimeList(ArrayList<String> obsSelectedTimeList) {
		this.obsSelectedTimeList = obsSelectedTimeList;
	}
	public NcSoundingProfile.ObsSndType getActiveObsSndType() {
		return activeObsSndType;
	}
	public void setActiveObsSndType(NcSoundingProfile.ObsSndType activeObsSndType) {
		this.activeObsSndType = activeObsSndType;
	}
	public NcSoundingProfile.PfcSndType getActivePfcSndType() {
		return activePfcSndType;
	}
	public void setActivePfcSndType(NcSoundingProfile.PfcSndType activePfcSndType) {
		this.activePfcSndType = activePfcSndType;
	}
	public String getActiveMdlSndMdlType() {
		return activeMdlSndMdlType;
	}
	public void setActiveMdlSndMdlType(String activeMdlSndMdlType) {
		this.activeMdlSndMdlType = activeMdlSndMdlType;
	}

	public List<String> getMdlSelectedFileList() {
		return mdlSelectedFileList;
	}
	public void setMdlSelectedFileList(List<String> mdlSelectedFileList) {
		this.mdlSelectedFileList = mdlSelectedFileList;
	}
	public List<String> getMdlSelectedTimeList() {
		return mdlSelectedTimeList;
	}
	public void setMdlSelectedTimeList(List<String> mdlSelectedTimeList) {
		this.mdlSelectedTimeList = mdlSelectedTimeList;
	}

	static int count = 0;

	public static NsharpLoadDialog getAccess() {
		return INSTANCE;
	}

	public NsharpLoadDialog(Shell parentShell)throws VizException  {
		super(parentShell);
		// set modeless, so mouse button can be used by others
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE |SWT.SHELL_TRIM);
		
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
        shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
        shell.setText( "Load" );
        mb = new MessageBox(shell, SWT.ICON_WARNING
				| SWT.OK);

		mb.setMessage( "User Input Error!");
		Font font = shell.getFont();
		FontData[] fontData = font.getFontData();
		for (int i = 0; i < fontData.length; i++) {
			fontData[i].setHeight(7);				
			//fontData[i].setName("courier");
		}
		newFont = new Font(font.getDevice(), fontData);
		shell.setFont(newFont);
    }

	private void createLoadContents(Composite parent) {
		/* CHIN 1331
		NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();//NsharpLoadDialog.getAccess().getNsharpMapResource();
		nsharpMapResource.setPoints(null);
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpResourceHandler rsc = editor.getRscHandler();
        	rsc.cleanUpRsc();
        	editor.refresh();
        }*/
		dialogParent = parent;
		obsDialog =  new ObservedSoundingDialogContents(dialogParent);
		pfcDialog = new PfcSoundingDialogContents(dialogParent);
		mdlDialog = new ModelSoundingDialogContents(dialogParent);
		switch(activeLoadSoundingType){
		case MODEL_SND:
			mdlDialog.createMdlDialogContents();
			break;
		case PFC_SND:
			pfcDialog.createPfcDialogContents();
			break;
		default: //OBSER_SND is default for all other cases, also set activeLoadSoundingType to OBSER_SND
			obsDialog.createObsvdDialogContents();
			activeLoadSoundingType = OBSER_SND;
			break;
		}
		
		soundingTypeList.setSelection(activeLoadSoundingType);
		
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
	        
	        //top.setLayout(mainLayout);
	        //System.out.println("createDialogArea called");
	        // Initialize all of the menus, controls, and layouts
	        createLoadContents(top);
	        //shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT);
	        if(waitCursor==null)
	        	waitCursor = new Cursor( top.getDisplay(), SWT.CURSOR_WAIT);
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
		//INSTANCE = null;
		if(waitCursor!=null)
			waitCursor.dispose();
		waitCursor=null;
		newFont.dispose();
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
		
		
		//cancelBtn.setBounds(cancelBtn.getBounds().x, cancelBtn.getBounds().y+DIALOG_HEIGHT, 20, 10);
		//System.out.println("createButtonsForButtonBar cancelBtn bound"+cancelBtn.getBounds());
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

   	//public void setShellSize(boolean big){
   	//	if(big == true) {
   			
   	//	}
   	//	else {
   	//		shell.setSize(DIALOG_WIDTH, DIALOG_HEIGHT-100);
   	//	}
   	//}
   	public void startWaitCursor(){
   		if(waitCursor!=null)
   			top.setCursor(waitCursor);
   	}
   	public void stopWaitCursor(){
   		top.setCursor(null);
   	}
}
