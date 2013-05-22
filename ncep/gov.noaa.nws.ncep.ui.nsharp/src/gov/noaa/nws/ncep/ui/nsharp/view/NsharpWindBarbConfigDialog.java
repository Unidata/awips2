package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpDataDisplayConfigDialog
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/21/2012	229			Chin Chen	Initial coding
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
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpWindBarbConfigDialog extends Dialog {
	private float curLineWidth= NsharpConstants.WINDBARB_WIDTH;
    private RGB curColor=NsharpConstants.color_yellow;
    private float curSize= NsharpConstants.WINDBARB_SIZE;
    private int curDist= NsharpConstants.WINDBARB_DISTANCE_DEFAULT;
    private static NsharpWindBarbConfigDialog instance=null;
    private NsharpConfigStore configStore=null;
	private ColorMatrixSelector cms;
	private Text windBarbWidthText, windBarbSizeText, windBarbDistText;
	private MessageBox mb;
	public static NsharpWindBarbConfigDialog getInstance(Shell parentShell) {
		if(instance == null)
			instance = new NsharpWindBarbConfigDialog(parentShell);
		return instance;
	}

	public NsharpWindBarbConfigDialog(Shell parentShell) {
		super(parentShell);
        
        instance = this;
        
        NsharpConfigManager mgr =NsharpConfigManager.getInstance();
        configStore = mgr.retrieveNsharpConfigStoreFromFs();
        if(configStore!=null){
        	curLineWidth = configStore.getGraphProperty().getWindBarbLineWidth();
        	curColor = configStore.getGraphProperty().getWindBarbColor();
        	curSize = configStore.getGraphProperty().getWindBarbSize();
        	curDist = configStore.getGraphProperty().getWindBarbDistance();
        }
	}
	

	private  Composite createDialog(Composite composite) {
		FormLayout layout0 = new FormLayout();
		composite.setLayout(layout0);
		//  Lay out the various groups within the dialog
		Group lineConfigGroup = new Group ( composite, SWT.SHADOW_NONE );
		lineConfigGroup.setText("Properties");
		GridLayout lineConfigGridLayout = new GridLayout();
		lineConfigGridLayout.numColumns = 1;
		lineConfigGridLayout.marginHeight = 18;
		lineConfigGridLayout.marginWidth = 18;
		lineConfigGridLayout.horizontalSpacing = 8;
		lineConfigGridLayout.verticalSpacing = 8;
		lineConfigGridLayout.makeColumnsEqualWidth = true;
		lineConfigGroup.setLayout(lineConfigGridLayout);

		FormData formData1 = new FormData();
		formData1.top = new FormAttachment(5,0);
		formData1.left = new FormAttachment(2,0);
		formData1.width = 170;
		formData1.height = 300;
		lineConfigGroup.setLayoutData(formData1);


		Group colorConfigGroup = new Group ( composite, SWT.SHADOW_NONE );
		colorConfigGroup.setText("Color");

		FormData formData5 = new FormData();
		formData5.top   = new FormAttachment(5,0);
		formData5.left  = new FormAttachment(lineConfigGroup, 10);
		formData5.right = new FormAttachment(98, 0);
		formData5.height = 300;
		colorConfigGroup.setLayoutData(formData5);
		//  Parameters to give a uniform look to all line width/style buttons

		final int lblWidth = 100;
		final int lblHeight = 20;
		final int textWidth = 100;


		//  Line Width
		Label widthLbl = new Label(lineConfigGroup, SWT.BORDER );
		widthLbl.setText("Width");
		widthLbl.setBounds(lineConfigGroup.getBounds().x, lineConfigGroup.getBounds().y, lblWidth,lblHeight);
		windBarbWidthText = new Text(lineConfigGroup, SWT.BORDER );
		windBarbWidthText.setBounds(widthLbl.getBounds().x, widthLbl.getBounds().y+lblHeight,textWidth,lblHeight);
		windBarbWidthText.setText(Float.toString(curLineWidth));
		windBarbWidthText.setEditable(true);
		windBarbWidthText.setVisible(true); 
		//listen to Float data type, use 0f as input
		windBarbWidthText.addListener (SWT.Verify, new InEditListener (0f) );
		//size
		Label sizeLbl = new Label(lineConfigGroup, SWT.BORDER );
		sizeLbl.setText("Size");
		sizeLbl.setBounds(windBarbWidthText.getBounds().x, windBarbWidthText.getBounds().y+lblHeight, lblWidth,lblHeight);
		windBarbSizeText = new Text(lineConfigGroup, SWT.BORDER | SWT.SINGLE);
		windBarbSizeText.setBounds(sizeLbl.getBounds().x, sizeLbl.getBounds().y+lblHeight, textWidth,lblHeight);
		windBarbSizeText.setText(Float.toString(curSize));
		windBarbSizeText.setEditable(true);
		windBarbSizeText.setVisible(true); 
		windBarbSizeText.addListener (SWT.Verify,new InEditListener (0f) );

		//dist
		Label distLbl = new Label(lineConfigGroup, SWT.BORDER );
		distLbl.setText("Min dist betw barbs, m");
		distLbl.setBounds(windBarbSizeText.getBounds().x, windBarbSizeText.getBounds().y+lblHeight, lblWidth,lblHeight);
		windBarbDistText = new Text(lineConfigGroup, SWT.BORDER | SWT.SINGLE);
		//windBarbDistText.setBounds(distLbl.getBounds().x, distLbl.getBounds().y+lblHeight,textWidth,lblHeight);
		windBarbDistText.setLocation(distLbl.getBounds().x, distLbl.getBounds().y+lblHeight);
		windBarbDistText.setText(Integer.toString(curDist));
		windBarbDistText.setEditable(true);
		windBarbDistText.setVisible(true); 
		//listen to Integer data type, use 0 as input
		windBarbDistText.addListener (SWT.Verify,new InEditListener ( 0) );

		//  Line Color

		cms = new ColorMatrixSelector(colorConfigGroup, false, false,
				28, 92, 18, 22, 28, 80, 4, 8, 5);
		cms.setColorValue(curColor);
		cms.addListener(new IPropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				curColor = cms.getColorValue() ;
				
			}
		});
		return composite;
	}
	
	private class InEditListener implements Listener {
		Object inType;
		public void handleEvent (Event e) {
			String string = e.text;
			char [] chars = new char [string.length ()];
			string.getChars (0, chars.length, chars, 0);
			//System.out.println("entered "+ string);
			if(inType instanceof Float){
				for (int i=0; i<chars.length; i++) {
					if (!('0' <= chars [i] && chars [i] <= '9') && ('.'!= chars [i])) {
						e.doit = false;
						return;
					}
				}
			} else if(inType instanceof Integer){
				for (int i=0; i<chars.length; i++) {
					if (!('0' <= chars [i] && chars [i] <= '9') ) {
						e.doit = false;
						return;
					}
				}
			}
			
		}

		public InEditListener( Object type) {
			super();
			inType = type;
		}
		
	}
	
	
	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Wind Barb" );
        mb = new MessageBox(shell, SWT.ICON_WARNING
				| SWT.OK);

		mb.setMessage( "Input Error! ");
    }
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		top.setLayout(mainLayout);
		
		Composite windbarbEditComp = new Composite( top, SWT.NONE );
        createDialog(windbarbEditComp);
        
		//top.pack();
		
		return null;
	}
	@Override
	public int open() {
		if ( this.getShell() == null ){
			this.create();
		}
		
		return super.open();
	}
	private boolean applyChanges(){
		String textStr = windBarbDistText.getText();
		if((textStr != null) && !(textStr.isEmpty())){
			curDist = Integer.decode(textStr);
			if(curDist <=0) {
				curDist = 400;
				mb.open();
				windBarbDistText.setText(Integer.toString(curDist));
				return false;
			}
		}
		textStr = windBarbSizeText.getText();
		if((textStr != null) && !(textStr.isEmpty()) ){
			try{
				curSize = Float.parseFloat(textStr);
			}
			catch (NumberFormatException e){		
				mb.open();
				return false;
			}
			if(curSize <= 0 || curSize > 15){
				curSize=1;
				windBarbSizeText.setText(Float.toString(curSize));
				mb.open();
				return false;
			}
		}
		textStr = windBarbWidthText.getText();
		if((textStr != null) && !(textStr.isEmpty())){
			try{
				curLineWidth = Float.parseFloat(textStr);
			}
			catch (NumberFormatException e){		
				mb.open();
				return false;
			}
			if(curLineWidth <=0 || curLineWidth > 7){
				curLineWidth=1;
				windBarbWidthText.setText(Float.toString(curLineWidth));
				mb.open();
				return false;
			}
		}
		configStore.getGraphProperty().setWindBarbColor(curColor);
		configStore.getGraphProperty().setWindBarbDistance(curDist);
		configStore.getGraphProperty().setWindBarbLineWidth(curLineWidth);
		configStore.getGraphProperty().setWindBarbSize(curSize);
		//inform editor
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpResourceHandler rsc = editor.getRscHandler();
        	rsc.setGraphConfigProperty(configStore.getGraphProperty());
        	
        	editor.refresh();
        }
        return true;
	}
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		Button appBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Apply", false);
		appBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("appBtn listener is called");
				applyChanges();
			}          		            	 	
		} );
		Button saveBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Save", false);
		saveBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("saveBtn listener is called");
				if(applyChanges() == true){
					NsharpConfigManager mgr =NsharpConfigManager.getInstance();
					try {
						//save to configuration file
						mgr.saveConfigStoreToFs(configStore);
					} catch (VizException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}          		            	 	
		} );
		Button closeBtn = createButton(parent, IDialogConstants.CLOSE_ID, IDialogConstants.CLOSE_LABEL,
				true);
		closeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				close();
				instance=null;
				
			}          		            	 	
		} );  
		
	}
	
	public float getCurLineWidth() {
		return curLineWidth;
	}

	public void setCurLineWidth(float curLineWidth) {
		this.curLineWidth = curLineWidth;
	}

	public RGB getCurColor() {
		return curColor;
	}

	public void setCurColor(RGB curColor) {
		this.curColor = curColor;
	}

	public float getCurSize() {
		return curSize;
	}

	public void setCurSize(float curSize) {
		this.curSize = curSize;
	}

	public int getCurDist() {
		return curDist;
	}

	public void setCurDist(int curDist) {
		this.curDist = curDist;
	}

}
