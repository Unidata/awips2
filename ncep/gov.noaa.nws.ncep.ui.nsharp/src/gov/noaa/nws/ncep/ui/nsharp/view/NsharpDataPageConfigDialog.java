package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpDataPageProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
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

public class NsharpDataPageConfigDialog extends Dialog {
	private MessageBox mb;
	private NsharpConfigStore configStore=null;
	private NsharpConfigManager mgr;
	private NsharpDataPageProperty dpp;
	private NsharpDataPageProperty editingDpp;
	private static NsharpDataPageConfigDialog instance=null;
	//this array is used to store page display "order"
	private int[] pageOrderNumberArray = new int[NsharpConstants.PAGE_MAX_NUMBER+1];//element 0 is dummy one
	private int[] editingOrderNumberArray = new int[NsharpConstants.PAGE_MAX_NUMBER+1];//element 0 is dummy one
	
	private int lblWidth = 200;
	private int lblHeight = 20;
	private int textWidth = 80;
	
	private int numberPagePerDisplay = 2;
	private Label curLbl;
	private Text  newText;
	private Text[] newOrderTextArray= new Text[NsharpConstants.PAGE_MAX_NUMBER+1];//element 0 is dummy one
	private Label[] curOrderLblArray = new Label[NsharpConstants.PAGE_MAX_NUMBER+1];//element 0 is dummy one
	private Label[] pageNameLblArray = new Label[NsharpConstants.PAGE_MAX_NUMBER+1];//element 0 is dummy one
	public static NsharpDataPageConfigDialog getInstance(Shell parentShell) {
		if(instance == null)
			instance = new NsharpDataPageConfigDialog(parentShell);
		return instance;
	}
	protected NsharpDataPageConfigDialog(Shell parentShell) {
		super(parentShell);
		instance = this;
		mgr =NsharpConfigManager.getInstance();
		configStore = mgr.retrieveNsharpConfigStoreFromFs();
		dpp = configStore.getDataPageProperty();
		pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY1 ] = dpp.getSummary1Page();
		pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY2 ] = dpp.getSummary2Page();
		pageOrderNumberArray[NsharpConstants.PAGE_PARCEL_DATA ] = dpp.getParcelDataPage();
		pageOrderNumberArray[NsharpConstants.PAGE_THERMODYNAMIC_DATA ]  = dpp.getThermodynamicDataPage();
		pageOrderNumberArray[NsharpConstants.PAGE_OPC_DATA ]  = dpp.getOpcDataPage();
		pageOrderNumberArray[NsharpConstants.PAGE_MIXING_HEIGHT]  = dpp.getMixingHeightPage();
		pageOrderNumberArray[NsharpConstants.PAGE_STORM_RELATIVE ]  = dpp.getStormRelativePage();
		pageOrderNumberArray[NsharpConstants.PAGE_MEAN_WIND ]  = dpp.getMeanWindPage();
		pageOrderNumberArray[NsharpConstants.PAGE_CONVECTIVE_INITIATION ]  = dpp.getConvectiveInitiationPage();
		pageOrderNumberArray[NsharpConstants.PAGE_SEVERE_POTENTIAL ]  = dpp.getSeverePotentialPage();
		editingOrderNumberArray = pageOrderNumberArray.clone();
		numberPagePerDisplay = dpp.getNumberPagePerDisplay();
		mb = new MessageBox(parentShell, SWT.ICON_WARNING
				| SWT.OK );
		
	}
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Data Page Display Configuration" );
        
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
				//System.out.println("save listener is called, also apply changes");
				if(sanityCheck()== false){
					System.out.println("sanity check failed");		
					return;
				}
				applyChange();
				dpp.setSummary1Page(pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY1 ]);
				dpp.setSummary2Page(pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY2 ]);
				dpp.setParcelDataPage(pageOrderNumberArray[NsharpConstants.PAGE_PARCEL_DATA ]);
				dpp.setThermodynamicDataPage(pageOrderNumberArray[NsharpConstants.PAGE_THERMODYNAMIC_DATA ] );
				dpp.setOpcDataPage(pageOrderNumberArray[NsharpConstants.PAGE_OPC_DATA ]);
				dpp.setMixingHeightPage(pageOrderNumberArray[NsharpConstants.PAGE_MIXING_HEIGHT]);
				dpp.setStormRelativePage(pageOrderNumberArray[NsharpConstants.PAGE_STORM_RELATIVE ] );
				dpp.setMeanWindPage(pageOrderNumberArray[NsharpConstants.PAGE_MEAN_WIND ]  );
				dpp.setConvectiveInitiationPage(pageOrderNumberArray[NsharpConstants.PAGE_CONVECTIVE_INITIATION ]  );
				dpp.setSeverePotentialPage(pageOrderNumberArray[NsharpConstants.PAGE_SEVERE_POTENTIAL ]  );
				dpp.setNumberPagePerDisplay(numberPagePerDisplay);
				try {
		        	//save to xml
					mgr.saveConfigStoreToFs(configStore);
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
	 * To make sure user does not configure same page order number for different page.
	 */
	private boolean sanityCheck(){
		for(int i=1; i <= NsharpConstants.PAGE_MAX_NUMBER; i++){
			String textStr = newOrderTextArray[i ].getText();
			if((textStr != null) && !(textStr.isEmpty())){
				if(!textStr.contains("-") || textStr.length() > 1){
					int pnum = Integer.decode(textStr);
					if(pnum >=1 && pnum <= NsharpConstants.PAGE_MAX_NUMBER)
						editingOrderNumberArray[i]= pnum;
					else{
						System.out.println("wrong order ="+pnum);
						mb.setMessage( "Wrong Configuration! Order number should be within [1-10]");
						mb.open();
						return false;
					}
				}
			}
		}
		for(int i=1; i<= NsharpConstants.PAGE_MAX_NUMBER; i++){
			for(int j=1; j< NsharpConstants.PAGE_MAX_NUMBER; j++){
				if((i!=j) && editingOrderNumberArray[i]==editingOrderNumberArray[j]){
					mb.setMessage( "Wrong Configuration! Multiple pages with same order.");
					mb.open();
					return false;
				}
			}
		}
		return true;
	}
	private void applyChange(){
		pageOrderNumberArray = editingOrderNumberArray.clone();
		for(int i=1; i <= NsharpConstants.PAGE_MAX_NUMBER; i++){
			curOrderLblArray[i].setText(Integer.toString(pageOrderNumberArray[i]));
		}
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpResourceHandler rsc = editor.getRscHandler();
        	editingDpp = new NsharpDataPageProperty();
        	editingDpp.setSummary1Page(pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY1 ]);
			editingDpp.setSummary2Page(pageOrderNumberArray[NsharpConstants.PAGE_SUMMARY2 ]);
			editingDpp.setParcelDataPage(pageOrderNumberArray[NsharpConstants.PAGE_PARCEL_DATA ]);
			editingDpp.setThermodynamicDataPage(pageOrderNumberArray[NsharpConstants.PAGE_THERMODYNAMIC_DATA ] );
			editingDpp.setOpcDataPage(pageOrderNumberArray[NsharpConstants.PAGE_OPC_DATA ]);
			editingDpp.setMixingHeightPage(pageOrderNumberArray[NsharpConstants.PAGE_MIXING_HEIGHT]);
			editingDpp.setStormRelativePage(pageOrderNumberArray[NsharpConstants.PAGE_STORM_RELATIVE ] );
			editingDpp.setMeanWindPage(pageOrderNumberArray[NsharpConstants.PAGE_MEAN_WIND ]  );
			editingDpp.setConvectiveInitiationPage(pageOrderNumberArray[NsharpConstants.PAGE_CONVECTIVE_INITIATION ]  );
			editingDpp.setSeverePotentialPage(pageOrderNumberArray[NsharpConstants.PAGE_SEVERE_POTENTIAL ]  );
			editingDpp.setNumberPagePerDisplay(numberPagePerDisplay);
        	rsc.setDataPageProperty(editingDpp);
         	editor.refresh();
        }
	}
	/*private void setDisplay(){
		int pageIndex = 0;
		for(int orderNum=1; orderNum <= NsharpConstants.PAGE_MAX_NUMBER; orderNum++){
			for(int j=1; j <= NsharpConstants.PAGE_MAX_NUMBER; j++){
				//find the page with order number equal to orderNum
				if(orderNum == pageOrderNumberArray[j]){
					pageIndex = j;
					break;
				}
			}
			curOrderLblArray[orderNum].setText(Integer.toString(pageOrderNumberArray[pageIndex]));
			newOrderTextArray[orderNum].setText(Integer.toString(pageOrderNumberArray[pageIndex]));
			pageNameLblArray[orderNum].setText(NsharpConstants.PAGE_NAME_ARRAY[pageIndex]);
		}
	}*/
	@Override
	public void okPressed() {
		//"Enter" key is pressed, or "Apply" button is pressed.
		//Chin: handle user entered configuration, sanity checking and apply its changes.
		//System.out.println("CR is pressed");	
		setReturnCode(OK);
		if(sanityCheck()== false){
			System.out.println("sanity check failed");		
			return;
		}
		applyChange();	
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);
		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);
		
		Group btnGp = new Group(top, SWT.SHADOW_ETCHED_IN);
		btnGp.setText("number of page per display");
		//.setFont(newFont);
		Button oneBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		oneBtn.setText("1");
		oneBtn.setEnabled( true );
		oneBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, btnGp.getBounds().y + NsharpConstants.labelGap, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		//oneBtn.setFont(newFont);
		oneBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {   
				numberPagePerDisplay=1;
				//System.out.println("new obvSnd dialog uair btn");
			}          		            	 	
		} ); 
		
		
		Button twoBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		twoBtn.setText("2");
		twoBtn.setEnabled( true );
		twoBtn.setBounds(btnGp.getBounds().x+ NsharpConstants.btnGapX, oneBtn.getBounds().y + oneBtn.getBounds().height+ NsharpConstants.btnGapY, NsharpConstants.btnWidth,NsharpConstants.btnHeight);
		//twoBtn.setFont(newFont);
		twoBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				numberPagePerDisplay=2;
			}          		            	 	
		} );  
		if(numberPagePerDisplay==1)
			oneBtn.setSelection(true);
		else
			twoBtn.setSelection(true);
		Group  configGp = new Group(top, SWT.SHADOW_ETCHED_IN | SWT.NO_RADIO_GROUP);
		Label nameLbl = new Label(configGp, SWT.BORDER );
		nameLbl.setText(" Page Name");
		nameLbl.setBounds(configGp.getBounds().x, configGp.getBounds().y, lblWidth,lblHeight);
		Label curOrderLbl = new Label(configGp, SWT.BORDER );
		curOrderLbl.setText("Current");
		curOrderLbl.setBounds(nameLbl.getBounds().x+nameLbl.getBounds().width, nameLbl.getBounds().y,textWidth,lblHeight);
		Label newOrderLbl =new Label(configGp, SWT.BORDER );
		//newOrderLbl.setText(Integer.toString(summary1Page));
		newOrderLbl.setBounds(curOrderLbl.getBounds().x+curOrderLbl.getBounds().width, nameLbl.getBounds().y,textWidth,lblHeight);
		newOrderLbl.setText("New Order");
		Label prevLbl = nameLbl;
		Label pageLbl;
		for(int i=1; i <= NsharpConstants.PAGE_MAX_NUMBER; i++){
			pageLbl = pageNameLblArray[i] = new Label(configGp, SWT.BORDER );
			pageLbl.setText(NsharpConstants.PAGE_NAME_ARRAY[i]);
			pageLbl.setBounds(configGp.getBounds().x, prevLbl.getBounds().y+lblHeight, lblWidth,lblHeight);
			curLbl = curOrderLblArray[i] =new Label(configGp, SWT.BORDER );
			curLbl.setText(Integer.toString(pageOrderNumberArray[i]));
			curLbl.setBounds(pageLbl.getBounds().x+pageLbl.getBounds().width, pageLbl.getBounds().y,textWidth,lblHeight);
			newText = newOrderTextArray[i] = new Text(configGp, SWT.BORDER | SWT.SINGLE);
			newText.setBounds(curLbl.getBounds().x+curLbl.getBounds().width, pageLbl.getBounds().y,textWidth,lblHeight);
			newText.setEditable(true);
			newText.setText(Integer.toString(pageOrderNumberArray[i]));	
			//to make sure user enter digits only
			newText.addListener (SWT.Verify, new EditListener (newText) );
			prevLbl = pageLbl;
		}
		return top;
	}
	public class EditListener implements Listener {
		Text newOrderText;
		public void handleEvent (Event e) {
			String string = e.text;
			char [] chars = new char [string.length ()];
			string.getChars (0, chars.length, chars, 0);
			//System.out.println("entered s="+ string);
			//Chin note: when "Delete", "Backspace" entered, this handler will be called, but
			// its chars.length = 0
			if(chars.length == 1 && newOrderText!=null){
				if(chars [0] == '-' ){
					//if "-" is not first char
					String textStr = newOrderText.getText();
					if((textStr != null) && (textStr.length() >= 1) && (textStr.contains("-"))){
						e.doit = false;
						return;
					}
				}else if (!('0' <= chars [0] && chars [0] <= '9')) {
					e.doit = false;
					return;
				}

			} 
		}

		public EditListener(Text newOrderText) {
			super();
			this.newOrderText = newOrderText;
		}
		
	}
	
}
