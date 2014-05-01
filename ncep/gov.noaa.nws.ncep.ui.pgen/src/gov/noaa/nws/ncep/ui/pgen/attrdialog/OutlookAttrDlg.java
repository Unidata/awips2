/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.OutlookAttrDlg
 * 
 * 31 March 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.IContours;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.graphtogrid.GraphToGridParamDialog;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for outlooks.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10					B. Yin   	Initial Creation.
 * 07/10		#215		J. Wu  		Added support for graph-to-grid.
 * 08/10		#215		J. Wu  		Added support for Contours Attributes.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 07/11        #450        G. Hull     NcPathManager
 * 01/12        #582        Q. Zhou     Added NHC line/label group. Added flagLabel and flagAction and enable function. 
 *                                      Fixed enable problem when dialog returns. 
 * 03/12        #599        Q. Zhou     Fixed selecting a outlook. need to enable btns from outlookType table.
 * 										Fixed label text combo width.
 * 03/13		#928		B. Yin		Removed some white space.
 * 11/13		#1049		B. Yin		Handle outlook type defined in layer.
 * </pre>
 * 
 * @author	B. Yin
 */

public class OutlookAttrDlg  extends AttrDlg implements IContours, ILine{

	//Outlook type document
	private static Document outlookTbl;
	
	//Default attributes of outlook lines
	private static HashMap<String, Line> settings;

	//Index for check boxes for multi-selection
	private static enum ChkBox { COLOR, WIDTH, SMOOTH, CLOSE, FILL };
	
	//Symbol icons in the symbol drop-down menu
	private static final String[] SYMBOL_LIST = new String[] { "PAST_WX_09", "PRESENT_WX_065", "PRESENT_WX_073",
		  "PRESENT_WX_075", "PRESENT_WX_073|PRESENT_WX_075", "PRESENT_WX_056",
		  "PRESENT_WX_079", "PRESENT_WX_056|PRESENT_WX_079" };
	
	public static final String OTLK_TYPE_IN_LAYER_META = "outlook type";
	public static final String OTLK_FORMAT_FLAG_IN_LAYER_META = "outlook format flag";
	
	//Single instance of the dialog
	private static OutlookAttrDlg INSTANCE;
	
	//xpath for outlook types in the outlook document
	public static String OTLK_XPATH = "/root/otlktype";

	//instance of the 'Format' dialog
	private OutlookFormatDlg fmtDlg;

	//Top container for all widgets
    private Composite top;
    
    //Panel to hold all line attribute widgets
    private Composite panel2;
    
    //Line color widgets
    private Label colorLbl;
    protected ColorButtonSelector cs;
    
    //Line width widgets
    private Label widthLbl;
    protected Slider widthSlider;
    protected org.eclipse.swt.widgets.Text widthText;
    protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider widthSpinnerSlider;
    
    //Line smooth level widgets
    private Label smoothLbl;
    protected Combo smoothLvlCbo;

    //Closed line check box
    private Button closedBtn;
    
    //Filled line check box
    private Button filledBtn;
    
    // info btn
	private Button infoBtn;
	
    //make grid btn
    private Button makeGridBtn;
    
    //Check boxes for multi-selection
    private Button chkBox[];
    
    //Outlook type drop-down menu
    private Combo outlookCombo;
    
    //Label check box
    private Button lblBtn;
    
    //Text-label check box
    private Button txtBtn;
    
    //Text-label drop-down menu
    private Combo txtCombo;
    
    //Symbol-label check box
    private Button symbolBtn;
    
    //Symbol-label drop-down menu
    private SymbolCombo symbolCombo;
    
    //Check box for 'Use Line Color'
    private Button lnColorBtn;

    //Add-Line button
    private Button addLineBtn;
    
    //Del-Line button
    private Button delLineBtn;
    
    //Set-Continue button
    private Button setContBtn;
    
    //Show-continue lines button
    private Button showContBtn;
    
    //Format button
    private Button fmtBtn;

    //previous selection of text label
    private String prevLbl;
    
    //previous selection of outlook type
    private String prevType;;
    
    //default value for 'UseLineColor' button
    private boolean useLineColor;

    // Dialog to get input for Graph-to-Grid
    private static GraphToGridParamDialog g2gDlg = null;
    
    // Dialog to get input for Contours attributes
	private static ContoursInfoDlg contoursInfoDlg = null;
	
	private String contourParm = "HGMT";

	private String contourLevel = "1000";
	private String contourFcstHr = "f000";
	
	private Calendar contourTime1 = (Calendar)Calendar.getInstance();
	private Calendar contourTime2 = (Calendar)Calendar.getInstance();
	private String contourCint = "10/0/100";

	private boolean needFromLine = false;

	private boolean showGrid = true;
	private boolean flagLabel = true;
	private boolean flagAction = true;
	
	private String defaultLinetype = "POINTED_ARROW";
	private String lineType = defaultLinetype;
	
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected OutlookAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        useLineColor = true;
    }
	
	/**
	 * Creates an outlook attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static OutlookAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new OutlookAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
			loadSettings();
		}
		
		return INSTANCE;
		
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
	        initializeComponents();

	        return top;
	    }   
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	protected void initializeComponents() {
		
        this.getShell().setText("Outlook Attributes");
        
        
	    // Button to pop up the dialog to editing the contour's info.
	    Composite infoComp = new Composite( top, SWT.NONE ); 
		infoComp.setLayoutData( new GridData(SWT.CENTER, SWT.DEFAULT, true, false) );
	    
		GridLayout layout = new GridLayout( 2, false );
	    layout.horizontalSpacing = 20;
	    infoComp.setLayout( layout );	    

	    infoBtn = new Button( infoComp, SWT.PUSH );
	    infoBtn.setToolTipText( "Bring up the contour attribute dialog" );  
	    infoBtn.setEnabled(showGrid); 
		setInfoBtnText();
		
		infoBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {    			
            	openContourInfoDlg();
            }
        }); 
		 
	    // Button to activate the graph-to-grid processing.
	    makeGridBtn = new Button( infoComp, SWT.PUSH );
        makeGridBtn.setText("Make Grid");
        makeGridBtn.setToolTipText( "Generate grid for this Outlook" ); 
        makeGridBtn.setEnabled(showGrid); 
		makeGridBtn.addSelectionListener( new SelectionAdapter() {
			@Override
            public void widgetSelected(SelectionEvent event) {    			
            	openG2GDlg();
            }
        }); 
		
		addSeparator( top );

        //Create a panel to hold outlook attributes(type, label etc.)
        Composite panel1 = new Composite(top, SWT.None);
        panel1.setLayout( new GridLayout(2, false));
        
        //create outlook type drop-down menu
        Label outlookLbl = new Label(panel1, SWT.LEFT);
		outlookLbl.setText("Type:");
		outlookCombo = new Combo( panel1, SWT.DROP_DOWN | SWT.READ_ONLY );

		List<String> types = getOutlookTypes();
		if ( !types.isEmpty()){
			for ( String str : types){
				outlookCombo.add(str);
			}
		}
		else {
			outlookCombo.add("OUTLOOK");
		}

		outlookCombo.select( 0 );
		outlookCombo.addSelectionListener(new SelectionAdapter(){
	
			@Override
			public void widgetSelected(SelectionEvent e) {
				 String type = ((Combo)(e.widget)).getText();
				 prevType = type; 

				 boolean warning = false;
				 
				 //Loop through current layer and see if there is a different type of outlook.
				 //If yes, show a warning message.
				 Iterator<AbstractDrawableComponent> it = drawingLayer.getActiveLayer().getComponentIterator();
				 while( it.hasNext()){
					 AbstractDrawableComponent adc = it.next();
					 if ( adc instanceof Outlook && !((Outlook)adc).getOutlookType().equalsIgnoreCase(type)){
						 warning = true;
						 break;
					 }
				 }
				
				 if ( warning) {
						MessageDialog msgDlg = new MessageDialog( 
								PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
								"Warning!", null, "There is at least one outlook with different type in the same layer!",
								MessageDialog.INFORMATION, new String[]{"OK"}, 0);
						msgDlg.open();
				 }
				 
				 //disable some fields such as "Make Grid" for some outlooks
				 showGrid = showMakeGrid(type);
				 flagLabel = showLabel(type);
				 flagAction = showAction(type);
				 
				 infoBtn.setEnabled(showGrid); 
				 makeGridBtn.setEnabled( showGrid );			 
				 lblBtn.setEnabled(flagLabel);
				 txtBtn.setEnabled(flagLabel);
				 symbolBtn.setEnabled(flagLabel);
				 lnColorBtn.setEnabled(flagLabel);
				 addLineBtn.setEnabled(flagAction);
				 delLineBtn.setEnabled(flagAction);
				 setContBtn.setEnabled(flagAction);
				 showContBtn.setEnabled(flagAction);
				 fmtBtn.setEnabled(flagAction);

				 needFromLine = setFromLineFlag(type); 
				 
				 //Set default labels for the selected outlook type
				 setDefaultLabels( type );
				 
				 //Set the default line attributes
				 setDefaultLineAttr( outlookCombo.getText() + txtCombo.getText() );
				 
			}
		});
        
		if ( prevType == null ){
			outlookCombo.select( 0 );
			prevType = outlookCombo.getText();
		}
		
		//Check box for label
		lblBtn = new Button( panel1, SWT.CHECK );
		lblBtn.setText("Label:");
		lblBtn.setEnabled(flagLabel);
		lblBtn.setSelection(true);
		lblBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)(e.widget)).getSelection()  ){
					txtBtn.setEnabled(flagLabel);
					txtBtn.setSelection(true);
					txtCombo.setEnabled(flagLabel);
					symbolBtn.setEnabled(flagLabel);
					symbolCombo.setEnabled(flagLabel);
					lnColorBtn.setEnabled(flagLabel);
				}
				else {
					txtBtn.setSelection(false);
					txtBtn.setEnabled(false);
					txtCombo.setEnabled(false);
					
					symbolBtn.setSelection(false);
					symbolBtn.setEnabled(false);
					symbolCombo.setEnabled(false);
					
					lnColorBtn.setSelection(false);
					lnColorBtn.setEnabled(false);
				}
			}
		});

		//Group for text label and symbol label
		Group lblGrp = new Group(panel1, SWT.NONE);
		GridLayout layout1 = new GridLayout(1,  false);
	    layout1.marginWidth = 10;
	    lblGrp.setLayout(layout1);
		
		//text-label check box
		Composite txtComp = new Composite( lblGrp, SWT.NONE);
		GridLayout txtLayout = new GridLayout(2,false);
		txtLayout.marginWidth = 0;
		txtComp.setLayout(txtLayout);
		
		txtBtn = new Button(txtComp, SWT.CHECK);
		txtBtn.setLayoutData(new GridData(55, SWT.DEFAULT));
		txtBtn.setText("Text");
		txtBtn.setEnabled(flagLabel);
		txtBtn.setSelection(true);
		txtBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() && symbolBtn.getSelection() ){
					//de-select symbol 
					symbolBtn.setSelection(false);
				}
			}
		});
		
		//text-label drop-down menu
		txtCombo = new Combo( txtComp, SWT.DROP_DOWN | SWT.READ_ONLY );
		txtCombo.setLayoutData(new GridData(108, SWT.DEFAULT));
		
		txtCombo.addSelectionListener(new SelectionAdapter(){
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				prevLbl = ((Combo)e.widget).getText(); 
				setDefaultLineAttr( outlookCombo.getText() + txtCombo.getText() );

			}
		});
		
		setDefaultLabels( outlookCombo.getText());
		
		if ( prevLbl == null ){
			txtCombo.select( 0 );
			prevLbl = txtCombo.getText();
		}
	
		//symbol check box
		symbolBtn = new Button(txtComp, SWT.CHECK);
		symbolBtn.setText("Symbol");
		symbolBtn.setEnabled(flagLabel);
		symbolBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() && txtBtn.getSelection() ){
					txtBtn.setSelection(false);
				}
			}
		});

		//symbol drop-down menu
		symbolCombo = new SymbolCombo(txtComp);
		symbolCombo.setLayoutData(new GridData(10, 1));
		symbolCombo.setItems( SYMBOL_LIST );
		symbolCombo.select( 0 );

		//'Use Line Color' check box
		lnColorBtn = new Button( lblGrp, SWT.CHECK);
		lnColorBtn.setText("Use Line Color");
		lnColorBtn.setEnabled(flagLabel);
		lnColorBtn.setSelection(useLineColor);
		lnColorBtn.addSelectionListener(new SelectionAdapter(){
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				useLineColor = ((Button)e.widget).getSelection();
			}
		});
		

		//Container to hold 'Add-Line', 'Del-Line', 'Set-Cont', and 'Show-Cont' buttons 
		Composite btnsComp = new Composite(top, SWT.None);
		GridLayout gl = new GridLayout(2, false);
		gl.marginLeft = 15;
		
		btnsComp.setLayout(gl);
		
		//'Add Line' button
		addLineBtn = new Button(btnsComp, SWT.PUSH);
		addLineBtn.setText("Add Line");
		addLineBtn.setLayoutData(new GridData(120,30));
		addLineBtn.setEnabled(flagAction);
		addLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				PgenUtil.loadOutlookDrawingTool();
			}
		});
		
		//'Del Line' button
		delLineBtn = new Button(btnsComp, SWT.PUSH);
		delLineBtn.setText("Del Line");
		delLineBtn.setLayoutData(new GridData(120,30));
		delLineBtn.setEnabled(flagAction);
		delLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( drawingLayer.getSelectedDE() != null && de != null && de instanceof Line && 
						de.getParent().getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)) {
					
					DECollection dec = (DECollection)de.getParent();
					
					//get the outlook that contains the current de. 
					Outlook oldOtlk = null;
					if (dec.getParent() instanceof Outlook){
						oldOtlk = (Outlook)dec.getParent();
					}
					else if ( dec.getParent().getParent() instanceof Outlook ){
						oldOtlk = (Outlook)dec.getParent().getParent();
					}
					
					if ( oldOtlk.size() <= 1 ) {
						//if this is the last line, remove the outlook.
						if ( drawingLayer.getActiveLayer().getDrawables().contains(oldOtlk))
						drawingLayer.removeElement( oldOtlk );
					}
					else {
						//to make the undo work, the entire outlook should be replaced.
						
						//save the category of the de and mark the deleting Line by setting category to 'del'
						String deCat = de.getPgenCategory();
						de.setPgenCategory("del");

						//make a copy of current outlook
						Outlook newOtlk = oldOtlk.copy();
						
						//remove the deleting line from the new outlook
						Iterator<DrawableElement> it = newOtlk.createDEIterator();
						while( it.hasNext() ){
							DrawableElement tmpDe = it.next();
							if ( tmpDe instanceof Line && tmpDe.getPgenCategory().equalsIgnoreCase("del")){
								newOtlk.removeLine((Line)tmpDe);
								break;
							}
						}
						
						//set back the de category
						de.setPgenCategory(deCat);
						
						//replace the old outlook with the new one
						drawingLayer.replaceElement( oldOtlk, newOtlk );

					}

					drawingLayer.removeSelected();

				}

				if ( mapEditor != null ) {
					mapEditor.refresh();
				}
			}
		});	

		//'Set-Cont' button
		setContBtn = new Button(btnsComp, SWT.PUSH);
		setContBtn.setText("Set Cont");
		setContBtn.setLayoutData(new GridData(120,30));
		setContBtn.setEnabled(flagAction);
		setContBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				Outlook ol = null;
				if ( de.getParent() instanceof Outlook ){
					ol = (Outlook)de.getParent();
				}
				else if ( de.getParent().getParent() instanceof Outlook ){
					ol = (Outlook)de.getParent().getParent();
				}
				drawingLayer.removeSelected();
				PgenUtil.loadOutlookSetContTool(ol);
				addLineBtn.setEnabled( false );
				delLineBtn.setEnabled( false );
			}
		});

		//'Show-Cont' button
		showContBtn = new Button(btnsComp, SWT.TOGGLE);
		showContBtn.setText("Show Cont");
		showContBtn.setLayoutData(new GridData(120,30));
		showContBtn.setEnabled(flagAction);
		showContBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (((Button) e.widget).getSelection() ) {
					
					if ( de.getParent().getParent() instanceof Outlook ){
						showContLines((Outlook)de.getParent().getParent());
					}
					else if ( de.getParent().getParent().getParent() != null &&  
							 de.getParent().getParent().getParent() instanceof Outlook ){
						showContLines((Outlook)de.getParent().getParent().getParent());
					}
					
				}
				else {
					drawingLayer.removeGhostLine();
					mapEditor.refresh();
				}
			}
		});

		//'Format' button
		fmtBtn = new Button(btnsComp, SWT.PUSH);
		fmtBtn.setText("Format");
		fmtBtn.setLayoutData(new GridData(120,30));
		fmtBtn.setEnabled(flagAction);
		fmtBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
			/*	if ( de != null ){
					if(fmtDlg == null || ( fmtDlg != null && fmtDlg.getShell() == null ) ) {
						Outlook otlk = null;
						if ( de.getParent().getParent() instanceof Outlook ){
							otlk = (Outlook)de.getParent().getParent();
						}
						else if ( de.getParent().getParent().getParent() != null && 
								de.getParent().getParent().getParent()instanceof Outlook){
							otlk = (Outlook)de.getParent().getParent().getParent();
						}
						if ( otlk != null ) {
				*/			fmtDlg = new OutlookFormatDlg(OutlookAttrDlg.this.getParentShell(), OutlookAttrDlg.this, findOtlk());
							fmtDlg.open();
			//			}
			//		}
			//	}
			}
		});
		
		AttrDlg.addSeparator(top);

		//Panel to hold line attributes
	    panel2 = new Composite(top, SWT.NONE);

        // layout for the line attributes
        GridLayout p2Layout = new GridLayout(3, false);
        p2Layout.marginHeight = 3;
        p2Layout.marginWidth = 3;
        panel2.setLayout(p2Layout);
        
        chkBox = new Button[5];

        createColorAttr();
        createWidthAttr();
        createSmoothAttr();
        createCloseAttr();
        createFillAttr();
    	
		AttrDlg.addSeparator(top);
	}

	/**
	 * @return the contourParm
	 */
	public String getParm() {
		return contourParm;
	}

	/**
	 * @param contourParm the contourParm to set
	 */
	public void setParm(String contourParm) {
		this.contourParm = contourParm;
	}

	/**
	 * @return the contourLevel
	 */
	public String getLevel() {
		return contourLevel;
	}

	/**
	 * @param contourLevel the contourLevel to set
	 */
	public void setLevel(String contourLevel) {
		this.contourLevel = contourLevel;
	}
	
	/**
	 * @return the contourLevel
	 */
	public String getForecastHour() {
		return contourFcstHr;
	}

	/**
	 * @param contourLevel the contourLevel to set
	 */
	public void setForecsatHour(String fcsthr ) {
		this.contourFcstHr = fcsthr;
	}

	/**
	 * @return the contourTime1
	 */
	public Calendar getTime1() {
		return contourTime1;
	}

	/**
	 * @return the contourTime2
	 */
	public Calendar getTime2() {
		return contourTime2;
	}

	/**
	 * @param contourTime the contourTime to set
	 */
	public void setTime1(Calendar contourTime) {
		this.contourTime1 = contourTime;
	}
	
	/**
	 * @param contourTime the contourTime to set
	 */
	public void setTime2(Calendar contourTime) {
		this.contourTime2 = contourTime;
	}

	/**
	 * @return the contourCint
	 */
	public String getCint() {
		return contourCint;
	}

	/**
	 * @param contourCint the contourCint to set
	 */
	public void setCint(String contourCint) {
		this.contourCint = contourCint;
	}

	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors(){
		if ( chkBox[ChkBox.COLOR.ordinal()].getSelection() ){
		  // IAttribute requires to return an array of colors
		  // The second color is the color to fill.		
	      Color[] colors = new Color[2];
          
          colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );
          
          colors[1] = colors[0];

          return colors;          
		}
		else {
			return null;
		}
	}
	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */	
	private void setColor( Color clr ){

		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));

	}
	
	/**
	 * Returns the line width from the dialog.
	 */	
	public float getLineWidth(){
		if ( chkBox[ChkBox.WIDTH.ordinal()].getSelection() ){
		return widthSpinnerSlider.getSelection();//widthSlider.getSelection();
		}
		else {
			return java.lang.Float.NaN;
		}
	}
	
	/**
	 * Sets the line width value of the dialog.
	 * @param lw
	 */	
	private void setLineWidth( float lw ){
		widthSpinnerSlider.setSelection((int)lw);
		//widthSlider.setSelection( (int)lw );
		//widthText.setText(String.valueOf((int)lw));
	}
	
	/**
	 * Returns the Close flag of the dialog.
	 */
	public Boolean isClosedLine(){
		if ( chkBox[ChkBox.CLOSE.ordinal()].getSelection() ){
			return closedBtn.getSelection();
		}
		else {
			return null;
		}
	}
	
	/**
	 * Returns the fill pattern. FILL_PATTERN_1 is used for outlooks.
	 */		
	public FillPattern getFillPattern(){
		return FillPattern.FILL_PATTERN_1;
	}
	
	/**
	 * Sets the Close flag of the dialog.
	 * @param cls
	 */
	private void setClosed( Boolean cls ){
		if ( closedBtn != null ){
			closedBtn.setSelection( cls );
		}
	}
	
	/**
	 * Returns the Filled flag of the dialog.
	 */	
	public Boolean isFilled(){
		if ( chkBox[ChkBox.FILL.ordinal()].getSelection() ){
	
			return filledBtn.getSelection();
		}
		else {
			return null;
		}
	}
	
	/**
	 * Sets the Filled flag of the dialog.
	 * @param filled
	 */
	private void setFilled( Boolean filled ){
		if ( filledBtn != null ){
			filledBtn.setSelection( filled );
		}
	}
	
	/**
	 * Returns the smooth level of the dialog.
	 */		
	public int getSmoothFactor(){
		if ( chkBox[ChkBox.SMOOTH.ordinal()].getSelection() ){
			return smoothLvlCbo.getSelectionIndex();
		}
		else {
			return -1;
		}
	}
	
	/**
	 * Sets the smooth level of the dialog.
	 * @param sl
	 */
	public void setSmoothLvl( int sl ){
		smoothLvlCbo.select( sl );
	}
	
	/**
	 * Sets values of all line attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr ){

		if ( iattr instanceof ILine ){
			ILine attr = (ILine)iattr;
			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor(clr);

			float lw =  attr.getLineWidth();
			if ( lw > 0 ) this.setLineWidth(lw);

			this.setClosed( attr.isClosedLine() );
			this.setFilled( attr.isFilled() );

			int sl = attr.getSmoothFactor();
			if ( sl >= 0 ) this.setSmoothLvl(sl);
		}
	}
	
	/**
	 *set attributes to the given attributes in an IContours
	 */
	public void setAttrForDlg(IContours ic) {
	     setAttributes( ic );
	}


	/**
	 * Create widgets for the Color attribute
	 */
	private void createColorAttr(){

		chkBox[ChkBox.COLOR.ordinal()] = new Button(panel2, SWT.CHECK);
		chkBox[ChkBox.COLOR.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.COLOR.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					colorLbl.setEnabled(true);
				}
				else {
					colorLbl.setEnabled(false);
				}
			}

		});  

		colorLbl = new Label( panel2, SWT.LEFT );
		colorLbl.setText("Color:");
//		Composite colorGroup = new Composite(top, SWT.NONE);
//		cs = new ColorMatrixSelector( colorGroup, true, true, 30, 30, 14, 16, 24, 24, 0, 2, 2) ;
		cs = new ColorButtonSelector( panel2 );
		cs.setColorValue( new RGB( 255,0,0 ) );
	}

	/**
	 * Create widgets for the Line Width attribute
	 */
	private void createWidthAttr(){

		chkBox[ChkBox.WIDTH.ordinal()] = new Button(panel2, SWT.CHECK);
		chkBox[ChkBox.WIDTH.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.WIDTH.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					widthLbl.setEnabled(true);
					widthSlider.setEnabled(true);
				}
				else {
					widthLbl.setEnabled(false);
					widthSlider.setEnabled(false);

				}
			}

		});  

		widthLbl = new Label(panel2, SWT.LEFT);
		widthLbl.setText("Line Width:");

		GridLayout gl = new GridLayout( 3, false );	
		Group widthGrp = new Group( panel2, SWT.NONE ) ;
		gl.horizontalSpacing = 1;
		gl.verticalSpacing = 0;
		gl.marginHeight = 1;
		gl.marginWidth = 1;
		widthGrp.setLayout( gl );
/*		
		widthSlider = new Slider(widthGrp, SWT.HORIZONTAL);
		widthSlider.setValues(2, 1, 20+2, 2, 1, 3); 
		
		widthText = new org.eclipse.swt.widgets.Text(widthGrp, SWT.SINGLE|SWT.BORDER);
		widthText.setLayoutData(new GridData(30,10));
		gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry listener = 
			new gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry(widthSlider, widthText,1,20);
		widthText.addKeyListener(listener );
		widthText.addVerifyListener(listener);
*/
		widthSpinnerSlider = 
        	new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(widthGrp, SWT.HORIZONTAL,1);
        widthSpinnerSlider.setLayoutData(new GridData(130,30));
        widthSpinnerSlider.setMinimum(1);            
        widthSpinnerSlider.setMaximum(10);
        widthSpinnerSlider.setIncrement(1);
        widthSpinnerSlider.setPageIncrement(3);        
        widthSpinnerSlider.setDigits(0);
	}

	/**
	 * Create widgets for the smooth Level attribute
	 */
	private void createSmoothAttr(){
		chkBox[ChkBox.SMOOTH.ordinal()] = new Button(panel2, SWT.CHECK);
		chkBox[ChkBox.SMOOTH.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.SMOOTH.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					smoothLbl.setEnabled(true);
					smoothLvlCbo.setEnabled(true);
				}
				else {
					smoothLbl.setEnabled(false);
					smoothLvlCbo.setEnabled(false);

				}
			}

		});  

		smoothLbl = new Label(panel2, SWT.LEFT);
		smoothLbl.setText("Smooth Level:");

		smoothLvlCbo = new Combo( panel2, SWT.DROP_DOWN | SWT.READ_ONLY );

		smoothLvlCbo.add("0");
		smoothLvlCbo.add("1");
		smoothLvlCbo.add("2");

		smoothLvlCbo.select( 0 );
	}

	/**
	 * Create widgets for the Closed attribute
	 */	
	private void createCloseAttr(){
		
		chkBox[ChkBox.CLOSE.ordinal()] = new Button(panel2, SWT.CHECK);
		chkBox[ChkBox.CLOSE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.CLOSE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					closedBtn.setEnabled(true);
				}
				else {
					closedBtn.setEnabled(false);

				}
			}

		});
		closedBtn  = new Button(panel2, SWT.CHECK);
		closedBtn.setText("Closed");
		
	}

	/**
	 * Create widgets for the Filled attribute
	 */
	private void createFillAttr(){

		Composite fillGrp = new Composite(panel2, SWT.NONE);
		fillGrp.setLayout(new GridLayout(2, false));

		chkBox[ChkBox.FILL.ordinal()] = new Button(fillGrp, SWT.CHECK);
		chkBox[ChkBox.FILL.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.FILL.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					filledBtn.setEnabled(true);
				}
				else {
					filledBtn.setEnabled(false);

				}
			}

		});

		filledBtn = new Button( fillGrp, SWT.CHECK );
		filledBtn.setText( "Filled" );     

	}

	@Override
	public int open(){
        		
		this.create();

		//show check boxes if for multi-selection
		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")){
			enableChkBoxes(true);
			enableAllWidgets(false);
		}
		else {
			if ( chkBox != null ){
				enableChkBoxes(false);
			}
		}
		
		//set outlook type
		if ( prevType == null ){
			outlookCombo.select( 0 );
		}
		else {
			int idx = (outlookCombo.indexOf(prevType));
			if ( idx >= 0 ) outlookCombo.select(idx);
			else outlookCombo.select(0);
			 //Set default labels for the selected outlook type
			 setDefaultLabels( prevType );
		}
		
		//set label text
		if ( prevLbl == null ){
			txtCombo.select( 0 );
		}
		else {
			int idx = (txtCombo.indexOf(prevLbl));
			if ( idx >= 0 ) txtCombo.select(idx);
			else txtCombo.select(0);
		}

		//set line attributes
		setDefaultLineAttr( outlookCombo.getText() + txtCombo.getText() );

		return super.open();
	}

	/**
	 * Set check boxes visible/invisible
	 * @param flag
	 */
	private void enableChkBoxes(boolean flag){

		if(!flag) {
			setAllChkBoxes();
		}	
		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setVisible(flag);
		}

	}

	/**
	 * enable/disable all line widgets
	 * @param flag
	 */
	private void enableAllWidgets(boolean flag){

		colorLbl.setEnabled(flag);

		widthLbl.setEnabled(flag);
		widthSlider.setEnabled(flag);

		smoothLbl.setEnabled(flag);
		smoothLvlCbo.setEnabled(flag);

		filledBtn.setEnabled(flag);
		closedBtn.setEnabled(flag);	

	}

	/**
	 * Set all multi-selection check boxes to true.
	 */
	private void setAllChkBoxes(){

		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setSelection(true);
		}
	}
	
	/*
	 * Return the status of the label check box
	 */
	public boolean addLabel(){
		return lblBtn.getSelection();
	}
	
	/*
	 * Return the status of the text-label check box
	 */
	public boolean addText(){
		return txtBtn.getSelection();
	}
	
	/*
	 * Return the status of the symbol-label check box
	 */
	public boolean addSymbol(){
		return symbolBtn.getSelection();
	}
	
	/*
	 * Return the status of the 'Use Line Color' check box
	 */
	public boolean useLineColor(){
		return lnColorBtn.getSelection();
	}

	/*
	 * Return the current selection of the text drop-down menu
	 */
	public String getLblTxt(){
		return txtCombo.getText();
	}
	
	/*
	 * Return the current selection of the symbol drop-down menu
	 */
	public String getSymbolType(){
		return symbolCombo.getSelectedText();
	}
	
	/*
	 * Return the symbol category of the current selection
	 */
	public String getSymbolCat(){
		if ( getSymbolType().contains("|")){
			return "Combo";
		}
		else {
			return "Symbol";
		}
	}
	
	/*
	 * Return the current selection of the outlook type
	 */
	public String getOutlookType(){
		return outlookCombo.getText();
	}

	/**
	 * Enable/disable the 'Add Line' and 'Del Line' buttons
	 * @param flag
	 */
	public void enableAddDel( boolean flag ){
		addLineBtn.setEnabled(flag);
		delLineBtn.setEnabled(flag);
	}
	
	/**
	 * Show the virtual continue lines
	 * @param ol - outlook
	 */
	public void showContLines(Outlook ol) {
		Iterator<AbstractDrawableComponent> it = ol.getComponentIterator();
		DECollection vLines = new DECollection("OutlookVirtualLines");
		
		//loop through all OUTLOOK_LINE_GROUP and set the virtual lines
		while ( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc.getName().equalsIgnoreCase( Outlook.OUTLOOK_LINE_GROUP) ){
				
				//get all lines in the group
				ArrayList<Line> lns = new ArrayList<Line>();
				
				Iterator<DrawableElement> itDe = adc.createDEIterator();
				while(itDe.hasNext()){
					DrawableElement dElem = itDe.next(); 
					if ( dElem instanceof Line ){
						lns.add((Line)dElem);
					}
				}
				
				//create virtual lines 
				for (int ii = 0; ii <lns.size()-1; ii++ ){
					ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
					pts.add(lns.get(ii).getPoints().get(lns.get(ii).getPoints().size()-1));
					pts.add(lns.get(ii+1).getPoints().get(0));

					Line vLn = new  Line(null, lns.get(0).getColors(),
							lns.get(0).getLineWidth(), lns.get(0).getSizeScale(), 
							false, false, pts,
							lns.get(0).getSmoothFactor(), 
							null, "Lines", "LINE_DASHED_4");

					vLines.add(vLn);
				}
				
			}
			
		}
		
		//show the virtual lines
		if (!vLines.isEmpty()){
			drawingLayer.setGhostLine(vLines);
			mapEditor.refresh();
		}
	}
	
	/**
	 * Read outlook type document
	 * @return - outlook type document
	 */
	public static Document readOutlookTbl() {
		
		if (outlookTbl == null) {
			try {
				String outlookTypeFile = PgenStaticDataProvider.getProvider().getFileAbsolutePath(
						   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "outlooktype.xml" );

				SAXReader reader = new SAXReader();
				outlookTbl = reader.read(outlookTypeFile);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		return outlookTbl;
	}
	
	/**
	 * Get outlook types from the outlook type document
	 */
	private List<String> getOutlookTypes() {

		List<String> otlkType = new ArrayList<String>();
		Document doc = readOutlookTbl();
		if (doc == null)
			return otlkType;
		
		List<Node> nodes = readOutlookTbl().selectNodes(OTLK_XPATH);
		for (Node node : nodes) {
			otlkType.add(node.valueOf("@name"));
		}

		return otlkType;
	}
	
	/**
	 * Get default text labels for the input outlook type
	 * @param type - outlook type
	 * @return
	 */
	private List<String> getLabelsForType(String type){
		
		List<String> lbls = new ArrayList<String>();
		String xpath = OTLK_XPATH + "[@name='" + type.toUpperCase() +"']";
		
		Node otlkType = readOutlookTbl().selectSingleNode(xpath);
		List<Node> nodes = otlkType.selectNodes("label");
		for (Node node : nodes) {
			lbls.add(node.valueOf("@name"));
		}

		return lbls;
	}
	
	/**
	 * Get output text string for the specified label of the outlook type
	 * @param type - outlook type
	 * @return
	 */
	public String getTextForLabel(String outlookType, String label){

		if (label == null || label.isEmpty() ) return "";
		
		String ret = "";
		String xpath = OTLK_XPATH + "[@name='" + outlookType.toUpperCase() +"']";
		
		Node otlkType = readOutlookTbl().selectSingleNode(xpath);
		List<Node> nodes = otlkType.selectNodes("label");
		
		for (Node node : nodes) {
			if ( label.equals(node.valueOf("@name"))){
				ret = node.valueOf("@text");
				break;
			}
		}

		if ( ret.isEmpty() ) ret = label;
		
		return ret;
	}
	/**
	 * Get default values of the text labels for the input outlook type
	 * @param type - outlook type
	 * @return
	 */
	public String getCatmapForType(String type){
		
		StringBuilder cmap = new StringBuilder("");
	      
		String xpath = OTLK_XPATH + "[@name='" + type.toUpperCase() +"']";
		
        boolean allNumbers = true;
		Node otlkType = readOutlookTbl().selectSingleNode(xpath);
		if ( otlkType != null ) {
			List<Node> nodes = otlkType.selectNodes( "label" );
            
		    for (Node node : nodes) {
			    String text = node.valueOf( "@name" );
			    cmap.append( text );
			    cmap.append( "=" );
			
			    String value = node.valueOf( "@value" );
			
			    StringBuilder cb = new StringBuilder( "" );			
			    if ( value == null || value.length() == 0 ) {
			        for ( int ii = 0; ii < text.length(); ii++ ) {
			            char ch = text.charAt( ii );
			            if ( ch == '.' || (ch >= '0'  && ch <= '9' ) ) {
			                cb.append( ch );
			            }
			            else {
			            	allNumbers = false;
			            }
			        }
				
			        value = cb.toString();
			    }
			    else {
			    	allNumbers = false;
			    }
			
			    cmap.append( value );
			    cmap.append( ";" );
			}
		}
		
		if ( allNumbers ) {
			return new String("");
		}
		else {
		    return cmap.toString();
		}
	}

	/**
	 * Set default labels for iput outlook type
	 * @param otlkType
	 */
	private void setDefaultLabels( String otlkType ){
		List<String> lbls = getLabelsForType( otlkType );
		if ( !lbls.isEmpty() ){
			txtCombo.removeAll();
			for ( String str : lbls ){
				txtCombo.add(str);
			}
		}
		else {
			txtCombo.add("2%");
			txtCombo.add("5%");
			txtCombo.add("10%");
		}
		
		txtCombo.add("Other");
		
		//set label text
		if ( prevLbl == null ){
			txtCombo.select( 0 );
		}
		else {
			int idx = (txtCombo.indexOf(prevLbl));
			if ( idx >= 0 ) txtCombo.select(idx);
			else txtCombo.select(0);
		}
	}
	
	/**
	 * Set the outlook type drop-down menu to the input type
	 * @param type
	 */
	public void setOtlkType( String type ){
		int idx = outlookCombo.indexOf(type.toUpperCase());
		if ( idx < 0 ) {
			idx = 0;
			type = outlookCombo.getItem(idx);
		}
		if ( idx >= 0 ){
			outlookCombo.select( outlookCombo.indexOf(type.toUpperCase()));
			
			 //disable some fields such as "Make Grid" for some outlooks
			 showGrid = showMakeGrid(type);
			 flagLabel = showLabel(type);
			 flagAction = showAction(type);
			 
			 infoBtn.setEnabled(showGrid); 
			 makeGridBtn.setEnabled( showGrid );			 
			 lblBtn.setEnabled(flagLabel);
			 txtBtn.setEnabled(flagLabel);
			 symbolBtn.setEnabled(flagLabel);
			 lnColorBtn.setEnabled(flagLabel);
			 addLineBtn.setEnabled(flagAction);
			 delLineBtn.setEnabled(flagAction);
			 setContBtn.setEnabled(flagAction);
			 showContBtn.setEnabled(flagAction);
			 fmtBtn.setEnabled(flagAction);

			 needFromLine = setFromLineFlag(type); 
			 
			setDefaultLabels( this.getOutlookType());
			setDefaultLineAttr( outlookCombo.getText() + txtCombo.getText());
		}
	}
		
	/**
	 * Set the text label to the input string
	 * @param lbl
	 */
	public void setLabel( String lbl ){
		int idx = txtCombo.indexOf(lbl);
		if ( idx >= 0 ) {
			txtCombo.select(idx);
			prevLbl = lbl;
		}
		
		String grpType = outlookCombo.getText();
		this.setDefaultLineAttr( grpType + txtCombo.getText());
		
		//disable some fields such as "Make Grid" for some outlooks
		 showGrid = showMakeGrid(grpType);
		 flagLabel = showLabel(grpType);
		 flagAction = showAction(grpType);
		 
		infoBtn.setEnabled(showGrid); 
		makeGridBtn.setEnabled( showGrid);			 
		lblBtn.setEnabled(flagLabel);
		txtBtn.setEnabled(flagLabel);
		symbolBtn.setEnabled(flagLabel);
		lnColorBtn.setEnabled(flagLabel);
		addLineBtn.setEnabled(flagAction);
		delLineBtn.setEnabled(flagAction);
		setContBtn.setEnabled(flagAction);
		showContBtn.setEnabled(flagAction);
		fmtBtn.setEnabled(flagAction);	
	}
	
	/**
	 *  Load default settings from outlooksettings.xml
	 *  Setting is a hash map with outlook type and label string as keys.
	 */
	private static void loadSettings() {		
		
		settings = new HashMap<String, Line>();
		
		String settingFile = PgenStaticDataProvider.getProvider().getFileAbsolutePath(
				   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "outlooksettings.xml" );
	
		gov.noaa.nws.ncep.ui.pgen.file.Products products = FileTools.read( settingFile );
		
        if ( products != null ){
        	List<Product> prds;

        	prds = ProductConverter.convert( products );

        	for ( Product p:prds ) {

        		for ( Layer layer:p.getLayers() ) {

        			Iterator<AbstractDrawableComponent> it = layer.getComponentIterator();
        			while( it.hasNext()){
        				AbstractDrawableComponent adc = it.next();
        				if ( adc.getName().equalsIgnoreCase("OUTLOOK")) {
        					Iterator<AbstractDrawableComponent> itLn = ((Outlook)adc).getComponentIterator();
        					while ( itLn.hasNext() ){
        						AbstractDrawableComponent lnGrp = itLn.next();
        						if ( lnGrp.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)) {
        							String key = null;
        							Line ln = null;
        							Iterator<DrawableElement> itDe = ((DECollection)lnGrp).createDEIterator();
        							while( itDe.hasNext() ){
        								DrawableElement de = itDe.next();
        								if( de instanceof Text ){
        									key = ((Outlook)adc).getOutlookType() + ((Text)de).getText()[0];
        								}
        								else if ( de instanceof Line ){
        									ln = (Line) de;
        								}
        							}
        							
        							if ( key != null && ln != null ){
        								settings.put( key, ln );        			

        							}
        						
        						}
        						
        					}
        					
        				}
        			}      	          		
        		}
        	}        
        }
	}
	
	/**
	 * Set default line attributes
	 * @param key - string of 'outlook type + label'
	 */
	private void setDefaultLineAttr( String key ){
		if ( settings != null ){
			Line ln = settings.get(key);
			if ( ln != null ){
				this.setAttrForDlg((IAttribute)ln);
				lineType = ln.getPgenType();
			}
		}
	}
	
	public String getLineType(){
		return lineType;
	}
	
	/*
	 * Open the dialog to do graph-to-grid processing.
	 */	
	private void openG2GDlg() {
    	
		if ( g2gDlg == null ) {
			try {
				
			    g2gDlg = new GraphToGridParamDialog( 
			    		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() );                	    
	    		g2gDlg.setCntAttrDlg( this );
	    			    		
			} catch (VizException e) {
				
				e.printStackTrace();				
			}	

		}
    	
    	if( g2gDlg != null ) {
    		g2gDlg.open();
    	}
   	
	}

    /**
     * Close all related dialogs.
     */
	@Override
	public boolean close(){
				
		if ( g2gDlg != null )  g2gDlg.close();
		
		return super.close();
		
	}

    /**
     * Get the outlook of the current type in current layer.
     * @return
     */
    public Outlook getCurrentOtlk(){
    	
    	Outlook ol = null;
    	
    	String type = getOutlookType();
    	
    	Iterator<AbstractDrawableComponent> it = drawingLayer.getActiveLayer().getComponentIterator();
    	while ( it.hasNext() ){
    		AbstractDrawableComponent adc = it.next();
    		if ( adc instanceof Outlook && adc.getPgenType().equalsIgnoreCase( type )){
    			ol = (Outlook)adc;
    			break;
    		}
    	}
    	
    	return ol;
    }

	/**
	 * Open the dialog to edit the contours info.
	 */	
	private void openContourInfoDlg() {
    	
		if ( contoursInfoDlg == null ) {
			contoursInfoDlg = new ContoursInfoDlg( this.getShell() );                	    
   	        contoursInfoDlg.setContoursAttrDlg( this );
		}
    	
    	contoursInfoDlg.open();
   	
	}
	
	/**
	 * Set the dialog attributes.
	 */		
	public void setAttributes( IContours attr ) {
		
		setParm( attr.getParm() );
		setLevel( attr.getLevel() );
		setTime1( attr.getTime1() );		
		setTime2( attr.getTime2() );		
		setCint( attr.getCint() );
		
		setInfoBtnText();
	}

	
	/**
	 * Set the text on the info button based on parm, level, and time.
	 */
	private void setInfoBtnText() {
		
		if ( contourParm == null || contourLevel == null 
				|| contourTime1 == null ) return;
		
		String str = contourParm + ", " + contourLevel + "\n" + 		             
		             contourTime1.get(Calendar.YEAR) + "-" +  
	                 (contourTime1.get(Calendar.MONTH)+ 1) + "-" + 
	                 contourTime1.get(Calendar.DAY_OF_MONTH) + "  " +
	                 contourTime1.get(Calendar.HOUR_OF_DAY)  + ":" +
	                 contourTime1.get(Calendar.MINUTE) + "Z";
		
		infoBtn.setText( str );
		
	}
	
	/**
	 * Updates the selected outlook and redraws the PGEN layer.
	 */
	public void okPressed(){

		DrawableElement de = drawingLayer.getSelectedDE();
		DECollection parentAdc = null;
		
		// Find the DECollection that contains the selected DE
		if ( de != null &&  de.getParent() instanceof DECollection ) {
			parentAdc = (DECollection)de.getParent();
		}
		
		/*
		 * Find the Outlook that contains the selected DE. Create a copy of it, and
		 * update its attributes as well as the label/line attributes.
		 */		
		if ( parentAdc != null && parentAdc.getParent() instanceof Outlook ) {
            
			DrawableElement newEl = (DrawableElement)de.copy();
						
			Outlook oldOutlook = (Outlook)de.getParent().getParent();			

			Outlook newOutlook = oldOutlook.copy();
			newOutlook.clear();
			
			Iterator<AbstractDrawableComponent> iterator = oldOutlook.getComponentIterator();
            			
			while ( iterator.hasNext() ){
				
				AbstractDrawableComponent oadc = iterator.next();
				AbstractDrawableComponent nadc = oadc.copy();
				nadc.setParent( newOutlook );
				
				if ( oadc.equals( parentAdc ) ) {
					
					Iterator<DrawableElement> deit = parentAdc.createDEIterator();
					((DECollection)nadc).clear();
					
					while ( deit.hasNext() ) {

						DrawableElement ode = deit.next();
						AbstractDrawableComponent nde = ode.copy();
						nde.setParent( nadc );
						
					    if ( nde instanceof Line ) {
							((Line)nde).update( this );
						    newEl = (DrawableElement)nde;
						}
				        else if ( nde instanceof Text ) {
				        	((Text)nde).setText( new String[] { this.getLblTxt() } );
								
							if ( useLineColor() ) {									
								((Text)nde).setColors( this.getColors() );
							}																						
					    }
						else if ( nde instanceof Symbol ) {
							((Symbol)nde).setPgenCategory( this.getSymbolCat() );
							((Symbol)nde).setPgenType( this.getSymbolType() );							
							if ( useLineColor() ) {									
								((Symbol)nde).setColors( this.getColors() );
							}
							
						}	
					    
					    ((DECollection)nadc).addElement( nde );
					}					
					
				}
				
				newOutlook.addElement( nadc );

			}
								
			/*
			 * Update the outlook attributes and replace the old one with the new outlook.
			 */
			newOutlook.update( this );	
			newOutlook.reorderLines();
			newOutlook.setOutlookType( this.getOutlookType() );
			
			drawingLayer.replaceElement( oldOutlook, newOutlook );
		    
			/*
			 * Reset the selected contours and DE to the updated ones.
			 */
			drawingLayer.removeSelected();
			drawingLayer.setSelected( newEl );
				            			
		}
		
		
		if ( mapEditor != null ) {
			mapEditor.refresh();
		}

	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Coordinate[] getLinePoints() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Check the outlook type table to see if the "Make Grid" need enable. 
	 * @param otlk
	 * @return
	 */
	private boolean showMakeGrid( String type ){
		boolean ret = true;
		if ( type != null ){
			type = type.toUpperCase();
			String xpath = OutlookAttrDlg.OTLK_XPATH +"[@name='" + type + "']"; 
			String makegrid = OutlookAttrDlg.readOutlookTbl().selectSingleNode(xpath).valueOf("@makegrid");
			if ( makegrid != null && !makegrid.isEmpty() && makegrid.equalsIgnoreCase("false")){
				ret = false;
			}
		}
		
		return ret;		
	}

	/**
	 * Check the outlook type table to see if the "Label" buttons need enable. 
	 * @param otlk
	 * @return
	 */
	private boolean showLabel( String type ){
		boolean ret = true;
		if ( type != null ){
			type = type.toUpperCase();
			String xpath = OutlookAttrDlg.OTLK_XPATH +"[@name='" + type + "']"; 
			String makegrid = OutlookAttrDlg.readOutlookTbl().selectSingleNode(xpath).valueOf("@flagLabel");
			if ( makegrid != null && !makegrid.isEmpty() && makegrid.equalsIgnoreCase("false")){
				ret = false;
			}
		}
		
		return ret;		
	}
	
	/**
	 * Check the outlook type table to see if the "Action" buttons need enable. 
	 * @param otlk
	 * @return
	 */
	private boolean showAction( String type ){
		boolean ret = true;
		if ( type != null ){
			type = type.toUpperCase();
			String xpath = OutlookAttrDlg.OTLK_XPATH +"[@name='" + type + "']"; 
			String makegrid = OutlookAttrDlg.readOutlookTbl().selectSingleNode(xpath).valueOf("@flagAction");
			if ( makegrid != null && !makegrid.isEmpty() && makegrid.equalsIgnoreCase("false")){
				ret = false;
			}
		}
		
		return ret;		
	}
	
	/**
	 * Check the outlook type table to see if the "Make Grid" need enable. 
	 * @param otlk
	 * @return
	 */
	private boolean setFromLineFlag( String type ){
		boolean ret = false;
		if ( type != null ){
			type = type.toUpperCase();
			String xpath = OutlookAttrDlg.OTLK_XPATH +"[@name='" + type + "']"; 
			String makegrid = OutlookAttrDlg.readOutlookTbl().selectSingleNode(xpath).valueOf("@fromline");
			if ( makegrid != null && !makegrid.isEmpty() && makegrid.equalsIgnoreCase("true")){
				ret = true;
			}
		}
		
		return ret;
		
	}
	
	public boolean fromLineFlag() {
		return needFromLine;
	}
	
	/**
	 * Find the outlook to format.
	 * 1. check if an outlook is selected
	 * 2. get the first outlook in the layer
	 * 3. null
	 * @return
	 */
	private Outlook findOtlk(){
		Outlook otlk = null;
		
		//check selected DE
		//Comment out because the selected outlook may not be the same type of the active layer
	/*	AbstractDrawableComponent selected =  drawingLayer.getSelectedComp();
		if ( selected != null )
			if ( selected.getParent().getParent() instanceof Outlook ){
				otlk = (Outlook)selected.getParent().getParent();
			}
			else if ( selected.getParent().getParent().getParent() != null && 
					selected.getParent().getParent().getParent()instanceof Outlook){
				otlk = (Outlook)selected.getParent().getParent().getParent();
			}



		}
		//loop through the layer
		else {
		*/	
		
			String otlkType = drawingLayer.getActiveLayer().getMetaInfoFromKey(OutlookAttrDlg.OTLK_TYPE_IN_LAYER_META);
			if ( otlkType == null || otlkType.isEmpty() ){
				otlkType = getOutlookType();
			}
			
			Iterator<AbstractDrawableComponent> it = drawingLayer.getActiveLayer().getComponentIterator();
		
		
			while ( it.hasNext() ){
				AbstractDrawableComponent adc = it.next();
				if (adc  instanceof Outlook && ((Outlook)adc).getOutlookType().equalsIgnoreCase(otlkType)){
					otlk = (Outlook) adc;
					break;
				}
			}
	//	}
		
		return otlk;
	}

}
