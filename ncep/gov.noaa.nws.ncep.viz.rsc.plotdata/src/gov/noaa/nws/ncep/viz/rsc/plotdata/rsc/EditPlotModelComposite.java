package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Scale;

/**
 *  UI for editing Point data resource attributes. 
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/15/2009    172       	M. Li  		Initial creation.
 * 12/05/2009    217        Greg Hull   broke out from plot manager dialog and reworked.
 * 08/09/2010	 291		G. Zhang	add support for more data resources
 * 03/07/2011               Greg Hull   remove duplicate topComposite which created a
 *                                      blank area at top of dialog.  
 * 03/31/2011    425        Greg Hull   Refactor and created PlotModelElemCenterButton
 * 07/13/2011    264        Archana     Updated initWidgets() to sort the names of the plot  
 *                                      parameters before populating the list.
 * 08/10/2011    450        Greg Hull   use PlotParameterDefns instead of PlotParameterDefnsMngr
 * 11/01/2011    482        Greg Hull   add tooTips, move font/size/style 
 * 11/03/2011    482        Greg Hull   add unimplemented Clear and Reset, move Text Attributes
 * 04/16/2012    615        S. Gurung   Adjusted size for PlotModelElemButton
 * 05/02/2012    778        Q. Zhou     Changed scale from integer to double  
 * 05/29/2012    654        S. Gurung   Added option "Apply to All" to apply text changes to all parameters;
 * 										Added additional options to textFontOptions and textStyleOptions;
 * 										Fixed the issue of Sky Coverage parameters not appearing in the Sky Coverage drop-down list.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class EditPlotModelComposite extends Composite {	
	
	private PlotModel editedPlotModel = null;
	
	private ColorMatrixSelector cms = null;

	private List  availParamsList = null;
		
	private Scale symbolSizeScale = null;
	private Combo textSizeCombo = null;
	private Combo textFontCombo = null;
	private Combo textStyleCombo = null;
	private Button applyToAllBtn = null;
		
	private final String[] textSizeOptions = {"6", "8", "10", "12", "14", "16", "18"};
	private final String[] textFontOptions = {"Courier", "Helvetica", "Times", "Standard"}; 		
	private final String[] textStyleOptions = {"Normal", "Italic", "Bold", "Bold-Italic"};   		

	private final String[] plotModelElementPositions = {
			      "TC", 
			"UL", "UC", "UR", 
			"ML", "MC", "MR",
			"LL", "LC", "LR",
			      "BC"
	};

	// 
	private HashMap<String,PlotModelElemButton> plotModelElementsUIMap = 
		                 new HashMap<String,PlotModelElemButton>();

	private PlotParameterDefns plotParamDefns = null;	

	private PlotModelElemButton seldPlotModelElemButton = null;
	
	private Composite topComposite = null;
	
	// for getting rsc related parameters
	INatlCntrsResourceData rscData = null;
	
	//group for center buttons
	private Group ctrGrp = null;
	
	private Combo comboSky = null;
	private Combo comboBrbk = null;
	
	private ArrayList<String> availWindBarpParams = null;
	private ArrayList<String> availSkyCoverParams = null;

	//declare here for setting the text to reflect the actual size
	private Label symSizeLabel = null;
	
	//center parameter only Buttons
//	private Button brGrpChkBtn = null;
//	private Button skycChkBtn = null;
	private Label skycLbl = null;
	private Label wndBrbLbl = null;
	
	public EditPlotModelComposite( Composite parent, int style, PlotModel pm, INatlCntrsResourceData rscData ) {
		super(parent,  style);
		editedPlotModel = pm;
		this.rscData = rscData;

		topComposite = this;//new Group( parent, SWT.SHADOW_NONE );
		
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		mainLayout.verticalSpacing = 5;
		topComposite.setLayout(mainLayout);

		plotParamDefns = PlotParameterDefnsMngr.getInstance().getPlotParamDefns( 
								editedPlotModel.getPlugin() );		
		
		createPlotModelGuiElements();
		
		createTextAttrControls();

		createCtrParamControls();
		
		createParmListControls();


		initWidgets();
	}

	/*
	 * Create text attributes -- size, font and style
	 */
	private void createTextAttrControls() {
		Group textAttrGrp = new Group ( topComposite, SWT.SHADOW_NONE );
		GridLayout gl = new GridLayout(7, false);
		gl.marginTop = 0;
		gl.marginBottom = 3;
		gl.marginRight = 0;
		textAttrGrp.setLayout( gl );
		textAttrGrp.setText("Text");

		// Text size attribute
		new Label(textAttrGrp, SWT.NONE).setText("size");
		textSizeCombo = new Combo(textAttrGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		textSizeCombo.setEnabled(false); // wait til a plot element is selected.
		textSizeCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				if (seldPlotModelElemButton != null) {
					seldPlotModelElemButton.getPlotModelElement().setTextSize( textSizeCombo.getText() );
					
					if (applyToAllBtn.getSelection()) {
		    			applyTextChangesToAllParameters();
		    		}
				}
			}
		});

		// Text font attribute
		new Label(textAttrGrp, SWT.NONE).setText(" font");
		textFontCombo = new Combo(textAttrGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		textFontCombo.setEnabled(false);
		textFontCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				if( seldPlotModelElemButton != null ) {
					seldPlotModelElemButton.getPlotModelElement().setTextFont(textFontCombo.getText() );
					
					if ("Standard".equals(textFontCombo.getText())) {
						textStyleCombo.select(0);
						textStyleCombo.setEnabled(false);
					} else {
						textStyleCombo.setEnabled(true);
					}
					
					if (applyToAllBtn.getSelection()) {
		    			applyTextChangesToAllParameters();
		    		}					
				}
			}
		});

		// Text style attribute
		new Label(textAttrGrp, SWT.NONE).setText(" style");
		textStyleCombo = new Combo(textAttrGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		textStyleCombo.setEnabled(false);
		textStyleCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				if (seldPlotModelElemButton != null) {
					seldPlotModelElemButton.getPlotModelElement().setTextStyle(textStyleCombo.getText() );
					
					if (applyToAllBtn.getSelection()) {
		    			applyTextChangesToAllParameters();
		    		}
				}
			}
		});
		
		applyToAllBtn = new Button ( textAttrGrp, SWT.CHECK );
        applyToAllBtn.setText("Apply to All");
        applyToAllBtn.setToolTipText("Apply changes to all text parameters");        
        applyToAllBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		
        		if (applyToAllBtn.getSelection()) {
        			applyTextChangesToAllParameters();
        		}
        	}
        });
        
	}

	/*
	 * Create Parameter List, symbol size and color picker
	 */
	private void createParmListControls() {
		Composite comp = new Composite( topComposite, SWT.NONE);
		GridLayout gl = new GridLayout(3, false);
		comp.setLayout(gl);

		// Parameter List
		GridData gd = new GridData(110, 149);
		Group parmListGrp = new Group ( comp, SWT.SHADOW_NONE );
		parmListGrp.setLayout(new GridLayout(1, false));
		parmListGrp.setText("Plot Parameters");
		availParamsList = new List(parmListGrp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
		availParamsList.setLayoutData(gd);
		availParamsList.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				String selectedParm = availParamsList.getSelection()[0];

				if( seldPlotModelElemButton != null) { // sanity check
					if( !seldPlotModelElemButton.isParamNameSelected() ) {
//						seldPlotModelElemButton.setColor( )
					}
					seldPlotModelElemButton.setParmName( selectedParm );
				}
			}	        	
		});

		// Symbol size
		GridData gd2 = new GridData(45,114);
		Group symbolSizeGrp = new Group ( comp, SWT.SHADOW_NONE );
		symbolSizeGrp.setLayout(new GridLayout(1, false));
		symbolSizeGrp.setText("Symbol\n   Size");

		symSizeLabel = new Label(symbolSizeGrp, SWT.BOLD);
		symSizeLabel.setText("    0.5");

		symbolSizeScale = new Scale(symbolSizeGrp, SWT.VERTICAL);
		symbolSizeScale.setLayoutData(gd2);
		symbolSizeScale.setMinimum(5);
		symbolSizeScale.setMaximum(30);
		symbolSizeScale.setIncrement(1);
		symbolSizeScale.setPageIncrement(1);
		symbolSizeScale.setSelection(5);
		symbolSizeScale.setEnabled(false); // wait til a plot element is selected
		symbolSizeScale.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				double selection = symbolSizeScale.getSelection();					
				
				symSizeLabel.setText(String.valueOf(selection/10));
				
				if(seldPlotModelElemButton != null ) {
					seldPlotModelElemButton.setSymbolSize( selection/10 );
				}
			}
		});

		// Color selector
		Group selColorGrp = new Group ( comp, SWT.SHADOW_NONE );
		selColorGrp.setText("Color");

		cms = new ColorMatrixSelector( selColorGrp, false, true,
				22, 88, 18, 22, 28, 86, 0, 4, 5);
		cms.setColorValue(new RGB(0,255,0));
		cms.addListener(new IPropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				if(seldPlotModelElemButton != null ) {
					RGB rgb = cms.getColorValue();
					seldPlotModelElemButton.setColor( rgb );
					seldPlotModelElemButton.setButtonAsSelected();
				}
			}
		});   	
	}
	
	private void createPlotModelGuiElements() {		
		// Create Top position button    
		Composite comp = new Composite( topComposite, SWT.NONE);
		GridLayout gl = new GridLayout(3, true);
		gl.verticalSpacing = 0;
		gl.marginHeight = 0;
		
		comp.setLayout(gl);

		GridData gd = new GridData();
		gd.horizontalAlignment = GridData.CENTER;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		
		comp.setLayoutData(gd);

		Button clearPlotModelBtn = new Button( comp, SWT.PUSH );
		clearPlotModelBtn.setText("Clear");
		gd = new GridData();
		gd.horizontalAlignment = GridData.BEGINNING;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = GridData.CENTER;
		clearPlotModelBtn.setLayoutData( gd );
		clearPlotModelBtn.setEnabled( false );
		
		PlotModelElement pme = editedPlotModel.getPlotModelElement( plotModelElementPositions[0] );
		
		// create a blank element and only add it to the editedPlotModel if the user selects a parameter.
		if( pme == null ) {
			pme = new PlotModelElement();
			pme.setPosition( plotModelElementPositions[0] );
		}
		
		PlotModelElemButton pmeBtn = new PlotModelElemButton(comp, pme);
		pmeBtn.init();

		plotModelElementsUIMap.put( plotModelElementPositions[0], pmeBtn ); 

		Button resetPlotModelBtn = new Button( comp, SWT.PUSH );
		resetPlotModelBtn.setText("Reset");
		gd = new GridData();
		gd.horizontalAlignment = GridData.END;
		gd.grabExcessHorizontalSpace = true;
		gd.verticalAlignment = GridData.CENTER;
		resetPlotModelBtn.setLayoutData( gd );
		resetPlotModelBtn.setEnabled( false );
		
		// Create Upper, middle and lower row position buttons.

		comp = new Composite(topComposite, SWT.NONE);
		gl = new GridLayout(3, true);
		gl.verticalSpacing = 0;
		gl.marginHeight = 0;
		comp.setLayout(gl);

		gd = new GridData();
		gd.horizontalAlignment = GridData.CENTER;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;

		comp.setLayoutData(gd);

		for ( int i=1; i <= 9; i++) {
			pme = editedPlotModel.getPlotModelElement( plotModelElementPositions[i] );
			if( pme == null ) {
				pme = new PlotModelElement();
				pme.setPosition( plotModelElementPositions[i] );
			}
			
			// the center button stores the sky coverage and wind barb params too.
			//
			if( plotModelElementPositions[i].equals("MC") ) {
				PlotModelElemCenterButton cntrBtn = new PlotModelElemCenterButton( comp, pme,
                		editedPlotModel.getSkyCoverageElement(), 
                		editedPlotModel.getWindBarbElement() );
				cntrBtn.init();
				
				plotModelElementsUIMap.put( plotModelElementPositions[i], cntrBtn );				
			}
			else {
				pmeBtn = new PlotModelElemButton(comp, pme);
				pmeBtn.init();
				
				plotModelElementsUIMap.put( plotModelElementPositions[i], pmeBtn ); 
			}
		}
		
		// Create Bottom position button    
	 
		comp = new Composite(topComposite, SWT.NONE);
		gl = new GridLayout(1, true);
		gl.horizontalSpacing = 0;
		gl.verticalSpacing = 0;
		gl.marginHeight = 0;
		comp.setLayout(gl);

		gd = new GridData();
		gd.horizontalAlignment = GridData.CENTER;
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessHorizontalSpace = true;
		comp.setLayoutData(gd);

		pme = editedPlotModel.getPlotModelElement( plotModelElementPositions[10] );
		if( pme == null ) {
			pme = new PlotModelElement();
			pme.setPosition( plotModelElementPositions[10] );
		}
		
		pmeBtn = new PlotModelElemButton(comp, pme);
		pmeBtn.init();

		plotModelElementsUIMap.put( plotModelElementPositions[10], pmeBtn ); 
	}    


	private class PlotModelElemButton {
		protected PlotModelElement pltMdlElmt = null;
		
		final PlotModelElemButton thisButton = this;
		protected Group grp = null;
		protected Button parmBtn = null;;
		protected Button checkBtn = null;

		// create the widgets and add the listeners. init() will be called to set the 
		public PlotModelElemButton(Composite topComp, final PlotModelElement pme ) {			
			pltMdlElmt = pme;
			
			// if this element hasn't been set yet then set to default values
			// (the position must be set.)
			grp = new Group ( topComp, SWT.SHADOW_ETCHED_OUT );
			grp.setLayout(new GridLayout(2, false));
			GridData gd = new GridData(SWT.FILL);//90,60);
			gd.widthHint = 90;
			gd.heightHint = 60;
			gd.minimumHeight = 60;
			gd.grabExcessVerticalSpace = true;
			
			grp.setLayoutData( gd );

			checkBtn = new Button(grp, SWT.CHECK | SWT.SHADOW_ETCHED_OUT);
			checkBtn.setLayoutData(new GridData(15, 12));
			
			parmBtn = new Button(grp, SWT.TOGGLE);
			parmBtn.setLayoutData(new GridData(62, 53));	
						
			checkBtn.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {						
					checkButtonSelected();							
				}				
			});

			parmBtn.addSelectionListener(new SelectionAdapter() { 
				@Override
				public void widgetSelected(SelectionEvent e) {		
					paramButtonSelected();
				}
			});
		}
		
		public void checkButtonSelected() {
//			pltMdlElmt.setEnable( checkBtn.getSelection() );
			
			// if unchecking then remove this element from the plotModel
			// else if checking and the param name is set, add it back to the plotModel
			// (if checking and no param is set then the element will be added to the 
			// plotModel when the param is selected.
			if( !checkBtn.getSelection() ) {
				editedPlotModel.removePlotModelElement( pltMdlElmt );				
			}
			else if( pltMdlElmt.getParamName() != null ) {
				editedPlotModel.putPlotModelElement( pltMdlElmt );
			}
		}
		
		public void paramButtonSelected( ) {
			// Turn the parm button ON
			if( parmBtn.getSelection() ) {
				
				availParamsList.deselectAll();
				availParamsList.setEnabled(true);

				checkBtn.setSelection(true);
				checkBtn.setEnabled(true);

				seldPlotModelElemButton = thisButton; 

				// Select this Parameter in the Parameters List
				if( isParamNameSelected() ) {
					
					for( int i = 0; i < availParamsList.getItemCount(); i++) {
						if( availParamsList.getItem(i).equals( parmBtn.getText() )) {
							availParamsList.setSelection(i);
							break;
						}    
					}

					// Set color in the ColorMatricSelector widget
					cms.setColorValue( getColor() );

					// set the fore/background colors of the button and group
					setButtonAsSelected();
				}
				else { // Parm not selected yet
					// Set color in the ColorMatricSelector widget
					cms.setColorValue( getColor() );
				}
				
				// unselect other buttons, change color back to original grey
				for( String pos : plotModelElementPositions ) {
					if( !pos.equals( getPosition() ) ) {
						plotModelElementsUIMap.get(pos).unselectButton();
					}
				}
			}
			else {  // Turn the parm button OFF
				seldPlotModelElemButton = null;

				unselectButton();

				availParamsList.deselectAll();
				availParamsList.setEnabled(false);			
			}

			updateTextAndSymbolWidgets();			
		}
		
		public boolean isParamNameSelected() {
			return (pltMdlElmt.getParamName() != null);
		}
		
		public void setSymbolSize(Double size) {
			pltMdlElmt.setSymbolSize( size );
		}

		public Double getSymbolSize() {
			if( pltMdlElmt.getParamName() != null )
				return pltMdlElmt.getSymbolSize();
			else
				return 1.0;
		}

		public String getButtonLabel() {
			return (isParamNameSelected() ? pltMdlElmt.getParamName() : "Parm");
		}
		
		public void setButtonAsSelected() {
			RGB rgb = getColor();

			grp.setBackground( new Color(getParent().getDisplay(), cms.getColorValue() ));


			seldPlotModelElemButton.parmBtn.setForeground(
					new Color(getParent().getDisplay(), rgb ) );
			seldPlotModelElemButton.parmBtn.setBackground(
					Display.getDefault().getSystemColor( SWT.COLOR_BLACK ) );
			seldPlotModelElemButton.grp.setBackground(
					new Color(getParent().getDisplay(), rgb ) );
		}

		public void unselectButton() {
			// change color back to original grey
			grp.setBackground( Display.getDefault().getSystemColor(
					SWT.COLOR_WIDGET_BACKGROUND));

			// unselect parm button
			parmBtn.setSelection(false);

			// unselect and disable check box for unused button
			if( !isParamNameSelected() ) {
				checkBtn.setSelection(false);
				checkBtn.setEnabled(false);
			}
		}
		
		public boolean init(  ) {

			parmBtn.setText( getButtonLabel() );
			
			// Set foreground and background color. 
			if( isParamNameSelected() ){		
				checkBtn.setEnabled(true);
				checkBtn.setSelection( true ); // pltMdlElmt.getEnable() );

				parmBtn.setForeground( new Color( getParent().getDisplay(), 
						getColor().red, getColor().green, getColor().blue ));
				parmBtn.setBackground(Display.getDefault().getSystemColor(
						SWT.COLOR_BLACK));

				PlotParameterDefn paramDefn = plotParamDefns.getPlotParamDefn( 
						pltMdlElmt.getParamName() );
				if( paramDefn == null ) {
					parmBtn.setToolTipText("");
				}
				else {
					if( paramDefn.getDeriveParams() != null ) {
						if( paramDefn.getDeriveParams().equals("all") ) {
							parmBtn.setToolTipText( "Derived Parameter" );
						}
						else {
							parmBtn.setToolTipText( "Derived from "+paramDefn.getDeriveParams() );
						}
					}
					else {
						parmBtn.setToolTipText( paramDefn.getMetParamName() );
					}
				}
			}
			else {
				checkBtn.setEnabled(false);
				checkBtn.setSelection(false);

				// the color of the text
				parmBtn.setForeground(new Color( getDisplay(), new RGB(0,0,0)));
				parmBtn.setBackground(Display.getDefault().getSystemColor(
						SWT.COLOR_WIDGET_BACKGROUND));
				
				parmBtn.setToolTipText("");
			}

			return true;
		}

		public PlotModelElement getPlotModelElement() {
			return pltMdlElmt;
		}
		
		protected String getPosition() {
			return pltMdlElmt.getPosition();
		}

		protected void setParmName(String parmName) {
			// 
			if( pltMdlElmt.getParamName() == null ) {
				editedPlotModel.putPlotModelElement( pltMdlElmt );
				pltMdlElmt.setParamName( parmName );
				updateTextAndSymbolWidgets(  );
				parmBtn.setToolTipText( "" );
			}
			else {
				pltMdlElmt.setParamName( parmName );
			}
			
			parmBtn.setText( getButtonLabel() );

			// Set foreground and background color
			parmBtn.setForeground( new Color( getParent().getDisplay(), 
					getColor().red, getColor().green, getColor().blue));
			parmBtn.setBackground(Display.getDefault().getSystemColor(
					SWT.COLOR_BLACK));
			
			// 
			PlotParameterDefn paramDefn = plotParamDefns.getPlotParamDefn( parmName );
			if( paramDefn == null ) {
				parmBtn.setToolTipText("");
			}
			else {
				if( paramDefn.getDeriveParams() != null ) {
					if( paramDefn.getDeriveParams().equals("all") ) {
						parmBtn.setToolTipText( "Derived Parameter" );
					}
					else {
						parmBtn.setToolTipText( "Derived from "+paramDefn.getDeriveParams() );
					}
				}
				else {
					parmBtn.setToolTipText( paramDefn.getMetParamName() );
				}
			}
		}
		

		protected RGB getColor() {
			return new RGB( pltMdlElmt.getColor().getRed(),
					        pltMdlElmt.getColor().getGreen(),
			         		pltMdlElmt.getColor().getBlue() );
		}

		protected void setColor(RGB rgb ) {
			pltMdlElmt.setColorRGB( rgb );			
		}
	}
	
	// 
	private class PlotModelElemCenterButton extends PlotModelElemButton {
		private PlotModelElement skyCovElmt = null;
		private PlotModelElement wndBrbElmt = null;
		
		public PlotModelElemCenterButton(Composite topComp, PlotModelElement pme,
				PlotModelElement sce, PlotModelElement wbe ) {
			super(topComp, pme);
			// if either is null then create a new PlotModelElement but don't add it to the 
			// editedPlotModel unless a param name is set.	
			skyCovElmt = sce;
			
			if( skyCovElmt == null ) {
				skyCovElmt = new PlotModelElement();
				skyCovElmt.setPosition("SC");
			}

			wndBrbElmt = wbe;
			
			if( wndBrbElmt == null ) {
				wndBrbElmt = new PlotModelElement();
				wndBrbElmt.setPosition("WD");
			}

			setColor( getColor() ); // sync the color for all the elements even if not initially set
		}

		@Override
		public void checkButtonSelected() {
			super.checkButtonSelected();
			
			// if unchecking then remove the wndBrb and skyCov elements from the plotModel
			// else add it back
			if( !checkBtn.getSelection() ) {
				editedPlotModel.removePlotModelElement( wndBrbElmt );				
				editedPlotModel.removePlotModelElement( skyCovElmt );				
			}	
			else {
				if( wndBrbElmt.getParamName() != null ) {
					editedPlotModel.putPlotModelElement( wndBrbElmt );					
				}
				if( skyCovElmt.getParamName() != null ) {
					editedPlotModel.putPlotModelElement( skyCovElmt );
				}
			}

		}
		
		// if a 'normal' param is selected in the center then the SkyC and WindBarb are disabled and if
		// a skyC or WindBarb is selected then the normal param is not allowed.
		@Override 
		public void setParmName( String prm ) {
			super.setParmName( prm );
			
//			skyCovElmt.setEnable( false );
//			wndBrbElmt.setEnable( false );
			setSkyCoverageParamName( null );
			setWindBarbParamName( null );
			
			// update the label and combo menus with new selections
			setSelectedSkyAndWindParams();			
			parmBtn.setText( getButtonLabel() );
			
//			skycChkBtn.setSelection( false );
//			brGrpChkBtn.setSelection( false );
		}

		public String getWindBarbParamName() {
			return wndBrbElmt.getParamName();
		}

		public void setWindBarbParamName( String prm ) {
			// in case the parameter is set for the first time we need to add
			// the plotModelElement to the plotModel
			if( prm != null ) {
				if( wndBrbElmt.getParamName() == null ) {
					wndBrbElmt.setParamName( prm );
					editedPlotModel.putPlotModelElement(wndBrbElmt);
					updateTextAndSymbolWidgets();

					// we can't have a skyc and a normal parameter
					editedPlotModel.removePlotModelElement( pltMdlElmt );
					pltMdlElmt.setParamName( null );
				}				
			}
			else {
				editedPlotModel.removePlotModelElement( wndBrbElmt );
			}
			
			wndBrbElmt.setParamName( prm );
			
			parmBtn.setText( getButtonLabel() );
		}

		public String getSkyCovParamName() {
			return skyCovElmt.getParamName();
		}
		
		public void setSkyCoverageParamName( String prm ) {
			// in case the parameter is set for the first time we need to add
			// the plotModelElement to the plotModel
			if( prm != null ) {
				if( skyCovElmt.getParamName() == null ) {
					skyCovElmt.setParamName( prm );
					editedPlotModel.putPlotModelElement(skyCovElmt);
					updateTextAndSymbolWidgets();
				}				
				
				// we can't have a skyc and a normal parameter
				editedPlotModel.removePlotModelElement( pltMdlElmt );
				pltMdlElmt.setParamName( null );
			}
			else {
				editedPlotModel.removePlotModelElement( skyCovElmt );
			}
			
			skyCovElmt.setParamName( prm );

			parmBtn.setText( getButtonLabel() );
		}

		@Override
		public String getButtonLabel() {
			if( skyCovElmt.getParamName() == null &&
				wndBrbElmt.getParamName() == null ) {
				
				return super.getButtonLabel(); //parmBtn.setText( getParmName() );				
			}
			else if( skyCovElmt.getParamName() == null ) {
				return wndBrbElmt.getParamName();
			}
			else if( wndBrbElmt.getParamName() == null ) {
				return skyCovElmt.getParamName();
			}
			else {
				return skyCovElmt.getParamName() + "\n" +
						         wndBrbElmt.getParamName();
			}
		}

		
		public PlotModelElement getSkyCoveragePlotModelElement() {
			return skyCovElmt;
		}

		public PlotModelElement getWindBarbPlotModelElement() {
			return wndBrbElmt;
		}		
//		public void setSkyCoveragePlotModelElement(PlotModelElement sce) {
//			skyCovElmt = sce;
//		}
//
//		public void setWindBarbPlotModelElement(PlotModelElement wbe) {
//			wndBrbElmt = wbe;
//		}
		
		@Override
		public void setSymbolSize( Double size ) {
			super.setSymbolSize(size);
			wndBrbElmt.setSymbolSize(size);
			skyCovElmt.setSymbolSize(size);
		}
		
		@Override
		public Double getSymbolSize() {
			if( pltMdlElmt.getParamName() != null ) {
				return pltMdlElmt.getSymbolSize();
			}
			else if( skyCovElmt.getParamName() != null ) {
				return skyCovElmt.getSymbolSize();
			}
			else if( wndBrbElmt.getParamName() != null ) {
				return wndBrbElmt.getSymbolSize();
			}
			else {
				return 1.0;
			}
		}
		
		// the color should be the same in any/all elements
		@Override
		protected RGB getColor() {
			gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.Color col=null;

			if( pltMdlElmt.getParamName() != null ) {
				col = pltMdlElmt.getColor();
			}
			else if( skyCovElmt.getParamName() != null ) {
				col = skyCovElmt.getColor();
			}
			else if( wndBrbElmt.getParamName() != null ) {
				col = wndBrbElmt.getColor();
			}
			else if( col == null ) {
				return new RGB( 100, 100, 100); // use a default
			}
			
			return new RGB( col.getRed(), col.getGreen(), col.getBlue() );
		}
		
		@Override
		public void setColor( RGB col ) {
			super.setColor(col);
			wndBrbElmt.setColorRGB(col);
			skyCovElmt.setColorRGB(col);
		}
		
		@Override
		public void unselectButton() {
			super.unselectButton();
			enableCenterParamWidgets( false );
		}
		
		@Override
		public void paramButtonSelected( ) {
			super.paramButtonSelected();

			if( parmBtn.getSelection() ) {

				// if any param is selected then set the background color
				// ('normal' param selections are checked in the super.)
				//
				if( wndBrbElmt.getParamName() != null || 
					skyCovElmt.getParamName() != null ) {
			
					// Set color in the ColorMatricSelector widget
					cms.setColorValue( getColor() );

					// Set grp background
					grp.setBackground( new Color(getParent().getDisplay(), cms.getColorValue() ));
				}
			}
			
			enableCenterParamWidgets( parmBtn.getSelection() );
		}

		@Override
		public boolean isParamNameSelected() {
			return ( wndBrbElmt.getParamName() != null ||
					 skyCovElmt.getParamName() != null ||
				     pltMdlElmt.getParamName() != null );
		}
	}

	public void initWidgets() {
		if( editedPlotModel == null ) {
			System.out.println("Plot Model to Edit is not set???");
			return;
		}
		
		textSizeCombo.setItems(textSizeOptions);
		textSizeCombo.select(4); 

		textFontCombo.setItems(textFontOptions);
		textFontCombo.select(0);  // to be replaced by init

		textStyleCombo.setItems(textStyleOptions);
		textStyleCombo.select(0);  // to be replaced by init

		String[] strArray = plotParamDefns.getAllParameterNames( false, false );
		Arrays.sort(strArray);
		availParamsList.setItems( strArray );
		availParamsList.setEnabled(false);		
	}
	
	/*
	 * create widgets for center position only parameters
	 */
	private void createCtrParamControls() {
		
		availWindBarpParams = plotParamDefns.getWindBarbParams();
		availSkyCoverParams = plotParamDefns.getSkyCoverageParams(); //plotParamDefns.getSpecialTableParams();
		
//		if( availWindBarpParams.size() == 0 &&
//			availSkyCoverParams.size() == 0 ) {
//			return;
//		}
		
		PlotModelElemCenterButton cntrBtn = 
			         (PlotModelElemCenterButton)plotModelElementsUIMap.get( "MC" );
		
		PlotModelElement wndBrbPme = cntrBtn.getWindBarbPlotModelElement();
		PlotModelElement skyCovPme = cntrBtn.getSkyCoveragePlotModelElement();
						
		ctrGrp = new Group ( topComposite, SWT.SHADOW_NONE );
		GridLayout gl = new GridLayout(6, false);
		gl.marginLeft = 25;
		gl.marginTop = 2;
		gl.marginBottom = 3;
		gl.marginRight = 52;
		ctrGrp.setLayout( gl );
		ctrGrp.setText("Center Only Parameters");
			
//		skycChkBtn = new Button( ctrGrp, SWT.CHECK );
//		skycChkBtn.setText( "Sky Coverage" );
		skycLbl = new Label( ctrGrp, SWT.None );
		skycLbl.setText( "Sky Coverage" );
		
		comboSky = new Combo(ctrGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		
		comboSky.setItems( availSkyCoverParams.toArray(new String[]{}) );
		comboSky.add("None", 0);

		comboSky.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				String name = comboSky.getItem(comboSky.getSelectionIndex());	

				if( seldPlotModelElemButton instanceof PlotModelElemCenterButton ) {
					((PlotModelElemCenterButton) seldPlotModelElemButton).setSkyCoverageParamName( 
							( name.equals("None") ? null : name ) );
				}
			}
		});			
		
		Label sepe = new Label(ctrGrp,0);
		sepe.setText( "     " );
			
//		brGrpChkBtn = new Button(ctrGrp, SWT.CHECK);
//		brGrpChkBtn.setText("Wind Barb ");
		wndBrbLbl = new Label(ctrGrp, SWT.None);
		wndBrbLbl.setText("Wind Barb ");
		
		comboBrbk = new Combo(ctrGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
	
		comboBrbk.setItems( availWindBarpParams.toArray(new String[]{}) );
		comboBrbk.add("None", 0);

		sepe = new Label(ctrGrp,0);
		sepe.setText( "          " );

		comboBrbk.addSelectionListener(new SelectionAdapter() { 
			public void widgetSelected(SelectionEvent e) {
				String name = comboBrbk.getItem( comboBrbk.getSelectionIndex() );

				if( seldPlotModelElemButton instanceof PlotModelElemCenterButton ) { // sanity check
					((PlotModelElemCenterButton) seldPlotModelElemButton).setWindBarbParamName( 
							( name.equals("None") ? null : name ) );
				}
			}
		});			

		setSelectedSkyAndWindParams();

		enableCenterParamWidgets( false );
	}
	
	private void setSelectedSkyAndWindParams() {
		PlotModelElemCenterButton cntrBtn = 
	           (PlotModelElemCenterButton)plotModelElementsUIMap.get( "MC" );
		
		String seldSkyCovParam = cntrBtn.getSkyCovParamName();

		if( seldSkyCovParam == null ) {
//					skycChkBtn.setSelection( false );
//					comboSky.deselectAll();
			comboSky.select(0);
		}
		else {
//					skycChkBtn.setSelection( skyCovPme.getEnable() );
			comboSky.select( availSkyCoverParams.indexOf(seldSkyCovParam)+1 ); // 0 indx is 'None'
		}

		String seldWndBarbParam = cntrBtn.getWindBarbParamName();

		if( seldWndBarbParam == null ) {
//					brGrpChkBtn.setSelection( false );
			comboBrbk.select(0);
		}
		else {
//					brGrpChkBtn.setSelection( wndBrbPme.getEnable() );
			comboBrbk.select( availWindBarpParams.indexOf(seldWndBarbParam)+1 );
		}
	}

	private void updateTextAndSymbolWidgets() {	
		// Set text widgets if applicable		
		PlotParameterDefn prmDefn = null;
		boolean isTextApplicable = false;
		boolean isSymbApplicable = false;
		
		if( seldPlotModelElemButton != null &&
			seldPlotModelElemButton.isParamNameSelected() ) {
			
//			if( seldPlotModelElemButton.getPlotModelElement().getParamName() != null ) {
				prmDefn = plotParamDefns.getPlotParamDefn(  
					seldPlotModelElemButton.getPlotModelElement().getParamName() );
				
				// add further constraints here on the plot class if necessary
				if( prmDefn != null ) {
					 isTextApplicable = prmDefn.getPlotMode().equalsIgnoreCase("text");
					 isSymbApplicable = prmDefn.getPlotMode().equalsIgnoreCase("table");
				}
				// if this is the center button and a 'regular' parameter isn't set then
				// check if a skyc or wind barb is set and check if the symbol is applicable
				//
				else if( seldPlotModelElemButton instanceof PlotModelElemCenterButton ) {
					PlotModelElemCenterButton cntrBtn = (PlotModelElemCenterButton)seldPlotModelElemButton;
					
					if( cntrBtn.getSkyCovParamName() != null ||
						cntrBtn.getWindBarbParamName() != null ) {						
//						prmDefn = plotParamMngr.getParamDefn( cntrBtn.getSkyCovParamName() );
						isTextApplicable = false;
						isSymbApplicable = true; //(prmDefn != null);
					}
				}
//			}
		}
							
		if( isTextApplicable ) {			
			textFontCombo.setEnabled(true);
			textSizeCombo.setEnabled(true);
			textStyleCombo.setEnabled(true);
			applyToAllBtn.setEnabled(true);
			
			for(int i = 0; i < textSizeCombo.getItemCount(); i++) {
				if( textSizeCombo.getItem(i).equals(
						seldPlotModelElemButton.getPlotModelElement().getTextSize())) {
					textSizeCombo.select(i);
					break;
				}
			}

			for(int i = 0; i < textFontCombo.getItemCount(); i++) {
				if (textFontCombo.getItem(i).equalsIgnoreCase(
						seldPlotModelElemButton.getPlotModelElement().getTextFont())) {
					textFontCombo.select(i);
					break;
				}
			}

			for(int i = 0; i < textStyleCombo.getItemCount(); i++) {
				if( textStyleCombo.getItem(i).equalsIgnoreCase(
						seldPlotModelElemButton.getPlotModelElement().getTextStyle())) {
					textStyleCombo.select(i);
					break;
				}
			}
			
			if ("Standard".equals(textFontCombo.getText())) {
				textStyleCombo.select(0);
				textStyleCombo.setEnabled(false);
			} 
			
			if (applyToAllBtn.getSelection()) {
    			applyTextChangesToAllParameters();
    		}
		}
		else {
			textFontCombo.deselectAll();
			textFontCombo.setEnabled(false);
			textSizeCombo.deselectAll();
			textSizeCombo.setEnabled(false);
			textStyleCombo.deselectAll();
			textStyleCombo.setEnabled(false);		
			applyToAllBtn.setEnabled(false);
			
		}

		// Set symbol size if applicable		
		if( isSymbApplicable ) {
			
			symbolSizeScale.setEnabled(true);
			
			if( seldPlotModelElemButton.getSymbolSize() > 3 ) {
				symbolSizeScale.setSelection( 30 );		
				symSizeLabel.setText("    "+3.0);
			}
			else {
				double size = seldPlotModelElemButton.getSymbolSize();				
				symbolSizeScale.setSelection( (int) (size*10) );				
				symSizeLabel.setText("    "+size);
			}			
		}
		else {
			symbolSizeScale.setSelection( 0 );
			symbolSizeScale.setEnabled(false);
			symSizeLabel.setText("    ");			
		}
	}
	
	private void enableCenterParamWidgets( boolean enable ) {
//		ctrGrp.setEnabled(  enable );
		skycLbl.setEnabled( enable );
		comboSky.setEnabled( enable );
		wndBrbLbl.setEnabled( enable );
		comboBrbk.setEnabled( enable );
	}
	
	PlotModel getEditedPlotModel() {
		return editedPlotModel;
	}
	
	private void applyTextChangesToAllParameters() {	
		for ( int i=0; i < 11; i++) {
			PlotModelElemButton pmeBtn = plotModelElementsUIMap.get( plotModelElementPositions[i] );
			if( pmeBtn != null ) {
				PlotParameterDefn prmDefn = plotParamDefns.getPlotParamDefn(  
						pmeBtn.getPlotModelElement().getParamName() );
					
				if( prmDefn != null ) {
					 if (prmDefn.getPlotMode().equalsIgnoreCase("text")) {
						 pmeBtn.getPlotModelElement().setTextSize( textSizeCombo.getText() );
						 pmeBtn.getPlotModelElement().setTextFont(textFontCombo.getText() );
						 pmeBtn.getPlotModelElement().setTextStyle(textStyleCombo.getText() );
					 }
				}        					
			}
		}
	}
}
