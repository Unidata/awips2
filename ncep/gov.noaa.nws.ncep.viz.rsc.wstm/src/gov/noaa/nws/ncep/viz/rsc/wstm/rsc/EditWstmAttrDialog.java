/*
 * EditWstmAttrDialog
 * 
 * Date created (November 08, 2010)
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.viz.rsc.wstm.rsc;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.PlatformUI;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.rsc.wstm.Activator;

/**
 * Creates a dialog box to edit the WSTM resource
 * 
  * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08-Nov-2010    247      Archana     Initial creation.
 *                                                
 * @author archana
 *</pre>
 */
public class EditWstmAttrDialog extends AbstractEditResourceAttrsDialog {
	private RscAttrValue wstmWarningEnableAttr = null;
	private RscAttrValue wstmWarningColorAttr = null;
	private RscAttrValue wstmWarningLineWidthAttr = null;
	private RscAttrValue  wstmWarningSymbolWidthAttr = null;	
	private RscAttrValue wstmWarningSymbolSizeAttr = null;
	private RscAttrValue wstmWatchEnableAttr = null;
	private RscAttrValue wstmWatchColorAttr = null;
	private RscAttrValue wstmWatchLineWidthAttr = null;
	private RscAttrValue  wstmWatchSymbolWidthAttr = null;	
	private RscAttrValue wstmWatchSymbolSizeAttr = null;
	private RscAttrValue wstmAdvisoryEnableAttr = null;
	private RscAttrValue wstmAdvisoryColorAttr = null;
	private RscAttrValue wstmAdvisoryLineWidthAttr = null;
	private RscAttrValue  wstmAdvisorySymbolWidthAttr = null;	
	private RscAttrValue wstmAdvisorySymbolSizeAttr = null;
	private RscAttrValue timeEnableAttr = null;
	private RscAttrValue zoneNameEnableAttr = null;
	private RscAttrValue outlineEnableAttr = null;
	
	private Button wstmWarningCheckBox = null;
	private Button wstmWatchCheckBox = null;
	private Button wstmAdvisoryCheckBox = null;
	private Button wstmTimeCheckBox = null;
	private Button wstmZoneCheckBox = null;
	private Button wstmOutlineCheckBox = null;
	
	private ColorButtonSelector wstmWarningColorButtonSelector = null;
	private ColorButtonSelector wstmWatchColorButtonSelector = null;
	private ColorButtonSelector wstmAdvisoryColorButtonSelector = null;
	
	private Spinner wstmWarningLineWidthSpinner = null;
	private Spinner wstmWarningSymbolWidthSpinner = null;
	private Spinner wstmWarningSymbolSizeSpinner = null;
	private Spinner wstmWatchLineWidthSpinner = null;
	private Spinner wstmWatchSymbolWidthSpinner = null;
	private Spinner wstmWatchSymbolSizeSpinner = null;
	private Spinner wstmAdvisoryLineWidthSpinner = null;
	private Spinner wstmAdvisorySymbolWidthSpinner = null;
	private Spinner wstmAdvisorySymbolSizeSpinner = null;	
	private final String symbolSizeTxt = "Symbol Size";
	private final String symbolWidthTxt = "Symbol Width";
	private final String lineWidthTxt = "Line Width";
	private final String DEFAULT_ICON_PATH ="icons/marker20.gif";
    public static final String DEFAULT_ICON_TYPE = "DIAMOND";
	private MarkerSelectionPanel markerSelectionPanel1 = null;
	private MarkerSelectionPanel markerSelectionPanel2 = null;
	private MarkerSelectionPanel markerSelectionPanel3 = null;
	public static String advisoryMarkerData = DEFAULT_ICON_TYPE;
	public static String warningMarkerData = DEFAULT_ICON_TYPE;
	public static String watchMarkerData    = DEFAULT_ICON_TYPE;
	

	public enum MarkerSelectionPanelIdentifier {PANEL1, PANEL2, PANEL3};
    public EditWstmAttrDialog(Shell parentShell, INatlCntrsResourceData r,
			Boolean apply) {
		super(parentShell, r, apply);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog#createDialog(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public Composite createDialog(Composite topComp) {
		checkAttributeValues();

		Composite gridComp = new Composite( topComp, SWT.NONE );
		GridLayout gridLayout = new GridLayout(7, false );
		gridLayout.marginHeight = 5;
		gridLayout.marginWidth = 20;
		gridLayout.verticalSpacing = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.makeColumnsEqualWidth = false;

		gridComp.setLayout( gridLayout );
        float scale = 0.0f;
		
        int warningSymbolSize = 0;
		int watchSymbolSize = 0;
		int advisorySymbolSize = 0;
		
		wstmWarningCheckBox =  drawCheckBox(gridComp);  	
		wstmWarningCheckBox.setText("Winter hazard warning");
		wstmWarningCheckBox.setSelection((Boolean)wstmWarningEnableAttr.getAttrValue());    		
		wstmWarningCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				wstmWarningEnableAttr.setAttrValue(wstmWarningCheckBox.getSelection());
			}
		});  

		wstmWarningColorButtonSelector = drawColorButtonSelector(gridComp);
		wstmWarningColorButtonSelector.setColorValue(( RGB)wstmWarningColorAttr.getAttrValue());
		wstmWarningColorButtonSelector.addListener( new IPropertyChangeListener(){
			@Override
			 public void propertyChange(PropertyChangeEvent event){
				wstmWarningColorAttr.setAttrValue(event.getNewValue());
			}
		} );
    	
		wstmWarningLineWidthSpinner = drawSpinnerWithLabel(gridComp, lineWidthTxt);
		wstmWarningLineWidthSpinner.setSelection((Integer)(wstmWarningLineWidthAttr.getAttrValue()));
		wstmWarningLineWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmWarningLineWidthAttr.setAttrValue(wstmWarningLineWidthSpinner.getSelection());
			}
		});
		
		wstmWarningSymbolWidthSpinner = drawSpinnerWithLabel(gridComp, symbolWidthTxt);
		wstmWarningSymbolWidthSpinner.setSelection((Integer)(wstmWarningSymbolWidthAttr.getAttrValue()));
		wstmWarningSymbolWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmWarningSymbolWidthAttr.setAttrValue(wstmWarningSymbolWidthSpinner.getSelection());
			}
		});		
		
		wstmWarningSymbolSizeSpinner = drawSpinnerWithLabel(gridComp, symbolSizeTxt);
		scale = (float)Math.pow(10, wstmWarningSymbolSizeSpinner.getDigits() );

		warningSymbolSize = Math.round( scale*((Float)wstmWarningSymbolSizeAttr.getAttrValue()).floatValue() );
		wstmWarningSymbolSizeSpinner.setSelection(warningSymbolSize );
		wstmWarningSymbolSizeSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				float scale2 =  (float)Math.pow(10, wstmWarningSymbolSizeSpinner.getDigits() );
				wstmWarningSymbolSizeAttr.setAttrValue( new Float( ((float)wstmWarningSymbolSizeSpinner.getSelection())/scale2));
			}
		});		
	
         Button markerWstmWarningButton = drawMarkerSelectionButtonAndItsLabel(gridComp, warningMarkerData);
         markerWstmWarningButton.addSelectionListener(new SelectionAdapter(){
        	 @Override
        	 public void widgetSelected(SelectionEvent event){
        	     openMarkerSelectionPanel((Button)event.widget, MarkerSelectionPanelIdentifier.PANEL1);
        	 }
         });
         
		wstmWatchCheckBox =  drawCheckBox(gridComp);  	
		wstmWatchCheckBox.setText("Winter hazard watch");
		wstmWatchCheckBox.setSelection((Boolean)wstmWatchEnableAttr.getAttrValue());    		
		wstmWatchCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				wstmWatchEnableAttr.setAttrValue(wstmWatchCheckBox.getSelection());
			}
		});  

		wstmWatchColorButtonSelector = drawColorButtonSelector(gridComp);
		wstmWatchColorButtonSelector.setColorValue(( RGB)wstmWatchColorAttr.getAttrValue());
		wstmWatchColorButtonSelector.addListener( new IPropertyChangeListener(){
			@Override
			 public void propertyChange(PropertyChangeEvent event){
				wstmWatchColorAttr.setAttrValue(event.getNewValue());
			}
		} );
    	
		wstmWatchLineWidthSpinner = drawSpinnerWithLabel(gridComp, lineWidthTxt);
		wstmWatchLineWidthSpinner.setSelection((Integer)(wstmWatchLineWidthAttr.getAttrValue()));
		wstmWatchLineWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmWatchLineWidthAttr.setAttrValue(wstmWatchLineWidthSpinner.getSelection());
			}
		});
		
		wstmWatchSymbolWidthSpinner = drawSpinnerWithLabel(gridComp, symbolWidthTxt);
		wstmWatchSymbolWidthSpinner.setSelection((Integer)(wstmWatchSymbolWidthAttr.getAttrValue()));
		wstmWatchSymbolWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmWatchSymbolWidthAttr.setAttrValue(wstmWatchSymbolWidthSpinner.getSelection());
			}
		});		
		
		wstmWatchSymbolSizeSpinner = drawSpinnerWithLabel(gridComp, symbolSizeTxt);
		scale = (float)Math.pow(10, wstmWatchSymbolSizeSpinner.getDigits() );

		watchSymbolSize = Math.round( scale*((Float)wstmWatchSymbolSizeAttr.getAttrValue()).floatValue() );
		wstmWatchSymbolSizeSpinner.setSelection(watchSymbolSize );
		wstmWatchSymbolSizeSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				float scale2 =  (float)Math.pow(10, wstmWatchSymbolSizeSpinner.getDigits() );
				wstmWatchSymbolSizeAttr.setAttrValue( new Float( ((float)wstmWatchSymbolSizeSpinner.getSelection())/scale2));
			}
		});		
	
        Button markerWstmWatchButton = drawMarkerSelectionButtonAndItsLabel(gridComp, watchMarkerData);
        markerWstmWatchButton.addSelectionListener(new SelectionAdapter(){
       	 @Override
       	 public void widgetSelected(SelectionEvent event){
       	     openMarkerSelectionPanel((Button)event.widget, MarkerSelectionPanelIdentifier.PANEL2);
       	 }
        });
        
		wstmAdvisoryCheckBox =  drawCheckBox(gridComp);  	
		wstmAdvisoryCheckBox.setText("Winter hazard advisory");
		wstmAdvisoryCheckBox.setSelection((Boolean)wstmAdvisoryEnableAttr.getAttrValue());    		
		wstmAdvisoryCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				wstmAdvisoryEnableAttr.setAttrValue(wstmAdvisoryCheckBox.getSelection());
			}
		});  

		wstmAdvisoryColorButtonSelector = drawColorButtonSelector(gridComp);
		wstmAdvisoryColorButtonSelector.setColorValue(( RGB)wstmAdvisoryColorAttr.getAttrValue());
		wstmAdvisoryColorButtonSelector.addListener( new IPropertyChangeListener(){
			@Override
			 public void propertyChange(PropertyChangeEvent event){
				wstmAdvisoryColorAttr.setAttrValue(event.getNewValue());
			}
		} );
    	
		wstmAdvisoryLineWidthSpinner = drawSpinnerWithLabel(gridComp, lineWidthTxt);
		wstmAdvisoryLineWidthSpinner.setSelection((Integer)(wstmAdvisoryLineWidthAttr.getAttrValue()));
		wstmAdvisoryLineWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmAdvisoryLineWidthAttr.setAttrValue(wstmAdvisoryLineWidthSpinner.getSelection());
			}
		});
		
		wstmAdvisorySymbolWidthSpinner = drawSpinnerWithLabel(gridComp, symbolWidthTxt);
		wstmAdvisorySymbolWidthSpinner.setSelection((Integer)(wstmAdvisorySymbolWidthAttr.getAttrValue()));
		wstmAdvisorySymbolWidthSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				wstmAdvisorySymbolWidthAttr.setAttrValue(wstmAdvisorySymbolWidthSpinner.getSelection());
			}
		});		
		
		wstmAdvisorySymbolSizeSpinner = drawSpinnerWithLabel(gridComp, symbolSizeTxt);
		scale = (float)Math.pow(10, wstmAdvisorySymbolSizeSpinner.getDigits() );

		advisorySymbolSize = Math.round( scale*((Float)wstmAdvisorySymbolSizeAttr.getAttrValue()).floatValue() );
		wstmAdvisorySymbolSizeSpinner.setSelection(advisorySymbolSize );
		wstmAdvisorySymbolSizeSpinner.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				float scale2 =  (float)Math.pow(10, wstmAdvisorySymbolSizeSpinner.getDigits() );
				wstmAdvisorySymbolSizeAttr.setAttrValue( new Float( ((float)wstmAdvisorySymbolSizeSpinner.getSelection())/scale2));
			}
		});			
	
        Button markerWstmAdvisoryButton = drawMarkerSelectionButtonAndItsLabel(gridComp, advisoryMarkerData);
        markerWstmAdvisoryButton.addSelectionListener(new SelectionAdapter(){
       	 @Override
       	 public void widgetSelected(SelectionEvent event){
             //bData = (String)(((Button)event.widget).getData());
       	     openMarkerSelectionPanel((Button)event.widget, MarkerSelectionPanelIdentifier.PANEL3);
       	 }
        });
		
		drawHorizontalSeparator(gridComp, 7);

		/*A separate grid layout to align the 3 check-boxes in a single column below the separator*/
		Composite gridComp2 = new Composite( topComp, SWT.NONE );
		GridLayout gd = new GridLayout(1, false);
		gd.marginHeight = 5;
		gd.marginWidth = 20;
		gd.verticalSpacing = 0;
		gd.horizontalSpacing = 0;
		gd.makeColumnsEqualWidth = false;
		gridComp2.setLayout(gd);		
		
		wstmTimeCheckBox =  drawCheckBox(gridComp2);  	
		wstmTimeCheckBox.setText("Time");
		wstmTimeCheckBox.setSelection((Boolean)timeEnableAttr.getAttrValue());    		
		wstmTimeCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				timeEnableAttr.setAttrValue(wstmTimeCheckBox.getSelection());
			}
		});  		

		wstmZoneCheckBox =  drawCheckBox(gridComp2);  	
		wstmZoneCheckBox.setText("Zone Name");
		wstmZoneCheckBox.setSelection((Boolean)zoneNameEnableAttr.getAttrValue());    		
		wstmZoneCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				zoneNameEnableAttr.setAttrValue(wstmZoneCheckBox.getSelection());
			}
		});  	
		
		wstmOutlineCheckBox =  drawCheckBox(gridComp2);  	
		wstmOutlineCheckBox.setText("Outline");
		wstmOutlineCheckBox.setSelection((Boolean)outlineEnableAttr.getAttrValue());    		
		wstmOutlineCheckBox.addSelectionListener(new SelectionAdapter(){
		
			@Override
		public void widgetSelected(SelectionEvent e) {
				outlineEnableAttr.setAttrValue(wstmOutlineCheckBox.getSelection());
			}
		});  	
		
		
		return topComp;
	}

	private void drawHorizontalSeparator(Composite parentComposite, int numCols){

		Composite sepComp = new Composite( parentComposite, SWT.NONE );
		sepComp.setLayout( new FillLayout() );

		GridData gd = new GridData( GridData.FILL_HORIZONTAL );
		gd.horizontalIndent = 0;
		gd.verticalIndent = 5;
		gd.heightHint = 15;
		// this should be in the first column and take up the whole row
		gd.horizontalSpan = numCols;
		sepComp.setLayoutData( gd );
		
		new Label( sepComp, SWT.SEPARATOR | SWT.HORIZONTAL );
	}
	
	/**
	 * 
	 * @param parentComposite
	 * @param labelText
	 * @return the spinner widget
	 */
	private Spinner drawSpinnerWithLabel(Composite parentComposite, String labelText){
		Composite spinnerComposite = new Composite( parentComposite, SWT.NONE );
		spinnerComposite.setLayout( new GridLayout(2,false) );
		
		GridData gridData = new GridData();
		gridData.horizontalIndent = 0;
		gridData.verticalIndent = 5;
		spinnerComposite.setLayoutData( gridData );
		
		Label lblWid = new Label( spinnerComposite, SWT.None );
		lblWid.setText(labelText);

		final Spinner thisSpinner = new Spinner( spinnerComposite, SWT.BORDER );
		thisSpinner.setMaximum(10);
		thisSpinner.setMinimum(1);
		thisSpinner.setDigits(0);
		thisSpinner.setIncrement(0);
		thisSpinner.setPageIncrement(2);
		return thisSpinner;
	}
	
	/**
	 * 
	 * @param parentComposite
	 * @return
	 */
	private Button drawCheckBox(Composite parentComposite){
		Composite chkComp = new Composite( parentComposite, SWT.NONE );

		chkComp.setLayout( new FormLayout() );
		
		GridData gridData = new GridData();
		gridData.horizontalIndent = 0;
		gridData.verticalIndent = 5;
		chkComp.setLayoutData( gridData );
			

    	Button thisCheckBox = new Button (chkComp, SWT.CHECK); 
        FormData	formData = new FormData( );
    	formData.left = new FormAttachment( 0, 0 );
    	formData.top = new FormAttachment( 0, 0 );
    	thisCheckBox.setLayoutData(formData);
    	thisCheckBox.setAlignment(SWT.RIGHT);
   	
    	return thisCheckBox;
	}
	
	/**
	 * 
	 * @param parentComposite
	 * @return
	 */
	private ColorButtonSelector drawColorButtonSelector(Composite parentComposite){
		Composite colComp = new Composite( parentComposite, SWT.NONE );
		GridData gd = new GridData();
		gd.horizontalIndent = 0;
		gd.verticalIndent = 5;
		colComp.setLayoutData( gd );

		GridLayout gridLayout = new GridLayout(1,true);
		gridLayout.verticalSpacing = 0;
		
		colComp.setLayout( gridLayout );
        final ColorButtonSelector colorSel = new ColorButtonSelector( colComp, 50, 20 );
        return colorSel;
	}
	
	/**
	 * Creates a button with an icon and a label with the text "Marker Type"
	 * Sets the icon on the button to the icon selected by the user from the MarkerSelectionPanel
	 * @param parentComposite
	 * @return
	 */
	private Button drawMarkerSelectionButtonAndItsLabel(Composite parentComposite, String dataToSet){
		Label markerLabel = new Label(parentComposite, SWT.NONE);
		markerLabel.setText(" Marker Type ");
         Button thisButton = new Button(parentComposite, SWT.PUSH);
         thisButton.setData(dataToSet);
         thisButton.setAlignment(SWT.RIGHT);
         String iconPath = new String( DEFAULT_ICON_PATH);
         if(MarkerSelectionPanel.getImageMap() != null && MarkerSelectionPanel.getImageMap().size() > 0){
           iconPath = new String(MarkerSelectionPanel.getImageMap().get(dataToSet));
         }
         thisButton.setImage(getFillPatternIcon(iconPath));
         return thisButton;
	}

	/**
	 * 
	 */
	private void checkAttributeValues(){
		wstmWarningEnableAttr =	editedRscAttrSet.getRscAttr("wstmWarningEnable");
		wstmWarningColorAttr =	editedRscAttrSet.getRscAttr("wstmWarningColor");
		wstmWarningLineWidthAttr =	editedRscAttrSet.getRscAttr("wstmWarningLineWidth");
		wstmWarningSymbolWidthAttr =	editedRscAttrSet.getRscAttr("wstmWarningSymbolWidth");
		wstmWarningSymbolSizeAttr =	editedRscAttrSet.getRscAttr("wstmWarningSymbolSize");

		wstmWatchEnableAttr =	editedRscAttrSet.getRscAttr("wstmWatchEnable");
		wstmWatchColorAttr =	editedRscAttrSet.getRscAttr("wstmWatchColor");
		wstmWatchLineWidthAttr =	editedRscAttrSet.getRscAttr("wstmWatchLineWidth");
		wstmWatchSymbolWidthAttr =	editedRscAttrSet.getRscAttr("wstmWatchSymbolWidth");
		wstmWatchSymbolSizeAttr =	editedRscAttrSet.getRscAttr("wstmWatchSymbolSize");

		wstmAdvisoryEnableAttr =	editedRscAttrSet.getRscAttr("wstmAdvisoryEnable");
		wstmAdvisoryColorAttr =	editedRscAttrSet.getRscAttr("wstmAdvisoryColor");
		wstmAdvisoryLineWidthAttr =	editedRscAttrSet.getRscAttr("wstmAdvisoryLineWidth");
		wstmAdvisorySymbolWidthAttr =	editedRscAttrSet.getRscAttr("wstmAdvisorySymbolWidth");
		wstmAdvisorySymbolSizeAttr =	editedRscAttrSet.getRscAttr("wstmAdvisorySymbolSize");
		timeEnableAttr = editedRscAttrSet.getRscAttr("timeEnable");
		zoneNameEnableAttr = editedRscAttrSet.getRscAttr("zoneNameEnable");
		outlineEnableAttr = editedRscAttrSet.getRscAttr("outlineEnable");
		
		
		//Sanity check
		if ( wstmWarningEnableAttr == null ){
			System.out.println("Error: wstmWarningEnableAttr is null");
		}
		if ( wstmWarningEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: wstmWarningEnableAttr is not of expected class Boolean");
		}		
		if ( wstmWarningColorAttr == null ){
			System.out.println("Error: wstmWarningColorAttr is null");
		}
		if ( wstmWarningColorAttr .getAttrClass() != RGB.class){
			System.out.println("Error: wstmWarningColorAttr is not of expected class RGB");
		}	
		
		if ( wstmWarningLineWidthAttr == null ){
			System.out.println("Error: wstmWarningLineWidthAttr is null");
		}
		if ( wstmWarningLineWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmWarningLineWidthAttr is not of expected class Integer");
		}	
		
		if ( wstmWarningSymbolWidthAttr == null ){
			System.out.println("Error: wstmWarningSymbolWidthAttr is null");
		}
		if ( wstmWarningSymbolWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmWarningSymbolWidthAttr is not of expected class Integer");
		}		

		if ( wstmWarningSymbolSizeAttr == null ){
			System.out.println("Error: wstmWarningSymbolSizeAttr is null");
		}
		if ( wstmWarningSymbolSizeAttr .getAttrClass() != Float.class){
			System.out.println("Error: wstmWarningSymbolSizeAttr is not of expected class Float");
		}		
				
		if ( wstmWatchEnableAttr == null ){
			System.out.println("Error: wstmWatchEnableAttr is null");
		}
		if ( wstmWatchEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: wstmWatchEnableAttr is not of expected class Boolean");
		}	
		
		if ( wstmWatchColorAttr == null ){
			System.out.println("Error: wstmWatchColorAttr is null");
		}
		if ( wstmWatchColorAttr .getAttrClass() != RGB.class){
			System.out.println("Error: wstmWatchColorAttr is not of expected class RGB");
		}			
	
		if ( wstmWatchLineWidthAttr == null ){
			System.out.println("Error: wstmWatchLineWidthAttr is null");
		}
		if ( wstmWatchLineWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmWatchLineWidthAttr is not of expected class Integer");
		}			
		
		if ( wstmWatchSymbolWidthAttr == null ){
			System.out.println("Error: wstmWatchSymbolWidthAttr is null");
		}
		if ( wstmWatchSymbolWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmWatchSymbolWidthAttr is not of expected class Integer");
		}		

		if ( wstmWatchSymbolSizeAttr == null ){
			System.out.println("Error: wstmWatchSymbolSizeAttr is null");
		}
		if ( wstmWatchSymbolSizeAttr .getAttrClass() != Float.class){
			System.out.println("Error: wstmWatchSymbolSizeAttr is not of expected class Float");
		}		
		
		if ( wstmAdvisoryEnableAttr == null ){
			System.out.println("Error: wstmAdvisoryEnableAttr is null");
		}
		if ( wstmAdvisoryEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: wstmAdvisoryEnableAttr is not of expected class Boolean");
		}	
		if ( wstmAdvisoryColorAttr == null ){
			System.out.println("Error: wstmAdvisoryColorAttr is null");
		}
		if ( wstmAdvisoryColorAttr .getAttrClass() != RGB.class){
			System.out.println("Error: wstmAdvisoryColorAttr is not of expected class RGB");
		}	
		if ( wstmAdvisoryLineWidthAttr == null ){
			System.out.println("Error: wstmAdvisoryLineWidthAttr is null");
		}
		if ( wstmAdvisoryLineWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmAdvisoryLineWidthAttr is not of expected class Integer");
		}
		if ( wstmAdvisorySymbolWidthAttr == null ){
			System.out.println("Error: wstmAdvisorySymbolWidthAttr is null");
		}
		if ( wstmAdvisorySymbolWidthAttr .getAttrClass() != Integer.class){
			System.out.println("Error: wstmAdvisorySymbolWidthAttr is not of expected class Integer");
		}		

		if ( wstmAdvisorySymbolSizeAttr == null ){
			System.out.println("Error: wstmAdvisorySymbolSizeAttr is null");
		}
		if ( wstmAdvisorySymbolSizeAttr .getAttrClass() != Float.class){
			System.out.println("Error: wstmAdvisorySymbolSizeAttr is not of expected class Float");
		}	
		
		if ( timeEnableAttr == null ){
			System.out.println("Error: timeEnableAttr is null");
		}
		if ( timeEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: timeEnableAttr is not of expected class Boolean");
		}	

		if ( zoneNameEnableAttr == null ){
			System.out.println("Error: zoneNameEnableAttr is null");
		}
		if ( zoneNameEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: zoneNameEnableAttr is not of expected class Boolean");
		}			
		
		if ( outlineEnableAttr == null ){
			System.out.println("Error: outlineEnableAttr is null");
		}
		if ( outlineEnableAttr .getAttrClass() != Boolean.class){
			System.out.println("Error: outlineEnableAttr is not of expected class Boolean");
		}			
		
	}
	/**
	 * 
	 * @param thisButton
	 * @param markerSelectionPanelId
	 */
	private void openMarkerSelectionPanel(Button thisButton, MarkerSelectionPanelIdentifier markerSelectionPanelId){

		if ( markerSelectionPanelId == MarkerSelectionPanelIdentifier.PANEL1){
			markerSelectionPanel1 = createOrOpenPanel(markerSelectionPanel1, thisButton);
			if(markerSelectionPanel1 != null && markerSelectionPanel1.close()){
				warningMarkerData = (String) (markerSelectionPanel1.getActivator().getData());
//				if(warningMarkerData.compareTo( (String) thisButton.getData()) != 0){
//					isWarningMarkerChanged = true;
//				}
			}
        }else if( markerSelectionPanelId == MarkerSelectionPanelIdentifier.PANEL2){
        	markerSelectionPanel2 = createOrOpenPanel(markerSelectionPanel2, thisButton);
			if(markerSelectionPanel2 != null && markerSelectionPanel2.close()){
				watchMarkerData = (String) (markerSelectionPanel2.getActivator().getData());
//				if(watchMarkerData.compareTo("DIAMOND") != 0){
//					isWatchMarkerChanged = true;
//				}			
			}
        }else{
        	markerSelectionPanel3 = createOrOpenPanel(markerSelectionPanel3, thisButton);
        	 if(markerSelectionPanel3 != null && markerSelectionPanel3.close()){
				advisoryMarkerData = (String) (markerSelectionPanel3.getActivator().getData());
			}
        }
	}

/**
 * 
 * @param markerSelectionPanel
 * @param thisButton
 */
	private MarkerSelectionPanel createOrOpenPanel(MarkerSelectionPanel markerSelectionPanel, Button thisButton){
		if(markerSelectionPanel == null){
			 Shell  shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			markerSelectionPanel = new MarkerSelectionPanel(shell, thisButton);
		}
		
		if(markerSelectionPanel != null){
			markerSelectionPanel.setActivator(thisButton);
			markerSelectionPanel.open();
		}
		
		return markerSelectionPanel;
	}
	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog#initWidgets()
	 */
	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub

	}
	
	/**
	 * 
	 * @param iconLocation
	 * @return
	 */
	private Image getFillPatternIcon( String iconLocation ){
		
		ImageDescriptor id = Activator.imageDescriptorFromPlugin(
				Activator.PLUGIN_ID, iconLocation );
		Image icon = null;
		if ( id != null ) {
			icon = id.createImage();
		}
		
		return icon;
	}
}
