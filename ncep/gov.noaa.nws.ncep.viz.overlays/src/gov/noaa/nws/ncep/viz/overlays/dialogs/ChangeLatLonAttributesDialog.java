package gov.noaa.nws.ncep.viz.overlays.dialogs;

import java.util.EnumMap;
import java.util.Map;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;

/**
 * The upper part of this dialog is copied from ChangeLineAttributesDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05 Sep. 2009  127        mgao	    Initial Creation.
 * 17 Jun 2009  115         Greg Hull   Integrated with AbstractEditResourceAttrsDialog
 * 31 Jul 2009              ghull       Migrate to to11
 * 27 Apr 2010   #245       Greg Hull   Added Apply Button
 * 
 * @author mgao
 * @version 1
 */

public class ChangeLatLonAttributesDialog extends AbstractEditResourceAttrsDialog {

	private final static org.apache.log4j.Logger log = 
				org.apache.log4j.Logger.getLogger(ChangeLatLonAttributesDialog.class);
	
	//  Current attribute values.
    private RscAttrValue lineStyle = null;
    private RscAttrValue lineColor = null;    
    private RscAttrValue lineWidth = null;
    private RscAttrValue latInterval = null;
    private RscAttrValue lonInterval = null;
	
    private float  	lineLengthFactor = 1;
    
    private Button[] selectLineWidthButtons;     
    
    private Map<LineStyle, Button> lineStyleButtonMap;
    
    private final LineStyle [] lineStyleButtonSequence = {  // ...for 2-column grid layout
    		LineStyle.DOTS,                  LineStyle.LONG_DASHED,
    		LineStyle.SOLID,                 LineStyle.LONG_DASH_THREE_SHORT_DASHES,
    		LineStyle.SHORT_DASHED,          LineStyle.LONG_DASH_DOT,  
    		LineStyle.MEDIUM_DASHED,         LineStyle.LONG_DASH_THREE_DOTS,
    		LineStyle.LONG_DASH_SHORT_DASH,  LineStyle.MEDIUM_DASH_DOT,
    		};

    private ColorMatrixSelector colorMatrixSelector; 

    private Button[] intervalButtonArray; 
    private final int[] incrementValueArray = {1, 2, 5, 10, 15, 20, 30}; 
    
    private Text latitudeText; 
    private Text longitudeText; 
    
    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ChangeLatLonAttributesDialog(Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
        super(parentShell, rd, apply);
    }

    @Override
    public Composite createDialog(Composite composite) {
    	final Display display = composite.getDisplay();
    	
    	FormLayout layout0 = new FormLayout();
    	composite.setLayout(layout0);

    	lineStyle = editedRscAttrSet.getRscAttr("lineStyle");
    	lineColor = editedRscAttrSet.getRscAttr("color");
    	lineWidth = editedRscAttrSet.getRscAttr("lineWidth");
    	latInterval = editedRscAttrSet.getRscAttr("latitudeInterval");
    	lonInterval = editedRscAttrSet.getRscAttr("longitudeInterval");

    	// confirm the classes of the attributes..
    	if( lineStyle.getAttrClass() != LineStyle.class ) {
    		System.out.println( "lineStyle is not of expected class? "+ lineStyle.getAttrClass().toString() );
    	}
    	else if( lineColor.getAttrClass() != RGB.class ) {
    		System.out.println( "lineColor is not of expected class? "+ lineColor.getAttrClass().toString() );
    	}
    	else if( lineWidth.getAttrClass() != Integer.class ) {
    		System.out.println( "lineWidth is not of expected class? "+ lineWidth.getAttrClass().toString() );
    	}
    	else if( latInterval.getAttrClass() != Integer.class ) {
    		System.out.println( "latInterval is not of expected class? "+ latInterval.getAttrClass().toString() );
    	}
    	else if( lonInterval.getAttrClass() != Integer.class ) {
    		System.out.println( "lonInterval is not of expected class? "+ lonInterval.getAttrClass().toString() );
    	}

        //  Lay out the various groups within the dialog
        
        Group linePreviewAreaGroup = new Group ( composite, SWT.SHADOW_NONE );
        linePreviewAreaGroup.setLayout(new FillLayout());
        
        FormData formData0 = new FormData();
        formData0.top = new FormAttachment(5,0);
        formData0.left = new FormAttachment(2,0);
        formData0.width = 196;
        formData0.height = 30;
        linePreviewAreaGroup.setLayoutData(formData0);
        
        Group selectLineWidthGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectLineWidthGroup.setText("Width");
        GridLayout lineWidthGridLayout = new GridLayout();
        lineWidthGridLayout.numColumns = 2;
        lineWidthGridLayout.marginHeight = 18;
        lineWidthGridLayout.marginWidth = 18;
        lineWidthGridLayout.horizontalSpacing = 8;
        lineWidthGridLayout.verticalSpacing = 8;
        selectLineWidthGroup.setLayout(lineWidthGridLayout);
        
        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(linePreviewAreaGroup, 16);
        formData1.left = new FormAttachment(2,0);
        selectLineWidthGroup.setLayoutData(formData1);
    
        Group selectLineStyleGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectLineStyleGroup.setText("Style");
        GridLayout lineStyleGridLayout = new GridLayout();
        lineStyleGridLayout.numColumns = 2;
        lineStyleGridLayout.marginHeight = 18;
        lineStyleGridLayout.marginWidth = 18;
        lineStyleGridLayout.horizontalSpacing = 8;
        lineStyleGridLayout.verticalSpacing = 8;
        selectLineStyleGroup.setLayout(lineStyleGridLayout);
        
        FormData formData2 = new FormData();
        formData2.top = new FormAttachment(selectLineWidthGroup, 16);
        formData2.left = new FormAttachment(2,0);
        selectLineStyleGroup.setLayoutData(formData2);
        
        Group selectLineColorGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectLineColorGroup.setText("Color");
        
        FormData formData5 = new FormData();
        formData5.top   = new FormAttachment(5,0);
        formData5.left  = new FormAttachment(selectLineWidthGroup, 10);
        formData5.right = new FormAttachment(98, 0);
        formData5.height = 300;
        selectLineColorGroup.setLayoutData(formData5);

        // Placing Lat/Lon increment interval selection
        Group selectLatLonIncrementIntrvalGroup = new Group ( composite, SWT.SHADOW_NONE );
        GridLayout latLonIncrementIntrvalGridLayout = new GridLayout();
        latLonIncrementIntrvalGridLayout.numColumns = 1;
        latLonIncrementIntrvalGridLayout.marginHeight = 8;
        latLonIncrementIntrvalGridLayout.marginWidth = 8;
        latLonIncrementIntrvalGridLayout.horizontalSpacing = 30;
        latLonIncrementIntrvalGridLayout.verticalSpacing = 8;
        selectLatLonIncrementIntrvalGroup.setLayout(latLonIncrementIntrvalGridLayout);
        
        FormData latLonIncrementIntrvalFormData = new FormData();
        latLonIncrementIntrvalFormData.top = new FormAttachment(selectLineStyleGroup, 6);
        latLonIncrementIntrvalFormData.left = new FormAttachment(2,0);
        selectLatLonIncrementIntrvalGroup.setLayoutData(latLonIncrementIntrvalFormData);
        
        Composite intervalLatLonButtonComposite = new Composite(selectLatLonIncrementIntrvalGroup, SWT.SHADOW_NONE); 
        GridLayout latLonIncrementIntrvalButtonGridLayout = new GridLayout();
        latLonIncrementIntrvalButtonGridLayout.numColumns = 8;
        intervalLatLonButtonComposite.setLayout(latLonIncrementIntrvalButtonGridLayout); 
        
        Composite intervalLatLonTextComposite = new Composite(selectLatLonIncrementIntrvalGroup, SWT.SHADOW_NONE); 
        GridLayout latLonIncrementIntrvalTextGridLayout = new GridLayout();
        latLonIncrementIntrvalTextGridLayout.numColumns = 4;
        intervalLatLonTextComposite.setLayout(latLonIncrementIntrvalTextGridLayout); 
        
        final Color black = display.getSystemColor(SWT.COLOR_BLACK);
        final Color white = display.getSystemColor(SWT.COLOR_WHITE);
        
        //  Associate with each line style a list of segment lengths (in pixels)
        //  of the repeating pattern.  Numbers are pixels on, pixels off, on, off, ...
        //  (Derived from similar structure in NMAP NxmLineA.c)
        //  CAUTION:  Duplication (of a sort).  This governs only local display of
        //  line patterns in this dialog (preview and line style selector buttons).
        //  Actual drawing of lines with these styles is up to the implementation
        //  of IGraphicsTarget being used.
        
        final Map<LineStyle,int[]> styleMap = new EnumMap<LineStyle,int[]>(LineStyle.class);
        styleMap.put(LineStyle.SOLID,                        new int[] { 4 });                        // GEMPAK line type  1
        styleMap.put(LineStyle.SHORT_DASHED,                 new int[] { 4, 4 });                     // GEMPAK line type  2 
        styleMap.put(LineStyle.MEDIUM_DASHED,                new int[] { 8, 8 });                     // GEMPAK line type  3
        styleMap.put(LineStyle.LONG_DASH_SHORT_DASH,         new int[] { 16, 8, 4, 8 });              // GEMPAK line type  4
        styleMap.put(LineStyle.LONG_DASHED,                  new int[] { 16, 8 });                    // GEMPAK line type  5
        styleMap.put(LineStyle.LONG_DASH_THREE_SHORT_DASHES, new int[] { 16, 8, 4, 8, 4, 8, 4, 8 });  // GEMPAK line type  6
        styleMap.put(LineStyle.LONG_DASH_DOT,                new int[] { 16, 8, 2, 8 });              // GEMPAK line type  7
        styleMap.put(LineStyle.LONG_DASH_THREE_DOTS,         new int[] { 16, 8, 2, 8, 2, 8, 2, 8 });  // GEMPAK line type  8
        styleMap.put(LineStyle.MEDIUM_DASH_DOT,              new int[] { 8, 8, 2, 8 });               // GEMPAK line type  9
        styleMap.put(LineStyle.DOTS,                         new int[] { 2, 4 });    			      // GEMPAK line type 10

	    
	    //  Line Preview Area
	    
        final Canvas linePreviewAreaCanvas = new Canvas(linePreviewAreaGroup, SWT.NONE);

        final int previewLineXmin =  16;
    	final int previewLineXmax = 180;
    	final int previewLineYctr =  16;

        linePreviewAreaCanvas.addPaintListener(new PaintListener() {
    		public void paintControl(PaintEvent event) {
    			GC gc = event.gc;
    			gc.setLineWidth((Integer)lineWidth.getAttrValue());
    			gc.setForeground(new Color(display, (RGB)lineColor.getAttrValue()));
    	        linePreviewAreaCanvas.setBackground(
    	        		((RGB)lineColor.getAttrValue()).getHSB()[2] > 0.2 ? black : white);
    			int x1 = previewLineXmin;
    			int x2 = previewLineXmin; 
    			int [] segLengths = styleMap.get((LineStyle)lineStyle.getAttrValue());
    			if( segLengths == null ){
    				return;
    			}
    			while (x2 < previewLineXmax)
    			{
    				boolean draw = true;
    				for (int eachLineLength : segLengths)
    				{	
    					int calculatedLineLength = (int)(eachLineLength * lineLengthFactor); 
    					x2 = Math.min(x1 + calculatedLineLength, previewLineXmax);
    					if (draw)
    					{
    						gc.drawLine(x1, previewLineYctr, x2, previewLineYctr);
    					}
    					if (x2 >= previewLineXmax)
    					{
    						break;
    					}
    					draw = !draw;
    					x1 = x2;
    				}    				
    			}
    		}
        });

        
        //  Parameters to give a uniform look to all line width/style buttons
        
        final int lineButtonHeight = 75;
        final int lineButtonWidth  = 15;
        final int buttonLineXmin = 8;
    	final int buttonLineXmax = 68;
    	final int buttonLineYctr = 7;

    	
        //  Line Width
        
        selectLineWidthButtons = new Button[4];
	    final int[] lineWidthButtonSequence = {0, 2,  // ...for 2-column grid layout
	    		                               1, 3};
        for (int i : lineWidthButtonSequence)
	    {
	        selectLineWidthButtons[i] = new Button(selectLineWidthGroup, SWT.TOGGLE);	        
            GridData gridData = new GridData();
            gridData.heightHint = lineButtonWidth;
            gridData.widthHint = lineButtonHeight;
            selectLineWidthButtons[i].setLayoutData(gridData);
            selectLineWidthButtons[i].setData(i+1);
	        selectLineWidthButtons[i].setToolTipText("Width " + (i+1));
            selectLineWidthButtons[i].addPaintListener(new PaintListener() {
    	    	public void paintControl(PaintEvent event) {
    	            GC gc = event.gc;
    	    		int width = (Integer) event.widget.getData();
    	    		gc.setLineWidth(width);
    	            gc.setForeground(black);
    	    		gc.drawLine(buttonLineXmin,buttonLineYctr,buttonLineXmax,buttonLineYctr);    	            
    	    	}
    	    });
	        selectLineWidthButtons[i].addSelectionListener(new SelectionAdapter() 
	        {
	        	public void widgetSelected(SelectionEvent event) 
	        	{
	        		selectLineWidthButtons[(Integer)lineWidth.getAttrValue()-1].setSelection(false);
	        		lineWidth.setAttrValue( (Integer) event.widget.getData() );
	        		linePreviewAreaCanvas.redraw();
	        		linePreviewAreaCanvas.update();
	        	}
	        });
	    }
        selectLineWidthButtons[(Integer)lineWidth.getAttrValue()-1].setSelection(true);  // set initial state

        
        //  Line Style
        lineStyleButtonMap = new EnumMap<LineStyle, Button>(LineStyle.class);
        
        for (LineStyle ls : lineStyleButtonSequence)
        {
        	Button lineStyleButton = new Button(selectLineStyleGroup, SWT.TOGGLE);
        	lineStyleButtonMap.put(ls, lineStyleButton);
        	GridData gridData = new GridData();
        	gridData.heightHint = lineButtonWidth;
        	gridData.widthHint = lineButtonHeight;
        	lineStyleButton.setLayoutData(gridData);
        	lineStyleButton.setData(ls);
        	lineStyleButton.setToolTipText(ls.name());
        	lineStyleButton.addPaintListener(new PaintListener() {
        		public void paintControl(PaintEvent event) {
        			GC gc = event.gc;
        			gc.setLineWidth(1);
        			gc.setForeground(black);
        			LineStyle ls = (LineStyle) event.widget.getData();
        			int [] segLengths = styleMap.get(ls);
        			if( segLengths == null ) return;
        			int x1 = buttonLineXmin;
        			int x2 = buttonLineXmin;        			
        			while (x2 < buttonLineXmax)
        			{
        				boolean draw = true;
        				for (int i : segLengths)
        				{
        					x2 = Math.min(x1 + i, buttonLineXmax);	
        					if (draw)
        					{
        						gc.drawLine(x1, buttonLineYctr, x2, buttonLineYctr);
        					}
        					if (x2 >= buttonLineXmax)
        					{
        						break;
        					}
        					draw = !draw;
        					x1 = x2;
        				}        				
        			}
        		}
        	});
        	lineStyleButton.addSelectionListener(new SelectionAdapter() 
        	{
        		public void widgetSelected(SelectionEvent event) 
        		{
	        		lineStyleButtonMap.get( (LineStyle)lineStyle.getAttrValue() ).setSelection(false);
	        		lineStyle.setAttrValue( (LineStyle)event.widget.getData() );
	        		linePreviewAreaCanvas.redraw();
	        		linePreviewAreaCanvas.update();
        		}
        	});
        }
		lineStyleButtonMap.get( (LineStyle)lineStyle.getAttrValue() ).setSelection(true);
 
        //  Line Color

		colorMatrixSelector = new ColorMatrixSelector(selectLineColorGroup, false, false,
				28, 92, 18, 22, 28, 80, 4, 8, 5);
		colorMatrixSelector.setColorValue( (RGB)lineColor.getAttrValue());
		colorMatrixSelector.addListener(new IPropertyChangeListener() {
	    	public void propertyChange(PropertyChangeEvent event) {
	    		lineColor.setAttrValue( colorMatrixSelector.getColorValue() );
	    		linePreviewAreaCanvas.redraw();
	    		linePreviewAreaCanvas.update();
	        }
	    });
	    
	    // Lat/Lon interval selections
        Label latLonIncrementsLabel = new Label(intervalLatLonButtonComposite, SWT.NONE);
        latLonIncrementsLabel.setText("Increments"); 
        intervalButtonArray = new Button[incrementValueArray.length]; 
        for(int i=0; i<incrementValueArray.length; i++) {
        	Button intervalButton = new Button(intervalLatLonButtonComposite, SWT.PUSH);
        	GridData gridData = new GridData();
        	gridData.heightHint = 30;
        	gridData.widthHint = 32;
        	intervalButton.setLayoutData(gridData);  
        	intervalButton.setText(String.valueOf(incrementValueArray[i])); 
        	intervalButton.setData(String.valueOf(incrementValueArray[i])); 
        	intervalButtonArray[i] = intervalButton; 
        }
	    
        int textWidth = 30; 
        int textHeight = 15; 
        Label latitudeLabel = new Label(intervalLatLonTextComposite, SWT.NONE);
        latitudeLabel.setText("Latitude:  ");
        latitudeText = new Text(intervalLatLonTextComposite, SWT.SINGLE);                        
        latitudeText.setLayoutData( new GridData( textWidth, textHeight ) );
        latitudeText.setText( String.valueOf( (Integer)latInterval.getAttrValue() )); 
        latitudeText.setEditable(true);  
        
        latitudeText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
				getValidInteger( latitudeText, latInterval );
        	}
        });
        // we could add one or more of Verify,Focus,or selection Listeners but the Modify seems to 
        // catch it all.

        Label longitudeLabel = new Label(intervalLatLonTextComposite, SWT.NONE|SWT.RIGHT_TO_LEFT);
        longitudeLabel.setText("        Longitude:  ");
        longitudeText = new Text(intervalLatLonTextComposite, SWT.SINGLE);                        
        longitudeText.setLayoutData( new GridData( textWidth, textHeight ) );
        longitudeText.setText( String.valueOf( (Integer)lonInterval.getAttrValue() )); 
        longitudeText.setEditable(true);   
        longitudeText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
				getValidInteger( longitudeText, lonInterval );
        	}
        }); 

        /*
         * Add SelectionListener to all interval buttons
         */
        for(int i=0; i<intervalButtonArray.length; i++ ) {
        	intervalButtonArray[i].addSelectionListener(new SelectionAdapter() 
        	{
        		public void widgetSelected(SelectionEvent event) 
        		{
        			String intervalStringValue = (String)event.widget.getData(); 
       				latitudeText.setText(intervalStringValue); 
       				longitudeText.setText(intervalStringValue); 
        		}
        	});
        }
  //      intervalButtonArray[3].setSelection(true);   // set initial state
       
        return composite;
    }
    
    // if an int can be parsed from the widget then set the rscAttr 
    // and if not then reset the widget with the current value of the RscAttrValue
    //
    public void getValidInteger( Text txtWid, RscAttrValue intAttr ) {
    	String intStr = txtWid.getText();
    	if( intStr.isEmpty() ) { 
    		return;
    	}
		try {
		    int ival = Integer.parseInt( txtWid.getText() );
		    intAttr.setAttrValue( Integer.valueOf(ival) );
		}
		catch( NumberFormatException exc ) {
			int ival = ((Integer)intAttr.getAttrValue()).intValue();
			txtWid.setText( Integer.toString( ival ) );
		}
    }
    
	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
	}
}

