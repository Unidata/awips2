package gov.noaa.nws.ncep.viz.overlays.dialogs;

import java.util.EnumMap;
import java.util.Map;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;

/**
 * Provides an interface to modify the line overlay parameters
 * 
 * This dialog must serve two purposes:  Allow 'live' editing
 * of an already loaded resource, and selection of preferences
 * for an RBD for future use.  For this reason, the dialog
 * (1) allows a caller to set preselected values (say, from
 * current resource settings, or from tables of defaults for
 * a particular resource type), (2) stores selected values in
 * private member variables, and (3) allows the caller to
 * retrieve selected values after the dialog has been closed.
 * 
 * (Design note:  Item (2) above is required because we can't
 * interrogate the settings of widgets after the dialog has
 * been closed, because they may have been destroyed by then.)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06 Feb 2009  53          bhebbard    Initial Creation.
 * 15 Apr 2009  53B         bhebbard    Implement preview; show line images on
 *                                      width/style buttons; remove LineScale;
 *                                      strengthen error checking in setAttributesFromMap;
 *                                      various layout and other cleanups.
 * 21 Apr 2009  90          bhebbard    Factor out color selection; use ColorMatrixSelector.
 * 17 Jun 2009  115         Greg Hull   Integrated with AbstractEditResourceAttrsDialog
 * 27 Apr 2010   #245       Greg Hull   Added Apply Button
 *
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */

public class ChangeLineAttributesDialog extends AbstractEditResourceAttrsDialog {

	//  Current attribute values.
	
	//  (Just initialize each to a 'neutral' default state on dialog
	//  creation; typically the caller will later use setter methods
	//  to apply particular defaults before opening the dialog.)
    
    private RscAttrValue lineStyle = null;
    private RscAttrValue lineColor = null;    
    private RscAttrValue lineWidth = null;    

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ChangeLineAttributesDialog( Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
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

    	// confirm the classes of the attributes..
    	if( lineStyle == null ||
    			lineStyle.getAttrClass() != LineStyle.class ) {
    		System.out.println( "lineStyle is null or not of expected class? "+ 
    				lineStyle.getAttrClass().toString() );
    	}
    	else if( lineColor == null ||
    			lineColor.getAttrClass() != RGB.class ) {
    		System.out.println( "lineColor is null or not of expected class? "+ 
    				lineColor.getAttrClass().toString() );
    	}
    	else if( lineWidth == null ||
    			lineWidth.getAttrClass() != Integer.class ) {
    		System.out.println( "lineWidth is null or not of expected class? "+ lineWidth.getAttrClass().toString() );
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

        
        final Color black = composite.getDisplay().getSystemColor(SWT.COLOR_BLACK);
        final Color white = composite.getDisplay().getSystemColor(SWT.COLOR_WHITE);
        
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
    			gc.setLineWidth(((Integer)lineWidth.getAttrValue()).intValue());
    			gc.setForeground(new Color(display, (RGB)lineColor.getAttrValue()));
    	        linePreviewAreaCanvas.setBackground(
    	        		((RGB)lineColor.getAttrValue()).getHSB()[2] > 0.2 ? black : white);
    			int x1 = previewLineXmin;
    			int x2 = previewLineXmin; 
    			int [] segLengths = styleMap.get((LineStyle)lineStyle.getAttrValue());
    			while (x2 < previewLineXmax)
    			{
    				boolean draw = true;
    				for (int i : segLengths)
    				{	
    					x2 = Math.min(x1 + i, previewLineXmax);
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
        
        final Button[] selectLineWidthButtons = new Button[4];
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
	        selectLineWidthButtons[i].addSelectionListener(new SelectionListener() 
	        {
	        	public void widgetSelected(SelectionEvent event) 
	        	{
	        		selectLineWidthButtons[(Integer)lineWidth.getAttrValue()-1].setSelection(false);
	        		lineWidth.setAttrValue( (Integer)event.widget.getData() );
	        		linePreviewAreaCanvas.redraw();
	        		linePreviewAreaCanvas.update();
	        	}
	        	public void widgetDefaultSelected(SelectionEvent event) 
	        	{
	        	}
	        });
	    }
        selectLineWidthButtons[(Integer)lineWidth.getAttrValue()-1].setSelection(true);  // set initial state
        
        //  Line Style

        final Map<LineStyle, Button> lineStyleButtonMap = new EnumMap<LineStyle, Button>(LineStyle.class);
        
        final LineStyle [] lineStyleButtonSequence = {  // ...for 2-column grid layout
        		LineStyle.DOTS,                  LineStyle.LONG_DASHED,
        		LineStyle.SOLID,                 LineStyle.LONG_DASH_THREE_SHORT_DASHES,
        		LineStyle.SHORT_DASHED,          LineStyle.LONG_DASH_DOT,  
        		LineStyle.MEDIUM_DASHED,         LineStyle.LONG_DASH_THREE_DOTS,
        		LineStyle.LONG_DASH_SHORT_DASH,  LineStyle.MEDIUM_DASH_DOT,
        		};
        
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
        	lineStyleButton.addSelectionListener(new SelectionListener() 
        	{
        		public void widgetSelected(SelectionEvent event) 
        		{
	        		lineStyleButtonMap.get((LineStyle)lineStyle.getAttrValue()).setSelection(false);
	        		lineStyle.setAttrValue( (LineStyle)event.widget.getData() );
	        		linePreviewAreaCanvas.redraw();
	        		linePreviewAreaCanvas.update();
        		}
        		public void widgetDefaultSelected(SelectionEvent event) 
        		{
        		}
        	});
        }
		lineStyleButtonMap.get( (LineStyle)lineStyle.getAttrValue() ).setSelection(true);  // set initial state
 
	    
        //  Line Color

		final ColorMatrixSelector cms = new ColorMatrixSelector(selectLineColorGroup, false, false,
				28, 92, 18, 22, 28, 80, 4, 8, 5);
	    cms.setColorValue( (RGB)lineColor.getAttrValue());
	    cms.addListener(new IPropertyChangeListener() {
	    	public void propertyChange(PropertyChangeEvent event) {
	    		lineColor.setAttrValue( cms.getColorValue() );
	    		linePreviewAreaCanvas.redraw();
	    		linePreviewAreaCanvas.update();
	        }
	    });
	
	    // from JFaceDialog
//	    applyDialogFont(composite);
        return composite;
    }

	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
		
	}
}

