package gov.noaa.nws.ncep.ui.nsharp.palette;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpLineConfigDialog
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
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;


import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpLineConfigDialog extends Dialog {
	private Map<LineStyle,int[]> styleMap = new EnumMap<LineStyle,int[]>(LineStyle.class);
    private LineStyle curLineStyle=LineStyle.SOLID;
    private int curLineWidth=1;
    private RGB curLineColor=null;
    private NsharpLineProperty curLineProperty;
    private String curLineName;
    private static NsharpLineConfigDialog instance=null;
    private  ArrayList<String> availLine = new ArrayList<String>();
	private Combo selLineCombo;
	private String seldLineName= "";
	private NsharpConfigStore configStore=null;
	private Canvas currentLineAreaCanvas;
	private Canvas linePreviewAreaCanvas;
	private ColorMatrixSelector cms;
	public static NsharpConfigStore setDefaultLineConfig(NsharpConfigStore cs){
		HashMap<String, NsharpLineProperty> linePropertyMap = cs.getLinePropertyMap();
		int i =0;
		for(String lnName: NsharpConstants.lineNameArray){
			NsharpLineProperty lp = NsharpConstants.defaultLineProperty[i];
			linePropertyMap.put(lnName, lp);
			i++;
		}
		return cs;
	}
	
	public static NsharpLineConfigDialog getInstance(Shell parentShell) {
		if(instance == null)
			instance = new NsharpLineConfigDialog(parentShell);
		return instance;
	}

	public NsharpLineConfigDialog(Shell parentShell) {
		super(parentShell);
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
        
        instance = this;
        for(int i=0; i < NsharpConstants.lineNameArray.length; i++)
        	availLine.add(NsharpConstants.lineNameArray[i]);
        
        NsharpConfigManager mgr =NsharpConfigManager.getInstance();
        configStore = mgr.retrieveNsharpConfigStoreFromFs();
        /*if (configStore== null) {
        	configStore = new NsharpConfigStore();
        	configStore = setDefaultLineConfig(configStore);
        	configStore=NsharpGraphConfigDialog.setDefaultGraphConfig(configStore);
        }
        else if( configStore.getLinePropertyMap().size() == 0){
        	configStore = setDefaultLineConfig(configStore);
        }*/
        curLineProperty = configStore.getLinePropertyMap().get(availLine.get(0));
    	curLineStyle =curLineProperty.getLineStyle();
    	curLineWidth = curLineProperty.getLineWidth();
    	curLineColor = curLineProperty.getLineColor();
    	curLineName = availLine.get(0);
	}
	
	
	public LineStyle getLineStyle() {
		return curLineStyle;
	}


	public int getLineWidth() {
		return curLineWidth;
	}


	public RGB getLineColor() {
		return curLineColor;
	}




	private  Composite createDialog(Composite composite) {
		final Display display = composite.getDisplay();

		FormLayout layout0 = new FormLayout();
		composite.setLayout(layout0);


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


		//  Line Preview Area

		linePreviewAreaCanvas = new Canvas(linePreviewAreaGroup, SWT.NONE);

		final int previewLineXmin =  16;
		final int previewLineXmax = 180;
		final int previewLineYctr =  16;

		linePreviewAreaCanvas.addPaintListener(new PaintListener() {
			public void paintControl(PaintEvent event) {
				GC gc = event.gc;
				gc.setLineWidth(curLineWidth);
				gc.setForeground(new Color(display, curLineColor));
				linePreviewAreaCanvas.setBackground(
						curLineColor.getHSB()[2] > 0.2 ? black : white);
				int x1 = previewLineXmin;
				int x2 = previewLineXmin; 
				int [] segLengths = styleMap.get(curLineStyle);
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
					selectLineWidthButtons[curLineWidth-1].setSelection(false);
					curLineWidth= (Integer)event.widget.getData();
					linePreviewAreaCanvas.redraw();
					linePreviewAreaCanvas.update();
				}
				public void widgetDefaultSelected(SelectionEvent event) 
				{
				}
			});
		}
		selectLineWidthButtons[curLineWidth-1].setSelection(true);  // set initial state

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
					lineStyleButtonMap.get(curLineStyle).setSelection(false);
					curLineStyle=( (LineStyle)event.widget.getData() );
					linePreviewAreaCanvas.redraw();
					linePreviewAreaCanvas.update();
				}
				public void widgetDefaultSelected(SelectionEvent event) 
				{
				}
			});
		}
		lineStyleButtonMap.get( curLineStyle).setSelection(true);  // set initial state


		//  Line Color

		cms = new ColorMatrixSelector(selectLineColorGroup, false, false,
				28, 92, 18, 22, 28, 80, 4, 8, 5);
		cms.setColorValue(curLineColor);
		cms.addListener(new IPropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				curLineColor = cms.getColorValue() ;
				linePreviewAreaCanvas.redraw();
				linePreviewAreaCanvas.update();
			}
		});
		return composite;
	}
	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Data Display" );
        
    }
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		top.setLayout(mainLayout);
		
		Composite lineComp = new Composite(top, SWT.NONE);
		GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        lineComp.setLayout(gl);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        lineComp.setLayoutData(gd);

        //Label selCmapLbl = new Label( lineComp, SWT.None );
        //selCmapLbl.setText("Nsharp Trace Lines:");
		selLineCombo = new Combo( lineComp, SWT.DROP_DOWN );
        selLineCombo.setItems( availLine.toArray(new String[0] ) );
        selLineCombo.select(0);
        
        selLineCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		String seldLine = selLineCombo.getText();
        		if( seldLine.equals( seldLineName ) ) {
        			return;
        		}
        		
        		seldLineName = seldLine;
        		curLineProperty = configStore.getLinePropertyMap().get(seldLineName);
        		if(curLineProperty == null){
        			//this only happened when configuration xml does not have this line property
        			curLineProperty = new NsharpLineProperty();
        			configStore.getLinePropertyMap().put(seldLineName, curLineProperty);
        		}
    			curLineStyle =curLineProperty.getLineStyle();
    			curLineWidth = curLineProperty.getLineWidth();
    			curLineColor = curLineProperty.getLineColor();
    			curLineName = seldLineName;
    			linePreviewAreaCanvas.redraw();
				linePreviewAreaCanvas.update();
    			currentLineAreaCanvas.redraw();
				currentLineAreaCanvas.update();
				cms.setColorValue(curLineColor);
        	}
        });
        Composite currentLineComp = new Composite( lineComp, SWT.NONE );
        FormLayout layout0 = new FormLayout();
        currentLineComp.setLayout(layout0);
        Group currentLineAreaGroup = new Group ( currentLineComp, SWT.SHADOW_NONE );
		currentLineAreaGroup.setLayout(new FillLayout());

		FormData formData0 = new FormData();
		formData0.top = new FormAttachment(5,0);
		formData0.left = new FormAttachment(2,0);
		formData0.width = 130;
		formData0.height = 30;
		currentLineAreaGroup.setLayoutData(formData0);
		currentLineAreaCanvas = new Canvas(currentLineAreaGroup, SWT.NONE);

		final int previewLineXmin =  10;//16;
		final int previewLineXmax = 120;//180;
		final int previewLineYctr =  16;
		final Display display = top.getDisplay();
		final Color black = display.getSystemColor(SWT.COLOR_BLACK);
		final Color white = display.getSystemColor(SWT.COLOR_WHITE);
		currentLineAreaCanvas.addPaintListener(new PaintListener() {
			public void paintControl(PaintEvent event) {
				GC gc = event.gc;
				gc.setLineWidth(curLineWidth);
				gc.setForeground(new Color(display, curLineColor));
				currentLineAreaCanvas.setBackground(
						curLineColor.getHSB()[2] > 0.2 ? black : white);
				int x1 = previewLineXmin;
				int x2 = previewLineXmin; 
				int [] segLengths = styleMap.get(curLineStyle);
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
		
		Composite lineEditComp = new Composite( top, SWT.NONE );
        createDialog(lineEditComp);
        
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
	private void notifyEditor(){
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpSkewTResource rsc = editor.getNsharpSkewTDescriptor().getSkewtResource();
        	//rsc.setWindBarbDistance(windBarbDistance);
        	rsc.setLinePropertyMap(configStore.getLinePropertyMap());
        	rsc.createRscPressTempCurveShapeAll();
        	rsc.createRscHodoWindShapeAll();
        	editor.refresh();
        }
	}
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		Button appBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Apply", false);
		appBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("appBtn listener is called");
				curLineProperty.setLineColor(curLineColor);
				curLineProperty.setLineStyle(curLineStyle);
				curLineProperty.setLineWidth(curLineWidth);
				currentLineAreaCanvas.redraw();
				currentLineAreaCanvas.update();
				//configStore.getLinePropertyMap().put(curLineName, curLineProperty);
				notifyEditor();
			}          		            	 	
		} );
		Button saveBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Save", false);
		saveBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("saveBtn listener is called");
				curLineProperty.setLineColor(curLineColor);
				curLineProperty.setLineStyle(curLineStyle);
				curLineProperty.setLineWidth(curLineWidth);
				currentLineAreaCanvas.redraw();
				currentLineAreaCanvas.update();
				//configStore.getLinePropertyMap().put(curLineName, curLineProperty);
				notifyEditor();
				NsharpConfigManager mgr =NsharpConfigManager.getInstance();
				/* for testing
				NsharpConfigStore st = configStore;
				HashMap<String, NsharpLineProperty> linePropertyMap = new HashMap<String, NsharpLineProperty>();
				for(int i=0; i < NsharpConstants.lineNameArray.length; i++){
					NsharpLineProperty tempLine = new NsharpLineProperty();
					tempLine.setLineWidth(1);
					tempLine.setLineStyle(LineStyle.SOLID);
					tempLine.setLineColor(new RGB(155,0,220));
					linePropertyMap.put(NsharpConstants.lineNameArray[i], tempLine);
				} 
				st.setLinePropertyMap(linePropertyMap);*/
				try {
					mgr.saveConfigStoreToFs(configStore);
				} catch (VizException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
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
	
}
