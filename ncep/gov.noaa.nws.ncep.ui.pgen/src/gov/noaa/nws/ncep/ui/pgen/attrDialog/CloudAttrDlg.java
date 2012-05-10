/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.CloudAttrDlg
 * 
 * 05 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrDialog;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MidCloudText;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.Cloud;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.tools.ILabeledLine;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is a singleton for CLoud attributes dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10					B. Yin   	Initial Creation.
 * 10/10		#?			B. Yin		Use MidLevelCloudText as label.	
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 12/11		#523		B. Yin		Save the label dialog location
 * 12/11		?			B. Yin		Added the open/close line button
 * </pre>
 * 
 * @author	B. Yin
 */

public class CloudAttrDlg extends AttrDlg implements ILine{
	
	//singleton instance
	private static CloudAttrDlg INSTANCE;
	
	/**
	 * drawing tool, can be PgenCloudDrawingTool or PgenCloudModifyTool
	 */
	private ILabeledLine cloudTool;
	
	private MidLevelCloudAttrDlg labelDlg;
	
	//composite to hold all widgets
	private Composite top;
	
    //Cloud line color
    protected Label colorLbl;
    protected ColorButtonSelector cs;
    
    //Closed line check box
    protected Button closedChkBox;
    
    //Add-Line button
    private Button addLineBtn;
    
    //Del-Line button
    private Button delLineBtn;
    
    //Add-Label button
    private Button addLabelBtn;
    
    //Del-Label button
    private Button delLabelBtn;
    
    //Flip button
    private Button flipBtn;
    private Button openCloseBtn;

    //Edit Label button
    private Button editLabelBtn;
    
    private float lineWidth = 2.0f;

	private Point labelDlgLocation;

    /**
     * Constructor
     * @param parShell
     * @throws VizException
     */
	private CloudAttrDlg(Shell parShell) throws VizException {
		super(parShell);
	}
	
	/**
	 * Creates a cloud attribute dialog if the dialog does not exist 
	 * and returns the instance. 
	 * If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static CloudAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new CloudAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
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
	        mainLayout.marginWidth = 20;
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * 
	 */
	private void initializeComponents() {
		
        getShell().setText("Cloud Attributes");
        
       // chkBox = new Button[9];

		Composite pane1 = new Composite(top, SWT.NONE);
		GridLayout gl = new GridLayout( 2, false );
		pane1.setLayout( gl );
		
        /*
         * color button
         */
        createColorAttr(pane1);

        /*
         * closed check box
         */
        createClosedAttr(pane1);
        
		//'Add Line' button
		addLineBtn = new Button(pane1, SWT.TOGGLE);
		addLineBtn.setText("Add Line");
		addLineBtn.setLayoutData(new GridData(120,30));
		addLineBtn.setSelection(true);
		addLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() ){
					
					cloudTool.resetMouseHandler();
					addLabelBtn.setSelection(false);
					if ( labelDlg != null ) labelDlg.close();
					delLineBtn.setSelection(false);
					delLabelBtn.setSelection(false);
					flipBtn.setSelection(false);
					openCloseBtn.setSelection(false);
				}
				else {
//					cloudTool.setAddLineMode(false);
				}
			}
		});
		
		//'Add Label button
		addLabelBtn = new Button(pane1, SWT.TOGGLE);
		addLabelBtn.setText("Add Label");
		addLabelBtn.setLayoutData(new GridData(120,30));
		addLabelBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() ) {
					if ( cloudTool != null && cloudTool.getLabeledLine() != null 
							&&  cloudTool.getLabeledLine().getLines().size() > 0 ){
						cloudTool.setAddingLabelHandler();
						addLineBtn.setSelection(false);
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
						flipBtn.setSelection(false);
						openCloseBtn.setSelection(false);
						try {
							labelDlg = new LabelAttrDlg( CloudAttrDlg.this.getParentShell() );
							//labelDlg = MidLevelCloudAttrDlg.getInstance( CloudAttrDlg.this.getParentShell());
							labelDlg.setBlockOnOpen(false);
							labelDlg.open();
							labelDlg.setPgenType("MID_LEVEL_CLOUD");
							labelDlg.setDefaultAttr();
							//labelDlg.setColor(CloudAttrDlg.this.getColors()[0]);
						} catch (VizException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
						
					}
					else {
						e.doit = false;
						((Button)e.widget).setSelection(false);
					}
				}
				else {
					if ( labelDlg != null ) labelDlg.close();
					if ( cloudTool != null ) cloudTool.resetMouseHandler();
				}
			}
			
		});
		
		
		//'Del Line' button
		delLineBtn = new Button(pane1, SWT.TOGGLE);
		delLineBtn.setText("Del Line");
		delLineBtn.setLayoutData(new GridData(120,30));
		delLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() ){
					cloudTool.setDeleteHandler(true, false, false);
					addLineBtn.setSelection(false);
					addLabelBtn.setSelection(false);
					if ( labelDlg != null ) labelDlg.close();
					delLabelBtn.setSelection(false);
					flipBtn.setSelection(false);
					openCloseBtn.setSelection(false);
				}
				else {
					cloudTool.resetMouseHandler();
				}
				
			}
		});	
		
		//'Del Label' button
		delLabelBtn = new Button(pane1, SWT.TOGGLE);
		delLabelBtn.setText("Del Label");
		delLabelBtn.setLayoutData(new GridData(120,30));
		delLabelBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ){
						cloudTool.setDeleteHandler(false, false, false);
						addLineBtn.setSelection(false);
						addLabelBtn.setSelection(false);
						if ( labelDlg != null ) labelDlg.close();
						delLineBtn.setSelection(false);
						flipBtn.setSelection(false);
						openCloseBtn.setSelection(false);
					}
					else {
						cloudTool.resetMouseHandler();
					}
					
			}
		});	
		
		//'Del Label' button
		flipBtn = new Button(pane1, SWT.TOGGLE);
		flipBtn.setText("Flip");
		flipBtn.setLayoutData(new GridData(120,30));
		flipBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ){
						cloudTool.setDeleteHandler(true, true, false);
						addLineBtn.setSelection(false);
						addLabelBtn.setSelection(false);
						if ( labelDlg != null ) labelDlg.close();
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
						openCloseBtn.setSelection(false);
					}
					else {
						cloudTool.resetMouseHandler();
					}
					
			}
		});	
		
		//Edit Label button
		editLabelBtn = new Button(pane1, SWT.PUSH);
		editLabelBtn.setText("Edit Label");
		editLabelBtn.setLayoutData(new GridData(120,30));
		editLabelBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
					if ( cloudTool != null && cloudTool.getLabeledLine() != null 
							&& cloudTool.getLabeledLine().getLines().size() > 0 
							&& !addLabelBtn.getSelection()){
						addLineBtn.setSelection(false);
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
						addLabelBtn.setSelection(false);
						flipBtn.setSelection(false);
						openCloseBtn.setSelection(false);
						//labelDlg = MidLevelCloudAttrDlg.getInstance( CloudAttrDlg.this.getParentShell());
						try {
							labelDlg = new LabelAttrDlg( CloudAttrDlg.this.getParentShell() );
							labelDlg.setBlockOnOpen(false);
							labelDlg.open();
							labelDlg.setPgenType("MID_LEVEL_CLOUD");
							labelDlg.setDefaultAttr();
							labelDlg.enableButtons();
							labelDlg.setDrawableElement(cloudTool.getLabeledLine());
							labelDlg.setDrawingLayer( drawingLayer );
							//labelDlg.setColor(CloudAttrDlg.this.getColors()[0]);
						} catch (VizException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
					}
			}
		});
		//'Open/close' button
		openCloseBtn = new Button(pane1, SWT.TOGGLE);
		openCloseBtn.setText("Open/Close");
		openCloseBtn.setLayoutData(new GridData(120,30));
		openCloseBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ){
						cloudTool.setDeleteHandler(true, false, true);
						addLineBtn.setSelection(false);
						addLabelBtn.setSelection(false);
						if ( labelDlg != null ) labelDlg.close();
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
						flipBtn.setSelection(false);
					}
					else {
						cloudTool.resetMouseHandler();
					}
					
			}
		});		
		
        addSeparator(top);
 
	}
	
	/**
	 * Create widgets for the Color attribute
	 */
	private void createColorAttr(Composite comp){

	/*	chkBox[ChkBox.COLOR.ordinal()] = new Button(top, SWT.CHECK);
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
	 */
		Composite colorGroup = new Composite(comp, SWT.NONE);
		GridLayout gl = new GridLayout( 2, false );
		gl.marginWidth = 0;
		colorGroup.setLayout( gl );
		
		colorLbl = new Label( colorGroup, SWT.LEFT );
		colorLbl.setText("Color:");
		cs = new ColorButtonSelector( colorGroup );
		cs.setColorValue( new RGB( 0, 178,238 ) );
	}
	
	/**
	 * Create 'Closed' check box
	 * @param comp
	 */
	private void createClosedAttr(Composite comp){
/*		chkBox[ChkBox.CLOSE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.CLOSE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.CLOSE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					closedChkBox.setEnabled(true);
				}
				else {
					closedChkBox.setEnabled(false);

				}
			}

		});
		
	*/
		closedChkBox  = new Button(comp, SWT.CHECK);
		closedChkBox.setText("Closed");
	}

	@Override
	public void setAttrForDlg(IAttribute ia) {
		if ( cloudTool != null && cloudTool.getLabeledLine() != null ){
			Line ln = (Line)cloudTool.getLabeledLine().getPrimaryDE();
			this.setColor(ln.getColors()[0]);
			this.closedChkBox.setSelection(ln.isClosedLine());
			
		}
	
	}
	
	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors(){
	//	if ( chkBox[ChkBox.COLOR.ordinal()].getSelection() ){
		  // IAttribute requires to return an array of colors
		  // Only the first color is used at this time.		
	      Color[] colors = new Color[2];
          
          colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );
          
          //use same color to fill
          colors[1] = colors[0];

          return colors;          
	//	}
	//	else {
	//		return null;
	//	}
	}
	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */	
	public void setColor( Color clr ){

		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));

	}
	

	/**
	 * Get the line width for cloud lines
	 */
    @Override 
    public float getLineWidth(){
    	return lineWidth;
    }
    
    /**
     * Set the cloud line width
     * @param lnWidth
     */
    public void setLineWidth( float lnWidth ){
    	this.lineWidth = lnWidth;
    }  
    
	/**
	 * Returns the Close flag of the dialog.
	 */
	public Boolean isClosedLine(){
	//	if ( chkBox[ChkBox.CLOSE.ordinal()].getSelection() ){

			return closedChkBox.getSelection();
	//	}
	//	else {
	//		return null;
	//	}

	}
	
	/**
	 * Sets the Close flag of the dialog.
	 * @param cls
	 */
	/*private void setClosed( Boolean cls ){
		
		if ( closedChkBox != null ){
			closedChkBox.setSelection( cls );
		}
	}
	*/
	

	/**
	 * Set the drawing tool
	 */
	public void setCloudDrawingTool(ILabeledLine pgenTool) {
		
		this.cloudTool = pgenTool;
		
	}
	
	@Override
	/**
	 * Updates the selected element and redraws the PGEN layer.
	 */
	public void okPressed(){
		if ( cloudTool != null && cloudTool.getLabeledLine() != null ){
			LabeledLine ll = cloudTool.getLabeledLine();
			LabeledLine newll = ll.copy();
			
			Iterator<AbstractDrawableComponent> it = newll.getComponentIterator();
			while( it.hasNext() ){
				AbstractDrawableComponent adc = it.next();
				if ( adc instanceof Line ){
					((Line)adc).setColors(this.getColors());
				}
			}
			
			drawingLayer.replaceElement(ll, newll);
			cloudTool.setLabeledLine(newll);
			
			//reset handle bar
			drawingLayer.removeSelected();
			Iterator<DrawableElement> iterator = newll.createDEIterator();
			while( iterator.hasNext() ){
				drawingLayer.addSelected(iterator.next());
			}
			
			mapEditor.refresh();
		}
	
	}
	
	@Override
	public void resetLabeledLineBtns(){
		addLineBtn.setSelection(false);
		addLabelBtn.setSelection(false);
		if ( labelDlg != null ) labelDlg.close();
		delLineBtn.setSelection(false);
		delLabelBtn.setSelection(false);
		flipBtn.setSelection(false);
	}
	
	@Override
	public boolean isAddLineMode(){
		return addLineBtn.getSelection();
	}
	
	/**
     * close all related dialogs.
     */
	@Override
	public boolean close(){
		
		if ( labelDlg != null ) labelDlg.close();
		return super.close();
		
	}
	
	/**
	 * Get the label attr dialog
	 * @return
	 */
	public MidLevelCloudAttrDlg getLabelDlg(){
		return labelDlg;
	}
	
	/**
	 * A class for cloud label attribute dialog. 
	 * @author bingfan
	 *
	 */
	private class LabelAttrDlg extends MidLevelCloudAttrDlg {
		

		/**
		 * constructor
		 * @param parShell
		 * @throws VizException
		 */
		private LabelAttrDlg(Shell parShell) throws VizException {
			
	        super(parShell);
			
	    }
		
		@Override
		public void okPressed(){
			
			Cloud newCloud = (Cloud)cloudTool.getLabeledLine().copy();
			
			//apply to all labels
			for (gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.Label lbl : newCloud.getLabels()){
				if ( lbl.getSpe() instanceof MidCloudText ){
					((MidCloudText)lbl.getSpe()).update(this);
					for ( Line ln : lbl.getArrows()){
						ln.setColors(this.getColors());
					}
				}
			}
			
			//save to settings
			if ( !newCloud.getLabels().isEmpty() ){
				AttrSettings.getInstance().setSettings( newCloud.getLabels().get(0).getSpe() );
			}
			
			//put the updated cloud in PGEN resource and refresh
			CloudAttrDlg.this.drawingLayer.replaceElement(cloudTool.getLabeledLine(), newCloud);
			drawingLayer.setSelected(newCloud);
			
			cloudTool.setLabeledLine(newCloud);
			
			CloudAttrDlg.this.mapEditor.refresh();
			close();
		}
		
		/**
		 * Set the location of the dialog
		 */
		@Override
		public int open(){
		
			if(labelDlgLocation != null){
				shellLocation = labelDlgLocation;
			}
			
	   	    return super.open();
			
		}
		
		/** 
		 * Save location of the dialog.
		 */
		public boolean close() {
			if(getShell() != null){
				Rectangle bounds = getShell().getBounds();
				labelDlgLocation = new Point(bounds.x, bounds.y);
			}
			return super.close();
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

	@Override
	public int getSmoothFactor() {
		return 2;
	}

	@Override
	public Boolean isFilled() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FillPattern getFillPattern() {
		// TODO Auto-generated method stub
		return null;
	}
}
