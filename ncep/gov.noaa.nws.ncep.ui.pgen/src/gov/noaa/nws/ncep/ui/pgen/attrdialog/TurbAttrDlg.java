/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TurbAttrDlg
 * 
 * 05 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AvnText;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.tools.ILabeledLine;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is a singleton for Turbulence attributes dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		#306			B. Yin   	Initial Creation.
 * 11/10		?				B. Yin		Use smooth level 2.
 * 04/11		#?				B. Yin		Re-factor IAttribute
 * </pre>
 * 
 * @author	B. Yin
 */

public class TurbAttrDlg extends AttrDlg implements ILine {
	
	//singleton instance
	private static TurbAttrDlg INSTANCE;
	
	/**
	 * drawing tool, can be PgenLabeledLineDrawingTool or PgenLabeledLineModifyTool
	 */
	private ILabeledLine turbTool;
	
	//composite to hold all widgets
	private Composite top;
    
	// Cloud top label and text field
	private Label topLabel;
    private Text topValue;
    
    //Cloud bottom label and text field
    private Label bottomLabel;
    private Text bottomValue;
    
    //Cloud line color
    protected Label colorLbl;
    protected ColorButtonSelector cs;
    
    //Closed line check box
    protected Button closedChkBox;
    private Button openCloseBtn;
    
    //Add-Line button
    private Button addLineBtn;
    
    //Del-Line button
    private Button delLineBtn;
    
    //Sel-Line button
    //private Button selLineBtn;
    
    //Add-Label button
    private Button addLabelBtn;
    
    //Del-Label button
    private Button delLabelBtn;
    
    //Sel-Lable button
    //private Button selLabelBtn;
    
    //Symbol menu
    private SymbolCombo symbolCombo;
	private static final String[] TURB_LIST = new String[] { "TURBULENCE_0", "TURBULENCE_1", "TURBULENCE_2",
			 "TURBULENCE_3", "TURBULENCE_4", 
			 "TURBULENCE_4|TURBULENCE_6", "TURBULENCE_5",
			 "TURBULENCE_6", "TURBULENCE_6|TURBULENCE_7",
			 "TURBULENCE_7", "TURBULENCE_8" };

    private float lineWidth = 2.0f;

    private String prevTop = "";
    private String prevBottom = "";
    private String prevSymbol = "";
    
    /**
     * Constructor
     * @param parShell
     * @throws VizException
     */
	private TurbAttrDlg(Shell parShell) throws VizException {
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
	public static TurbAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new TurbAttrDlg( parShell );
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
		
        getShell().setText("Turbulence Attributes");
        
       // chkBox = new Button[9];

		Composite pane1 = new Composite(top, SWT.NONE);
		GridLayout gl = new GridLayout( 2, false );
		pane1.setLayout( gl );
		
        /*
         * Top level
         */
        createTopAttr(pane1);
        
        /*
         * bottom level
         */
        createBottomAttr(pane1);
        
        /*
         * color button
         */
        createColorAttr(pane1);

        /*
         * symbol menu
         */
        createSymbolAttr(pane1);
        
        /*
         * closed check box
         */
        createClosedAttr(pane1);
        
		//'Open/close' button
		openCloseBtn = new Button(pane1, SWT.TOGGLE);
		openCloseBtn.setText("Open/Close");
		openCloseBtn.setLayoutData(new GridData(120,30));
		openCloseBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ){
						turbTool.setDeleteHandler(true, false, true);
						addLineBtn.setSelection(false);
						addLabelBtn.setSelection(false);
	//					if ( labelDlg != null ) labelDlg.close();
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
	//					flipBtn.setSelection(false);
					}
					else {
						turbTool.resetMouseHandler();
					}
					
			}
		});		
                
		//'Add Line' button
		addLineBtn = new Button(pane1, SWT.TOGGLE);
		addLineBtn.setText("Add Line");
		addLineBtn.setLayoutData(new GridData(120,30));
		addLineBtn.setSelection(true);
		addLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( ((Button)e.widget).getSelection() ){
					
					turbTool.resetMouseHandler();
					addLabelBtn.setSelection(false);
					delLineBtn.setSelection(false);
					delLabelBtn.setSelection(false);
					openCloseBtn.setSelection(false);
				}
				else {
//					turbTool.setAddLineMode(false);
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
					if ( turbTool != null && turbTool.getLabeledLine() != null 
							&&  turbTool.getLabeledLine().getLines().size() > 0 ){
						turbTool.setAddingLabelHandler();
						addLineBtn.setSelection(false);
						delLineBtn.setSelection(false);
						delLabelBtn.setSelection(false);
						openCloseBtn.setSelection(false);
					}
					else {
						e.doit = false;
						((Button)e.widget).setSelection(false);
					}
				}
				else {
					if ( turbTool != null )
						turbTool.resetMouseHandler();
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
					turbTool.setDeleteHandler(true, false, false);
					addLineBtn.setSelection(false);
					addLabelBtn.setSelection(false);
					delLabelBtn.setSelection(false);
					openCloseBtn.setSelection(false);
				}
				else {
					turbTool.resetMouseHandler();
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
						turbTool.setDeleteHandler(false, false, false);
						addLineBtn.setSelection(false);
						addLabelBtn.setSelection(false);
						delLineBtn.setSelection(false);
						openCloseBtn.setSelection(false);
					}
					else {
						turbTool.resetMouseHandler();
					}
					
			}
		});	
		
		//'Select Line' button
	/*	selLineBtn = new Button(pane1, SWT.PUSH);
		selLineBtn.setText("Select Line");
		selLineBtn.setLayoutData(new GridData(120,30));
		selLineBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				PgenUtil.loadOutlookDrawingTool();
			}
		});
	*/	
		//'Select Label button
	/*	selLabelBtn = new Button(pane1, SWT.PUSH);
		selLabelBtn.setText("Select Label");
		selLabelBtn.setLayoutData(new GridData(120,30));
		selLabelBtn.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				PgenUtil.loadOutlookDrawingTool();
			}
		});
	*/	

        addSeparator(top);
 
	}
	
	private void createTopAttr(Composite comp){

	//	chkBox[ChkBox.TOP.ordinal()] = new Button(top, SWT.CHECK);
	//	chkBox[ChkBox.TOP.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
	//	chkBox[ChkBox.TOP.ordinal()].addSelectionListener(new SelectionAdapter(){
/*
			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					topLabel.setEnabled(true);
					topValue.setEnabled(true);
				}
				else {
					topLabel.setEnabled(false);
					topValue.setEnabled(false);

				}
			}

		});
	*/
		topLabel = new Label( comp, SWT.NONE );
		topLabel.setText( "Top:" );

		topValue = new Text( comp, SWT.SINGLE | SWT.BORDER );                        
		topValue.setEditable( true );
		
		if ( prevTop.isEmpty() ){
			topValue.setText("XXX");
		}
		else {
			topValue.setText(prevTop);
		}
		
		topValue.addModifyListener(new ModifyListener(){

			@Override
			public void modifyText(ModifyEvent e) {
				prevTop = ((Text)e.widget).getText();
			}

		});
			
	}
	
	private void createBottomAttr(Composite comp){

		//	chkBox[ChkBox.TOP.ordinal()] = new Button(top, SWT.CHECK);
		//	chkBox[ChkBox.TOP.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		//	chkBox[ChkBox.TOP.ordinal()].addSelectionListener(new SelectionAdapter(){
		/*
				@Override
				public void widgetSelected(SelectionEvent e) {
					Button btn = (Button)e.widget;
					if(btn.getSelection()){
						topLabel.setEnabled(true);
						topValue.setEnabled(true);
					}
					else {
						topLabel.setEnabled(false);
						topValue.setEnabled(false);

					}
				}

			});
		 */
		bottomLabel = new Label( comp, SWT.NONE );
		bottomLabel.setText( "Bottom:" );

		bottomValue = new Text( comp, SWT.SINGLE | SWT.BORDER );                        
		bottomValue.setEditable( true );
		
		if ( prevBottom.isEmpty() ){
			bottomValue.setText("XXX");
		}
		else {
			bottomValue.setText(prevBottom);
		}
		
		bottomValue.addModifyListener(new ModifyListener(){

			@Override
			public void modifyText(ModifyEvent e) {
				prevBottom = ((Text)e.widget).getText();
			}

		});	}	
	
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
		cs.setColorValue( new RGB( 238, 238, 0 ) );
	}
	
	private void createSymbolAttr(Composite comp){
/*		chkBox[ChkBox.CLOSE.ordinal()] = new Button(top, SWT.CHECK);
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
		
	*/			
		symbolCombo = new SymbolCombo( comp );
		symbolCombo.setLayoutData(new GridData(10, 1));
		symbolCombo.setItems(TURB_LIST);
		
		if ( prevSymbol.isEmpty()) {
			symbolCombo.select(4);
		}
		else {
			symbolCombo.setSelectedText(prevSymbol);
		}
		
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
		closedChkBox.setSelection(true);
	}
	
	@Override
	public void setAttrForDlg(IAttribute ia) {
		if ( turbTool != null && turbTool.getLabeledLine() != null ){
			Iterator<DrawableElement> it = turbTool.getLabeledLine().createDEIterator();
			while( it.hasNext() ){
				DrawableElement de = it.next();
				if ( de instanceof AvnText ){
					this.setColor( de.getColors()[0] );
					this.topValue.setText(((AvnText)de).getTopValue());
					this.bottomValue.setText( ((AvnText)de).getBottomValue());
					break;
				}
				else if ( de instanceof Line ){
					this.setColor(de.getColors()[0]);
				}
			}
			Line ln = (Line)turbTool.getLabeledLine().getPrimaryDE();
			this.closedChkBox.setSelection(ln.isClosedLine());

		}
	
	}
	
	@Override
	public int getSmoothFactor(){
		return 2;
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
	
    
    @Override 
    public float getLineWidth(){
    	return lineWidth;
    }
    
    public void setLineWidth( float lnWidth ){
    	this.lineWidth = lnWidth;
    }  
	/**
	 * Returns the Close flag of the dialog.
	 */
	public Boolean isClosedLine(){
		return closedChkBox.getSelection();
	}
	
	public String getTopValue() {
		return topValue.getText();
	}
	
	public String getBottomValue() {
		return bottomValue.getText();
	}
	
	public void setTurbDrawingTool(ILabeledLine pgenTool) {
		
		this.turbTool = pgenTool;
		
	}
	
	@Override
	/**
	 * Removes ghost line, handle bars, and closes the dialog
	 */
	public void cancelPressed(){
		
		PgenUtil.setSelectingMode();
		super.cancelPressed();
		
	}
	@Override
	/**
	 * Updates the selected element and redraws the PGEN layer.
	 */
	public void okPressed(){
		if ( turbTool != null && turbTool.getLabeledLine() != null ){
			LabeledLine ll = turbTool.getLabeledLine();
			LabeledLine newll = ll.copy();
			
			Iterator<DrawableElement> it = newll.createDEIterator();
			while( it.hasNext() ){
				DrawableElement de = it.next();
				if ( de instanceof AvnText ){
					de.setColors(getColors());
					((AvnText) de).setTopValue( topValue.getText());
					((AvnText) de).setBottomValue( bottomValue.getText());
					((AvnText) de).setSymbolPatternName(getSymbolPatternName());
				}
				else if ( de instanceof Line ){
					de.setColors(this.getColors());
				}
			}
			
			drawingLayer.replaceElement(ll, newll);
			turbTool.setLabeledLine(newll);
			
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
		delLineBtn.setSelection(false);
		delLabelBtn.setSelection(false);
	}
	
	@Override
	public boolean isAddLineMode(){
		return addLineBtn.getSelection();
	}
	
	public String getSymbolPatternName() {
		return symbolCombo.getSelectedText();
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
	public Boolean isFilled() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FillPattern getFillPattern() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
     * close all related dialogs and re-set tool.
     */
	@Override
	public boolean close(){
		
		if ( !symbolCombo.isDisposed())
			prevSymbol = symbolCombo.getSelectedText();
		return super.close();
		
	}
}
