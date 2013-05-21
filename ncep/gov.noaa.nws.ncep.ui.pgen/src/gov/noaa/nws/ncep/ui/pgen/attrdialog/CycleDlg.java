/*
 * gov.noaa.nws.ncep.ui.pgen.tools.CycleDlg
 * 
 * Narch 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Create a dialog for PGEN cycle action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#263		M.Laryukhin	Initial creation
 * 01/11		TTR381		J. Wu	    Update cycle/hour for all GFA 
 *                                      elements.
 * 02/11		TTR381		J. Wu	    Reset PGEN IDisplayable for redraw. 
 * 03/13		#928		B. Yin		Added a separator above the button bar.
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class CycleDlg extends AttrDlg {

	static CycleDlg instance;

	private Composite top;
	private Combo dayCbo;
	private Combo cycleCbo;

	private Button routineBtn;

	private Button updateBtn;

	/**
	 * Private constructor
	 * 
	 * @param parShell
	 * @throws VizException
	 */
	private CycleDlg(Shell parShell) throws VizException {

		super(parShell);

	}

	/**
	 * Creates an extrapolation dialog if the dialog does not exist and returns
	 * the instance. If the dialog exists, return the instance.
	 * 
	 * @param parShell
	 * @return
	 */
	public static CycleDlg getInstance(Shell parShell) {
		if (instance == null) {
			try {
				instance = new CycleDlg(parShell);
			} catch (VizException e) {
				e.printStackTrace();
			}
		}

		return instance;
	}

	/*
	 * (non-Javadoc) Create all of the widgets on the Dialog
	 * 
	 * @see
	 * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
	 * .Composite)
	 */
	@Override
	public Control createDialogArea(Composite parent) {

		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(5, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		initializeComponents();

		return top;
	}

	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * 
	 * @param listener
	 */
	private void initializeComponents() {

		this.getShell().setText("Cycle Selection");

		Label dayLbl = new Label(top, SWT.LEFT);
		dayLbl.setText("Day:");

		dayCbo = new Combo(top, SWT.DROP_DOWN | SWT.READ_ONLY);
		for (int i = 1; i <= 31; i++) {
			dayCbo.add(i < 10 ? "0" + i : "" + i);
		}
		int day = PgenCycleTool.getCycleDay();
		int index = dayCbo.indexOf(day < 10 ? "0" + day : "" + day);
		dayCbo.select(index);

		Label cycleLbl = new Label(top, SWT.LEFT);
		cycleLbl.setText("Cycle:");

		cycleCbo = new Combo(top, SWT.DROP_DOWN | SWT.READ_ONLY);
		int[] cycles = PgenCycleTool.getCyclesArray();
		for (int i = 0; i < cycles.length; i++) {
			cycleCbo.add(cycles[i] < 10 ? "0" + cycles[i] : "" + cycles[i]);
		}
		int h = PgenCycleTool.getCycleHour();
		index = cycleCbo.indexOf(h < 10 ? "0" + h : "" + h);
		cycleCbo.select(index);

		Group group = new Group(top, SWT.NONE);
		GridData gridData = new GridData(GridData.BEGINNING);
		gridData.horizontalAlignment = GridData.FILL;
		group.setLayoutData(gridData);
		GridLayout layout = new GridLayout(1, false);
		layout.marginHeight = 3;
		layout.marginWidth = 3;
		group.setLayout(layout);
		routineBtn = new Button(group, SWT.RADIO);
		routineBtn.setSelection(PgenCycleTool.isRoutine());
		routineBtn.setText("Routine");
		updateBtn = new Button(group, SWT.RADIO);
		updateBtn.setSelection(!PgenCycleTool.isRoutine());
		updateBtn.setText("Update");
        addSeparator(top.getParent());

	}

	@Override
	public void okPressed() {

		String cycle = cycleCbo.getText();
		if (cycle.startsWith("0"))
			cycle = cycle.substring(1, cycle.length());
		PgenCycleTool.setCycleHour(Integer.parseInt(cycle));

		String day = dayCbo.getText();
		if (day.startsWith("0"))
			day = day.substring(1, day.length());
		PgenCycleTool.setCycleDay(Integer.parseInt(day));
		
		PgenCycleTool.setCycleRoutine(routineBtn.getSelection());

		/*
		 *  Update the cycle info for all GFA elements in ALL activities/layers?
		 *  
		 *  Legacy - only update all GFA elements on the active layer.
		 */
		if ( drawingLayer != null ) {
//			ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>();
//			ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>();
//			for(Product p: drawingLayer.getProducts()){
//				for(Layer l: p.getLayers()){
					
			        for(AbstractDrawableComponent adc: drawingLayer.getActiveLayer().getDrawables()){
						if ( adc instanceof Gfa){
							//Remove the IDisplayable in the container to activate the redraw.
							drawingLayer.resetElement( (Gfa)adc );  
							
							//Update Day/Cycle for redraw.
							((Gfa)adc).setGfaCycleDay(PgenCycleTool.getCycleDay());
							((Gfa)adc).setGfaCycleHour(PgenCycleTool.getCycleHour());
//							oldList.add(adc);
//							Gfa copy = (Gfa)adc.copy();
//							copy.setGfaCycleDay(PgenCycleTool.getCycleDay());
//							copy.setGfaCycleHour(PgenCycleTool.getCycleHour());
//							newList.add(copy);
						}
					}
//				}
//			}

//			drawingLayer.replaceElements(oldList, newList);
//			PgenUtil.refresh();
			
		}

		if ( mapEditor != null ) {			
			mapEditor.refresh();
		}
		
		close();
	}

	/**
	 * Set the location of the dialog
	 */
	public int open() {

		if (this.getShell() == null) {
			this.create();
		}
		if(shellLocation == null){
			shellLocation = centerOfParent();
		}

		return super.open();
	}

	public Point centerOfParent() {
		Rectangle parentSize = getParentShell().getBounds();
		Rectangle mySize = getShell().getBounds();

		int locationX, locationY;
		locationX = (parentSize.width - mySize.width) / 2 + parentSize.x;
		locationY = (parentSize.height - mySize.height) / 2 + parentSize.y;

		return new Point(locationX, locationY);
	}

	/**
	 * Gets values of all attributes of the dialog.
	 */
	public HashMap<String, Object> getAttrFromDlg() {

		HashMap<String, Object> attr = new HashMap<String, Object>();

		return attr;
	}

	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg(IAttribute attr) {
	}

}
