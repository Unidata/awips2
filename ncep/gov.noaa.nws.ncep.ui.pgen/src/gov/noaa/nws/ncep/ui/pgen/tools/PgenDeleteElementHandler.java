/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenDeleteElementHandler
 * 
 * 1 April 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.filter.AcceptFilter;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements input handler for mouse events for the deleting action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/13		927			B. Yin   	Moved from the PgenDeleteElement class
 * 
 * </pre>
 * 
 * @author bingfan
 */

public class PgenDeleteElementHandler extends InputHandlerDefaultImpl {

	protected AbstractEditor mapEditor;
	protected PgenResource pgenrsc;
	protected AbstractPgenTool tool;
	protected AttrDlg attrDlg;

	private boolean preempt;

	/**
	 * Constructor
	 * @param tool
	 */
	public PgenDeleteElementHandler( AbstractPgenTool tool ) {
		this.tool = tool;
		pgenrsc = tool.getDrawingLayer();
		mapEditor = tool.mapEditor;
		
		if ( tool instanceof AbstractPgenDrawingTool ) {
			attrDlg = ((AbstractPgenDrawingTool)tool).getAttrDlg();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
	 *      int, int)
	 */
	@Override	   	
	public boolean handleMouseDown(int anX, int aY, int button) {
		if ( !tool.isResourceEditable() ) return false;

		preempt = false;

		//  Check if mouse is in geographic extent
		Coordinate loc = mapEditor.translateClick(anX, aY);
		if ( loc == null ) return false;

		if ( button == 1 ) {

			if ( pgenrsc.getSelectedComp() != null ) {
				doDelete();
				preempt = false;
			}
			else {        	
				// Get the nearest element and set it as the selected element.
				AbstractDrawableComponent elSelected = pgenrsc.getNearestComponent( loc, new AcceptFilter(), true );

				//Delete watch status line
				if ( elSelected instanceof DECollection && elSelected.getName().equalsIgnoreCase("Watch")
						&& pgenrsc.getNearestElement(loc).getPgenType().equalsIgnoreCase("POINTED_ARROW")){
					elSelected =pgenrsc.getNearestElement(loc);
				}	
				else if ( elSelected instanceof Outlook && ((Outlook)elSelected).getDEs() > 1){
					AbstractDrawableComponent adc = pgenrsc.getNearestElement(loc);
					elSelected = adc.getParent(); 
				}

				if (elSelected != null) {
					pgenrsc.setSelected( elSelected );
					preempt = true;
				}
				mapEditor.refresh();  
			}

			return preempt;

		}
		else if ( button == 2 ){

			return true;

		}
		else if ( button == 3 ) {

			if (  pgenrsc.getSelectedComp() != null ){
				// de-select element
				pgenrsc.removeSelected();
				mapEditor.refresh();
			}
			else {
				// set selecting mode
				PgenUtil.setSelectingMode();
			}

			return true;

		}
		else{

			return true;

		}

	}

	@Override
	public boolean handleMouseDownMove(int x, int y, int mouseButton) {
		if (  !tool.isResourceEditable() || shiftDown ) return false;
		else return true;
	}
	
	/**
	 * Deletes the selected element and reset the handler. 
	 * For a single element, closes the attributes dialog when the element is deleted.   
	 */
	@Override
	public void preprocess(){
		
		if ( pgenrsc.getSelectedComp() != null ) {
			if ( attrDlg != null && 
					( pgenrsc.getSelectedComp().getParent() instanceof Layer 
							|| pgenrsc.getSelectedComp().getParent().getName().equalsIgnoreCase("labeledSymbol"))){
				attrDlg.close();
			}

			doDelete();
			tool.resetMouseHandler();
		}
	}
	
	/**
	 * Deletes the selected element or component from the PGEN resource.
	 */
	private void doDelete(){
		
		AbstractDrawableComponent adc = pgenrsc.getSelectedComp();
		
		if ( adc.getParent() instanceof ContourMinmax 
				|| adc.getParent().getName().equalsIgnoreCase("labeledSymbol") ){
			pgenrsc.removeElement(adc.getParent());
		}
		else {
			pgenrsc.removeElement(adc);
		}
		// de-select element
		pgenrsc.removeSelected();
		mapEditor.refresh();
	}

	public AbstractEditor getMapEditor() {
		return mapEditor;
	}

	public PgenResource getPgenrsc() {
		return pgenrsc;
	}


}
