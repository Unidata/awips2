/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenDeletePointHandler
 * 
 * 1 April 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements input handler for mouse events for the deleting point action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/13		#927		B. Yin   	Moved from the PgenDeletePointclass
 * 
 * </pre>
 * 
 * @author bingfan
 */

public class PgenDeletePointHandler extends PgenSelectHandler{

	private OperationFilter delPointFilter = new OperationFilter( Operation.DELETE_POINT );

	/**
	 * Constructor
	 * @param tool
	 */
	public PgenDeletePointHandler( AbstractPgenTool tool ) {
		super( tool, tool.mapEditor, tool.getDrawingLayer(), null);
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

		//  Check if mouse is in geographic extent
		Coordinate loc = mapEditor.translateClick(anX, aY);
		if ( loc == null || shiftDown ) return false;

		if ( button == 1 ) {

			if ( pgenrsc.getSelectedDE() == null ){ 

				// Get the nearest element and set it as the selected element.
				DrawableElement elSelected = pgenrsc.getNearestElement( loc, delPointFilter );
				if ( elSelected instanceof MultiPointElement &&
						!(elSelected instanceof WatchBox )) {
					pgenrsc.setSelected( elSelected );   
				}
				else { 
					return false;
				}
			}
			else if ( !ptSelected ) {

				//select the nearest point
				ptIndex = getNearestPtIndex((MultiPointElement)pgenrsc.getSelectedDE(), loc); 
				pgenrsc.addPtSelected( ptIndex );
				ptSelected = true;

			}
			else {

				//remove the selected point
				if ( pgenrsc.getSelectedDE() instanceof MultiPointElement ){
					DrawableElement newEl = (DrawableElement)pgenrsc.getSelectedDE().copy();
					if (((IMultiPoint)newEl).getLinePoints().length <= 2 )return true;
					newEl.getPoints().remove( ptIndex );

					if ( newEl instanceof Gfa ) {
						((Gfa)newEl).setGfaVorText( Gfa.buildVorText( (Gfa)newEl));
						GfaReducePoint.WarningForOverThreeLines( (Gfa)newEl );
					}

					if ( newEl instanceof Jet.JetLine ){

						Jet jet = (Jet)pgenrsc.getActiveLayer().search(pgenrsc.getSelectedDE());
						Jet newJet = jet.copy();
						pgenrsc.replaceElement(jet, newJet);
						newJet.getPrimaryDE().setPoints( ((MultiPointElement)newEl).getPoints());
						pgenrsc.setSelected(newJet.getPrimaryDE());

					}
					else {
						pgenrsc.replaceElement(pgenrsc.getSelectedDE(), newEl);
						//setPoints will do snap
						//((MultiPointElement)newEl).setPoints(((MultiPointElement)newEl).getPoints());
						pgenrsc.setSelected( newEl );
					}

					pgenrsc.removePtsSelected();
					ptSelected = false;

					if ( !(tool instanceof PgenDeletePoint) ){
						tool.resetMouseHandler();
					}
				}

			}

			mapEditor.refresh();  
			return true;

		}

		else{

			return false;

		}

	}

	/*
	 * overrides the function in selecting tool
	 */
	@Override
	public boolean handleMouseDownMove(int anX, int aY, int button){
		if (  !tool.isResourceEditable() || shiftDown ) return false;
		else return true;
	}  

	/*
	 * overrides the function in selecting tool
	 */
	@Override
	public boolean handleMouseUp(int x, int y, int button){
		if ( button == 3 ) {

			if ( pgenrsc.getSelectedDE() != null && tool instanceof PgenDeletePoint) {
				ptSelected = false;
				pgenrsc.removeSelected();
				mapEditor.refresh();
			}
			else {

				if ( tool instanceof PgenDeletePoint ){
					PgenUtil.setSelectingMode();  
				}
				else {
					pgenrsc.removePtsSelected();
					mapEditor.refresh();
					tool.resetMouseHandler();
				}
			}

			return true;

		}
		else {
			return false;
		}
	}

	/**
	 * Removes the selected element.
	 */
	public void cleanup(){
		ptSelected = false;
		pgenrsc.removeSelected();
	}

	/**
	 * Sets the nearest point as the point that is going to be deleted.
	 */
	@Override
	public void preprocess(){

		Coordinate lastClick = mapEditor.translateClick( mapEditor.getActiveDisplayPane().getLastMouseX(),
				mapEditor.getActiveDisplayPane().getLastMouseY());

		ptIndex = getNearestPtIndex((MultiPointElement)pgenrsc.getSelectedDE(), lastClick); 
		pgenrsc.addPtSelected( ptIndex );
		ptSelected = true;

		mapEditor.refresh();
	}

}
