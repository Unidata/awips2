/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenWatchStatusLineDrawingTool
 * 
 * 3 March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.LineAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.WatchBoxAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;

import java.awt.Color;
import java.util.ArrayList;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;


/**
 * Implements a modal map tool to draw watch status line.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10					B. Yin   	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 03/12        #697        Q. Zhou     Fixed line arrow head size for watch
 * 12/13		TTR 800		B. Yin		Add a flag when opening the specification dialog.
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenWatchStatusLineDrawingTool extends AbstractPgenDrawingTool{

	//the watch element working on
	private WatchBox wb;

	/**
	 * public constructor
	 */
	public PgenWatchStatusLineDrawingTool(){

		super();

	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
	 */
	@Override
	protected void activateTool( ) {
		super.activateTool();

		((LineAttrDlg)attrDlg).setSmoothLvl(0);
		((LineAttrDlg)attrDlg).setColor(new Color[]{Color.RED, Color.RED });

		if ( event.getTrigger() instanceof WatchBox ) wb = (WatchBox)event.getTrigger(); 

		return;
	}


	@Override
	/**
	 * Return the current mouse handler
	 */
	public IInputHandler getMouseHandler() {

		if ( this.mouseHandler == null ) {

			this.mouseHandler = new PgenWatchStatusLineDrawingHandler();

		}

		return this.mouseHandler;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
	 */
	@Override
	public void deactivateTool() {

		super.deactivateTool();


		PgenWatchStatusLineDrawingHandler wsh = (PgenWatchStatusLineDrawingHandler) mouseHandler;
		if (wsh != null) wsh.clearPoints();

	}

	/**
	 * Implements input handler for mouse events.
	 * @author bingfan
	 *
	 */
	private class PgenWatchStatusLineDrawingHandler extends InputHandlerDefaultImpl {

		/**
		 * Points of the new element.
		 */
		protected ArrayList<Coordinate> points = new ArrayList<Coordinate>();

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
		 *      int, int)
		 */
		@Override	
		public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;

			//  Check if mouse is in geographic extent
			Coordinate loc = mapEditor.translateClick(anX, aY);
			if ( loc == null || shiftDown ) return false;

			if ( button == 1 ) {

				//add a new point
				points.add( loc );                

				return true;

			}
			else if ( button == 3 ) {

				if ( points.size() == 0 ) {

					//close the line attr dialog 
					if ( attrDlg != null ) attrDlg.close(); 
					attrDlg = null;
					
					//return to watch modifying tool
					PgenUtil.loadWatchBoxModifyTool(wb);
					
					//set the watch element as selected
					drawingLayer.setSelected(wb);
					
					//open and initialize watch box attr dialog
					WatchBoxAttrDlg wbdlg = WatchBoxAttrDlg.getInstance(null);
					wbdlg.openSpecDlg( false );
					wbdlg.setDrawableElement(wb);
					wbdlg.setMouseHandlerName("Pgen Select");
					wbdlg.setAttrForDlg( (IAttribute)wb );
					wbdlg.enableButtons();
					wbdlg.setPgenCategory(wb.getPgenCategory());
					wbdlg.setPgenType( wb.getPgenType() );
					wbdlg.setDrawingLayer( drawingLayer );
					wbdlg.setMapEditor( mapEditor );


				}
				else if ( points.size() < 2 ){

					drawingLayer.removeGhostLine();
					points.clear();

					mapEditor.refresh();

				}
				else {


					// create a status line    
					Line statusLine =  new Line(null, attrDlg.getColors(), attrDlg.getLineWidth(),
							1.0, false, false, points, ((ILine)attrDlg).getSmoothFactor(),FillPattern.SOLID,
							"Lines","POINTED_ARROW");

					// add the line to watch DECollection
					((DECollection)wb.getParent()).add(statusLine);
					
					drawingLayer.removeGhostLine();
					points.clear();
					mapEditor.refresh();

				}

				return true;

			}
			else if ( button == 2 ){

				return true;

			}
			else{

				return false;

			}

		}
		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
		 *      int)
		 */
		@Override
		public boolean handleMouseMove(int x, int y) {
        	if ( !isResourceEditable() ) return false;

			//  Check if mouse is in geographic extent
			Coordinate loc = mapEditor.translateClick(x, y);
			if ( loc == null ) return false;

			// create the ghost element and put it in the drawing layer
			Line ghostLine =  new Line(null, attrDlg.getColors(),attrDlg.getLineWidth(),1.0,false,
					false, points, ((ILine)attrDlg).getSmoothFactor(),FillPattern.SOLID,"Lines","POINTED_ARROW");

			if ( points != null && points.size() >= 1) {

				ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
				ghostPts.add(loc);

				ghostLine.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );

				drawingLayer.setGhostLine(ghostLine);
				mapEditor.refresh();

			}

			return false;

		}    

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if ( !drawingLayer.isEditable() || shiftDown ) return false;
			else return true;
		}
		
		public void clearPoints(){
			points.clear();
		}

	}

}
