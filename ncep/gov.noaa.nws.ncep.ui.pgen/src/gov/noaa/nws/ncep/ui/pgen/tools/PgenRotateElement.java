/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenRotateElement
 * 
 * 6 January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import static java.lang.Math.*;

import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TrackExtrapPointInfoDlg;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN object rotat functions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/2010                  Mikhail L.  Initial Creation.
 * 06/2010		#280		Moved two methods to PgenToolUtils.
 * 
 * </pre>
 * 
 * @author Mikhail Laryukhin
 */

public class PgenRotateElement extends AbstractPgenDrawingTool {

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
	 */
	@Override
	protected void activateTool() {

		attrDlg = null;
		if (buttonName == null){
			buttonName = new String("Select");
		}

		super.activateTool();

	}

	/**
	 * Returns the current mouse handler.
	 * 
	 * @return
	 */
	public IInputHandler getMouseHandler() {
		if (mouseHandler == null) {
			mouseHandler = new PgenSelectRotateHandler();
		}
		return mouseHandler;
	}

	/**
	 * Implements input handler for mouse events.
	 * 
	 * @author bingfan, Mikhail L.
	 * 
	 */
	public class PgenSelectRotateHandler extends InputHandlerDefaultImpl {

		OperationFilter rotateFilter = new OperationFilter( Operation.ROTATE );
		
		/** Attribute dialog for displaying track points info */
		TrackExtrapPointInfoDlg trackExtrapPointInfoDlg = null;

		/** Flag if any point of the element is selected. */
		protected boolean ptSelected = false;

		/** The original direction is needed for undo. */
		private Double oldDir = null; // using Double instead of double because we need to use null value

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
		 * int, int)
		 */
		@Override
		public boolean handleMouseDown(int x, int y, int button) {
        	if ( !isResourceEditable() ) return false;
			
			// Check if mouse is in geographic extent
			Coordinate loc = mapEditor.translateClick(x, y);
			if (loc == null || shiftDown ) {
				return false;
			}

			if (button == 1) {

				// Return if an element or a point has been selected
				if (ptSelected || drawingLayer.getSelectedDE() != null) {
					return true;
				}

				// Get the nearest element and set it as the selected element.
				DrawableElement elSelected = drawingLayer.getNearestElement(loc, rotateFilter);

				/*
				if (elSelected instanceof TCAElement) {
					PgenUtil.loadTCATool(elSelected);
				} else if (elSelected instanceof WatchBox) {
					PgenUtil.loadWatchBoxModifyTool(elSelected);
				}
				*/

				if (elSelected != null) drawingLayer.setSelected(elSelected);

				mapEditor.refresh();
				return false;

			} else if (button == 3) {

				if (trackExtrapPointInfoDlg != null){
					trackExtrapPointInfoDlg.close();
					trackExtrapPointInfoDlg = null;
				}

				drawingLayer.removeGhostLine();
				ptSelected = false;
				drawingLayer.removeSelected();
				mapEditor.refresh();

				return false;
			} 

			return false;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
		 * int, int)
		 */
		@Override
		public boolean handleMouseDownMove(int x, int y, int button) {
        	if ( !isResourceEditable() ) return false;

			// Check if mouse is in geographic extent
			Coordinate loc = mapEditor.translateClick(x, y);
			if (loc == null || shiftDown  ) return false;

			DrawableElement el = drawingLayer.getSelectedDE();

			// The requirements are to rotate Vector and Text elements only
			// Do not modify other elements
			if (el != null) {
				Coordinate origin = ((ISinglePoint)el).getLocation();

				if (el instanceof Vector) {
					if (oldDir == null) {
						oldDir = ((Vector) el).getDirection(); // autoboxing to Double
					}
					double[] swtCoordinates = mapEditor.translateInverseClick(origin);
					Double newDir = PgenToolUtils.calculateAngle(oldDir, swtCoordinates[0], swtCoordinates[1], x, y);
					if("Hash".equals(((Vector) el).getPgenType())){
						newDir = PgenToolUtils.transformToRange0To360(180 - newDir);
						newDir -= southOffsetAngle(origin);
					} else {
						// offset for the location point
						newDir += southOffsetAngle(origin);
					}
					((Vector) el).setDirection(newDir);

				} else if (el instanceof Text) {
					if (oldDir == null) {
						oldDir = ((Text) el).getRotation(); // autoboxing to Double
					}
					double[] swtCoordinates = mapEditor.translateInverseClick(origin);
					Double newRotation = 180 - PgenToolUtils.calculateAngle(oldDir, swtCoordinates[0], swtCoordinates[1], x, y);
					newRotation = PgenToolUtils.transformToRange0To360(newRotation);
					if(((Text) el).getRotationRelativity() == TextRotation.NORTH_RELATIVE){
						// offset for the location point
						newRotation -= southOffsetAngle(origin);
					}
					((Text) el).setRotation(newRotation);
				}
				
				drawingLayer.resetElement(el); // reset display of this element
				mapEditor.refresh();
			}
			
			return true;
		}

		/**
		 * Calculates the angle difference of "south" relative to the screen's y-axis 
		 * at a given lat/lon location.
		 * @param loc - The point location in Lat/Lon coordinates
		 * @return The angle difference of "north" versus pixel coordinate's y-axis
		 */
		private double southOffsetAngle( Coordinate loc ) {
			
			double delta = 0.05;
			/*
			 * copy/paste from DisplayElementFactory
			 * 
			 * Calculate points in pixel coordinates just south and north of
			 * original location.  
			 */
	        double[] south = { loc.x, loc.y - delta, 0.0 };
	        double[] pt1 = drawingLayer.getDescriptor().worldToPixel(south);

	        double[] north = { loc.x, loc.y + delta, 0.0 };
	        double[] pt2 = drawingLayer.getDescriptor().worldToPixel(north);
	        
			return -90 - toDegrees(atan2( (pt2[1]-pt1[1]), (pt2[0]-pt1[0]) ));
		}
		

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
		 * int)
		 */
		@Override
		public boolean handleMouseUp(int x, int y, int button) {
        	if ( !isResourceEditable() ) return false;

			// Finish the editing
			if (button == 1 && drawingLayer != null) {

				// Create a copy of the currently selected element
				DrawableElement el = drawingLayer.getSelectedDE();

				if (el != null && oldDir != null) {

					DrawableElement newEl = (DrawableElement) el.copy();

					drawingLayer.resetElement(el);
					
					if (el instanceof Vector) {
						((Vector) el).setDirection(oldDir);
						oldDir = null;
					} else if (el instanceof Text) {
						((Text) el).setRotation(oldDir);
						oldDir = null;
					}

					drawingLayer.replaceElement(el, newEl);
					drawingLayer.setSelected(newEl);

					mapEditor.refresh();
				}
			}
			
			return false;
		}

		/**
		 * Gets the nearest point of an selected element to the input point
		 * 
		 * @param el
		 *            element
		 * @param pt
		 *            input point
		 * @return
		 */
		protected int getNearestPtIndex(MultiPointElement el, Coordinate pt) {

			int ptId = 0;
			double minDistance = -1;
			GeodeticCalculator gc;
			gc = new GeodeticCalculator(drawingLayer.getCoordinateReferenceSystem());
			gc.setStartingGeographicPoint(pt.x, pt.y);
			int index = 0;
			for (Coordinate elPoint : el.getPoints()) {
				gc.setDestinationGeographicPoint(elPoint.x, elPoint.y);
				double dist = gc.getOrthodromicDistance();
				if (minDistance < 0 || dist < minDistance) {
					minDistance = dist;
					ptId = index;
				}
				index++;
			}
			return ptId;
		}
	}
}
