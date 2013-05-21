/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenExtrapTool
 * 
 * June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.FromAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN gfa formatting.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer		Description
 * ------------	----------	-------------	--------------------------
 * 03/10		#263		M.Laryukhin		Created
 * 03/11					J. Wu			Implemented to perform
 * 											"Format Tag"
 * </pre>
 * 
 * @author M.Laryukhin
 */
public class PgenFromTool extends AbstractPgenDrawingTool {
	
	private FromAttrDlg fromAttrDlg = null;
	
	/**
	 * Input handler for mouse events.
	 */
	public PgenFromTool() {
		super();
	}

	/**
	 * Returns the current mouse handler.
	 * 
	 * @return
	 */
	public IInputHandler getMouseHandler() {

		if (this.mouseHandler == null) {
			this.mouseHandler = new PgenFromHandler();
		}

		return this.mouseHandler;

	}

    /**
     * Implements input handler for mouse events.
     * @author jwu
     *
     */       
    public class PgenFromHandler extends InputHandlerDefaultImpl {
    	    	
    	private boolean preempt;
    	OperationFilter fromFilter = new OperationFilter( Operation.GFA_FROM );
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown( int anX, int aY, int button ) { 
        	if ( !isResourceEditable() ) return false;

        	preempt = false;
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	       	        	
			fromAttrDlg = (FromAttrDlg)attrDlg;
			
			/*
			 * Left mouse button pressed
			 */
        	if ( button == 1 ) {
               if ( !fromAttrDlg.isFormatByTag() ) {
            	   return false;
               }
            	/*
            	 * If the nearest element is valid, set it as a "selected" element
            	 */
            	DrawableElement el1 = drawingLayer.getNearestElement( loc, fromFilter );
            	drawingLayer.setSelected( el1 );
            	if (el1 != null) preempt = true;
            	
            	fromAttrDlg.formatTagPressed();
         	    
                mapEditor.refresh();
                return preempt;	
       		                
            }
        	
        	/*
        	 * Right mouse button pressed
             */          	
            else if ( button == 3 ) {
	            	            
             	/*
            	 * return to Pgen Select mode
            	 */
        		drawingLayer.removeSelected();
            	PgenUtil.setSelectingMode();         

            	mapEditor.refresh();		    
            	return true;          
            
            } 
            else {            	
               	return false;               	          
            }
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        	if ( !isResourceEditable() ) return false;
			return preempt;
		}
    }
    
}
