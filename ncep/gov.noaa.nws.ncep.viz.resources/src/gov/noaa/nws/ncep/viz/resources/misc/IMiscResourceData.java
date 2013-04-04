/**
 * 
 */
package gov.noaa.nws.ncep.viz.resources.misc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
//import gov.noaa.nws.ncep.resources.attributes.ResourceAttrSet.RscAttr;

import java.util.ArrayList;

//import org.eclipse.swt.graphics.RGB;

//import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  05/06/09     #115      Greg Hull    Initial Creation.
 *  06/12/09     #115      Greg Hull    Integrate to work with INatlCntrsResource
 *                                      and ResourceAttrSet
 *  04/23/10     #245      Greg Hull    Added Spinner for Airmet attributes
 *  12/14/12     #861      Greg Hull    Added COLOR_PALLETTE for Pgen Rsc
 * 
 * </pre>
 *
 * @author ghull
 * @version 1
 */
public interface IMiscResourceData extends INatlCntrsResourceData {
	
	public static enum EditElement {
		CHECK_BOX,
		COLOR_SELECTOR,   // a button that pops up a color palette
		COLOR_PALLETE,     
		SPINNER,
		LABEL,
	//	SLIDER_TEXT,
		SEPARATOR,
		VERTICAL_SEPARATOR
	}
	
	public class MiscResourceAttr {
		public String rscAttrName = null; // must match an attrName in rscAttrSet
		public String attrLabel = null;
		public EditElement guiElem = null;
		public int    spinnerNumDigits = 0;
		public int    spinnerMax       = 10;
		public int    spinnerMin       = 1;
		public int    spinnerIncr      = 1;
		public int    spinnerPageIncr  = 2;
		public int    dispColumn;

		// TODO : we could add some configuration options for the COLOR_PALLETTE
		// if needed for other resources besides just the PGEN resource.
		//
		

		public MiscResourceAttr( String attrName, String l, EditElement e, int col ) {
			rscAttrName = attrName; // field name in the resource/rscAttrSet
			attrLabel = l;          // for the gui
			guiElem = e;
			dispColumn = col;
		}

		public String getAttrName() {
			return rscAttrName;
		}
		public void setSpinnerControls( int nDigits, int min, int max, int incr, int page ) {
			spinnerNumDigits = nDigits;
			spinnerMax       = max;
			spinnerMin       = min;
			spinnerIncr      = incr;
			spinnerPageIncr  = page;
		}
	}
	
	public class MiscRscAttrs {
		private int numDispColumns;
		private ArrayList<MiscResourceAttr> attrsList;
		
		public MiscRscAttrs( int cols ) {
			attrsList = new ArrayList<MiscResourceAttr>();
			numDispColumns = cols;
		}

		public int getNumDisplayColumns() {
			return numDispColumns;
		}
		
	    public ArrayList<MiscResourceAttr> getMiscResourceAttrs() {
	    	return attrsList;
	    }
	    
	    public void addAttr( MiscResourceAttr attr ) {
	    	attrsList.add( attr );
	    }
	}
    
    public abstract MiscRscAttrs getMiscResourceAttrs();
}
