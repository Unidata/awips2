package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import com.raytheon.uf.viz.core.drawables.IWireframeShape;

public class NsharpShapeAndLineProperty {
	private IWireframeShape shape;
	private NsharpLineProperty lp;
	public NsharpShapeAndLineProperty() {
		super();
		lp = new NsharpLineProperty();
	}
	public IWireframeShape getShape() {
		return shape;
	}
	public void setShape(IWireframeShape shape) {
		this.shape = shape;
	}
	public NsharpLineProperty getLp() {
		return lp;
	}
	public void setLp(NsharpLineProperty lp) {
		this.lp = lp;
	}
	
}
