/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.io.DataInput;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.TextAlignment;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

/**
 * @author bhebbard
 *
 */
public class NcTextAlignment extends TextAlignment implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
	public NcTextAlignment(int ec, int eid, int l, DataInput in)
			throws IOException {
		super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties, com.raytheon.uf.viz.core.drawables.IDescriptor, gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
	 */
	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps,
			IDescriptor descriptor, ImageBuilder ib) throws VizException {
		
		//  Map/convert CGM-style text alignments to their IGraphicsTarget equivalents.
		
		switch (this.horizontalAlignment) {
		case LEFT:
			ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
			break;
		case NORMAL_HORIZONTAL:
		case CONTINOUS_HORIZONTAL:  //TODO??
		case CENTRE:
			ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
			break;
		case RIGHT:
			ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.RIGHT;
			break;
		default:
			//TODO  fail
			ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
			break;
		}
		
		switch (this.verticalAlignment) {
		case TOP:
		case CAP:  //TODO??
			ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.TOP;
		case HALF:
			ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.MIDDLE;
		case NORMAL_VERTICAL:
		case CONTINOUS_VERTICAL:  //TODO??
		case BASE:  //TODO??
		case BOTTOM:
			ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
		default:
			//TODO  fail
			ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
			break;
		}
		
	}

}
