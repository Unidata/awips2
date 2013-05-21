/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.CircleElement;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

import java.io.DataInput;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 *
 */
public class NcCircleElement extends CircleElement implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
	public NcCircleElement(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties, com.raytheon.uf.viz.core.drawables.IDescriptor, gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
	 */
	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps,
			IDescriptor descriptor, ImageBuilder ib) throws VizException {
		
		//  Used only to draw (teeny) circles to mark lat/lon lines?
		//  If not, will need to revisit assumptions below... TODO
		
		DrawableCircle dc = new DrawableCircle();
		double[] newpoint = ib.scalePoint(this.center.x, this.center.y);
		dc.setCoordinates(newpoint[0],newpoint[1]);
		dc.radius = this.radius;
		//dc.screenRadius = this.radius;
        dc.filled = true;  //TODO -- SJ says this is what was intended; not encoded in CGM?
        dc.numberOfPoints = 6;  // 8? 16?  lower values improve performance (a lot)
        dc.basics.color = ib.currentLineColor;  //TODO  use currentCircleColor?  or...?
        ib.circles.add(dc);
		//target.drawCircle(dc);
		//logger.debug("Circle has been drawn -- " + this);
	}

}
