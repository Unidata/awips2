/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.io.*;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.InteriorStyle;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

/**
 * @author bhebbard
 *
 */
public class NcInteriorStyle extends InteriorStyle implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
    public NcInteriorStyle(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
    }

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties, gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
	 */
	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps,
			IDescriptor descriptor, ImageBuilder ib)
			throws VizException {
		switch (this.style) {
		case SOLID:  //TODO:  For now, SOLID is assumed for all filled polygons
			break;
		case HOLLOW:
		case PATTERN:
		case HATCH:
		case EMPTY:
		case GEOMETRIC_PATTERN:
		case INTERPOLATED:
			logger.warn("Paint not implemented for CGM command:  " + this);
			break;
		}
	}

}
