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

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.TextFontIndex;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

/**
 * @author bhebbard
 *
 */
public class NcTextFontIndex extends TextFontIndex implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??
	private static boolean notWarned = true;

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
	public NcTextFontIndex(int ec, int eid, int l, DataInput in)
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
		if (notWarned) {
			logger.warn("Paint not implemented for CGM command:  " + this);
			notWarned = false;
		}
	}

}
