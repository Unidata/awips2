/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.io.*;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.FillColour;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

/**
 * @author bhebbard
 *
 */
public class NcFillColour extends FillColour implements INcCommand {
	public NcFillColour(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
    }

	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps, IDescriptor descriptor, ImageBuilder ib)
			throws VizException {
		ib.currentFillColor = GempakColor.convertToRGB(this.colorIndex);
		
	}
}

