/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.InteriorStyle;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.ImageBuilder;

import java.io.DataInput;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 * 
 */
public class NcInteriorStyle extends InteriorStyle implements INcCommand {

    private final Log logger = LogFactory.getLog(this.getClass());

    public NcInteriorStyle(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
    }

    @Override
    public void contributeToPaintableImage(ImageBuilder ib, IGraphicsTarget target,
            PaintProperties paintProps, IDescriptor descriptor) throws VizException {
        switch (this.style) {
        case SOLID: // TODO: For now, SOLID is assumed for all filled polygons
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
