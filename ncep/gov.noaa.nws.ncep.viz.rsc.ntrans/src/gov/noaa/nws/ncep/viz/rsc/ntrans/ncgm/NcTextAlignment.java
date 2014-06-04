/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.TextAlignment;
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
public class NcTextAlignment extends TextAlignment implements INcCommand {

    private final Log logger = LogFactory.getLog(this.getClass()); // TODO
                                                                   // static
                                                                   // better??

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor,
     * gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps,
            IDescriptor descriptor, ImageBuilder ib) throws VizException {

        // Map/convert CGM-style text alignments to their IGraphicsTarget
        // equivalents.

        switch (this.horizontalAlignment) {
        case NORMAL_HORIZONTAL:
            // TODO: Following is sort of a hack, to deal with the way legacy
            // NTRANS metafiles are created by the NC driver code. A horizontal
            // alignment of CENTER appears to be coded (intentionally or
            // otherwise)
            // in the legacy generated CGM by a *vertical* alignment value of
            // CAP.
            // Might want to investigate, and possibly bring legacy code to CGM
            // compliance.
            if (this.verticalAlignment == TextAlignment.VerticalAlignment.CAP) {
                ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
            } else {
                ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
            }
            break;
        case LEFT:
            ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
            break;
        case CONTINOUS_HORIZONTAL: // TODO??
        case CENTRE:
            ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
            break;
        case RIGHT:
            ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.RIGHT;
            break;
        default:
            // TODO fail
            ib.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
            break;
        }

        switch (this.verticalAlignment) {
        case TOP:
        case CAP: // TODO??
            ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.TOP;
            ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
            break;
        case HALF:
            ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.MIDDLE;
            break;
        case NORMAL_VERTICAL:
        case CONTINOUS_VERTICAL: // TODO??
        case BASE: // TODO??
        case BOTTOM:
            ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
            break;
        default:
            // TODO fail
            ib.verticalAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
            break;
        }

    }

}
