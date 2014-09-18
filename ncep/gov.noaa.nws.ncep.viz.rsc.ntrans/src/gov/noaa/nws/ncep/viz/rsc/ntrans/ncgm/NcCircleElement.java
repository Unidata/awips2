/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.CircleElement;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.ImageBuilder;

import java.io.DataInput;
import java.io.IOException;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 * 
 */
public class NcCircleElement extends CircleElement implements INcCommand {

    // private final Log logger = LogFactory.getLog(this.getClass());

    public NcCircleElement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
    }

    @Override
    public void contributeToPaintableImage(ImageBuilder ib, IGraphicsTarget target,
            PaintProperties paintProps, IDescriptor descriptor) throws VizException {

        // Used only to draw (teeny) circles to mark lat/lon lines?
        // If not, will need to revisit assumptions below... TODO

        DrawableCircle dc = new DrawableCircle();
        double[] newpoint = ib.scalePoint(this.center.x, this.center.y);
        dc.setCoordinates(newpoint[0], newpoint[1]);
        dc.radius = this.radius;
        // dc.screenRadius = this.radius;
        dc.filled = true; // TODO -- SJ says this is what was intended; not
                          // encoded in CGM?
        dc.numberOfPoints = 6; // 8? 16? lower values improve performance (a
                               // lot)
        dc.basics.color = ib.currentLineColor; // TODO use currentCircleColor?
                                               // or...?
        ib.circles.add(dc);
        // target.drawCircle(dc);
        // logger.debug("Circle has been drawn -- " + this);
    }

}
