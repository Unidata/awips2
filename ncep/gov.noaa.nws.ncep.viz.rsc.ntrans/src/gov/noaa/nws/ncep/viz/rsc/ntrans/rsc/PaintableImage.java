package gov.noaa.nws.ncep.viz.rsc.ntrans.rsc;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Command;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.NcCGM;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.ImageBuilder.WireframeKey;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

public class PaintableImage {
    // This is just a container holding the ready-to-paint information for
    // a single NTRANS image.

    // Once generated (at first paint), cache here for future paints, instead of
    // regenerating each time

    // shaded shapes (filled polygons) especially necessary to avoid memory
    // (heap runaway) issues;
    public IShadedShape shadedShape;

    // Map of all now-compiled wireframes, keyed by unique output draw states
    public Map<WireframeKey, IWireframeShape> wireframes = new HashMap<WireframeKey, IWireframeShape>();

    // Sequence in which to paint the wireframes
    public List<WireframeKey> wireframePaintOrder = new ArrayList<WireframeKey>();

    // AWIPS II DrawableString text objects
    public List<DrawableString> strings = new ArrayList<DrawableString>();

    // AWIPS II DrawableCircle objects
    public List<DrawableCircle> circles = new ArrayList<DrawableCircle>();

    private IFont font = null; // TODO: Move this?

    private IGraphicsTarget target = null;

    private final Log logger = LogFactory.getLog(this.getClass());

    public PaintableImage(NcCGM cgmImage, IGraphicsTarget target,
            PaintProperties paintProps, IDescriptor descriptor, double scale)
            throws VizException {

        // Construct a PaintableImage (containing ready-to-paint,
        // compiled-where-necessary) AWIPS graphics elements, given
        // the (Java) CGM image.

        this.target = target;

        ImageBuilder ib = new ImageBuilder();

        if (this.font == null) { // TODO clean up font handling
            this.font = target.initializeFont("Monospace", 12,
                    new IFont.Style[] { Style.BOLD });
        }

        ib.currentFont = this.font;
        ib.scale = scale;
        ib.shadedShape = target.createShadedShape(false, // mutable
                descriptor.getGridGeometry(), false); // tesselate

        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        ib.scaleNoZoom = ib.scale / screenToWorldRatio;
        ib.scaleNoZoom = ib.scale * paintProps.getZoomLevel();

        // IExtent screenExtent = paintProps.getView().getExtent();
        // IExtent mapExtent = new PixelExtent(descriptor.getGridGeometry()
        // .getGridRange());

        // Loop through the CGM commands -- in order -- to build paintable
        // AWIPS image elements...

        for (Command c : cgmImage.getCommands()) {
            if (c instanceof INcCommand) {
                try {
                    ((INcCommand) c).contributeToPaintableImage(ib, target,
                            paintProps, descriptor);
                } catch (VizException e) {
                    logger.error("[EXCEPTION occurred processing CGM"
                            + " command " + c + "]");
                    e.printStackTrace();
                    throw (e);
                }
            }
        }

        // Compilation: Now that we've processed all the CGM commands, "compile"
        // those AWIPS graphics elements that need it once they're complete
        // (here, shaded shapes and wireframe shapes...

        ib.shadedShape.compile();

        for (WireframeKey key : ib.wireframes.keySet()) {
            IWireframeShape wireframeForThisKey = ib.wireframes.get(key);
            if (wireframeForThisKey == null || !wireframeForThisKey.isMutable()) {
                // TODO assert: Wireframe missing, or not compiled yet
            } else {
                wireframeForThisKey.compile();
            }
        }

        // Now retrieve from the ImageBuilder only the completed paintable
        // objects that we want to save as this PaintableImage.
        // (The temporary ImageBuilder object will then be discarded.)

        this.shadedShape = ib.shadedShape;
        this.wireframes = ib.wireframes;
        this.wireframePaintOrder = ib.wireframePaintOrder;
        this.circles = ib.circles;
        this.strings = ib.strings;
    }

    public void paint() throws VizException {

        // Finally! Actually paint the AWIPS graphics elements...

        // Shaded Shape (filled polygons)

        if (shadedShape != null) {
            float alpha = 1.0f; // TODO verify
            float brightness = 1.0f;
            target.drawShadedShape(shadedShape, alpha, brightness);
        }

        // Wireframes

        // int count = 0;
        for (WireframeKey key : wireframePaintOrder /* wireframes.keySet() */) {
            IWireframeShape wireframeForThisKey = wireframes.get(key);
            if (wireframeForThisKey == null /*
                                             * ||
                                             * wireframeForThisKey.isMutable()
                                             */) {
                // TODO assert: Wireframe missing, or not compiled yet
                throw new VizException();
            } else {
                target.drawWireframeShape(wireframeForThisKey, key.color,
                        (float) key.width);
                // System.out.println("[Drew wireframe " + ++count + " for "
                // + key.toString());
            }
        }

        // Strings

        target.drawStrings(strings);

        // Circles

        target.drawCircle(circles.toArray(new DrawableCircle[circles.size()]));
    }

    public void dispose() {
        if (shadedShape != null) {
            shadedShape.dispose();
            shadedShape = null;
        }
        // any of the following really needed??
        if (wireframes != null) {
            for (IWireframeShape wf : wireframes.values()) {
                wf.dispose();
            }
            wireframes.clear();
            wireframes = null;
        }
        if (strings != null) {
            for (DrawableString ds : strings) {
                // no such method ds.dispose();
                ds = null;
            }
            strings = null;
        }
        if (circles != null) {
            for (DrawableCircle dc : circles) {
                // no such method dc.dispose();
                dc = null;
            }
            circles = null;
        }
    }
}