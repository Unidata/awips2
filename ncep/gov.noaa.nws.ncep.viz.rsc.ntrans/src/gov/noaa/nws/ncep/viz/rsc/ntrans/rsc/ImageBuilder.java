/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package gov.noaa.nws.ncep.viz.rsc.ntrans.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;

/**
 * ImageBuilder - Class which holds the state of a single image while it's under
 * construction by sequential execution of CGM commands. For efficiency (and
 * other) reasons, it is useful not to draw some elements immediately to the
 * screen as each drawable CGM command is processed, but rather to accumulate
 * simple objects (e.g., polylines) into larger aggregate constructs
 * (wireframes) for drawing later by AWIPS II IGraphicsTarget commands. Also,
 * some CGM commands are dependent on modes or states set by previous CGM
 * commands (e.g., LineWidth); this structure also provides a place to hold
 * these states.
 * 
 * ImageBuilder is used to construct a PaintableImage, but is kept distinct so
 * that the former can be discarded (along with any temporary "building
 * materials" it contains) once the latter has been fully constructed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2014            bhebbard     Initial creation
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

// ------------------------------------------------------------

public class ImageBuilder {

    // This class holds the state of the image while it's under
    // construction by sequential execution of the CGM commands.

    public class WireframeKey {

        // An object of this class forms a unique key to an AWIPS II
        // wireframe (IWireframeShape) which can be drawn in a
        // single operation. As such, it contains as fields all
        // characteristics that must be held constant in a single
        // such wireframe draw operation.

        public RGB color;

        public double width;

        public WireframeKey(RGB color, double width) {
            this.color = color;
            this.width = width;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((color == null) ? 0 : color.hashCode());
            long temp;
            temp = Double.doubleToLongBits(width);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            WireframeKey other = (WireframeKey) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (color == null) {
                if (other.color != null)
                    return false;
            } else if (!color.equals(other.color))
                return false;
            if (Double.doubleToLongBits(width) != Double
                    .doubleToLongBits(other.width))
                return false;
            return true;
        }

        private ImageBuilder getOuterType() {
            return ImageBuilder.this;
        }

        public String toString() {
            return (color.toString() + " Line Width " + width);
        }
    }

    // Collection of all wireframes under construction, keyed by unique output
    // draw states
    public Map<WireframeKey, IWireframeShape> wireframes = new HashMap<WireframeKey, IWireframeShape>();

    // Sequence in which to paint the wireframes
    public List<WireframeKey> wireframePaintOrder = new ArrayList<WireframeKey>();

    // Line color set by the most recent CGM LineColour command. Default to
    // WHITE.
    public RGB currentLineColor = new RGB(255, 255, 255);

    // Line width set by the most recent CGM LineWidth command. Default to 1
    // pixel.
    public double currentLineWidth = 1.0;

    // Accumulator for AWIPS II DrawableString text objects
    public List<DrawableString> strings = new ArrayList<DrawableString>();

    public RGB currentTextColor = new RGB(255, 255, 255);

    public IFont currentFont = null;

    public TextStyle textStyle = TextStyle.NORMAL;

    public HorizontalAlignment horizontalAlignment = HorizontalAlignment.CENTER;

    public VerticalAlignment verticalAlignment = VerticalAlignment.TOP;

    public List<DrawableCircle> circles = new ArrayList<DrawableCircle>();

    public RGB currentCircleColor = new RGB(255, 0, 0);

    public IShadedShape shadedShape;

    public RGB currentFillColor = new RGB(0, 255, 0);

    public double scale = 1.0;

    public double scaleNoZoom = 1.0;

    public double[] scalePoint(double[] oldpoint) {
        return scalePoint(oldpoint[0], oldpoint[1]);
    }

    public double[] scalePointNoZoom(double[] oldpoint) {
        return scalePointNoZoom(oldpoint[0], oldpoint[1]);
    }

    public double[] scalePoint(double x, double y) {
        double[] newpoint = new double[2];
        newpoint[0] = x * scale;
        newpoint[1] = 1000.000 - y * scale; // TODO: Avoid hardcoding 1000
        return newpoint;
    }

    public double[] scalePointNoZoom(double x, double y) {
        double[] newpoint = new double[2];
        newpoint[0] = x * scaleNoZoom; // TODO plus translation
        newpoint[1] = 1000.000 - y * scaleNoZoom; // TODO plus translation
        return newpoint;
    }

}
