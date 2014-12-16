/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GeoMagSampling resource, draws sample text to the screen. Also picks up mouse
 * events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date           Ticket#    Engineer         Description
 * ------------   ---------- -----------      --------------------------
 * April 16, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagSampling {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagSampling.class);

    /**
     * The result of a hover operation: a set of strings and corresponding
     * colors
     * 
     */
    protected static class SampleResult {

        public SampleResult() {

        }

        public String[] labels;

        public RGB[] colors;
    }

    private IFont hoverFont = null;

    private boolean errorInHovering = false;

    private VerticalAlignment verticalAlignment = VerticalAlignment.TOP;

    public GeoMagSampling() {

    }

    protected SampleResult doHover(ReferencedCoordinate coord,
            ResourceList resources) throws VizException {
        SampleResult result = new SampleResult();
        List<String> labelList = new ArrayList<String>();
        List<RGB> colorList = new ArrayList<RGB>();
        try {
            int size = resources.size();

            for (int i = size - 1; i >= 0; --i) {
                ResourcePair rp = resources.get(i);
                String retVal = recursiveHoverSearch(rp, coord);

                if (retVal != null && retVal.length() > 0) {

                    RGB color = null;
                    if (rp.getResource().hasCapability(
                            ColorableCapability.class)) {
                        color = rp.getResource()
                                .getCapability(ColorableCapability.class)
                                .getColor();
                    }
                    int p1, p2;
                    p1 = 0;
                    while ((p2 = retVal.indexOf('\n', p1)) >= 0) {
                        colorList.add(color);
                        labelList.add(retVal.substring(p1, p2));
                        p1 = p2 + 1;
                    }
                    String s = retVal.substring(p1);
                    if (s.length() > 0) {
                        colorList.add(color);
                        labelList.add(retVal.substring(p1));
                    }

                    break;
                }
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM, "Error sampling resources: "
                    + t.getLocalizedMessage(), t);

        }

        result.labels = labelList.toArray(new String[labelList.size()]);
        result.colors = colorList.toArray(new RGB[colorList.size()]);
        return result;
    }

    private String recursiveHoverSearch(ResourcePair rp,
            ReferencedCoordinate coordinate) throws VizException {
        ResourceProperties props = rp.getProperties();
        AbstractVizResource<?, ?> rsc = rp.getResource();

        if (rsc != null && rsc.getStatus() == ResourceStatus.INITIALIZED
                && props.isVisible()) {
            String curVal = rsc.inspect(coordinate);

            if (curVal != null && curVal.length() > 0) {
                return curVal;
            }
        }

        return null;
    }

    protected void paintResult(IGraphicsTarget target, IDescriptor descriptor,
            PaintProperties paintProps, ReferencedCoordinate coord)
            throws VizException {
        if (hoverFont == null) {
            hoverFont = target.initializeFont(target.getDefaultFont()
                    .getFontName(), 12, new Style[] { Style.BOLD });
            hoverFont.setSmoothing(false);
            hoverFont.setScaleFont(false);
        }

        SampleResult result = doHover(coord, descriptor.getResourceList());

        verticalAlignment = VerticalAlignment.TOP;
        target.clearClippingPlane();
        try {
            if (result != null) {
                double[] world = new double[] { coord.getObject().x,
                        coord.getObject().y };
                double[] pixel = descriptor.worldToPixel(world);
                Coordinate c = new Coordinate(pixel[0], pixel[1]);
                int canvasWidth = paintProps.getCanvasBounds().width;
                double extentWidth = paintProps.getView().getExtent()
                        .getWidth();
                double ratioX = canvasWidth / extentWidth;

                if (result.labels.length > 0) {
                    List<String[]> strsToUse = new ArrayList<String[]>();
                    HorizontalAlignment[] alignments = new HorizontalAlignment[result.labels.length];
                    boolean[] modified = new boolean[result.labels.length];
                    for (int i = 0; i < modified.length; ++i) {
                        modified[i] = false;
                        alignments[i] = HorizontalAlignment.LEFT;
                        String[] tmp = new String[] { result.labels[i],
                                result.labels[i] };
                        strsToUse.add(tmp);
                    }

                    adjustStrings(target, paintProps, strsToUse, modified,
                            alignments, c, ratioX, null);

                    HorizontalAlignment horizontalAlignment = alignments[0];
                    boolean good = true;
                    for (int i = 1; i < alignments.length && good; ++i) {
                        if (horizontalAlignment != alignments[i]) {
                            good = false;
                        }
                    }

                    if (!good) {
                        // not all the same, figure out alignments!!!
                        int maxLen = 0;
                        int i = 0;
                        for (String[] s : strsToUse) {
                            if (s[0].length() > maxLen) {
                                maxLen = s[0].length();
                                horizontalAlignment = alignments[i];
                            }
                            ++i;
                        }

                        adjustStrings(target, paintProps, strsToUse, modified,
                                alignments, c, ratioX, horizontalAlignment);
                    }

                    List<String> actualStrs = new ArrayList<String>();
                    for (int i = 0; i < strsToUse.size(); ++i) {
                        String[] strs = strsToUse.get(i);
                        for (int j = 1; j < strs.length; ++j) {
                            actualStrs.add(strs[j]);
                            // colorsToUse.add(result.colors[i]);
                        }
                    }

                    String[] newStrs = actualStrs.toArray(new String[actualStrs
                            .size()]);

                    double referencePtY = adjustLabelWrapY(
                            target,
                            newStrs,
                            c.y
                                    + ((AbstractRenderableDisplay.CURSOR_HEIGHT) / ratioX),
                            paintProps.getView().getExtent(), ratioX);

                    if (horizontalAlignment == HorizontalAlignment.RIGHT) {
                        c.x -= (target.getStringBounds(hoverFont, newStrs,
                                TextStyle.BOXED).getWidth() / ratioX);
                    }

                    DrawableString hoverString = new DrawableString(newStrs,
                            new RGB(0, 0, 0));
                    hoverString.font = hoverFont;
                    hoverString.textStyle = TextStyle.BOXED;
                    hoverString.horizontalAlignment = HorizontalAlignment.LEFT;
                    hoverString.verticallAlignment = verticalAlignment;
                    hoverString.boxColor = new RGB(255, 255, 255);
                    hoverString.setCoordinates(c.x, referencePtY);

                    target.drawStrings(hoverString);
                }
            }
            errorInHovering = false;
        } catch (Exception e) {
            if (errorInHovering) {
                // Keep down the number of error messages

                statusHandler.handle(
                        Priority.PROBLEM,
                        "Error painting sample text: "
                                + e.getLocalizedMessage(), e);

            }
            errorInHovering = true;
        }
    }

    private void adjustStrings(IGraphicsTarget target,
            PaintProperties paintProps, List<String[]> strsToUse,
            boolean[] modified, HorizontalAlignment[] alignments, Coordinate c,
            double ratio, HorizontalAlignment targetAlignment) {
        List<String[]> strsToUseInternal = new ArrayList<String[]>();
        for (int i = 0; i < strsToUse.size(); ++i) {
            String str = strsToUse.get(i)[0];
            String[] split = str.split("[ ]");
            boolean done = false;
            int divideBy = strsToUse.get(i).length - 1;
            int maxDivisions = 0;
            for (int j = 0; j < split.length; ++j) {
                if (split[j].isEmpty() == false) {
                    ++maxDivisions;
                }
            }

            if (alignments[i] == targetAlignment) {
                strsToUseInternal.add(strsToUse.get(i));
            } else {
                String[] test = new String[] { str };
                while (!done) {
                    if (divideBy > maxDivisions
                            || alignments[i] == targetAlignment) {
                        done = true;
                        continue;
                    }

                    int approxLenPerStr = str.length() / divideBy;
                    List<String> strs = new ArrayList<String>();

                    for (int j = 0; j < split.length;) {
                        String line = split[j++];
                        while (j < split.length) {
                            String s = split[j];
                            if (s.length() + line.length() <= approxLenPerStr) {
                                if (!s.isEmpty()) {
                                    if (j == split.length - 1
                                            && split[1].equalsIgnoreCase("=")) {
                                        line = split[split.length - 1];
                                    } else {
                                        line += " " + s;
                                    }
                                } else {
                                    line += " ";
                                }
                                ++j;
                            } else {
                                break;
                            }
                        }
                        strs.add(line);
                    }

                    test = strs.toArray(new String[strs.size()]);

                    HorizontalAlignment alignment = adjustLabelWrapX(target,
                            test, c.x, paintProps.getView().getExtent(), ratio,
                            alignments[i]);
                    if (alignment == alignments[i]
                            && (targetAlignment == null || alignment == targetAlignment)) {
                        // the alignment was not changed and we are the target
                        // alignment, we are done
                        done = true;
                    } else {
                        if (targetAlignment == null) {
                            // alignment changed, check to see if it changes
                            // back
                            HorizontalAlignment tmpAlignment = alignment;
                            alignment = adjustLabelWrapX(target, test, c.x,
                                    paintProps.getView().getExtent(), ratio,
                                    alignment);
                            if (alignment != tmpAlignment) {
                                // we moved back, we need to divide and
                                // conquer
                                alignments[i] = HorizontalAlignment.LEFT;
                                modified[i] = true;
                                divideBy++;
                            } else {
                                // we are good at this alignment
                                alignments[i] = alignment;
                                done = true;
                            }
                        } else {
                            // we need to be the targetAlignment
                            alignment = adjustLabelWrapX(target, test, c.x,
                                    paintProps.getView().getExtent(), ratio,
                                    targetAlignment);
                            if (alignment == targetAlignment) {
                                // we are fine at other alignment also, use it:
                                alignments[i] = alignment;
                                done = true;
                            } else {
                                alignments[i] = targetAlignment;
                                modified[i] = true;
                                divideBy++;
                            }
                        }
                    }
                }

                String[] addTo = new String[test.length + 1];
                addTo[0] = str;
                for (int j = 0; j < test.length; ++j) {
                    addTo[j + 1] = test[j];
                }

                strsToUseInternal.add(addTo);
            }
        }
        strsToUse.clear();
        strsToUse.addAll(strsToUseInternal);
    }

    /**
     * Adjusts the x label if the width of the longest label extends the extent
     * 
     * @param target
     * @param labels
     * @param x
     * @param extent
     * @param ratio
     * @return
     */
    private HorizontalAlignment adjustLabelWrapX(IGraphicsTarget target,
            String[] labels, double x, IExtent extent, double ratio,
            HorizontalAlignment horizontalAlignment) {
        double referencePoint = x;

        // Find the max width of the label in pixels
        double maxWidth = 0;
        IFont font = hoverFont;
        for (String label : labels) {
            Rectangle2D bounds = target.getStringBounds(font, label);
            if (bounds.getWidth() > maxWidth) {
                maxWidth = bounds.getWidth();
            }
        }

        // Get the width in gl space
        double widthInGl = maxWidth / ratio;

        if (horizontalAlignment == HorizontalAlignment.LEFT) {
            // Check to see if text extends screen extent
            if (referencePoint + widthInGl > extent.getMaxX()) {
                horizontalAlignment = HorizontalAlignment.RIGHT;
            }
        } else {
            // Check to see if text extends screen extent
            if (referencePoint - widthInGl < extent.getMinX()) {
                horizontalAlignment = HorizontalAlignment.LEFT;
            }
        }

        return horizontalAlignment;
    }

    /**
     * Adjusts the y label position if the stacked labels exceeds the screen
     * extent height
     * 
     * @param target
     * @param labels
     * @param y
     * @param extent
     * @param ratio
     * @return
     */
    private double adjustLabelWrapY(IGraphicsTarget target, String[] labels,
            double y, IExtent extent, double ratio) {
        double referencePoint = y;

        double totalHeight = target.getStringBounds(hoverFont, labels,
                TextStyle.BLANKED).getHeight();

        // convert to gl space
        double maxHeightInGl = (totalHeight) / ratio;

        // check to see if height extends map height
        if (referencePoint + maxHeightInGl > extent.getMaxY()) {
            verticalAlignment = VerticalAlignment.BOTTOM;
            referencePoint -= (AbstractRenderableDisplay.CURSOR_HEIGHT + 2)
                    / ratio;
        }

        // return adjusted point
        return referencePoint;
    }

    public void dispose() {
        if (hoverFont != null) {
            hoverFont.dispose();
        }
    }

}
