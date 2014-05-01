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
package com.raytheon.viz.pointdata;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.dom.svg.SVGOMDocument;
import org.apache.batik.dom.svg.SVGOMPathElement;
import org.apache.batik.gvt.GraphicsNode;
import org.eclipse.swt.graphics.RGB;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.svg.SVGSVGElement;

/**
 * The PointWindDisplay class takes a wind spd and direction or wind-u and
 * wind-v and will return a wind barb or wind arrow from the information
 * provided. The barb/arrow information will be returned as a SVG path. This is
 * a port of the Fortran routines from D2D to generate these graphics.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/16/2007              brockwoo    Initial creation	
 * 06/04/2008              chammack    Added option for barb/arrow creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PointWindDisplay {

    public static enum DisplayType {
        BARB, ARROW, DUALARROW, STREAMLINE
    };

    private int x, y, red, green, blue;

    private Document document;

    private SVGOMPathElement pathElement;

    private GVTBuilder builder;

    private BridgeContext bridgeContext;

    private final double length;

    private double spd, ix, jy, uudd, vvff;

    final double circle, exclude, cos75, sin75;

    private double scale;

    private static double FLG = 99998.0;

    private int barbSense;

    private int strokeWidth;

    private boolean pathRelative;

    private boolean forceCircle;

    /**
     * This construct sets up some basic information about how the wind barbs
     * are to be displayed. This includes the length of the barb, the scale for
     * the wind spd, an exclusion factor for the staff, and information for the
     * circle drawn in the event of no wind.
     * 
     * @param length
     *            The length of the barb in pixels
     * @param scale
     *            The number to scale the wind spd by
     * @param exclude
     *            The distance from the center to exclude drawing the staff
     * @param circle
     *            The radius of the circle for wind spd less than 2.5. Zero
     *            means no circle, and less than zero mean just plot a zero.
     * @param forceCircle
     *            if true zero wind circle is always displayed
     */
    public PointWindDisplay(double length, double scale, double exclude,
            double circle, boolean forceCircle) {
        this.cos75 = 0.2588190;
        this.sin75 = 0.9659259;
        this.barbSense = 1;
        this.length = length;
        this.scale = scale;
        this.exclude = exclude;
        this.circle = circle;
        this.ix = 0;
        this.jy = 0;
        this.pathRelative = false;
        this.forceCircle = forceCircle;
    }

    /**
     * This construct sets up some basic information about how the wind barbs
     * are to be displayed. This includes the length of the barb, the scale for
     * the wind spd, an exclusion factor for the staff, and information for the
     * circle drawn in the event of no wind.
     * 
     * @param length
     *            The length of the barb in pixels
     * @param scale
     *            The number to scale the wind spd by
     * @param exclude
     *            The distance from the center to exclude drawing the staff
     * @param circle
     *            The radius of the circle for wind spd less than 2.5. Zero
     *            means no circle, and less than zero mean just plot a zero.
     */
    public PointWindDisplay(double length, double scale, double exclude,
            double circle) {
        this(length, scale, exclude, circle, false);
    }

    /**
     * @param scale
     *            the scale to set
     */
    public void setScale(double scale) {
        this.scale = scale;
    }

    /**
     * Sets whether the SVG path will be path relative (set to true) or font
     * relative (set to false). By calling setImageParameters, this is
     * automatically set to true.
     * 
     * @param relativity
     *            True to be path relative or false to be font relative
     */
    public void setPathRelative(boolean relativity) {
        this.pathRelative = relativity;
    }

    /**
     * Set the color of the display
     * 
     * @param color
     */
    public void setColor(RGB color) {
        this.setImageParameters(this.x, this.y, color.red, color.green,
                color.blue, this.strokeWidth);
    }

    public void setLineWidth(int lineWidth) {
        this.setImageParameters(this.x, this.y, this.red, this.green,
                this.blue, lineWidth);
    }

    /**
     * If using this class to create a wind barb in a buffered image, call this
     * method to set the image attributes.
     * 
     * @param x
     *            The width of the image
     * @param y
     *            The height of the image
     * @param red
     *            The RGB red value to color the barb
     * @param green
     *            The RGB green value to color the barb
     * @param blue
     *            The RGB blue value to color the barb
     * @param strokeWidth
     *            The width of the line
     */
    public void setImageParameters(int x, int y, int red, int green, int blue,
            int strokeWidth) {
        this.x = x;
        this.y = y;
        this.strokeWidth = strokeWidth;
        this.ix = x / 2;
        this.jy = y / 2;
        this.pathRelative = true;
        DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();

        document = impl.createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI,
                "svg", null);

        SVGSVGElement svgRoot = ((SVGOMDocument) document).getRootElement();
        svgRoot.setAttributeNS(null, "viewBox", "0 0 " + Integer.toString(x)
                + " " + Integer.toString(y));
        svgRoot.setAttributeNS(null, "width", String.valueOf(x));
        svgRoot.setAttributeNS(null, "height", String.valueOf(y));

        this.red = red;
        this.green = green;
        this.blue = blue;
        pathElement = (SVGOMPathElement) document.createElementNS(
                SVGDOMImplementation.SVG_NAMESPACE_URI, "path");
        pathElement.setAttributeNS(null, "x", String.valueOf(this.ix));
        pathElement.setAttributeNS(null, "y", String.valueOf(this.jy));
        pathElement.setAttributeNS(null, "fill", "none");
        pathElement.setAttributeNS(null, "stroke", "RGB(255,255,255)");
        pathElement.setAttributeNS(null, "stroke-width", Integer
                .toString(strokeWidth));
        // TODO: add support for dashed lines
        // pathElement.setAttributeNS(null, "stroke-dasharray", "4,4");
        svgRoot.appendChild(pathElement);
        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    /**
     * Sets the value of the wind spd and direction to be drawn. If <i>polar</i>
     * is set to true, <i>uudd</i> is the direction and <i>vvff</i> is the spd.
     * If set to false, they are the u and v component of wind, respectively.
     * 
     * @param uudd
     *            U component or direction of the wind
     * @param vvff
     *            V component or the spd of the wind
     * @param polar
     *            If true, use direction and spd, else use u and v
     * @return true if the wind set correctly or false if the wind was not valid
     */
    public boolean setWind(double uudd, double vvff, boolean polar) {
        if (uudd > PointWindDisplay.FLG || vvff > PointWindDisplay.FLG) {
            return false;
        }
        if (polar) {
            this.spd = vvff;
            double dir = Math.toRadians(uudd);
            this.uudd = -spd * Math.sin(dir);
            this.vvff = -spd * Math.cos(dir);
        } else {
            this.uudd = uudd;
            this.vvff = vvff;
            this.spd = Math.hypot(uudd, vvff);
        }
        return true;
    }

    /**
     * Returns the current SVG formatted path for the wind spd and direction
     * specified. <b>Note:</b> If the image parameters are specified, the path
     * is relative to 0,0 being in the upper left corner (SVG Path specific). If
     * they are not specified, the path is relative to 0,0 being in the lower
     * left corner (SVG Font specific). The latter is only used when generating
     * the font paths for the SVG file.
     * 
     * @return A string containing the SVG path
     */
    public String getBarbPath() {
        String path = "";
        if (spd < 2.5 || forceCircle) {
            path += drawZeroWind();
        }

        if (spd >= 2.5) {
            String s = drawWindBarb();

            if (s != null) {
                path += s;
            }
        }
        return path;
    }

    private String drawZeroWind() {
        StringBuffer barbPath = new StringBuffer();
        if (this.circle > 0.0) {
            double aa = this.circle;
            double saa = aa * 0.707;

            long ix1 = Math.round(this.ix + aa);
            long jy1 = Math.round(this.jy);
            long ix2 = Math.round(this.ix + saa);
            long jy2 = this.pathRelative ? Math.round(this.jy + saa) : Math
                    .round(this.jy - saa);
            barbPath.append("M" + ix1 + " " + jy1 + "L" + ix2 + " " + jy2);
            ix1 = Math.round(this.ix);
            jy1 = this.pathRelative ? Math.round(this.jy + aa) : Math
                    .round(this.jy - aa);
            barbPath.append("L" + ix1 + " " + jy1);
            ix2 = Math.round(this.ix - saa);
            jy2 = this.pathRelative ? Math.round(this.jy + saa) : Math
                    .round(this.jy - saa);
            ;
            barbPath.append("L" + ix2 + " " + jy2);
            ix1 = Math.round(this.ix - aa);
            jy1 = Math.round(this.jy);
            barbPath.append("L" + ix1 + " " + jy1);
            ix2 = Math.round(this.ix - saa);
            jy2 = this.pathRelative ? Math.round(this.jy - saa) : Math
                    .round(this.jy + saa);
            ;
            barbPath.append("L" + ix2 + " " + jy2);
            ix1 = Math.round(this.ix);
            jy1 = this.pathRelative ? Math.round(this.jy - aa) : Math
                    .round(this.jy + aa);
            ;
            barbPath.append("L" + ix1 + " " + jy1);
            ix2 = Math.round(this.ix + saa);
            jy2 = this.pathRelative ? Math.round(this.jy - saa) : Math
                    .round(this.jy + saa);
            ;
            barbPath.append("L" + ix2 + " " + jy2);
            ix1 = Math.round(this.ix + aa);
            jy1 = Math.round(this.jy);
            barbPath.append("L" + ix1 + " " + jy1);
        } else if (circle < 0.0) {
            // The stroke font for "0"
            barbPath.append("");
        }
        return barbPath.toString();
    }

    public String getDualArrowPath(double headlen) {
        String s = drawDualArrow(headlen);
        if (s != null) {
            return s;
        }
        return "";
    }

    private String drawDualArrow(double headlen) {
        StringBuffer arrowPath = new StringBuffer();
        if (this.spd == 0.0) {
            return null;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = this.spd * this.scale;
        } else {
            staff = Math.log10(this.spd * -this.scale) * 10 + 10;
        }
        double barb = 0.0;
        if (headlen < 1.0) {
            barb = staff * headlen;
        } else {
            barb = headlen;
        }
        if (staff < barb) {
            return null;
        }
        double dix, djy;
        // DIRECTIONS
        dix = (this.barbSense) * this.uudd / this.spd;
        djy = (this.barbSense) * vvff / this.spd;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;

        // DRAW BODY OF ARROW
        long ix1 = Math.round(this.ix + dix * staff);
        long jy1 = this.pathRelative ? Math.round(jy - djy * staff) : Math
                .round(jy + djy * staff);
        long ix2 = Math.round(this.ix + dix * this.exclude);
        long jy2 = this.pathRelative ? Math.round(this.jy - djy * this.exclude)
                : Math.round(jy + djy * this.exclude);

        long ix3 = Math.round(ix1 + dix1 * barb);
        long jy3 = this.pathRelative ? Math.round(jy1 - djy1 * barb) : Math
                .round(jy1 + djy1 * barb);
        long ix4 = Math.round(ix2 - dix1 * barb);
        long jy4 = this.pathRelative ? Math.round(jy2 + djy1 * barb) : Math
                .round(jy2 - djy1 * barb);
        arrowPath.append("M" + ix4 + " " + jy4 + "L" + ix2 + " " + jy2);
        arrowPath.append("M" + ix2 + " " + jy2 + "L" + ix1 + " " + jy1);
        arrowPath.append("M" + ix1 + " " + jy1 + "L" + ix3 + " " + jy3);

        return arrowPath.toString();
    }

    /**
     * Returns the current SVG formatted path for the wind gust and direction
     * specified. <b>Note:</b> If the image parameters are specified, the path
     * is relative to 0,0 being in the upper left corner (SVG Path specific). If
     * they are not specified, the path is relative to 0,0 being in the lower
     * left corner (SVG Font specific). The latter is only used when generating
     * the font paths for the SVG file.
     * 
     * @param headlen
     *            The length to draw the arrow head
     * @return A string containing the SVG path
     */
    public String getArrowPath(double headlen) {
        // According to gridarrows.f, Arrows should not draw circle for zero

        String s = drawWindArrow(headlen);
        if (s != null) {
            return s;
        }
        return "";
    }

    private String drawWindArrow(double headlen) {
        StringBuffer arrowPath = new StringBuffer();
        if (this.spd == 0.0) {
            return null;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = this.spd * this.scale;
        } else {
            staff = Math.log10(this.spd * -this.scale) * 10 + 10;
        }
        double barb = 0.0;
        if (headlen < 1.0) {
            barb = staff * headlen;
        } else {
            barb = headlen;
        }
        if (staff < barb) {
            return null;
        }
        double dix, djy;
        // DIRECTIONS
        dix = (this.barbSense) * this.uudd / this.spd;
        djy = (this.barbSense) * vvff / this.spd;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;
        double dix2 = -dix + djy;
        double djy2 = -dix - djy;

        // DRAW BODY OF ARROW
        long ix1 = Math.round(this.ix + dix * staff);
        long jy1 = this.pathRelative ? Math.round(jy - djy * staff) : Math
                .round(jy + djy * staff);
        long ix2 = Math.round(this.ix + dix * this.exclude);
        long jy2 = this.pathRelative ? Math.round(this.jy - djy * this.exclude)
                : Math.round(jy + djy * this.exclude);
        arrowPath.append("M" + ix2 + " " + jy2 + "L" + ix1 + " " + jy1);

        // DRAW HEAD OF ARROW.
        ix2 = ix1 + Math.round(dix1 * barb);
        jy2 = this.pathRelative ? jy1 - Math.round(djy1 * barb) : jy1
                + Math.round(djy1 * barb);
        arrowPath.append("L" + ix2 + " " + jy2);
        ix2 = ix1 + Math.round(dix2 * barb);
        jy2 = this.pathRelative ? jy1 - Math.round(djy2 * barb) : jy1
                + Math.round(djy2 * barb);
        arrowPath.append("M" + ix1 + " " + jy1 + "L" + ix2 + " " + jy2);

        return arrowPath.toString();
    }

    private String drawWindBarb() {
        // add 2.5 to round to nearest 5
        int speed = (int) (this.spd + 2.5);
        StringBuffer barbPath = new StringBuffer();
        double staff = this.length;
        double barb = staff * 0.30;
        double add = staff * 0.105;
        double dix, djy;
        // DIRECTIONS
        dix = (-this.uudd) / this.spd;
        djy = (-this.barbSense) * vvff / this.spd;
        double dix1 = this.cos75 * dix + barbSense * sin75 * djy;
        double djy1 = (-this.barbSense) * this.sin75 * dix + this.cos75 * djy;

        // SPEED AND COUNTERS:
        int n50 = speed / 50;
        int calcSpd = speed - 50 * n50;
        int n10 = calcSpd / 10;
        calcSpd = calcSpd - 10 * n10;
        int n5 = calcSpd / 5;
        double sx = ((n50 + n50 + n10 + n5 + 2)) * add;
        staff = Math.max(this.length, sx);

        // DRAW STAFF
        long ix1 = Math.round(this.ix + dix * staff);
        long jy1 = this.pathRelative ? Math.round(this.jy - djy * staff) : Math
                .round(this.jy + djy * staff);
        long ix2 = Math.round(this.ix + dix * this.exclude);
        long jy2 = this.pathRelative ? Math.round(this.jy - djy * this.exclude)
                : Math.round(this.jy + djy * this.exclude);
        barbPath.append("M" + ix2 + " " + jy2 + "L" + ix1 + " " + jy1);

        // PLOT LONE HALF-BARB, IF NECESSARY
        if (n50 == 0 && n10 == 0) {
            ix2 = ix1 - Math.round(dix * add);
            jy2 = this.pathRelative ? jy1 + Math.round(djy * add) : jy1
                    - Math.round(djy * add);
            ix1 = ix2 + Math.round(dix1 * barb / 2.0);
            jy1 = this.pathRelative ? jy2 - Math.round(djy1 * barb / 2.0) : jy2
                    + Math.round(djy1 * barb / 2.0);
            barbPath.append("M" + ix1 + " " + jy1 + "L" + ix2 + " " + jy2);
            return barbPath.toString();
        }

        // PLOT FLAGS, IF NECESSARY
        for (int i = 0; i < n50; i++) {
            ix2 = ix1 + Math.round(dix1 * barb);
            jy2 = this.pathRelative ? jy1 - Math.round(djy1 * barb) : jy1
                    + Math.round(djy1 * barb);
            barbPath.append("L" + ix2 + " " + jy2);
            ix1 = ix1 - Math.round(dix * add * 2);
            jy1 = this.pathRelative ? jy1 + Math.round(djy * add * 2) : jy1
                    - Math.round(djy * add * 2);
            barbPath.append("L" + ix1 + " " + jy1);
        }
        if (n50 > 0) {
            ix1 = ix1 - Math.round(dix * add / 2.0);
            jy1 = this.pathRelative ? jy1 + Math.round(djy * add / 2.0) : jy1
                    - Math.round(djy * add / 2.0);
        }

        // PLOT BARB, IF NECESSARY
        for (int i = 0; i < n10; i++) {
            ix2 = ix1 + Math.round(dix1 * barb);
            jy2 = this.pathRelative ? jy1 - Math.round(djy1 * barb) : jy1
                    + Math.round(djy1 * barb);
            barbPath.append("M" + ix1 + " " + jy1 + "L" + ix2 + " " + jy2);
            ix1 = ix1 - Math.round(dix * add);
            jy1 = this.pathRelative ? jy1 + Math.round(djy * add) : jy1
                    - Math.round(djy * add);
        }

        // PLOT HALF-BARB, IF NECESSARY
        if (n5 != 0) {
            ix2 = ix1 + Math.round(dix1 * barb / 2.0);
            jy2 = this.pathRelative ? jy1 - Math.round(djy1 * barb / 2.0) : jy1
                    + Math.round(djy1 * barb / 2.0);
            barbPath.append("M" + ix1 + " " + jy1 + "L" + ix2 + " " + jy2);
        }

        return barbPath.toString();
    }

    /**
     * Returns a buffered image with the wind barb drawn onto it.
     * 
     * @param doAntiAlias
     *            Perform antialiasing on the image
     * @param type
     *            The image type to create
     * @return The buffered image with the parameters specified
     */
    public BufferedImage getWindImage(boolean antiAlias, DisplayType type,
            double arrowSize) {

        if (type == DisplayType.BARB) {
            pathElement.setAttributeNS(null, "d", getBarbPath());
        } else if (type == DisplayType.ARROW) {
            pathElement.setAttributeNS(null, "d", getArrowPath(arrowSize));
        } else if (type == DisplayType.DUALARROW) {
            pathElement.setAttributeNS(null, "d", getDualArrowPath(arrowSize));
        } else {
            throw new IllegalArgumentException("Unhandled type: " + type);
        }

        GraphicsNode theGraphicsNode = this.builder.build(this.bridgeContext,
                this.document);

        // System.out.println(this.document.getTextContent());

        byte[] red = { 0, (byte) this.red };
        byte[] blue = { 0, (byte) this.green };
        byte[] green = { 0, (byte) this.blue };

        IndexColorModel tm = new IndexColorModel(8, 2, red, blue, green, 0);

        BufferedImage bufferedImage = new BufferedImage(this.x, this.y,
                BufferedImage.TYPE_BYTE_INDEXED, tm);

        Graphics2D g2d = bufferedImage.createGraphics();
        if (antiAlias) {
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        }
        // Element elt = ((SVGDocument) document).getRootElement();
        // SVGDocument myDocument = (SVGDocument) document;
        theGraphicsNode.paint(g2d);
        // Cleanup and return image
        g2d.dispose();
        return bufferedImage;
    }

    public static void main(String[] args) {
        PointWindDisplay windBarbGen = new PointWindDisplay(25.0, -25.0, 5.0,
                6.0);
        // windBarbGen.setImageParameters(256, 256, 0, 255, 0, 1.0);
        try {
            for (double i = 355.0; i >= 0.0; i -= 5.0) {
                int speed = (int) i;
                // FileOutputStream output = new FileOutputStream(new
                // File("/home/brockwoo/" + spd + ".png"));
                windBarbGen.setWind(i, 25.0, true);
                System.out.println("<glyph unicode=\"" + speed
                        + "\" horiz-adv-x=\"1500\" d=\""
                        + windBarbGen.getArrowPath(0.2) + "\"/>");
                // ImageIO.write(windBarbGen.getWindImage(true), "png", output);
                // output.close();
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

    }

}
