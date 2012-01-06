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
package fsl.tools.glyph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;

/**
 * The label adornment class, providing an adornment consisting of a text label
 * that sits on top of the <code>MultiPointGlyph</code> it is adorning, at a
 * particular point along the glyph and oriented so as to run parallel to the
 * glyph's path at that point.
 * 
 * @author Christopher Golden
 * @see fsl.tools.glyph.MultiPointGlyph
 */
public class LabelAdornment extends RenderedAdornment {

    // Protected Static Constants

    private static final long serialVersionUID = 1L;

    /**
     * Font to be used for standard adornments.
     */
    protected static final Font FONT = new Font("SansSerif", Font.PLAIN, 10);

    // Protected Variables

    /**
     * Fraction along the length of the glyph at which the adornment lies.
     */
    protected double fraction;

    /**
     * Font of label.
     */
    protected Font font;

    /**
     * Text of the label.
     */
    protected String text;

    /**
     * Font to be specified when converting to DGM; must be one of the font
     * constants defined by <code>DGMArray</code>.
     * 
     * @see fsl.util.DGMArray
     */
    // protected short dgmFont = DGMArray.FONT_7_BY_9_ASCII;
    /**
     * Point around which this label is centered; this is display-context
     * sensitive, and thus it is transient.
     */
    protected transient Point location = null;

    /**
     * Angle of the label; this is display-context sensitive, and thus is
     * transient.
     */
    protected transient double angle = 0.0;

    // Public Constructors

    /**
     * Create an instance with the specified text label at the specified point
     * along the glyph, with specified color and the standard font.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     * @param fraction
     *            Fraction indicating how far along the glyph this adornment
     *            lies; must be between 0.0 and 1.0, inclusive.
     * @param color
     *            Color to be used when painting this adornment.
     * @param text
     *            Text of the label.
     */
    public LabelAdornment(MultiPointGlyph glyph, double fraction, Color color,
            String text) {
        super(glyph, color);
        this.fraction = fraction;
        this.text = text;
        this.font = FONT;
    }

    /**
     * Create an instance with the specified text label at the specified point
     * along the glyph, with the specified font and color.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     * @param fraction
     *            Fraction indicating how far along the glyph this adornment
     *            lies; must be between 0.0 and 1.0, inclusive.
     * @param color
     *            Color to be used when painting this adornment.
     * @param font
     *            Font to be used for the label.
     * @param text
     *            Text of the label.
     * @param dgmFont
     *            DGM font code, from <code>
     *                 DGMArray</code>, to be used when
     *            converting to DGM.
     * 
     * @see fsl.util.DGMArray
     */
    public LabelAdornment(MultiPointGlyph glyph, double fraction, Color color,
            Font font, String text, short dgmFont) {
        super(glyph, color);
        this.fraction = fraction;
        this.font = font;
        this.text = text;
        // this.dgmFont = dgmFont;
    }

    // Public Methods

    /**
     * Determines whether another object is equal to this object.
     * 
     * @param obj
     *            The object to which this object is to be compared.
     * @return True if the objects are the same, false otherwise.
     */
    public boolean equals(Object obj) {

        // These are equal only if the superclass says so
        // and the various member variables are the same.
        return (super.equals(obj) && (obj instanceof LabelAdornment)
                && (fraction == ((LabelAdornment) obj).fraction)
                && font.equals(((LabelAdornment) obj).font) && (((text == null) && (((LabelAdornment) obj).text == null)) || text
                .equals(((LabelAdornment) obj).text)));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Use the hash code of the superclass combined
        // with those of the component variables of this
        // object.
        return (int) ((((long) super.hashCode())
                + ((long) (new Double(fraction)).hashCode())
                + ((long) font.hashCode()) + (text == null ? 0L : ((long) text
                .hashCode()))) % (long) Integer.MAX_VALUE);
    }

    /**
     * Get the fraction indicating how far along the glyph this adornment lies.
     * 
     * @return Fraction indicating how far along the glyph this adornment lies.
     */
    public double getFraction() {
        return fraction;
    }

    /**
     * Get the font of the adornment.
     * 
     * @return Font of the adornment.
     */
    public Font getFont() {
        return font;
    }

    /**
     * Get the text of the adornment.
     * 
     * @return Text of the adornment.
     */
    public String getText() {
        return text;
    }

    /**
     * Set the fraction indicating how far along the glyph this adornment lies.
     * 
     * @param fraction
     *            Fraction of the <code>
     *                 glyph</code>'s total length
     *            specifying the point at which this adornment sits; must be
     *            between 0.0 and 1.0 inclusive.
     */
    public void setFraction(double fraction) {
        this.fraction = fraction;
        flushDisplayCoordinates();
    }

    /**
     * Set the font of the adornment.
     * 
     * @param font
     *            Font of the adornment.
     * @param dgmFont
     *            DGM font code, from <code>
     *                DGMArray</code>, to be used when
     *            converting to DGM.
     */
    public void setFont(Font font, short dgmFont) {
        this.font = font;
        // this.dgmFont = dgmFont;
        flushDisplayCoordinates();
    }

    /**
     * Set the text of the adornment.
     * 
     * @param text
     *            Text of the adornment.
     */
    public void setText(String text) {
        this.text = text;
        flushDisplayCoordinates();
    }

    /**
     * Receive notification that the glyph that is being decorated by this
     * adornment has changed in some way, or the display upon which it is drawn
     * has changed. This tells the adornment to be sure to recreate its
     * <code>Shape</code> object before returning it the next time
     * <code>getShape()</code> is called.
     */
    public void flushDisplayCoordinates() {
        location = null;
    }

    /**
     * Translate the adornment by the specified deltas.
     * 
     * @param xDelta
     *            X delta by which to translate the adornment.
     * @param yDelta
     *            Y delta by which to translate the adornment.
     */
    public void translateBy(int xDelta, int yDelta) {
        if (location != null)
            location.translate(xDelta, yDelta);
    }

    // /**
    // * Paint this adornment.
    // *
    // * @param gc Graphics context in which this
    // * adornment is to be drawn.
    // * @param transformer Coordinate converter used to
    // * translate between lat-long pairs
    // * and display coordinates.
    // * @param properties Display properties to be used.
    // * @param useColor Color to be used when painting the
    // * glyph instead of whatever color(s)
    // * would usually be used; if <code>
    // * null</code>, standard painting
    // * will be done.
    // */
    // public void paint(Graphics gc, CoordConverter transformer,
    // DisplayProperties properties, Color useColor) {
    //
    // // If there is no text, do nothing.
    // if ((text == null) || text.equals(""))
    // return;
    //
    // // Make sure that the display coordinates have been
    // // calculated first.
    // mapToDisplayCoordinates(gc, transformer);
    //
    // // Set up the graphics context.
    // Graphics2D gc2 = (Graphics2D) gc;
    // gc2.setColor(properties.transform(useColor != null ? useColor : color));
    // gc2.setFont(font);
    //
    // // Rotate the max ascent for the font by the angle
    // // of the text; this yields the X and Y offsets
    // // needed to make up for the fact that the text is
    // // drawn by the graphic object with the point at
    // // the font's baseline.
    // FontMetrics fm = gc.getFontMetrics(font);
    // double adjustedAngle = Math.toRadians(angle);
    // double xDelta = -fm.getMaxAscent() * Math.sin(adjustedAngle);
    // double yDelta = fm.getMaxAscent() * Math.cos(adjustedAngle);
    // Dimension ascentOffsets =
    // new Dimension((int) (xDelta + (xDelta < 0.0 ? -0.5 : 0.5)),
    // (int) (yDelta + (yDelta < 0.0 ? -0.5 : 0.5)));
    //
    // // Get the bounding shape before rotation, and
    // // then rotate its upper left corner around the
    // // pixel location of this annotation to arrive
    // // at the starting point for text drawing.
    // Rectangle boundingRect = getBoundingShapeBeforeRotation(gc, transformer);
    // xDelta = ((boundingRect.x - location.x) * Math.cos(adjustedAngle)) -
    // ((boundingRect.y - location.y) * Math.sin(adjustedAngle));
    // yDelta = ((boundingRect.x - location.x) * Math.sin(adjustedAngle)) +
    // ((boundingRect.y - location.y) * Math.cos(adjustedAngle));
    // Point startPoint = new Point(location.x +
    // (int) (xDelta + (xDelta < 0.0 ? -0.5 : 0.5)),
    // location.y +
    // (int) (yDelta + (yDelta < 0.0 ? -0.5 : 0.5)));
    //
    // // Draw the text using the specified rotation.
    // gc2.rotate(Math.toRadians(angle), startPoint.x + ascentOffsets.width,
    // startPoint.y + ascentOffsets.height);
    // gc2.drawString(text, startPoint.x + ascentOffsets.width,
    // startPoint.y + ascentOffsets.height);
    // gc2.rotate(Math.toRadians(-angle), startPoint.x + ascentOffsets.width,
    // startPoint.y + ascentOffsets.height);
    // }

    /**
     * Get the bounding shape for this adornment.
     * 
     * @param gc
     *            Graphics context in which this adornment is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Bounding rectangle for this adornment.
     */
    public Shape getBoundingShape(IGraphicsTarget gc, CoordConverter transformer) {

        // Get the bounding rectangle before rotation.
        Rectangle boundingRect = getBoundingShapeBeforeRotation(gc, transformer);

        // Rotate the rectangle and return the resulting
        // shape.
        Rectangle2D.Double rect2D = new Rectangle2D.Double(boundingRect.x,
                boundingRect.y, boundingRect.width, boundingRect.height);
        AffineTransform at = new AffineTransform();
        at.rotate(Math.toRadians(angle), location.x, location.y);
        return at.createTransformedShape(rect2D);
    }

    // /**
    // * Convert this adornment to DGM format and place the
    // * result in the supplied byte array.
    // *
    // * @param dgm DGM array in which to place the
    // * translated adornment.
    // * @param gc Graphics context in which this
    // * adornment is drawn.
    // * @param transformer Coordinate converter to be used
    // * to convert from the display
    // * coordinates to lat-long pairs.
    // * @param cartesian Flag indicating whether or not
    // * the conversion should utilize
    // * Cartesian instead of lat-long
    // * coordinates. A 1024 by 1024
    // * coordinate space is assumed if
    // * this flag is true.
    // */
    // public void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
    // boolean cartesian) {
    //
    // // Make sure that the display coordinates have been
    // // calculated first.
    // mapToDisplayCoordinates(gc, transformer);
    //
    // // If there is no text, do nothing.
    // if ((text == null) || text.equals(""))
    // return;
    //			
    // // Set the text alignment to be centered.
    // dgm.setTextAlignment(DGMArray.TEXT_ALIGNMENT_HORIZONTAL_MIDDLE,
    // DGMArray.TEXT_ALIGNMENT_VERTICAL_MIDDLE);
    //
    // // Approximate the text's orientation as closely
    // // as possible. Text can only be oriented along
    // // one of the four cardinal directions in the DGM
    // // format.
    // short rotation = (short) ((angle + 45.0) % 360.0);
    // rotation = (short) ((rotation / 90) * 90);
    // switch (rotation) {
    // case 0:
    // rotation = DGMArray.TEXT_ORIENTATION_UPDATE_0;
    // break;
    // case 90:
    // rotation = DGMArray.TEXT_ORIENTATION_UPDATE_90;
    // break;
    // case 180:
    // rotation = DGMArray.TEXT_ORIENTATION_UPDATE_180;
    // break;
    // case 270:
    // rotation = DGMArray.TEXT_ORIENTATION_UPDATE_270;
    // break;
    // }
    // dgm.setTextOrientation(rotation,
    // DGMArray.TEXT_ORIENTATION_INDIVIDUAL_0);
    //
    // // Set the font to be used depending upon whether
    // // or not the text is Unicode.
    // dgm.setFont(dgmFont);
    //
    // // Set the font magnification to be used.
    // int size = font.getSize();
    // if (size < 14)
    // size = 1;
    // else if (size < 18)
    // size = 2;
    // else if (size < 22)
    // size = 3;
    // else if (size < 40)
    // size = 4;
    // else
    // size = 5;
    // dgm.setCharacterMagnification((short) size);
    //
    // // Add the symbol as text to the DGM file.
    // // The location is converted to minutes.
    // short x = 0, y = 0;
    // LatLong latLong = transformer.convert(location);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // x = (short) point.x;
    // y = (short) point.y;
    // } else {
    // x = (short) ((latLong.lon * (float) 60.0) + (float) 0.5);
    // y = (short) ((latLong.lat * (float) -60.0) + (float) 0.5);
    // }
    // if (dgmFont >= DGMArray.FONT_CHINESE)
    // dgm.addUnicodeText(x, y, text, DGMArray.CHARACTER_SETS[dgmFont -
    // DGMArray.
    // FONT_CHINESE]);
    // else
    // dgm.addText(x, y, text);
    // }

    // Protected Methods

    /**
     * Calculate the pixel location of the glyph on the display based on the
     * lat-long information.
     * 
     * @param gc
     *            Graphics context in which this adornment is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     */
    protected void mapToDisplayCoordinates(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Calculate the pixel location and angle of
        // orientation for the text label if it has not
        // been done since the last display change.
        if (location == null) {
            location = glyph.getPointAtFractionAlong(fraction, gc, transformer);
            angle = glyph.getAngleAtPointAlong(location, 20, gc, transformer);
            if ((angle > 90.0) && (angle < 270.0))
                angle = (angle + 180.0) % 360.0;
        }
    }

    /**
     * Get the bounding rectangle of the text label before taking into account
     * any rotation.
     * 
     * @param gc
     *            Graphics context in which this label is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Bounding rectangle of the text label before taking into account
     *         any rotation.
     */
    protected Rectangle getBoundingShapeBeforeRotation(IGraphicsTarget gc,
            CoordConverter transformer) {

        // // Get a font metrics object in order to perform
        // // bounding box calculations.
        // FontMetrics fm = gc.getFontMetrics(font);
        //
        // // Calculate the bounding box.
        // mapToDisplayCoordinates(gc, transformer);
        // Rectangle boundingRect = new Rectangle();
        // boundingRect.width = fm.stringWidth(text == null ? "" : text);
        // boundingRect.height = fm.getMaxAscent() + fm.getMaxDescent();
        // boundingRect.x = location.x - (boundingRect.width / 2);
        // boundingRect.y = location.y - (boundingRect.height / 2);

        // Return the result.
        // return boundingRect;
        return null;
    }

    /**
     * Create a clone of this adornment.
     * 
     * @return The new clone of this adornment.
     */
    protected Object clone() {
        LabelAdornment adornment = null;
        try {
            adornment = (LabelAdornment) super.clone();
            adornment.fraction = fraction;
            adornment.font = font;
            adornment.text = text;
            adornment.location = null;
            adornment.angle = 0.0;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return adornment;
    }

    @Override
    public void paint(IGraphicsTarget target, CoordConverter transformer,
            Color useColor) {
        // TODO Auto-generated method stub

    }

    @Override
    public void prepareShape(IWireframeShape wshape, IShadedShape sshape,
            CoordConverter converter) {
        // TODO Auto-generated method stub

    }
}
