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
import com.vividsolutions.jts.geom.Coordinate;



/**
 * The drawable representation for text annotations.
 * Such annotations consist of an arbitrary number of
 * characters on an arbitrary number of lines.
 *
 * @author Christopher Golden
 */
public class Annotation extends Symbol {

	// Public Static Constants

	/**
	 * Left-aligned text.
	 */
	public static final int LEFT_ALIGNED = 0;

	/**
	 * Top-aligned text.
	 */
	public static final int TOP_ALIGNED = 0;

	/**
	 * Right-aligned text.
	 */
	public static final int RIGHT_ALIGNED = 2;

	/**
	 * Bottom-aligned text.
	 */
	public static final int BOTTOM_ALIGNED = 2;

	/**
	 * Centered text.
	 */
	public static final int CENTERED = 1;


	// Protected Static Constants

	/**
	 * Font to be used for standard annotations.
	 */
	protected static final Font FONT = new Font("SansSerif", Font.PLAIN, 10);


	// Protected Variables

	/**
	 * Font of annotation.
	 */
	protected Font font;

	/**
	 * Text of annotation.
	 */
	protected String text;

	/**
	 * Flag indicating whether or not the annotation
	 * should have a shadow.
	 */
	protected boolean shadowed;

	/**
	 * Color to use for shadow.
	 */
	protected Color shadowColor;

	/**
	 * Font to be specified when converting to DGM;
	 * must be one of the font constants defined
	 * by <code>DGMArray</code>.
	 *
	 * @see fsl.util.DGMArray
	 */
	//protected short dgmFont = DGMArray.FONT_7_BY_9_ASCII;

	/**
	 * Horizontal alignment; must be one of <code>
	 * LEFT_ALIGNED</code>, <code>CENTERED</code>,
	 * or <code>RIGHT_ALIGNED</code>.
	 */
	protected int horizontalAlignment;

	/**
	 * Vertical alignment; must be one of <code>
	 * TOP_ALIGNED</code>, <code>CENTERED</code>,
	 * or <code>BOTTOM_ALIGNED</code>.
	 */
	protected int verticalAlignment;


	// Public Constructors

	/**
	 * Create an instance with a starting location
	 * of 0,0, a color of black, standard orientation,
	 * default font, no text, and default (top and
	 * left) alignment.
	 */
	public Annotation() {
		font = FONT;
		text = "";
		horizontalAlignment = LEFT_ALIGNED;
		verticalAlignment = TOP_ALIGNED;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, and text, but default
	 * (i.e. top and left) alignment.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param location    Location of the annotation in
	 *                    lat-long coordinates.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param text        Text string of the annotation.
	 */
	public Annotation(int frame, Coordinate location, Color color,
					  double orientation, Font font, String text) {
		super(frame, location, color, orientation);
		this.font = font;
		this.text = text;
		this.horizontalAlignment = LEFT_ALIGNED;
		this.verticalAlignment = TOP_ALIGNED;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, alignment, and text.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param location    Location of the annotation in
	 *                    lat-long coordinates.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param horizontal  Horizontal alignment; must be
	 *                    <code>LEFT_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>RIGHT_ALIGNED</code>.
	 * @param vertical    Vertical alignment; must be
	 *                    <code>TOP_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>BOTTOM_ALIGNED</code>.
	 * @param text        Text string of the annotation.
	 */
	public Annotation(int frame, Coordinate location, Color color, double orientation,
					  Font font, int horizontal, int vertical, String text) {
		super(frame, location, color, orientation);
		this.font = font;
		this.horizontalAlignment = horizontal;
		this.verticalAlignment = vertical;
		this.text = text;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, alignment, and text.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param location    Location of the annotation in
	 *                    lat-long coordinates.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param horizontal  Horizontal alignment; must be
	 *                    <code>LEFT_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>RIGHT_ALIGNED</code>.
	 * @param vertical    Vertical alignment; must be
	 *                    <code>TOP_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>BOTTOM_ALIGNED</code>.
	 * @param text        Text string of the annotation.
	 * @param dgmFont     DGM font code, from <code>
	 *                    DGMArray</code>, to be used
	 *                    when converting to DGM.
	 *
	 * @see fsl.util.DGMArray
	 */
	public Annotation(int frame, Coordinate location, Color color, double orientation,
					  Font font, int horizontal, int vertical, String text,
					  short dgmFont) {
		super(frame, location, color, orientation);
		this.font = font;
		this.horizontalAlignment = horizontal;
		this.verticalAlignment = vertical;
		this.text = text;
		//this.dgmFont = dgmFont;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, and text, but default
	 * (i.e. top and left) alignment.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param transformer Coordinate converter used to
	 *                    translate between X,Y and
	 *                    latitude-longitude coordinates.
	 * @param location    Location of the annotation.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param text        Text string of the annotation.
	 */
	public Annotation(int frame, CoordConverter transformer, Point location,
					  Color color, double orientation, Font font, String text) {
		super(frame, transformer, location, color, orientation);
		this.font = font;
		this.horizontalAlignment = LEFT_ALIGNED;
		this.verticalAlignment = TOP_ALIGNED;
		this.text = text;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, alignment, and text.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param transformer Coordinate converter used to
	 *                    translate between X,Y and
	 *                    latitude-longitude coordinates.
	 * @param location    Location of the annotation.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param shadowed    Flag indicating whether or not the
	 *                    annotation should have  drop shadow.
	 * @param shadowColor Color of the drop shadow under the
	 *                    annotation; if <code>null</code>, no
	 *                    shadow will be displayed.
	 * @param horizontal  Horizontal alignment; must be
	 *                    <code>LEFT_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>RIGHT_ALIGNED</code>.
	 * @param vertical    Vertical alignment; must be
	 *                    <code>TOP_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>BOTTOM_ALIGNED</code>.
	 * @param text        Text string of the annotation.
	 */
	public Annotation(int frame, CoordConverter transformer, Point location,
					  Color color, double orientation, Font font, boolean shadowed,
					  Color shadowColor, int horizontal, int vertical, String text) {
		super(frame, transformer, location, color, orientation);
		this.font = font;
		this.shadowed = shadowed;
		this.shadowColor = shadowColor;
		this.horizontalAlignment = horizontal;
		this.verticalAlignment = vertical;
		this.text = text;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, font, alignment, and text.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param transformer Coordinate converter used to
	 *                    translate between X,Y and
	 *                    latitude-longitude coordinates.
	 * @param location    Location of the annotation.
	 * @param color       Color of the annotation.
	 * @param orientation Orientation of the annotation; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the text is oriented along its
	 *                    latitude.
	 * @param font        Font of the annotation.
	 * @param horizontal  Horizontal alignment; must be
	 *                    <code>LEFT_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>RIGHT_ALIGNED</code>.
	 * @param vertical    Vertical alignment; must be
	 *                    <code>TOP_ALIGNED</code>,
	 *                    <code>CENTERED</code>, or
	 *                    <code>BOTTOM_ALIGNED</code>.
	 * @param text        Text string of the annotation.
	 * @param dgmFont     DGM font code, from <code>
	 *                    DGMArray</code>, to be used
	 *                    when converting to DGM.
	 *
	 * @see fsl.util.DGMArray
	 */
	public Annotation(int frame, CoordConverter transformer, Point location,
					  Color color, double orientation, Font font, int horizontal,
					  int vertical, String text, short dgmFont) {
		super(frame, transformer, location, color, orientation);
		this.font = font;
		this.horizontalAlignment = horizontal;
		this.verticalAlignment = vertical;
		this.text = text;
		//this.dgmFont = dgmFont;
	}


	// Public Methods

    /**
     * Determines whether another object is equal to this object.
	 *
     * @param  obj The object to which this object is to be compared.
     * @return True if the objects are the same, false otherwise.
     */
    public boolean equals(Object obj) {

		// These are equal only if the superclass says so and if
		// the fonts, alignments, and text strings are equal.
		return (super.equals(obj) && (obj instanceof Annotation) &&
				font.equals(((Annotation) obj).font) &&
				(shadowed == ((Annotation) obj).shadowed) &&
				((shadowColor == ((Annotation) obj).shadowColor) ||
				 ((shadowColor != null) &&
				  shadowColor.equals(((Annotation) obj).shadowColor))) &&
				(horizontalAlignment == ((Annotation) obj).horizontalAlignment) &&
				(verticalAlignment == ((Annotation) obj).verticalAlignment) &&
				text.equals(((Annotation) obj).text)); // &&
				//(dgmFont == ((Annotation) obj).dgmFont));
    }

    /**
     * Get the hash code of this object.
	 *
     * @return Hash code of this object.
     */
	public int hashCode() {

		// Combine the hash codes of the superclass and the font,
		// alignment, and text.
		return (int) ((((long) super.hashCode()) +
					   ((long) font.hashCode()) +
					   (shadowed ? 1L : 0L) +
					   (shadowColor != null ? (long) shadowColor.hashCode() : 0L) +
					   ((long) horizontalAlignment) + ((long) verticalAlignment) +
					   ((long) text.hashCode())));
	}

	/**
	 * Create a clone of this glyph.
	 */
	public Object clone() {
		Annotation glyph = null;
		try {
			glyph = (Annotation) super.clone();
		} catch (Exception e) {
			//Logger.logBug("Clone error.", e);
			e.printStackTrace();
		}
		return glyph;
	}

	/**
	 * Paint the glyph.
	 *
	 * @param gc          Graphics context in which the
	 *                    annotation is to be drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @param properties  Display properties to be used.
	 * @param useColor    Color to be used when painting the
	 *                    glyph instead of whatever color(s)
	 *                    would usually be used; if <code>
	 *                    null</code>, standard painting
	 *                    will be done.
	 */
//	public void paint(IGraphicsTarget target, CoordConverter transformer,
//					  Color useColor) {
//
//		// Make sure that the display coordinates have been
//		// calculated first.
//		mapToDisplayCoordinates(gc, transformer);
//
//		// Set up the graphics context.
//		Graphics2D gc2 = (Graphics2D) gc;
//		gc2.setFont(font);
//
//		// Figure out how many pixels exist between lines.
//		FontMetrics fm = gc.getFontMetrics(font);
//		int delta = fm.getMaxAscent() + fm.getMaxDescent();
//
//		// Get the actual orientation to be used.
//		double orientation = getActualOrientation(transformer);
//
//		// Rotate the delta between lines by the angle of
//		// the text; this yields the X and Y offsets needed
//		// to move from one line to the next.  Then do the
//		// same for the max ascent of the font, so as to
//		// come up with the offset that makes up for the
//		// fact that text is drawn by the graphics object
//		// with the point at the font's baseline.
//		double adjustedAngle = Math.toRadians(orientation) * -1.0;
//		double xDelta = -delta * Math.sin(adjustedAngle);
//		double yDelta = delta * Math.cos(adjustedAngle);
//		Dimension offsets =
//			new Dimension((int) (xDelta + (xDelta < 0.0 ? -0.5 : 0.5)),
//						  (int) (yDelta + (yDelta < 0.0 ? -0.5 : 0.5)));
//		xDelta = -fm.getMaxAscent() * Math.sin(adjustedAngle);
//		yDelta = fm.getMaxAscent() * Math.cos(adjustedAngle);
//		Dimension ascentOffsets =
//			new Dimension((int) (xDelta + (xDelta < 0.0 ? -0.5 : 0.5)),
//						  (int) (yDelta + (yDelta < 0.0 ? -0.5 : 0.5)));
//
//		// Get the bounding shape before rotation, and
//		// then rotate its upper left corner around the
//		// pixel location of this annotation to arrive
//		// at the starting point for text drawing.
//		Rectangle boundingRect = getBoundingShapeBeforeRotation(gc, transformer);
//		xDelta = ((boundingRect.x - pixelLoc.x) * Math.cos(adjustedAngle)) -
//			((boundingRect.y - pixelLoc.y) * Math.sin(adjustedAngle));
//		yDelta = ((boundingRect.x - pixelLoc.x) * Math.sin(adjustedAngle)) +
//			((boundingRect.y - pixelLoc.y) * Math.cos(adjustedAngle));
//		Point startPoint = new Point(pixelLoc.x +
//									 (int) (xDelta + (xDelta < 0.0 ? -0.5 : 0.5)),
//									 pixelLoc.y +
//									 (int) (yDelta + (yDelta < 0.0 ? -0.5 : 0.5)));
//
//		// Break the text into lines and draw each line
//		// of text.
//		StringTokenizer st = new StringTokenizer(text, "\n", true);
//		int lines = 0;
//		while (st.hasMoreTokens()) {
//
//			// Ignore blank lines.
//			String line = st.nextToken();
//			if (line.length() == 0)
//				continue;
//			else if (line.equals("\n")) {
//				lines++;
//				continue;
//			}
//
//			// Draw the text using the specified rotation,
//			// drawing a shadow first if the text is to
//			// be shadowed and is not being drawn in some-
//			// thing other than its usual color.
//			gc2.rotate(Math.toRadians(-orientation),
//					   startPoint.x + ascentOffsets.width + (offsets.width * lines),
//					   startPoint.y + ascentOffsets.height + (offsets.height * lines));
//			if (shadowed && (shadowColor != null)) {
//				gc2.setColor(properties.transform(useColor != null ? useColor :
//												  shadowColor));
//				gc2.drawString(line, startPoint.x + ascentOffsets.width +
//							   (offsets.width * lines) + getShadowOffset(),
//							   startPoint.y + ascentOffsets.height +
//							   (offsets.height * lines) + getShadowOffset());
//			}
//			gc2.setColor(properties.transform(useColor != null ? useColor : color));
//			gc2.drawString(line, startPoint.x + ascentOffsets.width +
//						   (offsets.width * lines),
//						   startPoint.y + ascentOffsets.height +
//						   (offsets.height * lines));
//			gc2.rotate(Math.toRadians(orientation),
//					   startPoint.x + ascentOffsets.width + (offsets.width * lines),
//					   startPoint.y + ascentOffsets.height + (offsets.height * lines));
//		}
//	}

	/**
	 * Indicate whether or not the annotation contains the
	 * specified point.
	 *
	 * @param point       Point to check to see if the
	 *                    annotation contains it.
	 * @param gc          Graphics context in which this
	 *                    annotation is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return True if the annotation contains the point,
	 *         otherwise false.
	 */
	public boolean contains(Point point, IGraphicsTarget gc, CoordConverter transformer) {

		// Return true if the point lies within the
		// bounding shape.
		return getBoundingShape(gc, transformer).contains(point.x, point.y);
	}

	/**
	 * Indicate whether or not the glyph intersects with
	 * the specified rectangle.
	 *
	 * @param rectangle   Rectangle to check to see if
	 *                    the glyph intersects with it.
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return True if the glyph intersects with the
	 *         rectangle, otherwise false.
	 */
	public boolean intersects(Rectangle rectangle, IGraphicsTarget gc,
							  CoordConverter transformer) {

		// Return true if the rectangle intersects with
		// the bounding shape.
		return getBoundingShape(gc, transformer).intersects(rectangle);
	}

	/**
	 * Get the bounding rectangle of the annotation.
	 *
	 * @param gc          Graphics context in which this
	 *                    annotation is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return Bounding rectangle of the annotation.
	 */
	public Rectangle getBoundingRect(IGraphicsTarget gc, CoordConverter transformer) {

		// Get the bounding rectangle of the annotation's
		// bounding shape.
		Rectangle boundingRect = getBoundingShape(gc, transformer).getBounds();

		// Inflate the rectangle a bit to allow for some
		// slop, especially since italic fonts can cause
		// the string to extend beyond its calculated
		// width.  Then inflate it a bit to the lower
		// right if it is shadowed as well.
		boundingRect.grow(5, 5);
		if (shadowed) {
			boundingRect.width += getShadowOffset();
			boundingRect.height += getShadowOffset();
		}
		return boundingRect;
	}

	/**
	 * Get the font of the annotation.
	 *
	 * @return Font of the annotation.
	 */
	public Font getFont() {
		return font;
	}

	/**
	 * Determines whether or not the annotation is
	 * shadowed.
	 *
	 * @return Flag indicating whether or not the
	 *         annotation is shadowed.
	 */
	public boolean isShadowed() {
		return shadowed;
	}

	/**
	 * Get the shadow color of the annotation.
	 *
	 * @return Shadow color of the annotation.
	 */
	public Color getShadowColor() {
		return shadowColor;
	}

	/**
	 * Get the horizontal alignment.
	 *
	 * @return Horizontal alignment.
	 */
	public int getHorizontalAlignment() {
		return horizontalAlignment;
	}

	/**
	 * Get the vertical alignment.
	 *
	 * @return Vertical alignment.
	 */
	public int getVerticalAlignment() {
		return verticalAlignment;
	}

	/**
	 * Get the text of the annotation.
	 *
	 * @return Text of the annotation.
	 */
	public String getText() {
		return text;
	}

	/**
	 * Set the font of the annotation.
	 *
	 * @param font New font of the annotation.
	 */
	public void setFont(Font font) {
		this.font = font;
	}

	/**
	 * Set the shadowed property of the annotation.
	 *
	 * @param shadowed Flag indicating whether or
	 *                 not the annotation is
	 *                 shadowed.
	 */
	public void setShadowed(boolean shadowed) {
		this.shadowed = shadowed;
	}

	/**
	 * Set the shadow color of the annotation.
	 *
	 * @param shadowColor Shadow color of the
	 *                    annotation.
	 */
	public void setShadowColor(Color shadowColor) {
		this.shadowColor = shadowColor;
	}

	/**
	 * Set the horizontal alignment.
	 *
	 * @param horizontal New horizontal alignment.
	 */
	public void setHorizontalAlignment(int horizontal) {
		horizontalAlignment = horizontal;
	}

	/**
	 * Set the vertical alignment.
	 *
	 * @param vertical New vertical alignment.
	 */
	public void setVerticalAlignment(int vertical) {
		verticalAlignment = vertical;
	}

	/**
	 * Set the text of the annotation to a non-Unicode
	 * (i.e. ASCII) string.
	 *
	 * @param text New text of the annotation.
	 */
	public void setText(String text) {
		this.text = text;
		//this.dgmFont = DGMArray.FONT_7_BY_9_ASCII;
	}

	/**
	 * Set the text of the annotation to a Unicode
	 * string.
	 *
	 * @param text    New text of the annotation.
	 * @param dgmFont DGM font code from <code>DGMArray
	 *                </code> to be used when converting
	 *                to DGM.
	 */
	public void setText(String text, short dgmFont) {
		this.text = text;
		//this.dgmFont = dgmFont;
	}

	/**
	 * Convert the annotation to DGM format and place the
	 * result in the supplied byte array.
	 *
	 * @param dgm         DGM array in which to place the
	 *                    translated annotation.
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate transformer to be
	 *                    used to convert from the
	 *                    display coordinates to the DGM
	 *                    format.
	 * @param cartesian   Flag indicating whether or not
	 *                    the conversion should utilize
	 *                    Cartesian instead of lat-long
	 *                    coordinates. A 1024 by 1024
	 *                    coordinate space is assumed if
	 *                    this flag is true.
	 */
//	public void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//					  boolean cartesian) {
//		try {
//
//
//			// FUTURE ENHANCEMENT:  It may be desirable to
//			// approximate the annotation's color.  For now,
//			// this is not being done.
//			
//			
//			// Set the text alignment to match the annotation
//			// alignment.
//			dgm.setTextAlignment((horizontalAlignment == LEFT_ALIGNED ?
//								  DGMArray.TEXT_ALIGNMENT_HORIZONTAL_LEFT :
//								  (horizontalAlignment == CENTERED ?
//								   DGMArray.TEXT_ALIGNMENT_HORIZONTAL_MIDDLE :
//								   DGMArray.TEXT_ALIGNMENT_HORIZONTAL_RIGHT)),
//								 (verticalAlignment == TOP_ALIGNED ?
//								  DGMArray.TEXT_ALIGNMENT_VERTICAL_TOP :
//								  (verticalAlignment == CENTERED ?
//								   DGMArray.TEXT_ALIGNMENT_VERTICAL_MIDDLE :
//								   DGMArray.TEXT_ALIGNMENT_VERTICAL_BOTTOM)));
//
//			// Get the actual orientation to be used.
//			double orientation = getActualOrientation(transformer);
//			
//			// Approximate the text's orientation as closely
//			// as possible.  Text can only be oriented along
//			// one of the four cardinal directions in the DGM
//			// format.
//			short rotation = (short) ((orientation + 45.0) % 360.0);
//			rotation = (short) ((rotation / 90) * 90);
//			switch (rotation) {
//			case 0:
//				rotation = DGMArray.TEXT_ORIENTATION_UPDATE_0;
//				break;
//			case 90:
//				rotation = DGMArray.TEXT_ORIENTATION_UPDATE_90;
//				break;
//			case 180:
//				rotation = DGMArray.TEXT_ORIENTATION_UPDATE_180;
//				break;
//			case 270:
//				rotation = DGMArray.TEXT_ORIENTATION_UPDATE_270;
//				break;
//			}
//			dgm.setTextOrientation(rotation,
//								   DGMArray.TEXT_ORIENTATION_INDIVIDUAL_0);
//
//			// Set the font to be used depending upon whether
//			// or not the text is Unicode.
//			dgm.setFont(dgmFont);
//
//			// Set the font magnification to be used.
//			int size = font.getSize();
//			if (size < 14)
//				size = 1;
//			else if (size < 18)
//				size = 2;
//			else if (size < 22)
//				size = 3;
//			else if (size < 40)
//				size = 4;
//			else
//				size = 5;
//			dgm.setCharacterMagnification((short) size);
//
//			// Add the symbol as text to the DGM file.
//			// The location is converted to minutes.
//			short x = 0, y = 0;
//			if (cartesian) {
//				Point point = transformer.convert(location, false);
//				x = (short) point.x;
//				y = (short) point.y;
//			} else {
//				x = (short) ((location.lon * (float) 60.0) + (float) 0.5);
//				y = (short) ((location.lat * (float) -60.0) + (float) 0.5);
//			}
//			if (dgmFont >= DGMArray.FONT_CHINESE)
//				dgm.addUnicodeText(x, y, text, DGMArray.CHARACTER_SETS[dgmFont -
//																	  DGMArray.
//																	  FONT_CHINESE]);
//			else
//				dgm.addText(x, y, text);
//		} catch (Exception e) {
//			Logger.logBug("Error.", e);
//		}
//	}
//
//
//	// Protected Methods
//
//	/**
//	 * Get the bounding rectangle of the annotation
//	 * before taking into account any rotation.
//	 *
//	 * @param  gc          Graphics context in which this
//	 *                     annotation is drawn.
//	 * @param  transformer Coordinate converter used to
//	 *                     translate between lat-long
//	 *                     pairs and display coordinates.
//	 * @return Bounding rectangle of the annotation
//	 *         before taking into account any rotation.
//	 */
	protected Rectangle getBoundingShapeBeforeRotation(IGraphicsTarget gc,
													   CoordConverter transformer) {

//		// Get a font metrics object in order to perform
//		// bounding box calculations.
//		FontMetrics fm = gc.getFontMetrics(font);
//
//		// Find out how many lines there are to the text,
//		// and the length of the longest line.
//		int lines = 0, length = 0;
//		StringTokenizer st = new StringTokenizer(text, "\n", true);
//		boolean lastWasNewline = false;
//		while (st.hasMoreTokens()) {
//
//			// Don't check the length of blank lines.
//			String line = st.nextToken();
//			if (line.length() == 0) {
//				lastWasNewline = false;
//				continue;
//			} else if (line.equals("\n")) {
//				lines++;
//				lastWasNewline = true;
//				continue;
//			}
//
//			// See if this length is the longest so far.
//			lastWasNewline = false;
//			int thisLength = fm.stringWidth(line);
//			if (thisLength > length)
//				length = thisLength;
//		}
//		if (lastWasNewline == false)
//			lines++;
//
//		// Calculate the bounding box.
//	    mapToDisplayCoordinates(gc, transformer);
//		Rectangle boundingRect = new Rectangle();
//		boundingRect.x = pixelLoc.x;
//		boundingRect.y = pixelLoc.y;
//		boundingRect.width = length;
//		boundingRect.height = (fm.getMaxAscent() + fm.getMaxDescent()) * lines;
//
//		// Adjust the bounding box according to the text
//		// alignment.
//		switch (horizontalAlignment) {
//		case LEFT_ALIGNED:
//			break;
//		case CENTERED:
//			boundingRect.x -= boundingRect.width / 2;
//			break;
//		case RIGHT_ALIGNED:
//			boundingRect.x -= boundingRect.width;
//			break;
//		}
//		switch (verticalAlignment) {
//		case TOP_ALIGNED:
//			break;
//		case CENTERED:
//			boundingRect.y -= boundingRect.height / 2;
//			break;
//		case BOTTOM_ALIGNED:
//			boundingRect.y -= boundingRect.height;
//			break;
//		}

		// Return the result.
//		return boundingRect;
		return null;
	}

	/**
	 * Get the bounding shape of the annotation.
	 *
	 * @param  gc          Graphics context in which this
	 *                     annotation is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return Bounding shape of the annotation.
	 */
	protected Shape getBoundingShape(IGraphicsTarget gc, CoordConverter transformer) {

		// Get the bounding rectangle before rotation.
		Rectangle boundingRect = getBoundingShapeBeforeRotation(gc, transformer);

		// Get the actual orientation to be used.
		double orientation = getActualOrientation(transformer);

		// Rotate the rectangle and return the resulting
		// shape.
		Rectangle2D.Double rect2D =
			new Rectangle2D.Double(boundingRect.x, boundingRect.y,
								   boundingRect.width, boundingRect.height);
		AffineTransform at = new AffineTransform();
		at.rotate(Math.toRadians(-orientation), pixelLoc.x, pixelLoc.y);
		return at.createTransformedShape(rect2D);
	}

	/**
	 * Get the offset to be used for a shadow if this
	 * annotation is shadowed.
	 *
	 * @return Shadow offset.
	 */
	protected int getShadowOffset() {
		return (font.getSize() < 26 ? 2 : (font.getSize() < 50 ? 3 :
										   (font.getSize() < 80 ? 4 : 5)));
	}

	@Override
	public void prepareShape(IWireframeShape ws, IShadedShape ss, CoordConverter transformer) {
		// TODO Auto-generated method stub
		
	}
}
