/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Point2D.Double;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.Map;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.HatchIndex.HatchType;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.InteriorStyle.Style;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.TextAlignment.HorizontalAlignment;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.TextAlignment.VerticalAlignment;



/**
 * This class is responsible for displaying the parsed commands of a CGM file
 * into a graphic context.
 *
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CGMDisplay {
	private Graphics2D g2d;

	/** Size of the canvas */
	private int canvasWidth, canvasHeight;

	protected CGM Cgm;
	private Color fillColor = null;
	private int fillColorIndex = 1;
	private Color edgeColor = null;
	private int edgeColorIndex = 1;
	private Color lineColor = null;
	private int lineColorIndex = 1;
	private Color textColor = null;
	private int textColorIndex = 1;
	private Color markerColor = null;
	private final int markerColorIndex = 1;

	private boolean isFilled = false;
	/** True if the edge should be drawn */
	private boolean drawEdge = false;
	/**
	 * Default is 1/100 of the length of the longest side of the rectangle
	 * defined by default VDC extent, see chapter 8
	 */
	double characterHeight = 32;

	/**
	 * The extent of the drawing. An array of 2 points, each with two
	 * coordinates (x, y). extent[0] always represents the lower left corner,
	 * extent[1] always represents the upper right corner:
	 * <pre>
	 *       extent[1].x,[1].y
	 *        ----------*
	 *        |         |
	 *        |         |
	 *        * ---------
	 *   [0].x,[0].y
	 *  </pre>
	 */
	private Point2D.Double[] extent;

	/** The current font list */
	private FontWrapper[] fonts;

	private HorizontalAlignment horizontalTextAlignment = HorizontalAlignment.NORMAL_HORIZONTAL;

	private VerticalAlignment verticalTextAlignment = VerticalAlignment.NORMAL_VERTICAL;

	private final Map<Integer, float[]> lineDashes;

	/** The color table */
	private Color[] colorTable;

	private Style interiorStyle = Style.HOLLOW;

	/** True if the "BEFORE PICTURE BODY" command hasn't been processed yet */
	private boolean beforeBeginPictureBody = true;

	private double additionalInterCharacterSpace = 0;

	private boolean isScaled = false;

	private Double upVector;

	private Double baselineVector;

	private boolean useSymbolEncoding = false;

	private BasicStroke lineStroke;
	private BasicStroke edgeStroke;

	private MarkerType.Type markerType = MarkerType.Type.ASTERISK;

	// this should depend on marker size spec mode
	private double markerSize = 32;

	private double continuousHorizontalAlignment;

	private double continuousVerticalAlignment;

	private TextPath.Type textPath;

	private HatchType hatchType = HatchType.HORIZONTAL_LINES;

	private boolean clipFlag = true;

	private AffineTransform scaleTransform;

	private TileArrayInfo tileArrayInfo;

	private boolean isTransparent = false;

	/**
	 * Whether the view has been cleared. FIXME: how the view is cleared depends
	 * on the profile we're using. We are not supporting profiles at this point,
	 * so this setting might not be correct for all uses.
	 */
	private boolean isViewCleared= false;

	public CGMDisplay(CGM cgm) {
		reset();
		this.lineDashes = new HashMap<Integer, float[]>();
		// the values below are chosen so that they match the files of the test suite
		this.lineDashes.put(DashType.SOLID,			new float[] { 100, 0 }); // solid
		this.lineDashes.put(DashType.DASH,			new float[] { 55, 20 }); // dash
		this.lineDashes.put(DashType.DOT,			new float[] { 13, 13 }); // dot
		this.lineDashes.put(DashType.DASH_DOT,		new float[] { 55, 20, 13, 20  }); // dash-dot
		this.lineDashes.put(DashType.DASH_DOT_DOT,	new float[] { 55, 20, 13, 20, 13, 20  }); // dash-dot-dot

		if (VDCType.getType().equals(VDCType.Type.INTEGER)) {
			this.extent = new Point2D.Double[] { new Point2D.Double(0, 0), new Point2D.Double(32767, 32767) };
		}
		else if (VDCType.getType().equals(VDCType.Type.REAL)) {
			this.extent = new Point2D.Double[] { new Point2D.Double(0, 0), new Point2D.Double(1.0, 1.0) };
		}
		else
			assert(false);

		Point2D.Double extent[] = cgm.extent();
		if (extent != null)
			this.extent = extent;
		this.Cgm = cgm;
	}

	public CGM getCGM() {
		return this.Cgm;
	}

	public void paint(Graphics g) {
		this.g2d = (Graphics2D)g;

		if (!this.isTransparent) {
			// start with a white background color
			this.g2d.setColor(getIndexedColor(0));
			this.g2d.fillRect(0, 0, this.canvasWidth, this.canvasHeight);
		}

		// force anti aliasing
		this.g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		double minX = this.extent[0].x, maxX = this.extent[1].x;
		double minY = this.extent[0].y, maxY = this.extent[1].y;

		// we're scaling and respecting the proportions, check which scale to use
		double sx = this.canvasWidth/Math.abs(maxX-minX);
		double sy = this.canvasHeight/Math.abs(maxY-minY);
		double s = Math.min(sx, sy);

		double m00, m11, m02, m12;
		if (minX < maxX) {
			m00 = s;
			m02 = -s*minX;
		}
		else {
			// inverted X axis
			m00 = -s;
			m02 = this.canvasWidth+s*maxX;
		}
		if (minY < maxY) {
			m11 = s;
			m12 = -s*minY;
		}
		else {
			// inverted Y axis
			m11 = -s;
			m12 = this.canvasHeight+s*maxY;
		}

		// scale to the available view port
		this.scaleTransform = new AffineTransform(m00, 0, 0, m11, m02, m12);

		// invert the Y axis since (0, 0) is at top left for AWT
		AffineTransform invertY = new AffineTransform(1, 0, 0, -1, 0, this.canvasHeight);
		invertY.concatenate(this.scaleTransform);

		this.g2d.transform(invertY);

		this.Cgm.paint(this);
	}

	public Graphics2D getGraphics2D() {
		return this.g2d;
	}

	public void setTransparent(boolean transparent) {
		this.isTransparent = transparent;
	}

	public boolean isTransparent() {
		return this.isTransparent;
	}

	public void setFillColor(Color c) {
		this.fillColor = c;
	}

	public void setFillColorIndex(int colorIndex) {
		assert (colorIndex < this.colorTable.length);
		this.fillColor = this.colorTable[colorIndex];
	}

	public Color getFillColor() {
		if (this.fillColor == null) {
			assert (this.fillColorIndex < this.colorTable.length);
			return this.colorTable[this.fillColorIndex];
		}
		return this.fillColor;
	}

	public void setFilled(boolean flag) {
		this.isFilled = flag;
	}

	public boolean isFilled() {
		return this.isFilled;
	}

	public void setEdgeColor(Color c) {
		this.edgeColor = c;
	}

	public void setEdgeColorIndex(int colorIndex) {
		assert (colorIndex < this.colorTable.length);
		this.edgeColor = this.colorTable[colorIndex];
	}

	public Color getEdgeColor() {
		if (this.edgeColor == null) {
			assert (this.edgeColorIndex < this.colorTable.length);
			return this.colorTable[this.edgeColorIndex];
		}
		return this.edgeColor;
	}

	public void setEdge(boolean flag) {
		this.drawEdge = flag;
	}

	/**
	 * Returns true if edge should be drawn
	 * @return true/false
	 */
	public boolean drawEdge() {
		return this.drawEdge;
	}

	public void setLineColor(Color c) {
		this.lineColor = c;
	}

	public void setLineColorIndex(int colorIndex) {
		assert (colorIndex < this.colorTable.length);
		this.lineColor = this.colorTable[colorIndex];
	}

	public Color getLineColor() {
		if (this.lineColor == null) {
			assert (this.lineColorIndex < this.colorTable.length);
			return this.colorTable[this.lineColorIndex];
		}
		return this.lineColor;
	}

	public void setMarkerColor(Color c) {
		this.markerColor = c;
	}

	public void setMarkerColorIndex(int colorIndex) {
		assert (colorIndex < this.colorTable.length);
		this.markerColor = this.colorTable[colorIndex];
	}

	public Color getMarkerColor() {
		if (this.markerColor == null) {
			assert (this.markerColorIndex < this.colorTable.length);
			return this.colorTable[this.markerColorIndex];
		}
		return this.markerColor;
	}

	public void setTextColor(Color c) {
		this.textColor = c;
	}

	public void setTextColorIndex(int colorIndex) {
		assert (colorIndex < this.colorTable.length);
		this.textColor = this.colorTable[colorIndex];
	}

	public Color getTextColor() {
		if (this.textColor == null) {
			assert (this.textColorIndex < this.colorTable.length);
			return this.colorTable[this.textColorIndex];
		}
		return this.textColor;
	}

	public void setCharacterHeight(double h) {
		this.characterHeight = h;
	}

	public double getCharacterHeight() {
		return this.characterHeight;
	}

	public void scale(Graphics g, int w, int h) {
		this.g2d = (Graphics2D)g;
		if (this.extent == null)
			return;

		double extentWidth = Math.abs(this.extent[1].x - this.extent[0].x);
		double extentHeight = Math.abs(this.extent[1].y - this.extent[0].y);

		double fx = w / extentWidth;
		if (fx * (extentHeight) > h) {
			fx = h / extentHeight;
		}
		this.canvasWidth = (int)(fx * extentWidth);
		this.canvasHeight = (int)(fx * extentHeight);

		this.isScaled  = true;
	}

	public boolean isScaled() {
		return this.isScaled;
	}

	/**
	 * Returns the extent of the drawing.
	 * 
	 * <pre>
	 *       extent[1].x,[1].y
	 *        ----------*
	 *        |         |
	 *        |         |
	 *        * ---------
	 *   [0].x,[0].y
	 * </pre>
	 * 
	 * @return An array of 2 points, each with two coordinates (x, y). extent[0]
	 *         always represents the lower left corner, extent[1] always
	 *         represents the upper right corner
	 */
	public Point2D.Double[] getExtent() {
		return this.extent;
	}

	protected final double angle(double x, double y) {
		return normalizeAngle(Math.atan2(y, x));
	}

	/**
	 * Normalizes an angle in the range -pi..pi to 0..2pi
	 * @param a
	 * @return
	 */
	final protected double normalizeAngle(double a) {
		if (a < 0) {
			return a + 2*Math.PI;
		}
		return a;
	}

	/**
	 * Returns a transformation to apply to transform from the given coordinate system
	 * @param op The origin of the coordinate system
	 * @param ip The first axis vector
	 * @param jp The second axis vector
	 * @return The transformation to apply
	 */
	protected final AffineTransform getCoordinateSystemTransformation(Point2D.Double op,
			Point2D.Double ip, Point2D.Double jp) {

		double ipAngle = Math.atan2(ip.y, ip.x);
		AffineTransform rotationTransform = AffineTransform.getRotateInstance(ipAngle);
		AffineTransform invertedRotationTransform = AffineTransform.getRotateInstance(-ipAngle);

		Point2D.Double rotatedSecondConjugate = new Point2D.Double();
		invertedRotationTransform.transform(jp, rotatedSecondConjugate);

		AffineTransform shearTransform;
		if (rotatedSecondConjugate.y != 0) {
			shearTransform = AffineTransform.getShearInstance(rotatedSecondConjugate.x /
					rotatedSecondConjugate.y, 0);
		}
		else {
			// identity
			shearTransform = new AffineTransform();
		}

		// first, apply the shear, then the rotation and finally the translation
		rotationTransform.concatenate(shearTransform);

		AffineTransform translateInstance = AffineTransform.getTranslateInstance(op.x, op.y);
		translateInstance.concatenate(rotationTransform);

		return translateInstance;
	}

	public void setFonts(FontWrapper[] fontWrappers) {
		this.fonts = fontWrappers;
		setFontIndex(1);
	}

	/**
	 * Sets the font to use
	 * @param fontIndex Font index starts from one
	 */
	public void setFontIndex(int fontIndex) {
		assert this.fonts != null && (fontIndex-1) < this.fonts.length;
		if ((fontIndex-1) <= 0 || (fontIndex - 1) >= this.fonts.length) {
			// annex D says that if the font index should be out of range, the
			// default font index should be used
			fontIndex = 1;
		}
		this.g2d.setFont(this.fonts[fontIndex-1].font);
		this.useSymbolEncoding  = this.fonts[fontIndex-1].useSymbolEncoding;
	}

	/**
	 * @return True if the current font is using the symbol encoding, false otherwise
	 */
	public boolean useSymbolEncoding() {
		return this.useSymbolEncoding;
	}

	public HorizontalAlignment getHorizontalTextAlignment() {
		return this.horizontalTextAlignment;
	}

	public VerticalAlignment getVerticalTextAlignment() {
		return this.verticalTextAlignment;
	}

	public double getContinuousHorizontalAlignment() {
		return this.continuousHorizontalAlignment;
	}

	public double getContinuousVerticalAlignment() {
		return this.continuousVerticalAlignment;
	}

	public void setTextAlignment(HorizontalAlignment horizontalAlignment,
			VerticalAlignment verticalAlignment, double continuousHorizontalAlignment,
			double continuousVerticalAlignment) {
		this.horizontalTextAlignment = horizontalAlignment;
		this.verticalTextAlignment = verticalAlignment;
		this.continuousHorizontalAlignment = continuousHorizontalAlignment;
		this.continuousVerticalAlignment = continuousVerticalAlignment;
	}

	/**
	 * Resets the parameters to default settings. This is called before the
	 * picture is scaled.
	 */
	public void reset() {
		this.beforeBeginPictureBody = true;

		// clipping
		this.clipFlag = true;

		this.characterHeight = 32;
		this.additionalInterCharacterSpace = 0;
		this.lineStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
		this.edgeStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
		this.drawEdge = false;
		this.hatchType = HatchType.HORIZONTAL_LINES;
		setInteriorStyle(Style.HOLLOW);

		this.colorTable = new Color[63];
		initializeColorTable();
		this.fillColor = null;
		this.fillColorIndex = 1;
		this.edgeColor = null;
		this.edgeColorIndex = 1;
		this.lineColor = null;
		this.lineColorIndex = 1;
		this.textColor = null;
		this.textColorIndex = 1;
		this.markerType = MarkerType.Type.ASTERISK;
		if (SpecificationMode.ABSOLUTE.equals(MarkerSizeSpecificationMode.getMode())) {
			this.markerSize = 32767 / 100;
		}
		else if (SpecificationMode.SCALED.equals(MarkerSizeSpecificationMode.getMode())) {
			this.markerSize = 1.0;
		}
		else if (SpecificationMode.FRACTIONAL.equals(MarkerSizeSpecificationMode.getMode())) {
			this.markerSize = 0.01;
		}
		else if (SpecificationMode.MM.equals(MarkerSizeSpecificationMode.getMode())) {
			this.markerSize = 2.50;
		}

		this.fonts = null;
		this.useSymbolEncoding = false;
		this.horizontalTextAlignment = HorizontalAlignment.NORMAL_HORIZONTAL;
		this.verticalTextAlignment = VerticalAlignment.NORMAL_VERTICAL;

		this.upVector = new Point2D.Double(0, 32767);
		this.baselineVector = new Point2D.Double(32767, 0);
		this.textPath = TextPath.Type.RIGHT;
	}

	private void initializeColorTable() {
		this.colorTable[0] = Color.WHITE;
		for (int c = 1; c < this.colorTable.length; c++) {
			this.colorTable[c] = Color.BLACK;
		}
	}

	public void addLineType(int lineType, int[] dashElements, double dashCycleRepeatLength) {
		// here, convert the dash definitions from CGM to BasicStroke
		// For BasicStroke, dash attributes alternate opaque and transparent sections
		float[] convertedElements; // defaults to solid
		if (dashElements.length == 1) {
			convertedElements = null; // solid
		}
		else {
			// normalize the length of all elements to match dashCycleRepeatLength
			double sum = 0;
			for (int element : dashElements) {
				sum += element;
			}
			double factor = dashCycleRepeatLength / sum;

			convertedElements = new float[dashElements.length];
			for (int i = 0; i < dashElements.length; i++) {
				convertedElements[i] = (float)(dashElements[i] * factor);
			}
		}
		this.lineDashes.put(new Integer(lineType), convertedElements);
	}

	public void setLineType(int type) {
		this.lineStroke = new BasicStroke(this.lineStroke.getLineWidth(),
				this.lineStroke.getEndCap(),
				this.lineStroke.getLineJoin(),
				this.lineStroke.getMiterLimit(),
				this.lineDashes.get(type),
				this.lineStroke.getDashPhase());
	}

	public BasicStroke getLineStroke() {
		return this.lineStroke;
	}

	public void setEdgeType(int type) {
		this.edgeStroke = new BasicStroke(this.edgeStroke.getLineWidth(),
				this.edgeStroke.getEndCap(),
				this.edgeStroke.getLineJoin(),
				this.edgeStroke.getMiterLimit(),
				this.lineDashes.get(type),
				this.edgeStroke.getDashPhase());
	}

	public BasicStroke getEdgeStroke() {
		return this.edgeStroke;
	}

	public void setMaximumColorIndex(int maxColorIndex) {
		assert (maxColorIndex+1 > 0);
		this.colorTable = new Color[maxColorIndex+1];
		initializeColorTable();
	}

	public void setIndexedColor(int i, Color color) {
		assert (i < this.colorTable.length);
		this.colorTable[i] = color;
	}

	public Color getIndexedColor(int i) {
		assert (i < this.colorTable.length);
		return this.colorTable[i];
	}

	public Style getInteriorStyle() {
		return this.interiorStyle;
	}

	public void setInteriorStyle(Style interiorStyle) {
		this.interiorStyle = interiorStyle;
		this.isFilled = Style.SOLID.equals(this.interiorStyle) || Style.INTERPOLATED.equals(this.interiorStyle);
	}

	public boolean isBeforeBeginPictureBody() {
		return this.beforeBeginPictureBody;
	}

	public void reachedBeginPictureBody() {
		this.beforeBeginPictureBody = false;
	}

	/**
	 * @param startAngle
	 * @param endAngle
	 * @param normalizedStartAngle
	 * @param normalizedEndAngle
	 */
	public double[] normalizeAngleRange(double startAngle, double endAngle) {
		endAngle = normalizeAngle(-endAngle);
		if (startAngle < endAngle) {
			return new double[] { startAngle, endAngle };
		}

		return new double[] { endAngle, startAngle };
	}

	/**
	 * @param d
	 * @return
	 */
	public double radiansToDegrees(double angle) {
		return normalizeAngle(angle) * 180 / Math.PI;
	}

	/**
	 * AWT always expect increasing X to the right and increasing Y to the
	 * bottom. If our user coordinate system is oriented differently, the
	 * returned matrix will convert the user space to AWT space and be applied
	 * on the drawn string.
	 *
	 * @return The matrix to apply to the string before drawing
	 */
	public AffineTransform getTextTransform() {
		if (this.extent[0].x < this.extent[1].x) {
			if (this.extent[0].y < this.extent[1].y) {
				return new AffineTransform(1, 0, 0, -1, 0, 0);
			}
			return new AffineTransform(1, 0, 0, 1, 0, 0);
		}
		if (this.extent[0].y < this.extent[1].y) {
			return new AffineTransform(1, 0, 0, 1, 0, 0);
		}
		return new AffineTransform(1, 0, 0, -1, 0, 0);
	}

	/**
	 * @param additionalInterCharacterSpace
	 */
	public void setCharacterSpacing(double additionalInterCharacterSpace) {
		this.additionalInterCharacterSpace = additionalInterCharacterSpace;
	}

	/**
	 * @return the additionalInterCharacterSpace
	 */
	public double getCharacterSpacing() {
		return this.additionalInterCharacterSpace;
	}

	/**
	 * @param upVector
	 * @param baselineVector
	 */
	public void setCharacterOrientation(Point2D.Double upVector, Point2D.Double baselineVector) {
		this.upVector = upVector;
		this.baselineVector = baselineVector;
	}

	public Point2D.Double getCharacterOrientationUpVector() {
		return this.upVector;
	}

	public Point2D.Double getCharacterOrientationBaselineVector() {
		return this.baselineVector;
	}

	private double scaleWidth(double width, SpecificationMode mode) {
		double scaledWidth;
		if (SpecificationMode.ABSOLUTE.equals(mode)) {
			scaledWidth = width;
		}
		else if (SpecificationMode.SCALED.equals(mode)) {
			double scaleX = this.scaleTransform.getScaleX();
			if (scaleX != 0) {
				scaledWidth = width / scaleX;
			}
			else {
				scaledWidth = width;
			}
		}
		else {
			scaledWidth = width;
		}
		return scaledWidth;
	}

	public void setLineWidth(double width) {
		SpecificationMode mode = LineWidthSpecificationMode.getMode();
		this.lineStroke = new BasicStroke((float) scaleWidth(width, mode),
				this.lineStroke.getEndCap(),
				this.lineStroke.getLineJoin(),
				this.lineStroke.getMiterLimit(),
				this.lineStroke.getDashArray(),
				this.lineStroke.getDashPhase());
	}

	public void setEdgeWidth(double width) {
		SpecificationMode mode = EdgeWidthSpecificationMode.getMode();
		this.edgeStroke = new BasicStroke((float) scaleWidth(width, mode),
				this.edgeStroke.getEndCap(),
				this.edgeStroke.getLineJoin(),
				this.edgeStroke.getMiterLimit(),
				this.edgeStroke.getDashArray(),
				this.edgeStroke.getDashPhase());
	}

	public void setLineCap(LineCapIndicator lineIndicator) {
		this.lineStroke = new BasicStroke(this.lineStroke.getLineWidth(),
				lineIndicator.getBasicStrokeConstant(),
				this.lineStroke.getLineJoin(),
				this.lineStroke.getMiterLimit(),
				this.lineStroke.getDashArray(),
				this.lineStroke.getDashPhase());
	}

	public void setEdgeCap(LineCapIndicator lineIndicator) {
		this.edgeStroke = new BasicStroke(this.edgeStroke.getLineWidth(),
				lineIndicator.getBasicStrokeConstant(),
				this.edgeStroke.getLineJoin(),
				this.edgeStroke.getMiterLimit(),
				this.edgeStroke.getDashArray(),
				this.edgeStroke.getDashPhase());
	}

	public void setLineJoin(JoinIndicator type) {
		this.lineStroke = new BasicStroke(this.lineStroke.getLineWidth(),
				this.lineStroke.getEndCap(),
				type.getBasicStrokeConstant(),
				this.lineStroke.getMiterLimit(),
				this.lineStroke.getDashArray(),
				this.lineStroke.getDashPhase());
	}

	public void setEdgeJoin(JoinIndicator type) {
		this.edgeStroke = new BasicStroke(this.edgeStroke.getLineWidth(),
				this.edgeStroke.getEndCap(),
				type.getBasicStrokeConstant(),
				this.edgeStroke.getMiterLimit(),
				this.edgeStroke.getDashArray(),
				this.edgeStroke.getDashPhase());
	}

	public void setMarkerType(MarkerType.Type type) {
		this.markerType = type;
	}

	public MarkerType.Type getMarkerType() {
		return this.markerType;
	}

	public void setMarkerSize(double width) {
		this.markerSize = scaleWidth(width, MarkerSizeSpecificationMode.getMode());
	}

	public double getMarkerSize() {
		return this.markerSize;
	}

	public void setTextPath(TextPath.Type path) {
		this.textPath = path;
	}

	public TextPath.Type getTextPath() {
		return this.textPath;
	}

	/**
	 * Checks the interior style and fills the given shape accordingly if necessary
	 * @param s The shape to fill
	 */
	public void fill(Shape s) {
		if (InteriorStyle.Style.SOLID.equals(getInteriorStyle())) {
			this.g2d.setColor(getFillColor());
			this.g2d.fill(s);
		}
		else if (InteriorStyle.Style.HOLLOW.equals(getInteriorStyle())) {
			this.g2d.setColor(getFillColor());
			this.g2d.draw(s);
		}
		else if (InteriorStyle.Style.HATCH.equals(getInteriorStyle())) {
			drawHatch(s);
		}
	}

	private void drawHatch(Shape s) {
		// remember the clip and the stroke since we're overwriting them here
		Shape previousClippingArea = this.g2d.getClip();
		Stroke previousStroke = this.g2d.getStroke();

		Rectangle2D bounds = s.getBounds2D();
		this.g2d.setClip(s);

		this.g2d.setStroke(new BasicStroke(1));

		this.g2d.setColor(getFillColor());

		final double stepX = 20;
		final double stepY = 20;
		final double slopeStep = stepX * 1.41; // sqrt(2) since the sloped lines are at 45 degree

		if (HatchType.HORIZONTAL_LINES.equals(this.hatchType)) {
			drawHorizontalLines(bounds, stepY);
		}
		else if (HatchType.VERTICAL_LINES.equals(this.hatchType)) {
			drawVerticalLines(bounds, stepX);
		}
		else if (HatchType.POSITIVE_SLOPE_LINES.equals(this.hatchType)) {
			drawPositiveSlopeLines(bounds, slopeStep);
		}
		else if (HatchType.NEGATIVE_SLOPE_LINES.equals(this.hatchType)) {
			drawNegativeSlopeLines(bounds, slopeStep);
		}
		else if (HatchType.HORIZONTAL_VERTICAL_CROSSHATCH.equals(this.hatchType)) {
			drawHorizontalLines(bounds, stepY);
			drawVerticalLines(bounds, stepX);
		}
		else if (HatchType.POSITIVE_NEGATIVE_CROSSHATCH.equals(this.hatchType)) {
			drawPositiveSlopeLines(bounds, slopeStep);
			drawNegativeSlopeLines(bounds, slopeStep);
		}

		// restore the previous clipping area and stroke
		this.g2d.setClip(previousClippingArea);
		this.g2d.setStroke(previousStroke);
	}

	private void drawVerticalLines(Rectangle2D bounds, final double stepX) {
		for (double x = bounds.getX(); x < bounds.getX() + bounds.getWidth(); x += stepX) {
			this.g2d.draw(new Line2D.Double(x, bounds.getY(), x, bounds.getY() + bounds.getHeight()));
		}
	}

	private void drawHorizontalLines(Rectangle2D bounds, final double stepY) {
		for (double y = bounds.getY(); y < bounds.getY() + bounds.getHeight(); y += stepY) {
			this.g2d.draw(new Line2D.Double(bounds.getX(), y, bounds.getX() + bounds.getWidth(), y));
		}
	}

	private void drawPositiveSlopeLines(Rectangle2D bounds, final double slopeStep) {
		Point2D.Double currentBegin = new Point2D.Double(bounds.getX(), bounds.getY() + bounds.getHeight());
		Point2D.Double currentEnd = currentBegin;

		boolean done = false;
		while (!done) {
			// move begin
			if (currentBegin.y > bounds.getY()) {
				// move the begin down the Y axis
				currentBegin = new Point2D.Double(currentBegin.x, currentBegin.y - slopeStep);
			}
			else {
				// move the begin right the X axis
				currentBegin = new Point2D.Double(currentBegin.x + slopeStep, currentBegin.y);
			}

			// move end
			if (currentEnd.x < bounds.getX() + bounds.getWidth()) {
				// move end right the X axis
				currentEnd = new Point2D.Double(currentEnd.x + slopeStep, currentEnd.y);
			}
			else {
				// move end down the Y axis
				currentEnd = new Point2D.Double(currentEnd.x, currentEnd.y - slopeStep);
			}

			this.g2d.draw(new Line2D.Double(currentBegin.x, currentBegin.y, currentEnd.x, currentEnd.y));

			if (currentBegin.x > bounds.getX() + bounds.getWidth() || currentEnd.getY() < bounds.getY()) {
				done = true;
			}
		}
	}

	private void drawNegativeSlopeLines(Rectangle2D bounds, final double slopeStep) {
		Point2D.Double currentBegin = new Point2D.Double(bounds.getX(), bounds.getY());
		Point2D.Double currentEnd = currentBegin;

		boolean done = false;
		while (!done) {
			// move begin
			if (currentBegin.y < bounds.getY() + bounds.getHeight()) {
				// move the begin up the Y axis
				currentBegin = new Point2D.Double(currentBegin.x, currentBegin.y + slopeStep);
			}
			else {
				// move the begin right the X axis
				currentBegin = new Point2D.Double(currentBegin.x + slopeStep, currentBegin.y);
			}

			// move end
			if (currentEnd.x < bounds.getX() + bounds.getWidth()) {
				// move end right the X axis
				currentEnd = new Point2D.Double(currentEnd.x + slopeStep, currentEnd.y);
			}
			else {
				// move end up the Y axis
				currentEnd = new Point2D.Double(currentEnd.x, currentEnd.y + slopeStep);
			}

			this.g2d.draw(new Line2D.Double(currentBegin.x, currentBegin.y, currentEnd.x, currentEnd.y));

			if (currentBegin.x > bounds.getX() + bounds.getWidth() || currentEnd.getY() < bounds.getY()) {
				done = true;
			}
		}
	}

	/**
	 * @param type
	 */
	public void setHatchStyle(HatchType type) {
		this.hatchType = type;
	}

	/**
	 * @param flag
	 */
	public void setClipFlag(boolean flag) {
		this.clipFlag = flag;
	}

	public boolean getClipFlag() {
		return this.clipFlag;
	}

	/**
	 * Returns an information structure to be able to draw tiles of a tile
	 * array.
	 * 
	 * @return
	 */
	TileArrayInfo getTileArrayInfo() {
		return this.tileArrayInfo;
	}

	/**
	 * Sets information to be able to draw tiles of a tile array
	 * @param tileArrayInfo
	 */
	void setTileArrayInfo(TileArrayInfo tileArrayInfo) {
		this.tileArrayInfo = tileArrayInfo;
	}

	public boolean isViewCleared() {
		return this.isViewCleared;
	}

	public void setViewCleared(boolean b) {
		this.isViewCleared = b;
	}

}

/*
 * vim:encoding=utf8
 */
