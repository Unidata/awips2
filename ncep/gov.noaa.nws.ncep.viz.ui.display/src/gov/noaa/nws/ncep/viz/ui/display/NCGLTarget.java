/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.ui.display;


/**
 * 
 * Implements the GL graphics target
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                
 * Date            Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/03/10        #259      Greg Hull     Initial Creation.
 * 
 * </pre>
 * 
 * 
 * @author ghull
 * @version 1
 * 
 */
//public class NCGLTarget extends GLTarget implements IGLTarget {
//
//    private boolean rotateTextAroundPoint;
//    
//    private ColorBar colorBar;
//
//	public NCGLTarget(Canvas canvas, float width, float height)
//			throws VizException {
//		super(canvas, width, height);
//		
//        rotateTextAroundPoint = false;
//
//        colorBar = null;
//	}
//
//	// if GLTarget defined these methods as protected instead of private
//	// then we wouldn't need to have these here
//    protected void pushGLState() {
//        gl.glPushAttrib(GL.GL_COLOR_BUFFER_BIT | GL.GL_CURRENT_BIT
//                | GL.GL_ENABLE_BIT | GL.GL_TEXTURE_BIT | GL.GL_LINE_BIT);
//    }
//
//    protected void popGLState() {
//        gl.glPopAttrib();
//    }
//    protected double getScaleX() {
//        return viewExtent.getWidth() / this.canvasSize.width;
//
//    }
//    protected double getScaleY() {
//        return viewExtent.getHeight() / this.canvasSize.height;
//
//    }
//
//    
//    
//	// GLTarget's version will check for lastColormapUsed != null but NC's
//	// colorbars aren't necessarily based on images. 
//    @Override
//    public void endFrame() {
//    	useBuiltinColorbar = false; // don't draw GLTargets colorbar
//    	// draw the NC colorBar
//
//    	super.endFrame();
//    	
//    }
//    protected void drawColorbar( ) {
//
//        this.clearClippingPlane();
//        this.makeContextCurrent();
//        this.pushGLState();
//        try {
//
//            double x1 = this.viewExtent.getMinX();
//            double x2 = x1 + ((this.viewExtent.getMaxX() - x1) / 2.0);
//            double y1 = this.viewExtent.getMinY();
//            double y2 = y1 + ((x2 - x1) / 20.0);
//
//            PixelExtent pixelExtent = new PixelExtent(x1, x2, y1, y2);
//
//        } finally {
//            this.popGLState();
//        }
//    }
//
//    /*
//     * (non-Javadoc)
//     * 
//     * @see
//     * com.raytheon.viz.core.IGraphicsTarget#drawString(com.raytheon.viz.core
//     * .drawables.IFont, java.lang.String, double, double, double
//     * com.raytheon.viz.core.IGraphicsTarget.TextStyle,
//     * org.eclipse.swt.graphics.RGB,
//     * com.raytheon.viz.core.IGraphicsTarget.Alignment,
//     * com.raytheon.viz.core.IGraphicsTarget.VertAlignment, java.lang.Double)
//     */
//    @Override
//    public void drawString(IFont font, String string, double xPos, double yPos,
//            double zPos, TextStyle textStyle, RGB color,
//            HorizontalAlignment horizontalAlignment,
//            VerticalAlignment verticalAlignment, Double rotation)
//            throws VizException {
//    	
//        drawString(font, string, xPos, yPos, zPos, textStyle, color, horizontalAlignment,
//                verticalAlignment, rotation, null);
//
//    }
//
//    /*
//     * (non-Javadoc)
//     *
//     * @see
//     * com.raytheon.viz.core.IGraphicsTarget#drawString(com.raytheon.viz.core
//     * .drawables.IFont, java.lang.String, double, double, double
//     * com.raytheon.viz.core.IGraphicsTarget.TextStyle,
//     * org.eclipse.swt.graphics.RGB,
//     * com.raytheon.viz.core.IGraphicsTarget.Alignment,
//     * com.raytheon.viz.core.IGraphicsTarget.VertAlignment, java.lang.Double)
//     */
//    @Override
//    public void drawString(IFont font, String string, double xPos, double yPos,
//            double zPos, TextStyle textStyle, RGB color,
//            HorizontalAlignment horizontalAlignment,
//            VerticalAlignment verticalAlignment, Double rotation, Rectangle2D bounds)
//            throws VizException {
//
//        if (string == null || string.length() == 0) {
//            return;
//        }
//
//        if (font == null) {
//            font = getDefaultFont();
//        }
//
//        if (!(font instanceof GLFont)) {
//            throw new VizException("Font was not prepared using GLTarget");
//        }
//
//        if (color == null) {
//            color = DEFAULT_LABEL_COLOR;
//        }
//
//        this.makeContextCurrent();
//
//        TextRenderer textRenderer = ((GLFont) font).getTextRenderer();
//
//        double width, height;
//        if ( bounds == null ) {
//            width = textRenderer.getBounds(string).getWidth();
//            height = textRenderer.getBounds("Hy").getHeight();
//        }
//        else {
//                width = bounds.getWidth();
//                height = bounds.getHeight();
//        }
//
//        double adjustedWidth = width * getScaleX();
//        double adjustedHeight = height * getScaleY();
//
//        double rotationPtX = xPos;
//        double rotationPtY = yPos;
//        float startX = 0.0f;
//        float startY = 0.0f;
//
//        //  Rotate text around xPos, yPos
//        if ( rotateTextAroundPoint ) {
//                rotationPtX = xPos;
//                rotationPtY = yPos;
//            // Calculate the starting point of string based on horizontal alignment
//            if (horizontalAlignment == HorizontalAlignment.LEFT) {
//                startX = 0.0f;
//            } else if (horizontalAlignment == HorizontalAlignment.CENTER) {
//                startX = -(float)width / 2.0f;
//            } else if (horizontalAlignment == HorizontalAlignment.RIGHT) {
//                startX = -(float)width;
//            }
//            // Calculate the starting point of string based on vertical alignment
//            if (verticalAlignment == VerticalAlignment.BOTTOM) { // normal
//                startY = 0.0f;
//            } else if (verticalAlignment == VerticalAlignment.MIDDLE) {
//                startY = - (float)height / 2.0f;
//            } else if (verticalAlignment == VerticalAlignment.TOP) {
//                startY = -(float)height;
//            }
//        }
//        else {
//        	//  rotate text around starting position of string
//        	startX = 0.0f;
//        	startY = 0.0f;
//        	// Calculate the rotation point based on horizontal alignment
//        	if (horizontalAlignment == HorizontalAlignment.LEFT) {
//        		rotationPtX = xPos;
//        	} else if (horizontalAlignment == HorizontalAlignment.CENTER) {
//        		rotationPtX = xPos - adjustedWidth / 2;
//        	} else if (horizontalAlignment == HorizontalAlignment.RIGHT) {
//        		rotationPtX = xPos - adjustedWidth;
//        	}
//        	// Calculate the rotation point based on vertical alignment
//        	if (verticalAlignment == VerticalAlignment.BOTTOM) { // normal
//        		rotationPtY = yPos;
//        	} else if (verticalAlignment == VerticalAlignment.MIDDLE) {
//        		rotationPtY = yPos + adjustedHeight / 2;
//        	} else if (verticalAlignment == VerticalAlignment.TOP) {
//        		rotationPtY = yPos + adjustedHeight;
//        	}
//        }
//
//        gl.glMatrixMode(GL.GL_MODELVIEW);
//        gl.glEnable(GL.GL_BLEND);
//        // gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
//
//        gl.glEnable(GL.GL_TEXTURE_2D);
//
//        gl.glPushMatrix();
//
//        // flip the image
//        gl.glScalef(1, -1, 1);
//        gl.glTranslated(0, -viewExtent.getHeight(), 0);
//
//        // make the point the new origin
//        gl.glTranslated(rotationPtX, viewExtent.getHeight() - rotationPtY, zPos);
//
//        // do not zoom text
//        // TODO no z scale. doesn't seem to make sense. keep the 1.
//        gl.glScaled(getScaleX(), getScaleY(), 1);
//
//        // rotate
//        if (rotation != null) {
//            gl.glRotated(rotation, 0, 0, 1);
//        }
//        
//        double x1 = startX - PADDING;
//        double y1 = startY - PADDING;
//        double x2 = startX + width + PADDING;
//        double y2 = startY + height;
//
//        if (textStyle == TextStyle.BOXED || textStyle == TextStyle.BLANKED) {
//
//            gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
//            gl.glColor3d(0, 0, 0);
//
//            gl.glRectd(x1, y2, x2, y1);
//
//        }
//        if (textStyle == TextStyle.BOXED || textStyle == TextStyle.OUTLINE) {
//        	gl.glPolygonMode(GL.GL_BACK, GL.GL_LINE);
//        	gl.glColor3d(color.red / 255.0, color.green / 255.0,
//        			color.blue / 255.0);
//        	gl.glLineWidth(2);
//        	gl.glRectd(x1, y2, x2, y1);
//
//        }
//        gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
//
//        textRenderer.begin3DRendering();
//        textRenderer.setColor(color.red / 255.0f, color.green / 255.0f,
//                color.blue / 255.0f, 1.0f);
//        if (textStyle == TextStyle.WORD_WRAP) {
//            int i = 0;
//            int j = -1;
//            float y = 0.0f;
//            while (true) {
//                j = string.indexOf(' ', j + 1);
//                if (j < 0) {
//                    break;
//                }
//                if ((j - i) >= MIN_WRAP_LEN) {
//                    textRenderer.draw3D(string.substring(i, j), startX, y, 0.0f,
//                            1.0f);
//                    i = j + 1;
//                    y -= height;
//                }
//            }
//            // draw at the calculated starting location since we shifted the origin to the point
//            textRenderer.draw3D(string.substring(i), startX, y, 0.0f, 1.0f);
//
//        } else if (textStyle == TextStyle.DROP_SHADOW) {
//            textRenderer.setColor(0.0f, 0.0f, 0.0f, 1.0f);
//            textRenderer.draw3D(string, startX-0.5f, startY-0.5f, 0.0f, 1.0f);
//            textRenderer.draw3D(string, startX+0.5f, startY+0.5f, 0.0f, 1.0f);
//            textRenderer.setColor(color.red / 255.0f, color.green / 255.0f,
//                    color.blue / 255.0f, 1.0f);
//            textRenderer.draw3D(string, startX, startY, 0.0f, 1.0f);
//
//        } else {
//            // draw at the calculated starting location since we shifted the origin to the point
//            textRenderer.draw3D(string, startX, startY, 0.0f, 1.0f);
//        }
//        textRenderer.end3DRendering();
//
//        gl.glPopMatrix();
//        gl.glDisable(GL.GL_TEXTURE_2D);
//        gl.glPolygonMode(GL.GL_BACK, GL.GL_LINE);
//    }
//
//
//    protected void handleLineStyle(LineStyle lineStyle) {
//        if (lineStyle != null && lineStyle != LineStyle.SOLID) {
//            gl.glEnable(GL.GL_LINE_STIPPLE);
//            if (lineStyle == LineStyle.DASHED) {
//                gl.glLineStipple(4, (short) 0xAAAA);
//            } else if (lineStyle == LineStyle.DASHED_LARGE) {
//                gl.glLineStipple(4, (short) 0xEEEE);
//            } else if (lineStyle == LineStyle.DOTTED) {
//                gl.glLineStipple(1, (short) 0xAAAA);
//            } else if (lineStyle == LineStyle.DASH_DOTTED) {
//            	gl.glLineStipple(1, (short) 0xE4E4);
//            }
//            // NMAP Task 69 to add line styles from GEMPAK
//            else if( lineStyle == LineStyle.DOTS )
//            {
//            	gl.glLineStipple(1, (short) 0x8888);
//            }
//            else if( lineStyle == LineStyle.SHORT_DASHED )
//            {
//            	gl.glLineStipple(8, (short) 0xAAAA);
//            }
//            else if( lineStyle == LineStyle.MEDIUM_DASHED )
//            {
//            	gl.glLineStipple(3, (short) 0xF8F8);
//            }
//            else if( lineStyle == LineStyle.LONG_DASHED )
//            {
//            	gl.glLineStipple(3, (short) 0xFCFC);
//            }
//            else if( lineStyle == LineStyle.LONG_DASH_DOT )
//            {
//            	gl.glLineStipple(2, (short) 0xFFE4 );
//            }
//            else if( lineStyle == LineStyle.LONG_DASH_SHORT_DASH )
//            {
//            	gl.glLineStipple(3, (short) 0xFC38 );
//            }
//            else if( lineStyle == LineStyle.LONG_DASH_THREE_DOTS )
//            {
//            	//                gl.glLineStipple(2, (short) 0xFFAA);
//            	//                gl.glLineStipple(3, (short) 0xFFAA);
//            	gl.glLineStipple(2, (short) 0xFC92 );
//            }
//            else if( lineStyle == LineStyle.LONG_DASH_THREE_SHORT_DASHES )
//            {
//            	//                gl.glLineStipple(5, (short) 0xFFAA);
//            	gl.glLineStipple(3, (short) 0xFDB6);
//            }
//            else if( lineStyle == LineStyle.MEDIUM_DASH_DOT )
//            {
//            	// GEMPAK appears to have a bug where line type 9 draws as 'long dash 2 dots'
//            	// This line type will draw a MEDIUM DASH DOT            	
//            	//                gl.glLineStipple(2, (short) 0xFF24 ); // long dash 2 dots as drawn by GEMPAK
//            	//                gl.glLineStipple(2, (short) 0xFF18);
//            	gl.glLineStipple(2, (short) 0xFF88);
//            }
//        }
//    }
//
//    
//    
//    /*
//     * (non-Javadoc)
//     *
//     * @see com.raytheon.viz.core.IGraphicsTarget#drawStrings1Box(com.raytheon.viz.core.drawables.IFont,
//     *      java.lang.String[], double, double, double,
//     *      com.raytheon.viz.core.IGraphicsTarget.TextStyle,
//     *      org.eclipse.swt.graphics.RGB[],
//     *      com.raytheon.viz.core.IGraphicsTarget.Alignment, java.lang.Double)
//     */
//    @Override
//    public void drawStrings1Box(IFont font, String[] text, double x, double y, double z,
//            TextStyle textStyle, RGB[] colors, HorizontalAlignment alignment,
//            Double rotation) throws VizException {
//
//        TextRenderer renderer = this.defaultFont.getTextRenderer();
//
//        if (font != null) {
//            renderer = ((GLFont) font).getTextRenderer();
//        }
//
//        double height = renderer.getBounds(text[0]).getHeight();
//        height += PADDING * 2;
//
//        /*
//         * Find max line width
//         */
//        double maxWidth = 0.0;
//        for (int j=0; j < text.length; j++) {
//                maxWidth = Math.max(maxWidth, renderer.getBounds(text[j]).getWidth());
//        }
//
//        /*
//         * create bounds for box that encompasses all lines of text
//         */
//        Rectangle2D.Double bounds = new Rectangle2D.Double(0.0, 0.0, maxWidth, (height*text.length));
//
//        /*
//         * Adjust vertical position of box
//         */
//        double vertAdjust = height * text.length * 0.5;
//
//        /*
//         * Calculate rotation offsets
//         */
//        double scaleCosine = getScaleX() * Math.cos(Math.toRadians(90-rotation));
//        double scaleSine = getScaleY() * Math.sin(Math.toRadians(90-rotation));
//
//        /*
//         * Draw a blank at the appropriate position to create
//         * an encompassing bounds for box and/or mask
//         */
//        RGB textColor;
//        if (textStyle == TextStyle.BOXED || textStyle == TextStyle.BLANKED
//                                                 || textStyle == TextStyle.OUTLINE ) {
//                textColor = (colors == null || colors[0] == null ? DEFAULT_LABEL_COLOR
//                                : colors[0]);
//                drawString(font, new String(" "), x + (vertAdjust * scaleCosine), y + (vertAdjust * scaleSine), z,
//                                textStyle, textColor, alignment, VerticalAlignment.BOTTOM, rotation, (Rectangle2D)bounds);
//        }
//
//        /*
//         * Draw all lines in NORMAL mode ( no box or mask )
//         */
//        double xpos = x - (vertAdjust * scaleCosine );
//        double xinc = height * scaleCosine;
//        double ypos = y - (vertAdjust * scaleSine);
//        double yinc = height * scaleSine ;
//        for (int i = 0; i < text.length; i++) {
//            textColor = (colors == null || colors[i] == null ? DEFAULT_LABEL_COLOR
//                    : colors[i]);
//            drawString(font, text[i], xpos += xinc, ypos += yinc, z,
//                    TextStyle.NORMAL, textColor, alignment, rotation);
//            //System.out.println("SAG xpos = "+xpos+"    ypos = "+ ypos);
//        }
//    }
//
//    @Override
//    public void setRotateTextAroundPoint( boolean flag ) {
//        this.rotateTextAroundPoint = flag;
//    }
//    
//    public void setColorbar( ColorBar cbar ) {
//    	colorBar = cbar;
//    }
//}