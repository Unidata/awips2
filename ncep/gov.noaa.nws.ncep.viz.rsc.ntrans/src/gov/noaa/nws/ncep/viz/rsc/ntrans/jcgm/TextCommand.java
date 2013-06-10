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

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D.Double;
import java.io.DataInput;
import java.io.IOException;

/**
 * Common class for text support
 * @author Philippe Cadé
 * @version $Id$
 */
public abstract class TextCommand extends Command {

	/** The string to display */
	protected String string;
	
	/** The position at which the string should be displayed */
	protected Point2D.Double position;

	public TextCommand(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
	}
	
	/**
	 * Returns an offset to apply to the defined text position
	 * @param d
	 * @return Offset to apply to the text position
	 */
	abstract Double getTextOffset(CGMDisplay d);

	protected void scaleText(CGMDisplay d, FontMetrics fontMetrics,
			GlyphVector glyphVector, double width, double height) {
		// default: do not scale
	}

	@Override
	public void paint(CGMDisplay d) {
		if (this.string.length() == 0) {
			// ignore empty strings
			return;
		}

		Graphics2D g2d = d.getGraphics2D();

		// save the transformation since we are going to apply another one that
		// is specific to this string
		AffineTransform savedTransform = g2d.getTransform();

		AffineTransform coordinateSystemTransformation = d
				.getCoordinateSystemTransformation(this.position, d
						.getCharacterOrientationBaselineVector(), d
						.getCharacterOrientationUpVector());

		AffineTransform textTransform = d.getTextTransform();
		coordinateSystemTransformation.concatenate(textTransform);

		g2d.transform(coordinateSystemTransformation);

		Point2D.Double textOrigin = getTextOffset(d);
		g2d.translate(textOrigin.x, textOrigin.y);

		g2d.setColor(d.getTextColor());

		String decodedString = d.useSymbolEncoding() ? SymbolDecoder
				.decode(this.string) : this.string;

		// the text path left is easy: just flip the string
		if (TextPath.Type.LEFT.equals(d.getTextPath())) {
			decodedString = flipString(decodedString);
		}

		Font font = g2d.getFont();

		// adjust the size of the font depending on the extent. If the extent is
		// very big, having small font sizes may create problems
		Point2D.Double[] extent = d.getExtent();
		Font adjustedFont = font.deriveFont((float) (Math.abs(extent[0].y
				- extent[1].y) / 100));
		g2d.setFont(adjustedFont);
		FontRenderContext fontRenderContext = g2d.getFontRenderContext();
		GlyphVector glyphVector = adjustedFont.createGlyphVector(
				fontRenderContext, decodedString);
		Rectangle2D logicalBounds = glyphVector.getLogicalBounds();

		FontMetrics fontMetrics = g2d.getFontMetrics(adjustedFont);
		// XXX: unfortunately, getAscent() does not return correct values,
		// see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6623223
		// so we are always going to be a bit off
		int screenResolution;
		if (GraphicsEnvironment.isHeadless()) {
			// if we're in a headless environment, assume 96 dots per inch
			// (default setting for Windows XP)
			screenResolution = 96;
		} else {
			screenResolution = Toolkit.getDefaultToolkit()
					.getScreenResolution();
		}
		double height = fontMetrics.getAscent() * 72 / screenResolution;

		scaleText(d, fontMetrics, glyphVector, logicalBounds.getWidth(), height);

		if (TextPath.Type.UP.equals(d.getTextPath())
				|| TextPath.Type.DOWN.equals(d.getTextPath())) {
			applyTextPath(d, glyphVector);
		}

		g2d.drawGlyphVector(glyphVector, 0, 0);

		// restore the transformation that existed before painting the string
		g2d.setTransform(savedTransform);
	}

	/**
	 * Flip the given string for left text path
	 * @param s
	 * @return
	 */
	protected String flipString(String s) {
		StringBuilder sb = new StringBuilder();
		for (int i = s.length() - 1; i >= 0; i--) {
			sb.append(s.charAt(i));
		}
		return sb.toString();
	}

	/**
	 * @param glyphVector
	 */
	protected void applyTextPath(CGMDisplay d, GlyphVector glyphVector) {
		double height = glyphVector.getLogicalBounds().getHeight();
		
		if (TextPath.Type.DOWN.equals(d.getTextPath())) {
			float[] glyphPositions = glyphVector.getGlyphPositions(0, glyphVector.getNumGlyphs(), null);
			
			int glyphIndex = 0;
			for (int i = 0; i < (glyphPositions.length / 2); i++) {
				Point2D.Float newPos = new Point2D.Float(glyphPositions[0], (float)(i*height));
				glyphVector.setGlyphPosition(glyphIndex++, newPos);
			}
		}
		else if (TextPath.Type.DOWN.equals(d.getTextPath())) {
			
		}
	}

    public String getString() {
        return string;
    }
}