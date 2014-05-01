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

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.io.DataInput;
import java.io.IOException;


/**
 * Cell Array.
 * Class=4, Element=9
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CellArray extends Command {
	private final int representationFlag;
	private final int nx;
	private final int ny;
	private final Point2D p;
	private final Point2D q;
	private final Point2D r;

	// either the colors are filled or the colorIndexes depending on the color selection mode
	private Color[] colors;
	private int[] colorIndexes;
	private BufferedImage bufferedImage = null;

	public CellArray(int ec, int eid, int l, DataInput in)
			throws IOException {
		super(ec, eid, l, in);

		// 3P, 3I, E, CLIST
		this.p = makePoint();
		this.q = makePoint();
		this.r = makePoint();
		this.nx = makeInt(); // number of cells per row
		this.ny = makeInt(); // number of rows

		int localColorPrecision = makeInt();
		if (localColorPrecision == 0) {
			if (ColourSelectionMode.getType() == ColourSelectionMode.Type.INDEXED) {
				localColorPrecision = ColourIndexPrecision.getPrecision();
			}
			else {
				localColorPrecision = ColourPrecision.getPrecision();
			}
		}

		this.representationFlag = makeEnum();

		int nColor = this.nx * this.ny;
		if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.DIRECT)) {
			this.colors = new Color[nColor];

			if (this.representationFlag == 0) {
				// run length list mode
				int c = 0;
				while (c < nColor) {
					int numColors = makeInt();
					Color color = makeDirectColor();

					// don't directly fill the array with numColors in case we
					// encounter a erroneous CGM file, e.g. SCHEMA03.CGM that
					// returns an incorrect number of colors; only fill at most
					// the number of colors left in the array
					int maxIndex = Math.min(numColors, nColor - c);
					for (int i = 0; i < maxIndex; i++) {
						this.colors[c++] = color;
					}
					if (c > 0 && c % this.nx == 0) {
						// align on word at the end of a line
						alignOnWord();
					}
				}
			}
			else if (this.representationFlag == 1) {
				// packed list mode
				int i = 0;
				for (int row = 0; row < this.ny; row++) {
					for (int col = 0; col < this.nx; col++) {
						this.colors[i++] = makeDirectColor();
					}
					// align on word
					alignOnWord();
				}
			}
			else {
				unsupported("unsupported representation flag "+this.representationFlag);
			}
		}
		else if (ColourSelectionMode.getType().equals(ColourSelectionMode.Type.INDEXED)) {
			this.colorIndexes = new int[nColor];

			if (this.representationFlag == 0) {
				// run length list mode
				int c = 0;
				while (c < nColor) {
					int numColors = makeInt();
					int colorIndex = makeColorIndex(localColorPrecision);

					// don't directly fill the array with numColors in case we
					// encounter a erroneous CGM file, e.g. SCHEMA03.CGM that
					// returns an incorrect number of colors; only fill at most
					// the number of colors left in the array
					int maxIndex = Math.min(numColors, nColor - c);
					for (int i = 0; i < maxIndex; i++) {
						this.colorIndexes[c++] = colorIndex;
					}
					if (c > 0 && c % this.nx == 0) {
						// align on word at the end of a line
						alignOnWord();
					}
				}
			}
			else if (this.representationFlag == 1) {
				// packed list mode
				int i = 0;
				for (int row = 0; row < this.ny; row++) {
					for (int col = 0; col < this.nx; col++) {
						this.colorIndexes[i++] = makeColorIndex(localColorPrecision);
					}
					// align on word
					alignOnWord();
				}
			}
			else {
				unsupported("unsupported representation flag "+this.representationFlag);
			}
		}
		else {
			unsupported("unsupported color selection mode "+ColourSelectionMode.getType());
		}

		// make sure all the arguments were read
		// XXX
		// assert (this.currentArg == this.args.length);
	}

	@Override
	public void paint(CGMDisplay d) {
		// 1. create the image if it hasn't been created yet
		if (this.bufferedImage == null) {
			this.bufferedImage = new BufferedImage(this.nx, this.ny, BufferedImage.TYPE_INT_RGB);
			WritableRaster raster = this.bufferedImage.getRaster();
			int currentPixel = 0;
			for (int row = 0; row < this.ny; row++) {
				for (int col = 0; col < this.nx; col++) {
					Color c = (this.colors != null) ? this.colors[currentPixel++] : d
							.getIndexedColor(this.colorIndexes[currentPixel++]);
					raster.setPixel(col, row, new int[] { c.getRed(), c.getGreen(), c.getBlue() });
				}
			}
			// we don't need the color information anymore. Set them to null to save some memory
			this.colors = null;
			this.colorIndexes = null;
		}

		// 2. the image is then painted on the graphic context, scaled and translated.
		// XXX: shear is not supported yet
		Graphics2D g2d = d.getGraphics2D();

		AffineTransform transform = AffineTransform.getTranslateInstance(this.p.getX(), this.p.getY());
		assert (this.nx != 0 && this.ny != 0);
		double sx = (this.q.getX() - this.p.getX()) / this.nx;
		double sy = (this.q.getY() - this.r.getY()) / this.ny;
		transform.scale(sx, sy);
		g2d.drawRenderedImage(this.bufferedImage, transform);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("CellArray");
		sb.append(" nx=").append(this.nx);
		sb.append(" ny=").append(this.ny);
		sb.append(" representation flag=").append(this.representationFlag);
		sb.append(" p=").append(this.p).append(",");
		sb.append(" q=").append(this.q).append(",");
		sb.append(" r=").append(this.r).append(",");

		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
