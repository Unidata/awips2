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

import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.font.GlyphVector;
import java.awt.geom.Point2D;
import java.awt.geom.Point2D.Double;
import java.io.DataInput;
import java.io.IOException;
import java.io.UnsupportedEncodingException;


/**
 * Class=4, Element=4
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class Text extends TextCommand {
	
	protected byte nextByte = 0;
	
	public Text(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		this.position = makePoint();

		//ORIGINAL//int finalNotFinal = makeEnum();
		//int finalNotFinal = makeEnum();

		this.string = makeString();

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	//ORIGINAL DOES NOT CONTAIN...
	@Override
	protected String makeString() {
		int length = this.args.length - 4; // getStringCount();
		//if (length % 2 == 1) length++;
		byte[] c = new byte[length];
		for (int i = 0; i < length; i++) {
			c[i] = makeByte();
		}

		//CHANGE
		//if (length % 2 == 1) {
		//	nextByte = makeByte();
		//}
		
		try {
			return new String(c, "ISO8859-1");
		}
		catch (UnsupportedEncodingException e) {
			return new String(c);
		}
	}

	@Override
	Double getTextOffset(CGMDisplay d) {
		return new Point2D.Double(0, 0);
	}

	@Override
	protected void scaleText(CGMDisplay d, FontMetrics fontMetrics,
			GlyphVector glyphVector, double width, double height) {
		Graphics2D g2d = d.getGraphics2D();

		double characterHeight = d.getCharacterHeight() / height;
		g2d.scale(characterHeight, characterHeight);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Text position=");
		sb.append(this.position);
		sb.append(" string=").append(this.string);
		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
