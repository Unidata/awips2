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
import java.io.DataInput;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


/**
 * Class=1, Element=13
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class FontList extends Command {
	String fontNames[];
	FontWrapper[] fonts;

	static Map<String, FontWrapper> fontMapping;

	final static int DEFAULT_FONT_SIZE = 32;

	static {
		fontMapping = new HashMap<String, FontWrapper>();
		fontMapping.put("times-roman",				new FontWrapper(new Font(Font.SERIF, Font.PLAIN, DEFAULT_FONT_SIZE), false));
		fontMapping.put("times-bold",				new FontWrapper(new Font(Font.SERIF, Font.BOLD, DEFAULT_FONT_SIZE), false));
		fontMapping.put("times-italic",				new FontWrapper(new Font(Font.SERIF, Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("times-bolditalic",			new FontWrapper(new Font(Font.SERIF, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("times-bold-italic",		new FontWrapper(new Font(Font.SERIF, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));

		fontMapping.put("helvetica",				new FontWrapper(new Font(Font.SANS_SERIF, Font.PLAIN, DEFAULT_FONT_SIZE), false));
		fontMapping.put("helvetica-bold",			new FontWrapper(new Font(Font.SANS_SERIF, Font.BOLD, DEFAULT_FONT_SIZE), false));
		fontMapping.put("helvetica-oblique",		new FontWrapper(new Font(Font.SANS_SERIF, Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("helvetica-boldoblique",	new FontWrapper(new Font(Font.SANS_SERIF, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("helvetica-bold-oblique",	new FontWrapper(new Font(Font.SANS_SERIF, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));

		fontMapping.put("courier",					new FontWrapper(new Font(Font.MONOSPACED, Font.PLAIN, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-bold",				new FontWrapper(new Font(Font.MONOSPACED, Font.BOLD, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-italic",			new FontWrapper(new Font(Font.MONOSPACED, Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-oblique",			new FontWrapper(new Font(Font.MONOSPACED, Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-bolditalic",		new FontWrapper(new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-boldoblique",		new FontWrapper(new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-bold-italic",		new FontWrapper(new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));
		fontMapping.put("courier-bold-oblique",		new FontWrapper(new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, DEFAULT_FONT_SIZE), false));

		// this has to be a font that is able to display all characters, typically a unicode font
		fontMapping.put("symbol",					new FontWrapper(new Font(Font.SERIF, Font.PLAIN, DEFAULT_FONT_SIZE), true));
	}

	public FontList(int ec, int eid, int l, DataInput in)
			throws IOException {
		super(ec, eid, l, in);
		int count = 0, i = 0;
		while (i < this.args.length) {
			count++;
			i += this.args[i] + 1;
		}
		this.fontNames = new String[count];
		count = 0;
		i = 0;
		while (i < this.args.length) {
			char a[] = new char[this.args[i]];
			for (int j = 0; j < this.args[i]; j++)
				a[j] = (char) this.args[i + j + 1];
			this.fontNames[count] = new String(a);
			count++;
			i += this.args[i] + 1;
		}

		this.fonts = new FontWrapper[this.fontNames.length];
		i = 0;
		for (String fontName: this.fontNames) {
			FontWrapper mappedFont = fontMapping.get(normalizeFontName(fontName));
			if (mappedFont != null) {
				this.fonts[i++] = mappedFont;
			}
			else {
				Font decodedFont = Font.decode(fontName);
				// XXX: assume non symbolic encoding, is that right?
				this.fonts[i++] = new FontWrapper(decodedFont.deriveFont(Float.valueOf(DEFAULT_FONT_SIZE)), false);
			}
		}
	}

	private String normalizeFontName(String fontName) {
		return fontName.toLowerCase().replace('_', '-');
	}

	@Override
	public void paint(CGMDisplay d) {
		d.setFonts(this.fonts);
	}

	@Override
	public String toString() {
		String s = "FontList ";
		for (int i = 0; i < this.fontNames.length - 1; i++)
			s = s + this.fontNames[i] + ", ";
		s = s + this.fontNames[this.fontNames.length - 1];
		return s;
	}
}

/*
 * vim:encoding=utf8
 */
