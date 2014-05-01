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

/**
 * Element class 1: Metafile Descriptor Elements
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum MetafileDescriptorElement {
	UNUSED_0(0),
	METAFILE_VERSION(1),
	METAFILE_DESCRIPTION(2),
	VDC_TYPE(3),
	INTEGER_PRECISION(4),
	REAL_PRECISION(5),
	INDEX_PRECISION(6),
	COLOUR_PRECISION(7),
	COLOUR_INDEX_PRECISION(8),
	MAXIMUM_COLOUR_INDEX(9),
	COLOUR_VALUE_EXTENT(10),
	METAFILE_ELEMENT_LIST(11),
	METAFILE_DEFAULTS_REPLACEMENT(12),
	FONT_LIST(13),
	CHARACTER_SET_LIST(14),
	CHARACTER_CODING_ANNOUNCER(15),
	NAME_PRECISION(16),
	MAXIMUM_VDC_EXTENT(17),
	SEGMENT_PRIORITY_EXTENT(18),
	COLOUR_MODEL(19),
	COLOUR_CALIBRATION(20),
	FONT_PROPERTIES(21),
	GLYPH_MAPPING(22),
	SYMBOL_LIBRARY_LIST(23),
	PICTURE_DIRECTORY(24);

	private final int elementCode;

	MetafileDescriptorElement(int ec) {
		elementCode = ec;
	}

	public static MetafileDescriptorElement getElement(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);
		
		return values()[ec];
	}

    public int getElementCode() {
        return elementCode;
    }
	
	public String toString() {
		return name().concat("(").concat(String.valueOf(elementCode)).concat(")");
	}
}
