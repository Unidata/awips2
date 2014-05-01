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
 * Element class 0: Delimiter Elements
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum DelimiterElement {
	NO_OP(0),
	BEGIN_METAFILE(1),
	END_METAFILE(2),
	BEGIN_PICTURE(3),
	BEGIN_PICTURE_BODY(4),
	END_PICTURE(5),
	BEGIN_SEGMENT(6),
	END_SEGMENT(7),
	BEGIN_FIGURE(8),
	END_FIGURE(9),
	UNUSED_10(10),
	UNUSED_11(11),
	UNUSED_12(12),
	BEGIN_PROTECTION_REGION(13),
	END_PROTECTION_REGION(14),
	BEGIN_COMPOUND_LINE(15),
	END_COMPOUND_LINE(16),
	BEGIN_COMPOUND_TEXT_PATH(17),
	END_COMPOUND_TEXT_PATH(18),
	BEGIN_TILE_ARRAY(19),
	END_TILE_ARRAY(20),
	BEGIN_APPLICATION_STRUCTURE(21),
	BEGIN_APPLICATION_STRUCTURE_BODY(22),
	END_APPLICATION_STRUCTURE(23);

	private final int elementCode;

	DelimiterElement(int ec) {
		elementCode = ec;
	}

    public int getElementCode() {
        return elementCode;
    }

	public static DelimiterElement getElement(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);
		
		return values()[ec];
	}
	
	public String toString() {
		return name().concat("(").concat(String.valueOf(elementCode)).concat(")");
	}
}
