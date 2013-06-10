/*
 * <copyright>
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
 * Element class 3: Control Elements
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum ControlElement {
	UNUSED_0(0),
	VDC_INTEGER_PRECISION(1),
	VDC_REAL_PRECISION(2),
	AUXILIARY_COLOUR(3),
	TRANSPARENCY(4),
	CLIP_RECTANGLE(5),
	CLIP_INDICATOR(6),
	LINE_CLIPPING_MODE(7),
	MARKER_CLIPPING_MODE(8),
	EDGE_CLIPPING_MODE(9),
	NEW_REGION(10),
	SAVE_PRIMITIVE_CONTEXT(11),
	RESTORE_PRIMITIVE_CONTEXT(12),
	UNUSED_13(13),
	UNUSED_14(14),
	UNUSED_15(15),
	UNUSED_16(16),
	PROTECTION_REGION_INDICATOR(17),
	GENERALIZED_TEXT_PATH_MODE(18),
	MITRE_LIMIT(19),
	TRANSPARENT_CELL_COLOUR(20);

	private final int elementCode;

	ControlElement(int ec) {
		elementCode = ec;
	}

	public static ControlElement getElement(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);

		return values()[ec];
	}

	public String toString() {
		return name().concat("(").concat(String.valueOf(elementCode)).concat(")");
	}

}
