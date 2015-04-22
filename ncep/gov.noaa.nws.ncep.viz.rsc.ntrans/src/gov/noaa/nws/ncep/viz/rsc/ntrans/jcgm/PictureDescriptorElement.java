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
 * Element class 2: Picture Descriptor Elements 
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum PictureDescriptorElement {
	UNUSED_0(0),
	SCALING_MODE(1),
	COLOUR_SELECTION_MODE(2),
	LINE_WIDTH_SPECIFICATION_MODE(3),
	MARKER_SIZE_SPECIFICATION_MODE(4),
	EDGE_WIDTH_SPECIFICATION_MODE(5),
	VDC_EXTENT(6),
	BACKGROUND_COLOUR(7),
	DEVICE_VIEWPORT(8),
	DEVICE_VIEWPORT_SPECIFICATION_MODE(9),
	DEVICE_VIEWPORT_MAPPING(10),
	LINE_REPRESENTATION(11),
	MARKER_REPRESENTATION(12),
	TEXT_REPRESENTATION(13),
	FILL_REPRESENTATION(14),
	EDGE_REPRESENTATION(15),
	INTERIOR_STYLE_SPECIFICATION_MODE(16),
	LINE_AND_EDGE_TYPE_DEFINITION(17),
	HATCH_STYLE_DEFINITION(18),
	GEOMETRIC_PATTERN_DEFINITION(19),
	APPLICATION_STRUCTURE_DIRECTORY(20);

	private final int elementCode;

	PictureDescriptorElement(int ec) {
		elementCode = ec;
	}

	public static PictureDescriptorElement getElement(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);

		return values()[ec];
	}

	public String toString() {
		return name().concat("(").concat(String.valueOf(elementCode)).concat(")");
	}

}
