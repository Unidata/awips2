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
 * Element class 4: Graphical Primitive Elements
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum GraphicalPrimitiveElements {
	UNUSED_0(0),
	POLYLINE(1),
	DISJOINT_POLYLINE(2),
	POLYMARKER(3),
	TEXT(4),
	RESTRICTED_TEXT(5),
	APPEND_TEXT(6),
	POLYGON(7),
	POLYGON_SET(8),
	CELL_ARRAY(9),
	GENERALIZED_DRAWING_PRIMITIVE(10),
	RECTANGLE(11),
	CIRCLE(12),
	CIRCULAR_ARC_3_POINT(13),
	CIRCULAR_ARC_3_POINT_CLOSE(14),
	CIRCULAR_ARC_CENTRE(15),
	CIRCULAR_ARC_CENTRE_CLOSE(16),
	ELLIPSE(17),
	ELLIPTICAL_ARC(18),
	ELLIPTICAL_ARC_CLOSE(19),
	CIRCULAR_ARC_CENTRE_REVERSED(20),
	CONNECTING_EDGE(21),
	HYPERBOLIC_ARC(22),
	PARABOLIC_ARC(23),
	NON_UNIFORM_B_SPLINE(24),
	NON_UNIFORM_RATIONAL_B_SPLINE(25),
	POLYBEZIER(26),
	POLYSYMBOL(27),
	BITONAL_TILE(28),
	TILE(29);

	private final int elementCode;

	GraphicalPrimitiveElements(int ec) {
		elementCode = ec;
	}

	public static GraphicalPrimitiveElements getElement(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);

		return values()[ec];
	}

	public String toString() {
		return name().concat("(").concat(String.valueOf(elementCode)).concat(")");
	}

}
