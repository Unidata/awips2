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
 * Element classes of CGM commands.
 * @version $Id$ 
 * @author  Philippe Cadé
 * @since Dec 15, 2009
 */
public enum ElementClass { 
	DELIMITER_ELEMENTS(0),
	METAFILE_DESCRIPTOR_ELEMENTS(1),
	PICTURE_DESCRIPTOR_ELEMENTS(2),
	CONTROL_ELEMENTS(3),
	GRAPHICAL_PRIMITIVE_ELEMENTS(4),
	ATTRIBUTE_ELEMENTS(5),
	ESCAPE_ELEMENTS(6),
	EXTERNAL_ELEMENTS(7),
	SEGMENT_ELEMENTS(8),
	APPLICATION_STRUCTURE_ELEMENTS(9);
	
	private final int elementClass;
	
	private ElementClass(int c) {
		elementClass = c;
	}

	/**
	 * Returns the element class for the given class number
	 * @param ec The class number to get
	 * @return The corresponding element class
	 */
	public static ElementClass getElementClass(int ec) {
		if (ec < 0 || ec >= values().length)
			throw new ArrayIndexOutOfBoundsException(ec);
		
		return values()[ec];
	}

	/**
	 * Returns the element for the given element class and element code
	 * 
	 * @param elementClass
	 *            The class number to get
	 * @param elementCode
	 *            The class code to get (depends on the class number)
	 * @return The element as an object, will be one of the element code
	 *         enumerations.
	 */
	public static Object getElement(int elementClass, int elementCode) {
		ElementClass clazz = getElementClass(elementClass);
		return clazz.getElementCode(elementCode);
	}

	/**
	 * Returns the element code 
	 * @param elementCode
	 * @return
	 */
	private Object getElementCode(int elementCode) {
		switch (this) {
		case DELIMITER_ELEMENTS: // 0
			return DelimiterElement.getElement(elementCode);
		case METAFILE_DESCRIPTOR_ELEMENTS: // 1
			return MetafileDescriptorElement.getElement(elementCode);
		case PICTURE_DESCRIPTOR_ELEMENTS: // 2
			return PictureDescriptorElement.getElement(elementCode);
		case CONTROL_ELEMENTS: // 3
			return ControlElement.getElement(elementCode);
		case GRAPHICAL_PRIMITIVE_ELEMENTS: // 4
			return GraphicalPrimitiveElements.getElement(elementCode);
		case ATTRIBUTE_ELEMENTS: // 5
			return AttributeElement.getElement(elementCode);
		case EXTERNAL_ELEMENTS: // 7
			return ExternalElements.getElement(elementCode);
		}
		return "null";
	}

	public String toString() {
		return name().concat("(").concat(String.valueOf(elementClass)).concat(")");
	}

}
