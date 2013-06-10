/**
 *  Adapted from jcgm class Command; original copyright below
 *  
 *  Factory-like methods have been modified to generate "Nc" versions of
 *  commands, where they exist.
 */
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

package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.EOFException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.*;

/**
 * Base class for all the CGM commands.
 * <p>
 * Notes from the Java Language Reference: The integral types are byte, short,
 * int, and long, whose values are 8-bit, 16-bit, 32-bit and 64-bit signed
 * two's-complement integers, respectively, and char, whose values are 16-bit
 * unsigned integers representing Unicode characters.
 *
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class NcCommand extends Command implements Cloneable {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	public NcCommand(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
	}

	/**
	 * Reads one command from the given input stream.
	 * @param in Where to read the command from
	 * @return The command or {@code null} if the end of stream was found
	 * @throws IOException On I/O error
	 */
	public static Command read(DataInput in) throws IOException {
		int k;
		try {
			k = in.readUnsignedByte();
			k = (k << 8) | in.readUnsignedByte();
		}
		catch (EOFException e) {
			return null;
		}

		// the element class
		int ec = k >> 12;
			int eid = (k >> 5) & 127;
			int l = k & 31;
			return readCommand(in, ec, eid, l);
	}

	protected static Command readCommand(DataInput in, int ec, int eid, int l) throws IOException {
		switch (ElementClass.getElementClass(ec)) {

		case DELIMITER_ELEMENTS: // 0
			return readDelimiterElements(in, ec, eid, l);

		case METAFILE_DESCRIPTOR_ELEMENTS: // 1
			return readMetaFileDescriptorElements(in, ec, eid, l);

		case PICTURE_DESCRIPTOR_ELEMENTS: // 2
			return readPictureDescriptorElements(in, ec, eid, l);

		case CONTROL_ELEMENTS: // 3
			return readControlElements(in, ec, eid, l);

		case GRAPHICAL_PRIMITIVE_ELEMENTS: // 4
			return readGraphicalPrimitiveElements(in, ec, eid, l);

		case ATTRIBUTE_ELEMENTS: // 5
			return readAttributeElements(in, ec, eid, l);

		case ESCAPE_ELEMENTS: // 6
			return new Escape(ec, eid, l, in);

		case EXTERNAL_ELEMENTS: // 7
			return readExternalElements(in, ec, eid, l);

		case SEGMENT_ELEMENTS: // 8
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case APPLICATION_STRUCTURE_ELEMENTS: // 9
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		default:
			assert (10 <= ec && ec <= 15) : "//TODO  unsupported element class";
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 0
	private static Command readDelimiterElements(DataInput in, int ec, int eid, int l)
	throws IOException {
		// Delimiter elements
		switch (DelimiterElement.getElement(eid)) {

		// 0, 0
		case NO_OP:
			return new NoOp(ec, eid, l, in);

			// 0, 1
		case BEGIN_METAFILE:
			return new BeginMetafile(ec, eid, l, in);

			// 0, 2
		case END_METAFILE:
			return new EndMetafile(ec, eid, l, in);

			// 0, 3
		case BEGIN_PICTURE:
			return new BeginPicture(ec, eid, l, in);

			// 0, 4
		case BEGIN_PICTURE_BODY:
			return new BeginPictureBody(ec, eid, l, in);

			// 0, 5
		case END_PICTURE:
			return new EndPicture(ec, eid, l, in);

			// 0, 6
		case BEGIN_SEGMENT:
			// 0, 7
		case END_SEGMENT:
			// 0, 8
		case BEGIN_FIGURE:
			// 0, 9
		case END_FIGURE:
			// 0, 13
		case BEGIN_PROTECTION_REGION:
			// 0, 14
		case END_PROTECTION_REGION:
			// 0, 15
		case BEGIN_COMPOUND_LINE:
			// 0, 16
		case END_COMPOUND_LINE:
			// 0, 17
		case BEGIN_COMPOUND_TEXT_PATH:
			// 0, 18
		case END_COMPOUND_TEXT_PATH:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

			// 0, 19
		case BEGIN_TILE_ARRAY:
			return new BeginTileArray(ec, eid, l, in);

			// 0, 20
		case END_TILE_ARRAY:
			return new EndTileArray(ec, eid, l, in);

			// 0, 21
		case BEGIN_APPLICATION_STRUCTURE:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

			// 0, 22
		case BEGIN_APPLICATION_STRUCTURE_BODY:
			return new BeginApplicationStructureBody(ec, eid, l, in);

			// 0, 23
		case END_APPLICATION_STRUCTURE:
			return new EndApplicationStructure(ec, eid, l, in);

		default:
			assert false : "unsupported element ID=" + eid;
		return new NcCommand(ec, eid, l, in);
		}
	}

	// Class: 1
	private static Command readMetaFileDescriptorElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (MetafileDescriptorElement.getElement(eid)) {

		case METAFILE_VERSION: // 1
			return new MetafileVersion(ec, eid, l, in);

		case METAFILE_DESCRIPTION: // 2
			return new MetafileDescription(ec, eid, l, in);

		case VDC_TYPE: // 3
			return new VDCType(ec, eid, l, in);

		case INTEGER_PRECISION: // 4
			return new IntegerPrecision(ec, eid, l, in);

		case REAL_PRECISION: // 5
			return new RealPrecision(ec, eid, l, in);

		case INDEX_PRECISION: // 6
			return new IndexPrecision(ec, eid, l, in);

		case COLOUR_PRECISION: // 7
			return new ColourPrecision(ec, eid, l, in);

		case COLOUR_INDEX_PRECISION: // 8
			return new ColourIndexPrecision(ec, eid, l, in);

		case MAXIMUM_COLOUR_INDEX: // 9
			return new MaximumColourIndex(ec, eid, l, in);

		case COLOUR_VALUE_EXTENT: // 10
			return new ColourValueExtent(ec, eid, l, in);

		case METAFILE_ELEMENT_LIST: // 11
			return new MetafileElementList(ec, eid, l, in);

		case METAFILE_DEFAULTS_REPLACEMENT: // 12
			return new MetafileDefaultsReplacement(ec, eid, l, in);

		case FONT_LIST: // 13
			return new FontList(ec, eid, l, in);

		case CHARACTER_SET_LIST: // 14
			return new CharacterSetList(ec, eid, l, in);

		case CHARACTER_CODING_ANNOUNCER: // 15
			return new CharacterCodingAnnouncer(ec, eid, l, in);

		case NAME_PRECISION: // 16
			return new NamePrecision(ec, eid, l, in);

		case MAXIMUM_VDC_EXTENT: // 17
			return new MaximumVDCExtent(ec, eid, l, in);

		case SEGMENT_PRIORITY_EXTENT: // 18
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case COLOUR_MODEL: // 19
			return new ColourModel(ec, eid, l, in);

		case COLOUR_CALIBRATION: // 20
		case FONT_PROPERTIES: // 21
		case GLYPH_MAPPING: // 22
		case SYMBOL_LIBRARY_LIST: // 23
		case PICTURE_DIRECTORY: // 24
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		default:
			assert false : "unsupported element ID="+eid;
		return new Command(ec, eid, l, in);
		}
	}

	// Class: 2
	private static Command readPictureDescriptorElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (PictureDescriptorElement.getElement(eid)) {
		// 2, 1
		case SCALING_MODE:
			return new ScalingMode(ec, eid, l, in);

			// 2, 2
		case COLOUR_SELECTION_MODE:
			return new ColourSelectionMode(ec, eid, l, in);

			// 2, 3
		case LINE_WIDTH_SPECIFICATION_MODE:
			return new LineWidthSpecificationMode(ec, eid, l, in);

			// 2, 4
		case MARKER_SIZE_SPECIFICATION_MODE:
			return new MarkerSizeSpecificationMode(ec, eid, l, in);

			// 2, 5
		case EDGE_WIDTH_SPECIFICATION_MODE:
			return new EdgeWidthSpecificationMode(ec, eid, l, in);

			// 2, 6
		case VDC_EXTENT:
			return new VDCExtent(ec, eid, l, in);

			// 2, 7
		case BACKGROUND_COLOUR:
			return new BackgroundColour(ec, eid, l, in);

			// 2, 8
		case DEVICE_VIEWPORT:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

			// 2, 9
		case DEVICE_VIEWPORT_SPECIFICATION_MODE:
			return new DeviceViewportSpecificationMode(ec, eid, l, in);

			// 2, 10
		case DEVICE_VIEWPORT_MAPPING:
			// 2, 11
		case LINE_REPRESENTATION:
			// 2, 12
		case MARKER_REPRESENTATION:
			// 2, 13
		case TEXT_REPRESENTATION:
			// 2, 14
		case FILL_REPRESENTATION:
			// 2, 15:
		case EDGE_REPRESENTATION:
			//TODO  unsupported(ec, eid);
			return new NcCommand(ec, eid, l, in);

			// 2, 16
		case INTERIOR_STYLE_SPECIFICATION_MODE:
			return new InteriorStyleSpecificationMode(ec, eid, l, in);

			// 2, 17
		case LINE_AND_EDGE_TYPE_DEFINITION:
			return new LineAndEdgeTypeDefinition(ec, eid, l, in);

			// 2, 18
		case HATCH_STYLE_DEFINITION:
			// 2, 19
		case GEOMETRIC_PATTERN_DEFINITION:
			// 2, 20
		case APPLICATION_STRUCTURE_DIRECTORY:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		default:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 3
	private static Command readControlElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (ControlElement.getElement(eid)) {
		case VDC_INTEGER_PRECISION:
			return new VDCIntegerPrecision(ec, eid, l, in);
		case VDC_REAL_PRECISION:
			return new VDCRealPrecision(ec, eid, l, in);
		case CLIP_RECTANGLE:
			return new ClipRectangle(ec, eid, l, in);
		case CLIP_INDICATOR:
			return new ClipIndicator(ec, eid, l, in);
		default:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 4
	private static Command readGraphicalPrimitiveElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (GraphicalPrimitiveElements.getElement(eid)) {

		case POLYLINE: // 1
			return new NcPolyline(ec, eid, l, in);

		case DISJOINT_POLYLINE: // 2
			return new DisjointPolyline(ec, eid, l, in);

		case POLYMARKER: // 3
			return new PolyMarker(ec, eid, l, in);

		case TEXT: // 4
			return new NcText(ec, eid, l, in);

		case RESTRICTED_TEXT: // 5
			return new RestrictedText(ec, eid, l, in);

		case APPEND_TEXT: // 6
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case POLYGON: // 7
			return new NcPolygonElement(ec, eid, l, in);

		case POLYGON_SET: // 8
			return new PolygonSet(ec, eid, l, in);

		case CELL_ARRAY: // 9
			return new CellArray(ec, eid, l, in);

		case GENERALIZED_DRAWING_PRIMITIVE: // 10
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case RECTANGLE: // 11
			return new RectangleElement(ec, eid, l, in);

		case CIRCLE: // 12
			return new NcCircleElement(ec, eid, l, in);

		case CIRCULAR_ARC_3_POINT: // 13
			return new CircularArc3Point(ec, eid, l, in);

		case CIRCULAR_ARC_3_POINT_CLOSE: // 14
			return new CircularArc3PointClose(ec, eid, l, in);

		case CIRCULAR_ARC_CENTRE: // 15
			return new CircularArcCentre(ec, eid, l, in);

		case CIRCULAR_ARC_CENTRE_CLOSE: // 16
			return new CircularArcCentreClose(ec, eid, l, in);

		case ELLIPSE: // 17
			return new EllipseElement(ec, eid, l, in);

		case ELLIPTICAL_ARC: // 18
			return new EllipticalArc(ec, eid, l, in);

		case ELLIPTICAL_ARC_CLOSE: // 19
			return new EllipticalArcClose(ec, eid, l, in);

		case CIRCULAR_ARC_CENTRE_REVERSED: // 20
		case CONNECTING_EDGE: // 21
		case HYPERBOLIC_ARC: // 22
		case PARABOLIC_ARC: // 23
		case NON_UNIFORM_B_SPLINE: // 24
		case NON_UNIFORM_RATIONAL_B_SPLINE: // 25
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case POLYBEZIER: // 26
			return new PolyBezier(ec, eid, l, in);

		case POLYSYMBOL: // 27
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case BITONAL_TILE: // 28
			return new BitonalTile(ec, eid, l, in);

		case TILE: // 29
			return new Tile(ec, eid, l, in);

		default:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 5
	private static Command readAttributeElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (AttributeElement.getElement(eid)) {
		case LINE_BUNDLE_INDEX: // 1
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case LINE_TYPE: // 2
			return new LineType(ec, eid, l, in);

		case LINE_WIDTH: // 3
			return new NcLineWidth(ec, eid, l, in);

		case LINE_COLOUR: // 4
			return new NcLineColour(ec, eid, l, in);

		case MARKER_BUNDLE_INDEX: // 5
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case MARKER_TYPE: // 6
			return new MarkerType(ec, eid, l, in);

		case MARKER_SIZE: // 7
			return new MarkerSize(ec, eid, l, in);

		case MARKER_COLOUR: // 8
			return new MarkerColour(ec, eid, l, in);

		case TEXT_BUNDLE_INDEX: // 9:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case TEXT_FONT_INDEX: // 10
			return new NcTextFontIndex(ec, eid, l, in);

		case TEXT_PRECISION: // 11
			return new TextPrecision(ec, eid, l, in);

		case CHARACTER_EXPANSION_FACTOR: // 12
			return new CharacterExpansionFactor(ec, eid, l, in);

		case CHARACTER_SPACING: // 13
			return new CharacterSpacing(ec, eid, l, in);

		case TEXT_COLOUR: // 14
			return new NcTextColour(ec, eid, l, in);

		case CHARACTER_HEIGHT: // 15
			return new NcCharacterHeight(ec, eid, l, in);

		case CHARACTER_ORIENTATION: // 16
			return new CharacterOrientation(ec, eid, l, in);

		case TEXT_PATH: // 17
			return new TextPath(ec, eid, l, in);

		case TEXT_ALIGNMENT: // 18
			return new NcTextAlignment(ec, eid, l, in);

		case CHARACTER_SET_INDEX: // 19
			return new CharacterSetIndex(ec, eid, l, in);

		case ALTERNATE_CHARACTER_SET_INDEX: // 20
			return new AlternateCharacterSetIndex(ec, eid, l, in);

		case FILL_BUNDLE_INDEX: // 21
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case INTERIOR_STYLE: // 22
			return new NcInteriorStyle(ec, eid, l, in);

		case FILL_COLOUR: // 23
			return new NcFillColour(ec, eid, l, in);

		case HATCH_INDEX: // 24
			return new HatchIndex(ec, eid, l, in);

		case PATTERN_INDEX: // 25
		case EDGE_BUNDLE_INDEX: // 26
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case EDGE_TYPE: // 27
			return new EdgeType(ec, eid, l, in);

		case EDGE_WIDTH: // 28
			return new EdgeWidth(ec, eid, l, in);

		case EDGE_COLOUR: // 29
			return new EdgeColour(ec, eid, l, in);

		case EDGE_VISIBILITY: // 30
			return new EdgeVisibility(ec, eid, l, in);

		case FILL_REFERENCE_POINT: // 31
		case PATTERN_TABLE: // 32
		case PATTERN_SIZE: // 33
			//TODO  unsupported(ec, eid);
			return new NcCommand(ec, eid, l, in);

		case COLOUR_TABLE: // 34
			return new ColourTable(ec, eid, l, in);

		case ASPECT_SOURCE_FLAGS: // 35
		case PICK_IDENTIFIER: // 36
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case LINE_CAP: // 37
			return new LineCap(ec, eid, l, in);

		case LINE_JOIN: // 38
			return new LineJoin(ec, eid, l, in);

		case LINE_TYPE_CONTINUATION: // 39
		case LINE_TYPE_INITIAL_OFFSET: // 40
		case TEXT_SCORE_TYPE: // 41
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case RESTRICTED_TEXT_TYPE: // 42
			return new RestrictedTextType(ec, eid, l, in);

		case INTERPOLATED_INTERIOR: // 43
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case EDGE_CAP: // 44
			return new EdgeCap(ec, eid, l, in);

		case EDGE_JOIN: // 45
			return new EdgeJoin(ec, eid, l, in);

		case EDGE_TYPE_CONTINUATION: // 46
		case EDGE_TYPE_INITIAL_OFFSET: // 47
		case SYMBOL_LIBRARY_INDEX: // 48
		case SYMBOL_COLOUR: // 49
		case SYMBOL_SIZE: // 50
		case SYMBOL_ORIENTATION: // 51
		default:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 7
	private static Command readExternalElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (ExternalElements.getElement(eid)) {
		case MESSAGE: // 1
			return new MessageCommand(ec, eid, l, in);

		case APPLICATION_DATA: // 2
			return new ApplicationData(ec, eid, l, in);
		default:
			//TODO  unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

}

/*
 * vim:encoding=utf8
 */
