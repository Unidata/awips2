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

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourModel.Model;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.RealPrecision.Precision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.StructuredDataRecord.StructuredDataType;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.VDCRealPrecision.Type;

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
public class Command implements Cloneable {
	/** All the command parameters */
	protected int args[];

	/** The current command parameter we're reading */
	protected int currentArg = 0;

	/** The current bit in the current argument we're reading */
	private int posInArg = 0;

	private final int elementClass;
	private final int elementCode;

	/**
	 * The base class for all commands.
	 * @param ec The element class
	 * @param eid The element ID
	 * @param l The number of arguments for the command
	 * @param in The input stream used to read the command
	 * @throws IOException
	 */
	public Command(int ec, int eid, int l, DataInput in)
	throws IOException {

		this.elementClass = ec;
		this.elementCode = eid;
		if (l != 31) {
			this.args = new int[l];
			for (int i = 0; i < l; i++)
				this.args[i] = in.readUnsignedByte();
			if (l % 2 == 1) {
				try {
					int skip = in.readUnsignedByte();
				}
				catch (EOFException e) {
					// we've reached the end of the data input. Since we're only
					// skipping data here, the exception can be ignored.
				}
			}
		} else {
			// this is a long form command
			boolean done = true;
			int a = 0;
			do {
				l = read16(in);
				if (l == -1)
					break;
				if ((l & (1 << 15)) != 0) {
					// data is partitioned and it's not the last partition
					done = false;
					// clear bit 15
					l = l & ~(1 << 15);
				}
				else {
					done = true;
				}
				if (this.args == null) {
					this.args = new int[l];
				}
				else {
					// resize the args array
					this.args = Arrays.copyOf(this.args, this.args.length + l);
				}
				for (int i = 0; i < l; i++)
					this.args[a++] = in.readUnsignedByte();

				// align on a word if necessary
				if (l % 2 == 1) {
					int skip = in.readUnsignedByte();
					assert(skip == 0): "skipping data";
				}
			}
			while (!done);
		}
	}

	/**
	 * Removes the reference to the arguments, important for commands that have
	 * large arguments such as cell arrays, etc.
	 */
	public void cleanUpArguments() {
		this.args = null;
	}

	private int read16(DataInput in) throws IOException {
		return (in.readUnsignedByte() << 8) | in.readUnsignedByte();
	}

	@Override
	public String toString() {
		return "Unsupported " + this.elementClass + "," + this.elementCode + " (" + (this.args != null ? this.args.length : "arguments cleared") + ")";
	}

	final protected String makeFixedString() {
		int length = getStringCount();
		char[] c = new char[length];
		for (int i = 0; i < length; i++) {
			c[i] = makeChar();
		}

		return new String(c);
	}

	protected String makeString() {
		int length = getStringCount();
		byte[] c = new byte[length];
		for (int i = 0; i < length; i++) {
			c[i] = makeByte();
		}

		try {
			return new String(c, "ISO8859-1");
		}
		catch (UnsupportedEncodingException e) {
			return new String(c);
		}
	}

	private int getStringCount() {
		int length = makeUInt8();
		if (length == 255) {
			length = makeUInt16();
			if ((length & (1 << 16)) != 0) {
				length = (length << 16) | makeUInt16();
			}
		}
		return length;
	}

	final protected byte makeByte() {
		skipBits();
		assert (this.currentArg < this.args.length);
		return (byte)this.args[this.currentArg++];
	}

	final protected char makeChar() {
		skipBits();
		assert (this.currentArg < this.args.length);
		return (char)this.args[this.currentArg++];
	}

	final protected int makeSignedInt8() {
		skipBits();
		assert(this.currentArg < this.args.length);
		return (byte) this.args[this.currentArg++];
	}

	final protected int makeSignedInt16() {
		skipBits();
		assert(this.currentArg+1 < this.args.length);
		return ((short) (this.args[this.currentArg++] << 8) + this.args[this.currentArg++]);
	}

	final protected int makeSignedInt24() {
		skipBits();
		assert(this.currentArg+2 < this.args.length);
		return (this.args[this.currentArg++] << 16) + (this.args[this.currentArg++] << 8) + this.args[this.currentArg++];
	}

	final protected int makeSignedInt32() {
		skipBits();
		assert(this.currentArg+3 < this.args.length);
		return (this.args[this.currentArg++] << 24) + (this.args[this.currentArg++] << 16) + (this.args[this.currentArg++] << 8) + this.args[this.currentArg++];
	}

	final protected int makeInt() {
		int precision = IntegerPrecision.getPrecision();
		return makeInt(precision);
	}

	final protected int sizeOfInt() {
		int precision = IntegerPrecision.getPrecision();
		return precision / 8;
	}

	final protected int makeIndex() {
		int precision = IndexPrecision.getPrecision();
		return makeInt(precision);
	}

	final protected int makeName() {
		int precision = NamePrecision.getPrecision();
		return makeInt(precision);
	}

	private int makeInt(int precision) {
		skipBits();
		if (precision == 8) {
			return makeSignedInt8();
		}
		if (precision == 16) {
			return makeSignedInt16();
		}
		if (precision == 24) {
			makeSignedInt24();
		}
		if (precision == 32) {
			makeSignedInt32();
		}

		unsupported("unsupported integer precision "+precision);
		// return default
		return makeSignedInt16();
	}

	final protected int makeUInt(int precision) {
		if (precision == 1) {
			return makeUInt1();
		}
		if (precision == 2) {
			return makeUInt2();
		}
		if (precision == 4) {
			return makeUInt4();
		}
		if (precision == 8) {
			return makeUInt8();
		}
		if (precision == 16) {
			return makeUInt16();
		}
		if (precision == 24) {
			return makeUInt24();
		}
		if (precision == 32) {
			return makeUInt32();
		}

		unsupported("unsupported uint precision "+precision);
		// return default
		return makeUInt8();
	}

	private int makeUInt32() {
		skipBits();
		assert this.currentArg+3 < this.args.length;
		return (char)(this.args[this.currentArg++] << 24) + (char)(this.args[this.currentArg++] << 16) + (char)(this.args[this.currentArg++] << 8) + (char)this.args[this.currentArg++];
	}

	private int makeUInt24() {
		skipBits();
		assert this.currentArg+2 < this.args.length;
		return (char)(this.args[this.currentArg++] << 16) + (char)(this.args[this.currentArg++] << 8) + (char)this.args[this.currentArg++];
	}

	private int makeUInt16() {
		skipBits();

		if (this.currentArg+1 < this.args.length) {
			// this is the default, two bytes
			return (char)(this.args[this.currentArg++] << 8) + (char)this.args[this.currentArg++];
		}

		// some CGM files request a 16 bit precision integer when there are only 8 bits left
		if (this.currentArg < this.args.length) {
			// TODO: add logging
			return (char)this.args[this.currentArg++];
		}

		assert false;
		return 0;
	}

	private int makeUInt8() {
		skipBits();
		assert this.currentArg < this.args.length;
		return (char)this.args[this.currentArg++];
	}

	private int makeUInt4() {
		return makeUIntBit(4);
	}

	private int makeUInt2() {
		return makeUIntBit(2);
	}

	private int makeUInt1() {
		return makeUIntBit(1);
	}

	private int makeUIntBit(int numBits) {
		assert (this.currentArg < this.args.length);

		int bitsPosition = 8 - numBits - this.posInArg;
		int mask = ((1 << numBits)-1) << bitsPosition;
		int ret = (char)((this.args[this.currentArg] & mask) >> bitsPosition);
		this.posInArg += numBits;
		if (this.posInArg % 8 == 0) {
			// advance to next byte
			this.posInArg = 0;
			this.currentArg++;
		}
		return ret;
	}

	private void skipBits() {
		if (this.posInArg % 8 != 0) {
			// we read some bits from the current arg but aren't done, skip the rest
			this.posInArg = 0;
			this.currentArg++;
		}
	}

	final protected double makeVdc() {
		if (VDCType.getType().equals(VDCType.Type.REAL)) {
			Type precision = VDCRealPrecision.getPrecision();
			if (precision.equals(Type.FIXED_POINT_32BIT)) {
				return makeFixedPoint32();
			}
			if (precision.equals(Type.FIXED_POINT_64BIT)) {
				return makeFixedPoint64();
			}
			if (precision.equals(Type.FLOATING_POINT_32BIT)) {
				return makeFloatingPoint32();
			}
			if (precision.equals(Type.FLOATING_POINT_64BIT)) {
				return makeFloatingPoint64();
			}

			unsupported("unsupported precision "+precision);
			return makeFixedPoint32();
		}

		// defaults to integer
		// if (VDCType.getType().equals(VDCType.Type.INTEGER)) {
		int precision = VDCIntegerPrecision.getPrecision();
		if (precision == 16) {
			return makeSignedInt16();
		}
		if (precision == 24) {
			return makeSignedInt24();
		}
		if (precision == 32) {
			return makeSignedInt32();
		}

		unsupported("unsupported precision "+precision);
		return makeSignedInt16();
		// }
	}

	final protected int sizeOfVdc() {
		if (VDCType.getType().equals(VDCType.Type.INTEGER)) {
			int precision = VDCIntegerPrecision.getPrecision();
			return (precision / 8);
		}

		if (VDCType.getType().equals(VDCType.Type.REAL)) {
			Type precision = VDCRealPrecision.getPrecision();
			if (precision.equals(Type.FIXED_POINT_32BIT)) {
				return sizeOfFixedPoint32();
			}
			if (precision.equals(Type.FIXED_POINT_64BIT)) {
				return sizeOfFixedPoint64();
			}
			if (precision.equals(Type.FLOATING_POINT_32BIT)) {
				return sizeOfFloatingPoint32();
			}
			if (precision.equals(Type.FLOATING_POINT_64BIT)) {
				return sizeOfFloatingPoint64();
			}
		}
		return 1;
	}

	final protected double makeVc() {
		switch (DeviceViewportSpecificationMode.getMode()) {
		case MillimetersWithScaleFactor:
		case PhysicalDeviceCoordinates:
			return makeInt();
		case FractionOfDrawingSurface:
		default:
			return makeReal();
		}
	}

	final protected double makeReal() {
		Precision precision = RealPrecision.getPrecision();
		if (precision.equals(RealPrecision.Precision.FIXED_32)) {
			return makeFixedPoint32();
		}
		if (precision.equals(RealPrecision.Precision.FIXED_64)) {
			return makeFixedPoint64();
		}
		if (precision.equals(RealPrecision.Precision.FLOATING_32)) {
			return makeFloatingPoint32();
		}
		if (precision.equals(RealPrecision.Precision.FLOATING_64)) {
			return makeFloatingPoint64();
		}

		unsupported("unsupported real precision "+precision);
		// return default
		return makeFixedPoint32();
	}

	final protected double makeFixedPoint() {
		Precision precision = RealPrecision.getPrecision();
		if (precision.equals(RealPrecision.Precision.FIXED_32)) {
			return makeFixedPoint32();
		}
		if (precision.equals(RealPrecision.Precision.FIXED_64)) {
			return makeFixedPoint64();
		}
		unsupported("unsupported real precision "+precision);
		// return default
		return makeFixedPoint32();
	}

	final protected double makeFloatingPoint() {
		Precision precision = RealPrecision.getPrecision();
		if (precision.equals(RealPrecision.Precision.FLOATING_32)) {
			return makeFloatingPoint32();
		}
		if (precision.equals(RealPrecision.Precision.FLOATING_64)) {
			return makeFloatingPoint64();
		}
		return makeFloatingPoint32();
	}

	private double makeFixedPoint32() {
		double wholePart = makeSignedInt16();
		double fractionPart = makeUInt16();

		return wholePart + (fractionPart / (2 << 15));
	}

	private int sizeOfFixedPoint32() {
		return 2+2;
	}

	private double makeFixedPoint64() {
		double wholePart = makeSignedInt32();
		double fractionPart = makeUInt32();

		return wholePart + (fractionPart / (2 << 31));
	}

	private int sizeOfFixedPoint64() {
		return 4+4;
	}

	protected final double makeFloatingPoint32() {
		skipBits();
		int bits = 0;
		for (int i = 0; i < 4; i++) {
			bits = (bits << 8) | makeChar();
		}
		return Float.intBitsToFloat(bits);
	}

	private int sizeOfFloatingPoint32() {
		return 2*2;
	}

	private double makeFloatingPoint64() {
		skipBits();
		long bits = 0;
		for (int i = 0; i < 8; i++) {
			bits = (bits << 8) | makeChar();
		}
		return Double.longBitsToDouble(bits);
	}

	private int sizeOfFloatingPoint64() {
		return 2*4;
	}

	protected int makeEnum() {
		return makeSignedInt16();
	}

	protected int sizeOfEnum() {
		return 2;
	}

	final protected Point2D.Double makePoint() {
		return new Point2D.Double(makeVdc(), makeVdc());
	}

	final protected int sizeOfPoint() {
		return 2 * sizeOfVdc();
	}

	final protected int makeColorIndex() {
		int precision = ColourIndexPrecision.getPrecision();
		return makeUInt(precision);
	}

	final protected int makeColorIndex(int precision) {
		return makeUInt(precision);
	}

	final protected Color makeDirectColor() {
		int precision = ColourPrecision.getPrecision();
		Model model = ColourModel.getModel();

		if (model.equals(Model.RGB)) {
			int[] scaled = scaleColorValueRGB(makeUInt(precision), makeUInt(precision), makeUInt(precision));
			return new Color(scaled[0], scaled[1], scaled[2]);
		}

		if (model.equals(Model.CIELAB)) {
			unimplemented("CIELAB");
			makeUInt(precision);
			makeUInt(precision);
			makeUInt(precision);
			return Color.CYAN;
		}

		if (model.equals(Model.CIELUV)) {
			unimplemented("CIELUV");
			makeUInt(precision);
			makeUInt(precision);
			makeUInt(precision);
			return Color.CYAN;
		}

		if (model.equals(Model.CMYK)) {
			float[] components = new float[4];
			components[0] = makeUInt(precision);
			components[1] = makeUInt(precision);
			components[2] = makeUInt(precision);
			components[3] = makeUInt(precision);
			CMYKColorSpace colorSpace = new CMYKColorSpace();
			return new Color(colorSpace, components, 1.f);
		}

		if (model.equals(Model.RGB_RELATED)) {
			unimplemented("CIELUV");
			makeUInt(precision);
			makeUInt(precision);
			makeUInt(precision);
			return Color.CYAN;
		}

		assert false : "unsupported color mode";
		return Color.CYAN;
	}

	final protected int sizeOfDirectColor() {
		int precision = ColourPrecision.getPrecision();
		Model model = ColourModel.getModel();

		if (model.equals(Model.RGB)) {
			return 3 * precision / 8;
		}

		assert false;
		return 0;
	}

	private int[] scaleColorValueRGB(int r, int g, int b) {
		int[] min = ColourValueExtent.getMinimumColorValueRGB();
		int[] max = ColourValueExtent.getMaximumColorValueRGB();

		r = clamp(r, min[0], max[0]);
		g = clamp(g, min[0], max[0]);
		b = clamp(b, min[0], max[0]);

		assert (min[0] != max[0] && min[1] != max[1] && min[2] != max[2]);

		return new int[] {
				255 * (r - min[0])/(max[0] - min[0]),
				255 * (g - min[1])/(max[1] - min[1]),
				255 * (b - min[2])/(max[2] - min[2])
		};
	}

	/**
	 * Clamp the given value between the given minimum and maximum
	 * @param r The value to clamp
	 * @param min The minimum value
	 * @param max The maximum value
	 * @return The clamped value
	 */
	private int clamp(int r, int min, int max) {
		return Math.max(Math.min(r, max), min);
	}

	final protected double makeFloat32(int i) {
		skipBits();
		int sign = this.args[2 * i] & (1 << 16);
		int exponent = (this.args[2 * i] >> 8) & 255;
		int fraction = ((this.args[2 * i] & 127) << 16) | this.args[2 * i + 1];

		// only base 10 supported
		return Math.pow(1, sign) * fraction * Math.pow(10, exponent);
	}

	final protected double makeSizeSpecification(SpecificationMode specificationMode) {
		if (specificationMode.equals(SpecificationMode.ABSOLUTE)) {
			return makeVdc();
		}
		return makeReal();
	}

	final protected StructuredDataRecord makeSDR() {
		StructuredDataRecord ret = new StructuredDataRecord();
		int sdrLength = getStringCount();
		int startPos = this.currentArg;
		while (this.currentArg < (startPos + sdrLength)) {
			StructuredDataType dataType = StructuredDataType.get(makeIndex());
			int dataCount = makeInt();
			List<Object> data = new ArrayList<Object>();
			for (int i = 0; i < dataCount; i++) {
				switch (dataType) {
				case SDR:
					data.add(makeSDR());
					break;
				case CI:
					data.add(makeColorIndex());
					break;
				case CD:
					data.add(makeDirectColor());
					break; 
				case N:
					data.add(makeName());
					break;
				case E:
					data.add(makeEnum());
					break;
				case I:
					data.add(makeInt());
					break;
				case RESERVED:
					// reserved
					break;
				case IF8:
					data.add(makeSignedInt8());
					break;
				case IF16:
					data.add(makeSignedInt16());
					break;
				case IF32:
					data.add(makeSignedInt32());
					break;
				case IX:
					data.add(makeIndex());
					break;
				case R:
					data.add(makeReal());
					break;
				case S:
					data.add(makeString());
					break;
				case SF:
					data.add(makeString());
					break;
				case VC:
					data.add(makeVc());
					break;
				case VDC:
					data.add(makeVdc());
					break;
				case CCO:
					data.add(makeDirectColor());
					break;
				case UI8:
					data.add(makeUInt8());
					break;
				case UI32:
					data.add(makeUInt32());
					break;
				case BS:
					// bit stream? XXX how do we know how many bits to read?
					break;
				case CL:
					// color list? XXX how to read?
					break;
				case UI16:
					data.add(makeUInt16());
					break;
				default:
					unsupported("makeSDR()-unsupported dataTypeIndex "
							+ dataType);
				}
			}
			ret.add(dataType, dataCount, data);
		}
		return ret;
	}

	/**
	 * Align on a word boundary
	 */
	final protected void alignOnWord() {
		if (this.currentArg >= this.args.length) {
			// we reached the end of the array, nothing to skip
			return;
		}

		if (this.currentArg % 2 == 0 && this.posInArg > 0) {
			this.posInArg = 0;
			this.currentArg += 2;
		}
		else if (this.currentArg % 2 == 1) {
			this.posInArg = 0;
			this.currentArg++;
		}
	}

	public void paint(@SuppressWarnings("unused") CGMDisplay d) {
		// default empty implementation
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case APPLICATION_STRUCTURE_ELEMENTS: // 9
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		default:
			assert (10 <= ec && ec <= 15) : "unsupported element class";
			unsupported(ec, eid);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

			// 0, 19
		case BEGIN_TILE_ARRAY:
			return new BeginTileArray(ec, eid, l, in);

			// 0, 20
		case END_TILE_ARRAY:
			return new EndTileArray(ec, eid, l, in);

			// 0, 21
		case BEGIN_APPLICATION_STRUCTURE:
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

			// 0, 22
		case BEGIN_APPLICATION_STRUCTURE_BODY:
			return new BeginApplicationStructureBody(ec, eid, l, in);

			// 0, 23
		case END_APPLICATION_STRUCTURE:
			return new EndApplicationStructure(ec, eid, l, in);

		default:
			assert false : "unsupported element ID=" + eid;
		return new Command(ec, eid, l, in);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case COLOUR_MODEL: // 19
			return new ColourModel(ec, eid, l, in);

		case COLOUR_CALIBRATION: // 20
		case FONT_PROPERTIES: // 21
		case GLYPH_MAPPING: // 22
		case SYMBOL_LIBRARY_LIST: // 23
		case PICTURE_DIRECTORY: // 24
			unsupported(ec, eid);
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
			unsupported(ec, eid);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		default:
			unsupported(ec, eid);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 4
	private static Command readGraphicalPrimitiveElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (GraphicalPrimitiveElements.getElement(eid)) {

		case POLYLINE: // 1
			return new Polyline(ec, eid, l, in);

		case DISJOINT_POLYLINE: // 2
			return new DisjointPolyline(ec, eid, l, in);

		case POLYMARKER: // 3
			return new PolyMarker(ec, eid, l, in);

		case TEXT: // 4
			return new Text(ec, eid, l, in);

		case RESTRICTED_TEXT: // 5
			return new RestrictedText(ec, eid, l, in);

		case APPEND_TEXT: // 6
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case POLYGON: // 7
			return new PolygonElement(ec, eid, l, in);

		case POLYGON_SET: // 8
			return new PolygonSet(ec, eid, l, in);

		case CELL_ARRAY: // 9
			return new CellArray(ec, eid, l, in);

		case GENERALIZED_DRAWING_PRIMITIVE: // 10
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case RECTANGLE: // 11
			return new RectangleElement(ec, eid, l, in);

		case CIRCLE: // 12
			return new CircleElement(ec, eid, l, in);

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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case POLYBEZIER: // 26
			return new PolyBezier(ec, eid, l, in);

		case POLYSYMBOL: // 27
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case BITONAL_TILE: // 28
			return new BitonalTile(ec, eid, l, in);

		case TILE: // 29
			return new Tile(ec, eid, l, in);

		default:
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	// Class: 5
	private static Command readAttributeElements(DataInput in, int ec, int eid, int l) throws IOException {
		switch (AttributeElement.getElement(eid)) {
		case LINE_BUNDLE_INDEX: // 1
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case LINE_TYPE: // 2
			return new LineType(ec, eid, l, in);

		case LINE_WIDTH: // 3
			return new LineWidth(ec, eid, l, in);

		case LINE_COLOUR: // 4
			return new LineColour(ec, eid, l, in);

		case MARKER_BUNDLE_INDEX: // 5
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case MARKER_TYPE: // 6
			return new MarkerType(ec, eid, l, in);

		case MARKER_SIZE: // 7
			return new MarkerSize(ec, eid, l, in);

		case MARKER_COLOUR: // 8
			return new MarkerColour(ec, eid, l, in);

		case TEXT_BUNDLE_INDEX: // 9:
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case TEXT_FONT_INDEX: // 10
			return new TextFontIndex(ec, eid, l, in);

		case TEXT_PRECISION: // 11
			return new TextPrecision(ec, eid, l, in);

		case CHARACTER_EXPANSION_FACTOR: // 12
			return new CharacterExpansionFactor(ec, eid, l, in);

		case CHARACTER_SPACING: // 13
			return new CharacterSpacing(ec, eid, l, in);

		case TEXT_COLOUR: // 14
			return new TextColour(ec, eid, l, in);

		case CHARACTER_HEIGHT: // 15
			return new CharacterHeight(ec, eid, l, in);

		case CHARACTER_ORIENTATION: // 16
			return new CharacterOrientation(ec, eid, l, in);

		case TEXT_PATH: // 17
			return new TextPath(ec, eid, l, in);

		case TEXT_ALIGNMENT: // 18
			return new TextAlignment(ec, eid, l, in);

		case CHARACTER_SET_INDEX: // 19
			return new CharacterSetIndex(ec, eid, l, in);

		case ALTERNATE_CHARACTER_SET_INDEX: // 20
			return new AlternateCharacterSetIndex(ec, eid, l, in);

		case FILL_BUNDLE_INDEX: // 21
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case INTERIOR_STYLE: // 22
			return new InteriorStyle(ec, eid, l, in);

		case FILL_COLOUR: // 23
			return new FillColour(ec, eid, l, in);

		case HATCH_INDEX: // 24
			return new HatchIndex(ec, eid, l, in);

		case PATTERN_INDEX: // 25
		case EDGE_BUNDLE_INDEX: // 26
			unsupported(ec, eid);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case COLOUR_TABLE: // 34
			return new ColourTable(ec, eid, l, in);

		case ASPECT_SOURCE_FLAGS: // 35
		case PICK_IDENTIFIER: // 36
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case LINE_CAP: // 37
			return new LineCap(ec, eid, l, in);

		case LINE_JOIN: // 38
			return new LineJoin(ec, eid, l, in);

		case LINE_TYPE_CONTINUATION: // 39
		case LINE_TYPE_INITIAL_OFFSET: // 40
		case TEXT_SCORE_TYPE: // 41
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);

		case RESTRICTED_TEXT_TYPE: // 42
			return new RestrictedTextType(ec, eid, l, in);

		case INTERPOLATED_INTERIOR: // 43
			unsupported(ec, eid);
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
			unsupported(ec, eid);
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
			unsupported(ec, eid);
			return new Command(ec, eid, l, in);
		}
	}

	private static void unsupported(int ec, int eid) {
		if (ec == 0 && eid == 0)
			// 0, 0 is NO-OP
			return;

		Messages.getInstance().add(
				new Message(Message.Severity.UNIMPLEMENTED, ec, eid, "unsupported", null));
	}

	protected final void info(String message) {
		Messages.getInstance().add(
				new Message(Message.Severity.INFO, this.elementClass, this.elementCode,
						message, toString()));
	}

	protected final void unsupported(String message) {
		Messages.getInstance().add(
				new Message(Message.Severity.UNSUPPORTED, this.elementClass, this.elementCode,
						message, toString()));
	}

	protected final void unimplemented(String message) {
		Messages.getInstance().add(
				new Message(Message.Severity.UNIMPLEMENTED, this.elementClass, this.elementCode,
						message, toString()));
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Returns the element class for this command
	 * @return An integer representing the class
	 */
	public int getElementClass() {
		return this.elementClass;
	}


	/**
	 * Returns the element ID for this command
	 * @return An integer representing the identifier
	 */
	public int getElementCode() {
		return this.elementCode;
	}

	/**
	 * Returns a descriptive string of the given shape
	 * @param s The shape to describe
	 * @return A string containing the coordinates to make the shape
	 */
	protected String printShape(Shape s) {
		StringBuilder sb = new StringBuilder();

		PathIterator pathIterator = s.getPathIterator(new AffineTransform());
		double[] coords = new double[6];
		while (!pathIterator.isDone()) {
			int currentSegment = pathIterator.currentSegment(coords);
			switch (currentSegment) {
			case PathIterator.SEG_MOVETO:
				sb.append("MOVETO=");
				sb.append(coords[0]).append(",").append(coords[1]).append(";");
				break;
			case PathIterator.SEG_LINETO:
				sb.append("LINETO=");
				sb.append(coords[0]).append(",").append(coords[1]).append(";");
				break;
			case PathIterator.SEG_QUADTO:
				sb.append("QUADTO=");
				sb.append(coords[0]).append(",").append(coords[1]).append(",");
				sb.append(coords[2]).append(",").append(coords[3]).append(";");
				break;
			case PathIterator.SEG_CUBICTO:
				sb.append("CUBICTO=");
				sb.append(coords[0]).append(",").append(coords[1]).append(",");
				sb.append(coords[2]).append(",").append(coords[3]).append(",");
				sb.append(coords[4]).append(",").append(coords[5]).append(";");
				break;
			case PathIterator.SEG_CLOSE:
				sb.append("CLOSE=");
			}
			pathIterator.next();
		}

		return sb.toString();
	}

}

/*
 * vim:encoding=utf8
 */
