/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.decodertools.bufr.descriptors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.edex.decodertools.bufr.exceptions.BUFRDecoderException;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRFloatPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRNumericPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRStringPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * 
 * <pre>
 * (1) Where a code table or flag table is appropriate, &quot;code table&quot; or &quot;flag table&quot;
 *     respectively is entered in the UNITS column.
 * (2) The code tables and flag tables associated with Table B are numbered to
 *     correspond with the F, X and Y part of the table reference.
 * (3) To encode values into BUFR, the data (with units as specified in the UNITS
 *     column) must be multiplied by 10 to the power SCALE.  Then subtract the
 *     REFERENCE VALUE to give the coded value found in Section 4 of the BUFR message.
 *     For example, a measured latitude is -45.76 degrees.  The coarse accuracy
 *     descriptor is 0 05 002 and the encoded value is -45.76 x 10&circ;2 - (-9000) = 4424.
 * (4) Where UNITS are given as CCITT IA5, data shall be coded as character data
 *     left justified within the field width indicated using CCITT International
 *     Alphabet No. 5, and blank filled to the full field width indicated.
 * (5) Classes 48 to 63 are reserved for local use; all other classes are reserved
 *     for future development.
 * (6) Entries 192 to 255 within all classes are reserved for local use.
 * (7) The use of local descriptors, as defined in Notes (5) and (6), in messages
 *     intended for non-local or international exchange is strongly discouraged.
 *     They should be kept to the barest minimum possible and must also be by-passed
 *     by the use of descriptor 2 06 YYY.
 * (8) First-order statistics are included in Table B only when they are produced,
 *     as such, by the observing system.
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * 07/2009              55 T. Lee      Process the associated field for Table C
 * 04/21/2010          208 F. J. Yen   Fix handling of associated field for compressed data * 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRTableB extends BUFRDescriptor {

    public static final String UNITS_NUMERIC = "NUMERIC";

    public static final String UNITS_CODETABLE = "CODE TABLE";

    public static final String UNITS_FLAGTABLE = "FLAG TABLE";

    public static final String UNITS_CCITT_IA5 = "CCITT IA5";

    // This array added by NCO for associated fields in compressed data.
    // For now this follows Raytheon's arrayValues.
    private int[] arrayAssocValues = null;
    
    private static final HashMap<String, Unit<?>> UNITS_MAP = new HashMap<String, Unit<?>>();
    static {
        UNITS_MAP.put("%", NonSI.PERCENT);

        UNITS_MAP.put("DEG K", SI.KELVIN);
        UNITS_MAP.put("DEGREES KELVIN", SI.KELVIN);
        UNITS_MAP.put("DEG K/SEC", SI.KELVIN.divide(SI.SECOND));

        UNITS_MAP.put("M", SI.METER);
        UNITS_MAP.put("M**2", SI.METER.pow(2));

        UNITS_MAP.put("M/SEC", SI.METERS_PER_SECOND);
        UNITS_MAP.put("METERS/SECOND", SI.METERS_PER_SECOND);
        UNITS_MAP.put("M/SEC**2", SI.METERS_PER_SQUARE_SECOND);

        UNITS_MAP.put("M**2/SEC", SI.METER.pow(2).divide(SI.SECOND));
        // Geopotential
        UNITS_MAP.put("M**2/SEC**2", SI.METER.pow(2).divide(SI.SECOND.pow(2)));
        UNITS_MAP.put("M**3/SEC", SI.METER.pow(3).divide(SI.SECOND));

        UNITS_MAP.put("DEG", NonSI.DEGREE_ANGLE);
        UNITS_MAP.put("DEG/SEC", NonSI.DEGREE_ANGLE.divide(SI.SECOND));
        UNITS_MAP.put("HZ", SI.HERTZ);

        UNITS_MAP.put("RAD/SEC", SI.RADIAN.divide(SI.SECOND));

        UNITS_MAP.put("DEGREES TRUE", NonSI.DEGREE_ANGLE);
        UNITS_MAP.put("DEG TRUE", NonSI.DEGREE_ANGLE);

        UNITS_MAP.put("KG/KG", SI.KILOGRAM.divide(SI.KILOGRAM));
        UNITS_MAP.put("KG/M**2", SI.KILOGRAM.divide(SI.METER.pow(2)));
        UNITS_MAP.put("KG/M**2/SEC", SI.KILOGRAM.divide(SI.METER.pow(2).divide(
                SI.SECOND)));
        UNITS_MAP.put("KG/M**3", SI.KILOGRAM.divide(SI.METER.pow(3)));

        UNITS_MAP.put("J/M**2", SI.JOULE.divide(SI.METER.pow(2)));
        UNITS_MAP.put("PA/SEC", SI.PASCAL.divide(SI.SECOND));
        UNITS_MAP.put("PA", SI.PASCAL);
        UNITS_MAP.put("W", SI.WATT);
        UNITS_MAP.put("W/M**2", SI.WATT.divide(SI.SECOND.pow(2)));

        UNITS_MAP.put("DOBSON", SI.MILLIMETER.divide(100));

        UNITS_MAP.put("DB", NonSI.DECIBEL);
        UNITS_MAP.put("DB/M", NonSI.DECIBEL.divide(SI.METER));

        UNITS_MAP.put("BQ", SI.BECQUEREL);
        UNITS_MAP.put("BQ/L", SI.BECQUEREL.divide(NonSI.LITER));
        UNITS_MAP.put("BQ/M**3", SI.BECQUEREL.divide(SI.METER.pow(3)));
        UNITS_MAP.put("MILLI-SIEV", SI.SIEVERT.divide(1000));

        // Vorticity | Divergence
        UNITS_MAP.put("1/SEC", Unit.ONE.divide(SI.SECOND));
        UNITS_MAP.put("PARTS/1000", Unit.ONE.divide(1000));

        UNITS_MAP.put("MIN", NonSI.MINUTE);
        UNITS_MAP.put("HOURS", NonSI.HOUR);

        UNITS_MAP.put(UNITS_NUMERIC, Dimensionless.UNIT);
        UNITS_MAP.put(UNITS_FLAGTABLE, Dimensionless.UNIT);
        UNITS_MAP.put(UNITS_CODETABLE, Dimensionless.UNIT);
    }

    private String unit = null;

    private int scale = 0;

    private double scaleValue = 1.0;

    private long referenceValue = 0L;

    private int numBits = 0;

    private String comments = null;

    private long missingMask = 0L;

    private boolean dataMissing = false;

    public BUFRTableB(int descriptor) {
        super(descriptor);

    }

    /**
     * Construct a table B entry with given values for f, x, and y.
     * 
     * @param f
     * @param x
     * @param y
     */
    public BUFRTableB(int f, int x, int y) {
        super(f, x, y);

    }

    // /**
    // * Make a deep copy of a Table B descriptor.
    // * @param descriptor The descriptor to copy.
    // */
    // public BUFRTableB(BUFRTableB descriptor) {
    // super(descriptor) ;
    // unit = descriptor.unit;
    // scale = descriptor.scale;
    // scaleValue = descriptor.scaleValue;
    // referenceValue = descriptor.referenceValue;
    // numBits = descriptor.numBits;
    // comments = descriptor.comments;
    // missingMask = descriptor.missingMask;
    // dataMissing = descriptor.dataMissing;
    // }

    /**
     * 
     * @return
     */
    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        if (unit != null) {
            this.unit = unit.trim();
        }
    }

    public int getScale() {
        return scale;
    }

    public void setScale(int scale) {
        this.scale = scale;

        scaleValue = Math.pow(10, -scale);
    }

    public double getScaleValue() {
        return scaleValue;
    }

    public long getReferenceValue() {
        return referenceValue;
    }

    public void setReferenceValue(long referenceValue) {
        this.referenceValue = referenceValue;
    }

    public int getNumBits() {
        return numBits;
    }

    public void setNumBits(int numBits) {
        this.numBits = numBits;

        missingMask = 0L;
        for (int i = 0; i < numBits; i++) {
            missingMask <<= 1;
            missingMask |= 1;
        }
    }

    /**
     * Get the comments field.
     * 
     * @return The comments description of this descriptor.
     */
    public String getComments() {
        return comments;
    }

    /**
     * Set the comments field.
     * 
     * @param comments
     *            The comments description of this descriptor.
     */
    public void setComments(String comments) {
        this.comments = comments;
    }

    /**
     * @return the dataMissing
     */
    public boolean isDataMissing() {
        return dataMissing;
    }

    /**
     * 
     * @param bitStream
     */
    // changes made by NCEP/NCO/SIB: skip bits for Tamdar.
    // Also added parameters compressedFlag and repetitionCount to handle for compressed format
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream,
            int scaleOverride, int widthOverride, int skipBits, boolean compressedFlag, int repetitionCount) {

        /*
         * Add skipBits as an argument to the execute method in order
         * to decode Tamdar data.  Skip bits for the associated field
         * of significance for Table C.
         * NCEP/NCO/SIB added code to check for compressed data.  For now,
         * bits are being skipped as in the noncompressed Tamdar case.
         * If the need arises, this will be revisited.
         */
        if (skipBits > 0 && !(getF() == 0 && getX() == 31 && getY() == 21)) {
            bitStream.skip(skipBits);
            if (compressedFlag) {
               // changed made by NCEP/NCO/SIB for Tamdar and compressed data
                   if (skipBits > 0) {                     
                       arrayAssocValues = new int[repetitionCount];
                       int numBits = (int) bitStream.read(6);
                       bitStream.read(repetitionCount * numBits);
                   }
            }
        }

        IBUFRDataPacket packet = null;
        if (UNITS_CCITT_IA5.equals(unit)) {
            int numChars = numBits / 8;
            byte[] charData = null;
            if (numBits % 8 == 0) {

                charData = new byte[numChars];
                // Arrays.fill(charData, (byte) 0xFF);

                for (int n = 0; n < numChars; n++) {
                    if (bitStream.available() >= 8) {
                        charData[n] = (byte) (bitStream.read(8) & 0xFF);
                    } else {
                        BUFRDecoderException e = new BUFRDecoderException(
                                "Out of data");
                        e.addDescriptorToTrace(this);
                        throw (e);
                    }
                }
            }
            // If the character data read contains trailing "missing" data,
            // remove them before creating the string.
            int i = charData.length - 1;
            for (; i >= 0; i--) {
                if (charData[i] != ((byte) 0xFF)) {
                    break;
                }
            }
            i += 1;
            String s = null;
            if (i > 0) {
                s = new String(charData, 0, i);
            }
            packet = new BUFRStringPacket(s, unit, this);
        } else if (UNITS_NUMERIC.equals(unit)) {
            long data = constructValue(bitStream, widthOverride);
            if (numBits > 1) {
                dataMissing = ((missingMask & data) == missingMask);
            }
            if (dataMissing) {
                packet = new BUFRNumericPacket(null, unit, this);
            } else {
                packet = new BUFRNumericPacket(data, unit, this);
            }
        } else if (UNITS_CODETABLE.equals(unit)) {
            long data = constructValue(bitStream, 0);
            if (numBits > 1) {
                dataMissing = ((missingMask & data) == missingMask);
            }
            if (dataMissing) {
                packet = new BUFRNumericPacket(null, unit, this);
            } else {
                packet = new BUFRNumericPacket(data, unit, this);
            }
        } else if (UNITS_FLAGTABLE.equals(unit)) {
            long data = constructValue(bitStream, 0);
            if (numBits > 1) {
                dataMissing = ((missingMask & data) == missingMask);
            }
            if (dataMissing) {
                packet = new BUFRNumericPacket(null, unit, this);
            } else {
                packet = new BUFRNumericPacket(data, unit, this);
            }
        } else {
            long data = constructValue(bitStream, widthOverride);
            if (numBits > 1) {
                dataMissing = ((missingMask & data) == missingMask);
            }

            if (dataMissing) {
                packet = new BUFRFloatPacket(null, unit, this);
            } else {
                double lScale = scaleValue;
                if (scaleOverride != 0) {
                    lScale = Math.pow(10, -(scale + scaleOverride));
                }
                Double value = new Double((data + referenceValue) * lScale);

                packet = new BUFRFloatPacket(value, unit, this);
                ((BUFRFloatPacket) packet).setRawValue(data);
            }
        }
        return packet;
    }

    /**
     * 
     * @param bitStream
     */
    // changes made by NCEP/NCO/SIB for Tamdar decoder
    // NCO added compressedFlag parameter--For now set to false
    @Override
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream) {
        return execute(bitStream, 0, 0, 0, false, 0);
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    @Override
    public BUFRDescriptor copy() {

        BUFRTableB newDescriptor = new BUFRTableB(getF(), getX(), getY());
        newDescriptor.unit = unit;
        newDescriptor.scale = scale;
        newDescriptor.scaleValue = scaleValue;
        newDescriptor.referenceValue = referenceValue;
        newDescriptor.numBits = numBits;
        newDescriptor.comments = comments;
        newDescriptor.missingMask = missingMask;
        newDescriptor.dataMissing = dataMissing;
        newDescriptor.setDefined(isDefined());

        // Table B descriptors shouldn't have sublists, doing this for
        // completeness.
        List<BUFRDescriptor> repList = getSubList();
        if (repList != null) {
            List<BUFRDescriptor> newList = new ArrayList<BUFRDescriptor>();
            for (BUFRDescriptor d : repList) {
                BUFRDescriptor nd = d.copy();
                newList.add(nd);
            }
            newDescriptor.setSubList(newList);
        }

        return newDescriptor;
    }

    /**
     * Formats this object into a String representation. Called by
     * super.toString().
     * 
     * @param buffer
     *            A StringBuilder that will receive the data.
     * @return The populated buffer object.
     */
    public StringBuilder getStringData(StringBuilder buffer) {
        final String outFmt = "\n  Scale: %6d :Bits: %3d refVal: %16d units: %s\n  Comments: %s";
        // Make sure to call super. This will create a new buffer in case we
        // were passed a null reference.
        buffer = super.getStringData(buffer);
        buffer.append(String.format(outFmt, scale, numBits, referenceValue,
                unit, comments));
        return buffer;
    }

    /**
     * Factory method that creates a Table B descriptor from text data. The
     * format for the table B line entry is
     * 
     * <pre>
     * 
     * 0_xx_yyy_s_r_b_u_c
     * 
     * where the underscore represents the tab character '\t'
     * 
     *   0  (zero) Indicator for table B entries
     *  xx  The x part  [0..63] of a table B descriptor
     * yyy  The y part [0..255] of a table B descriptor
     *   s  The descriptor scale factor. 10**s
     *   r  The descriptor reference value.
     *   b  The number of bits in the data described by this descriptor.
     *   u  The descriptor units
     *   c  The descriptor comments
     * </pre>
     * 
     * @param tableEntry
     * @return
     */
    static BUFRTableB createEntry(String tableEntry) {

        BUFRTableB entry = null;
        if (tableEntry != null) {
            StringTokenizer st = new StringTokenizer(tableEntry, "\t", true);
            ArrayList<String> tokens = new ArrayList<String>();
            String lastToken = null;
            while (st.hasMoreTokens()) {

                String s = st.nextToken();
                if ("\t".equals(s)) {
                    if ("\t".equals(lastToken)) {
                        tokens.add("");
                    }
                } else {
                    tokens.add(s);
                }
                lastToken = s;
            }
            if (tokens.size() == 8) {
                try {
                    int f = Integer.parseInt(tokens.get(0).trim());
                    int x = Integer.parseInt(tokens.get(1).trim());
                    int y = Integer.parseInt(tokens.get(2).trim());

                    entry = new BUFRTableB(f, x, y);

                    // scale
                    entry.setScale(Integer.parseInt(tokens.get(3).trim()));
                    entry.setReferenceValue(Integer.parseInt(tokens.get(4)
                            .trim()));
                    entry.setNumBits(Integer.parseInt(tokens.get(5).trim()));

                    entry.unit = tokens.get(6).trim().toUpperCase();
                    entry.comments = tokens.get(7).trim();
                } catch (NumberFormatException nfe) {
                    entry = null;
                }
            }
        }
        return entry;
    }

    /**
     * Construct a value from the data bitstream.
     * 
     * @param bitStream
     * @param widthOverride
     *            Number of bits to be added to the defined width for this
     *            descriptor. This value may be positive or negative.
     * @return The constructed value from the bitstream data.
     */
    private long constructValue(BUFRBitInputStream bitStream, int widthOverride) {
        long data = 0;
        int bits = numBits + widthOverride;

        if (bitStream.available() >= bits) {
            data = bitStream.read(bits);
        } else {
            BUFRDecoderException e = new BUFRDecoderException("Out of data");
            e.addDescriptorToTrace(this);
            throw (e);
        }

        return data;
    }

    /**
     * Get a units reference that corresponds to the specified units string.
     * 
     * @param units
     *            A string representation of data units.
     * @return The corresponding units. If no correspondence can be made, a
     *         Dimensionless.UNIT reference is returned.
     */
    public static Unit<?> mapUnits(String units) {
        Unit<?> retValue = UNITS_MAP.get(units);
        if (retValue == null) {
            retValue = Dimensionless.UNIT;
        }
        return retValue;
    }
    
    /**
     * Added by NCO for associated fields
     * Set the value array built when decoding compressed data.
     * 
     * @param values
     */
    public void setArrayAssocValues(int[] values) {
        arrayAssocValues = values;
    }

    /**
     * Added by NCO for associated fields
     * Get the value array built when decoding compressed data.
     * 
     */
    public int[] getArrayAssocValues() {
        return arrayAssocValues;
    }
}
