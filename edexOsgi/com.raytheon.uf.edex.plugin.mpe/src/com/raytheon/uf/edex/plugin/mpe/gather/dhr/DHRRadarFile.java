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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.FileInputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.file.Path;

/**
 * POJO representation of the DHR Radar file that provides a way to read DHR
 * Radar files. Write is not currently (June 2016) implemented because the files
 * are currently only read and converted to a different format before the
 * information is rewritten. Structure of the DHR Radar file is based on:
 * decode_dhr_dsp/TEXT/decodeDHR.c.
 * 
 * TODO: in the future, it may be possible to abstract portions of this class to
 * support reading other radar formats that MPE is capable of reading.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRRadarFile {

    public static final int MAX_AZIMUTH = 360;

    public static final int MAX_RANGE = 230;

    private static final String DHR_EOF_ERR_PATTERN = "End of file reached before fully reading the %s. Invalid DHR product?";

    private static final String EOF_READ_HEADER = "dhr header";

    private static final String EOF_READ_DATA = "dhr data";

    private static final String EOF_READ_PARAMS = "dhr params";

    private static final short HEADER_BEGIN_VALUE = 32;

    private static final int SHORT_BYTES = 2;

    private static final char PARAM_A_BEGIN = 'A';

    private static final char PARAM_D_BEGIN = 'D';

    private static final int PARAM_COUNT_STR_LEN = 6;

    private static final int PARAM_VALUE_STR_LEN = 8;

    private static final String PARAM_VALUE_TRUE = "T";

    private static final String PARAM_VALUE_FALSE = "F";

    private static final String RAD_PARAM_REGEX = "AP\\((\\d{2})\\)";

    private static final Pattern radParamPattern = Pattern
            .compile(RAD_PARAM_REGEX);

    private final int version;

    private DHRHeader dhrHeader;

    private List<DHRDataRecord> data = Collections.emptyList();

    private DHRParameters dhParams;

    /**
     * Attempts to load a DHR radar file to produce a {@link DHRRadarFile}.
     * 
     * @param radarDHRFilePath
     *            the {@link Path} to the DHR radar file
     * @param version
     *            the file version. Normally retrieved from Apps_defaults or
     *            defaulted to 8
     * @return the {@link DHRRadarFile} that was produced based on the file
     * @throws DHRInputException
     */
    public static DHRRadarFile loadDHRRadarFile(final Path radarDHRFilePath,
            final int version) throws DHRInputException {
        DHRRadarFile dhrRadarFile = null;
        try (FileInputStream fis = new FileInputStream(
                radarDHRFilePath.toFile())) {
            FileChannel fc = fis.getChannel();
            ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0,
                    fis.available());
            dhrRadarFile = new DHRRadarFile(version);
            dhrRadarFile.skipTo32(byteBuffer);
            dhrRadarFile.readHeader(byteBuffer);
            dhrRadarFile.readData(byteBuffer);
            dhrRadarFile.readParameters(byteBuffer);

            /*
             * Based on the params, set the remaining fields.
             */
        } catch (Exception e) {
            throw new DHRInputException(radarDHRFilePath, e);
        }

        return dhrRadarFile;
    }

    /**
     * Constructor
     * 
     * @param version
     *            the DHR Radar file version
     */
    public DHRRadarFile(final int version) {
        this.version = version;
    }

    /**
     * Read the file bytes until the first 32 is reached indicating the
     * beginning of the DHR header.
     * 
     * @param byteBuffer
     *            the {@link ByteBuffer} that the file has been read into
     * @throws InvalidDHRException
     */
    protected void skipTo32(final ByteBuffer byteBuffer)
            throws InvalidDHRException {
        boolean read = true;
        while (read) {
            final int beforeReadPosition = byteBuffer.position();
            short value = byteBuffer.getShort();
            if (value == HEADER_BEGIN_VALUE) {
                /*
                 * found the start of the product header.
                 */
                read = false;
                /*
                 * Reset the position to before the value was read because it is
                 * part of the header.
                 */
                byteBuffer.position(beforeReadPosition);
            }

            /*
             * Ensure that the end of the file has not been reached.
             */
            if (!byteBuffer.hasRemaining()
                    || byteBuffer.remaining() < DHRHeader.HEADER_LENGTH) {
                throw new InvalidDHRException(
                        "Failed to find the start of the header. Invalid DHR product?");
            }
        }
    }

    /**
     * Reads the DHR header from the specified {@link ByteBuffer} to produce a
     * {@link DHRHeader}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     * @throws InvalidDHRException
     */
    protected void readHeader(final ByteBuffer byteBuffer)
            throws InvalidDHRException {
        checkFileRemaining(byteBuffer, EOF_READ_HEADER, SHORT_BYTES
                * DHRHeader.HEADER_LENGTH);
        final short[] rawHeader = new short[DHRHeader.HEADER_LENGTH];
        for (int i = 0; i < DHRHeader.HEADER_LENGTH; i++) {
            rawHeader[i] = byteBuffer.getShort();
        }
        this.dhrHeader = new DHRHeader(rawHeader);
    }

    /**
     * Read the DHR data and produces {@link DHRDataRecord}s for every "row" of
     * data that is read.
     * 
     * @param byteBuffer
     *            the buffer to read the data from
     * @throws InvalidDHRException
     */
    protected void readData(final ByteBuffer byteBuffer)
            throws InvalidDHRException {
        /*
         * A single row of data consists of: short, short, short, char (C++) *
         * MAX_RANGE. The expectation is that there are MAX_AZIMUTH rows of
         * data.
         */
        final int requiredSize = ((SHORT_BYTES * 3) + MAX_RANGE) * MAX_AZIMUTH;
        checkFileRemaining(byteBuffer, EOF_READ_DATA, requiredSize);
        if (dhrHeader.getRadialNumber() != MAX_AZIMUTH) {
            throw new InvalidDHRException(
                    "Insufficient number or radials encountered. Read: "
                            + dhrHeader.getRadialNumber() + "; expected: "
                            + MAX_AZIMUTH + ". Invalid DHR product?");
        }
        data = new LinkedList<>();

        for (int i = 0; i < MAX_AZIMUTH; i++) {
            /*
             * The first value that is read should indicate the number of data
             * elements in the current row.
             */
            final short bytesLength = byteBuffer.getShort();
            if (bytesLength != MAX_RANGE) {
                throw new InvalidDHRException(
                        "Insufficient data length encountered for row: " + i
                                + ". Read: " + bytesLength + "; expected: "
                                + MAX_RANGE + ". Invalid DHR product?");
            }

            /*
             * Read the radar angles.
             */
            final short radarStartAngle = byteBuffer.getShort();
            final short radarDeltaAngle = byteBuffer.getShort();

            short[] data = new short[MAX_RANGE];
            for (int j = 0; j < MAX_RANGE; j++) {
                data[j] = (short) byteBuffer.get();
            }
            this.data.add(new DHRDataRecord(radarStartAngle, radarDeltaAngle,
                    data));
        }
    }

    /**
     * Reads the DHR params from the specified {@link ByteBuffer}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     * @throws InvalidDHRException
     */
    protected void readParameters(final ByteBuffer byteBuffer)
            throws InvalidDHRException {
        /*
         * First need to determine where the parameters start. Looking for char
         * 'A' followed by char 'D'.
         */
        boolean read = true;
        boolean firstCharFound = false;
        while (read) {
            checkFileRemaining(byteBuffer, EOF_READ_PARAMS, 1);
            char character = (char) byteBuffer.get();

            if (character == PARAM_A_BEGIN) {
                /*
                 * The first character in the sequence has been found.
                 */
                firstCharFound = true;
            } else if (firstCharFound && character == PARAM_D_BEGIN) {
                /*
                 * The second character in the sequence has been found following
                 * the first character in the sequence.
                 */
                read = false;
            }
        }

        checkFileRemaining(byteBuffer, EOF_READ_PARAMS, PARAM_COUNT_STR_LEN);
        byte[] paramCountBytes = new byte[PARAM_COUNT_STR_LEN];
        byteBuffer.get(paramCountBytes);
        String paramCountStr = new String(paramCountBytes);
        /*
         * Attempt to extract the number of parameters.
         */
        final Matcher matcher = radParamPattern.matcher(paramCountStr);
        if (!matcher.matches()) {
            throw new InvalidDHRException(
                    "Failed to parse the number of parameters from: "
                            + paramCountStr + ". Invalid DHR product?");
        }
        /*
         * Guaranteed to be numerical due to the regex.
         */
        final int paramCount = Integer.parseInt(matcher.group(1));
        /*
         * Ensure that the array is sized to at least the default size.
         */
        final float[] adaptableParams = new float[Math.max(paramCount,
                DHRParameters.DEFAULT_PARAMS_COUNT)];
        Boolean adaptableFlag = null;
        /*
         * parameters are read in eight byte chunks. They are initially read as
         * {@link String}s and converted to the appropriate data type.
         */
        checkFileRemaining(byteBuffer, EOF_READ_PARAMS, PARAM_VALUE_STR_LEN
                * paramCount);
        byte[] paramValueBytes = new byte[PARAM_VALUE_STR_LEN];
        for (int i = 0; i < paramCount; i++) {
            byteBuffer.get(paramValueBytes);
            final String paramValueStr = new String(paramValueBytes).trim();
            if (paramValueStr.isEmpty()) {
                adaptableParams[i] = DHRParameters.DEFAULT_PARAM_VALUE;
            } else if (PARAM_VALUE_TRUE.equals(paramValueStr)) {
                adaptableFlag = Boolean.TRUE;
            } else if (PARAM_VALUE_FALSE.equals(paramValueStr)) {
                adaptableFlag = Boolean.FALSE;
            } else {
                try {
                    adaptableParams[i] = Float.parseFloat(paramValueStr);
                } catch (NumberFormatException e) {
                    throw new InvalidDHRException(
                            "Failed to parse param value: " + paramValueStr
                                    + ".", e);
                }
            }
        }
        dhParams = new DHRParameters(paramCount, adaptableParams,
                adaptableFlag, version);
    }

    /**
     * Verifies that both the end of the file has not been reached and that
     * there is a sufficient number of bytes required for the next read
     * operation.
     * 
     * @param byteBuffer
     *            the {@link ByteBuffer} to check
     * @param readContent
     *            a description of the content that is currently being read from
     *            the buffer
     * @param bytesNeeded
     *            the number of bytes that should still hopefully be available
     *            from the buffer
     * @throws InvalidDHRException
     */
    private void checkFileRemaining(final ByteBuffer byteBuffer,
            final String readContent, final int bytesNeeded)
            throws InvalidDHRException {
        if (byteBuffer.hasRemaining() && byteBuffer.remaining() >= bytesNeeded) {
            return;
        }
        throw new InvalidDHRException(String.format(DHR_EOF_ERR_PATTERN,
                readContent));
    }

    public int getVersion() {
        return version;
    }

    public DHRHeader getDhrHeader() {
        return dhrHeader;
    }

    public List<DHRDataRecord> getData() {
        return data;
    }

    public DHRParameters getDhParams() {
        return dhParams;
    }
}