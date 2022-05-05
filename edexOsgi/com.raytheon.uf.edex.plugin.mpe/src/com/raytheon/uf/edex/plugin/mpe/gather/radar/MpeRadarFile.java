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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.io.File;
import java.io.FileInputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * POJO representation of the Graphic Radar file that provides a way to read
 * Graphic Radar files. Write is not currently (June 2016) implemented because
 * the files are currently only read and converted to a different format before
 * the information is rewritten. Structure of the Radar file is based on: RPG to
 * Class 1 User ICD, decode_dhr_dsp/TEXT/decodeDHR.c, and
 * decode_dhr_dsp/TEXT/decodeDSP.c.
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
 * Nov 22, 2016 5588       nabowle     Refactor from DHRFile
 *
 * </pre>
 *
 * @author bkowal
 */
public abstract class MpeRadarFile<T extends MpeRadarProductDescription> {

    private static final int RADAR_ID_LEN = 3;

    public static final float DEFAULT_PARAM_VALUE = -99.0f;

    /*
     * Two build versions were recognized by the legacy code. A build version of
     * five and a build version of 8 or greater.
     */
    public static final int BUILD_VERSION_5 = 5;

    public static final int BUILD_VERSION_8 = 8;

    private int version;

    private String radarId;

    private MpeRadarMessageHeader header;

    private MpeRadarSymbologyData symbologyData;

    private String productType;

    private short productCode;

    private String gridFilename;

    /**
     * Attempts to load a DHR radar file to produce a {@link MpeRadarFile} .
     *
     * @param radarId
     *
     * @param radarFilePath
     *            the {@link Path} to the DHR radar file
     * @param version
     *            the file version. Normally retrieved from Apps_defaults or
     *            defaulted to 8
     * @return the {@link MpeRadarFile} that was produced based on the file
     * @throws MpeRadarInputException
     */
    public MpeRadarFile(final Path radarFilePath, final int version,
            final String productType, final short productCode)
            throws MpeRadarInputException {
        this.version = version;
        this.productType = productType;
        this.productCode = productCode;
        File f = radarFilePath.toFile();
        findRadarId(f);
        try (FileInputStream fis = new FileInputStream(f);
                FileChannel fc = fis.getChannel()) {
            ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0, f.length());
            skipToStart(byteBuffer);
            readHeader(byteBuffer);
            readDescription(byteBuffer);
            verifyProductCode();
            readData(byteBuffer);
            this.gridFilename = this.productType + this.radarId
                    + MpeRadarDecodeConstants.filenameDateFormat.get().format(
                            getDescription().getProductDateTime().getTime());
            initialize();
        } catch (Exception e) {
            throw new MpeRadarInputException(radarFilePath, e);
        }

    }

    /**
     * @param f
     * @return
     * @throws MpeRadarInputException
     */
    private void findRadarId(File f) throws MpeRadarInputException {
        /*
         * The radarId is the 3 characters following the product type in the
         * filename, assuming the product type is in the filename.
         *
         * Ported from decode_dhr_dsp/TEXT/get_radid_from_filename.c
         */
        String filename = f.getName();
        String productType = getProductType();
        String patternStr = productType + "([A-Za-z]{" + RADAR_ID_LEN + "}).*";
        Pattern radIdPattern = Pattern.compile(patternStr);
        Matcher m = radIdPattern.matcher(filename);
        if (m.matches()) {
            this.radarId = m.group(1);
            return;
        }

        /*
         * The filename didn't match the expected format. Check for the product
         * type in the file contents and if found, pull it from there if
         * possible.
         */
        this.radarId = findRadIdInFile(f, radIdPattern);
        if (this.radarId == null) {
            throw new MpeRadarInputException(f.toPath(), null);
        }
    }

    /**
     * @param patternStr
     * @param radIdPattern
     * @param bytes
     * @param byteBuffer
     * @throws MpeRadarInputException
     */
    private String findRadIdInFile(File f, Pattern pattern)
            throws MpeRadarInputException {

        try (FileInputStream fis = new FileInputStream(f);
                FileChannel fc = fis.getChannel()) {
            ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0, f.length());
            String productType = getProductType();
            char firstChar = productType.charAt(0);
            int searchLength = productType.length() + RADAR_ID_LEN;
            byte[] bytes = new byte[searchLength];
            while (byteBuffer.remaining() > searchLength) {
                byteBuffer.get(bytes);
                String checkStr = new String(bytes, StandardCharsets.US_ASCII);
                Matcher m = pattern.matcher(checkStr);
                if (m.matches()) {
                    return m.group(1);
                }
                int firstPos = checkStr.indexOf(firstChar, 1);
                if (firstPos > 0) {
                    byteBuffer.position(
                            byteBuffer.position() - searchLength + firstPos);
                }
                byteBuffer.get(bytes);
                checkStr = new String(bytes, StandardCharsets.US_ASCII);
            }

        } catch (Exception e) {
            throw new MpeRadarInputException(f.toPath(), e);
        }

        return null;
    }

    /** Get the product type. */
    protected String getProductType() {
        return this.productType;
    }

    /** Get the product code. */
    protected short getProductCode() {
        return this.productCode;
    }

    /** Initialize internal records. */
    protected abstract void initialize();

    /**
     * Read the file bytes until the header marker is reached indicating the
     * beginning of the header.
     *
     * @param byteBuffer
     *            the {@link ByteBuffer} that the file has been read into
     * @throws InvalidMpeRadarException
     */
    protected void skipToStart(final ByteBuffer byteBuffer)
            throws InvalidMpeRadarException {
        boolean read = true;
        while (read) {
            final int beforeReadPosition = byteBuffer.position();
            short value = byteBuffer.getShort();
            if (value == this.productCode) {
                /*
                 * found the start of the product header.
                 */
                read = false;
                /*
                 * Reset the position to before the value was read because it is
                 * part of the header.
                 */
                byteBuffer.position(beforeReadPosition);
            } else {
                /*
                 * Go back one-byte since an odd-number of preceding bytes would
                 * cause a misalignment of half-words.
                 */
                byteBuffer.position(beforeReadPosition + 1);
            }

            /*
             * Ensure that the end of the file has not been reached.
             */
            if (!byteBuffer.hasRemaining() || byteBuffer
                    .remaining() < MpeRadarMessageHeader.BYTE_SIZE) {
                throw new InvalidMpeRadarException(
                        "Failed to find the start of the header. Invalid radar product?");
            }
        }
    }

    /**
     * Reads the radar header from the specified {@link ByteBuffer} to produce a
     * {@link MpeRadarMessageHeader}.
     *
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     * @throws InvalidMpeRadarException
     */
    protected void readHeader(final ByteBuffer buf)
            throws InvalidMpeRadarException {
        this.header = new MpeRadarMessageHeader(buf);
    }

    /**
     * Reads the MPE radar product description from the specified
     * {@link ByteBuffer} to produce a {@link MpeRadarProductDescription}.
     *
     * @param expectedProductCode
     *
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     * @throws InvalidMpeRadarException
     */
    protected abstract void readDescription(final ByteBuffer buf)
            throws InvalidMpeRadarException;

    protected abstract T getDescription();

    /**
     * @param byteBuffer
     * @throws InvalidMpeRadarException
     */
    protected void readData(ByteBuffer byteBuffer)
            throws InvalidMpeRadarException {
        this.symbologyData = new MpeRadarSymbologyData(byteBuffer);
    }

    /**
     * @throws InvalidMpeRadarException
     */
    protected void verifyProductCode() throws InvalidMpeRadarException {
        short expected = getProductCode();
        short actual = getDescription().getProductCode();
        if (expected != actual) {
            throw new InvalidMpeRadarException(
                    "Description contained an unexpected product code: "
                            + actual + ". Was expecting a product code of: "
                            + expected + ". Invalid radar product?");
        }
    }

    /**
     * @return the version
     */
    public int getVersion() {
        return version;
    }

    /**
     * @return the radarId
     */
    public String getRadarId() {
        return radarId;
    }

    /**
     * @return the header
     */
    public MpeRadarMessageHeader getHeader() {
        return header;
    }

    /**
     * @return the symbologyData
     */
    public MpeRadarSymbologyData getSymbologyData() {
        return symbologyData;
    }

    public String getGridFilename() {
        return gridFilename;
    }
}