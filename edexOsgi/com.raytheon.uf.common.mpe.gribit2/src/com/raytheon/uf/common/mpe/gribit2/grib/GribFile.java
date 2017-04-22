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
package com.raytheon.uf.common.mpe.gribit2.grib;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.file.Files;
import java.nio.file.Path;

import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConversionException;

/**
 * POJO representation of a grib (specifically GRIB 1) file (based on the xmrg
 * to grib conversion) that provides a way to both read and write grib files.
 * Grib files are written as binary files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Utilize {@link GridDefinitionSectionFactory}.
 * Aug 18, 2016 4619       bkowal      Implemented grib file generation/write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class GribFile {

    public static final int GRIB_HEADER_LENGTH = 21;

    private static final String GRIB_ENDING = "7777";

    private String header;

    private String ending;

    private IndicatorSection is;

    private ProductDefinitionSection pds;

    private GridDefinitionSection<?> gds;

    private BitMapSection bms;

    private BinaryDataSection bds;

    /**
     * Attempts to read the GRIB 1 file at the specified {@link Path} and
     * constructs a {@link GribFile}.
     * 
     * @param gribFilePath
     *            the specified {@link Path} to the grib file
     * @return the constructed {@link GribFile}.
     * @throws InvalidGribException
     */
    public static GribFile loadGribFile(final Path gribFilePath)
            throws InvalidGribException {
        if (gribFilePath == null) {
            throw new IllegalArgumentException(
                    "Required argument 'gribFilePath' cannot be NULL.");
        }
        try (FileInputStream fis = new FileInputStream(gribFilePath.toFile())) {
            FileChannel fc = fis.getChannel();
            ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0,
                    fis.available());

            GribFile gribFile = new GribFile();
            gribFile.readHeader(byteBuffer);
            gribFile.readIndicatorSection(byteBuffer);
            gribFile.readProductDefinitionSection(byteBuffer);
            if (Boolean.TRUE.equals(gribFile.pds.getIncludeGDS())) {
                /*
                 * Only read the Grid Definition Section (GDS) if it has been
                 * included.
                 */
                gribFile.readGridDefinitionSection(byteBuffer);
            }
            if (Boolean.TRUE.equals(gribFile.pds.getIncludeBMS())) {
                /*
                 * Only read the Bit Map Section (BMS) if it has been included.
                 */
                gribFile.readBitMapSection(byteBuffer);
            }
            /*
             * Next will be the BDS.
             */
            gribFile.readBinaryDataSection(byteBuffer);
            /*
             * Read the end of the file.
             */
            gribFile.readEnding(byteBuffer);
            return gribFile;
        } catch (IOException | GridDefinitionLookupException e) {
            throw new InvalidGribException("Failed to read Grib file: "
                    + gribFilePath.toString() + ".", e);
        }
    }

    /**
     * Writes a GRIB 1 file to the specified destination {@link Path}.
     * 
     * @param destinationPath
     *            the specified destination {@link Path}
     * @return the number of bytes that were written to the file
     * @throws XmrgToGribConversionException
     */
    public int writeGrib(final Path destinationPath)
            throws XmrgToGribConversionException {
        validateFileStructure(true);

        try (OutputStream os = new BufferedOutputStream(
                Files.newOutputStream(destinationPath))) {
            writeHeader(os);
            is.writeSection(os);
            pds.writeSection(os);
            if (Boolean.TRUE.equals(pds.getIncludeGDS())) {
                /*
                 * Only write the Grid Definition Section (GDS) if it has been
                 * included.
                 */
                gds.writeSection(os);
            }
            if (Boolean.TRUE.equals(pds.getIncludeBMS())) {
                /*
                 * Only write the Bit Map Section (BMS) if it has been included.
                 */
                bms.writeSection(os);
            }
            bds.writeSection(os);
            writeEnding(os);
        } catch (IOException e) {
            throw new XmrgToGribConversionException(
                    "Failed to write grib file: " + destinationPath.toString()
                            + ".", e);
        }

        return is.getTotalGribSize() + GRIB_HEADER_LENGTH;
    }

    /**
     * Constructs an {@link IndicatorSection} based on the current state of the
     * Grib File. One of the primary data components of an Indicator Section is
     * the total size of the file excluding the size of the file header.
     * 
     * @throws XmrgToGribConversionException
     */
    public void prepareIndicatorSection() throws XmrgToGribConversionException {
        validateFileStructure(false);

        is = new IndicatorSection();
        int totalGribSize = IndicatorSection.NUM_BYTES;
        totalGribSize += pds.getNumberBytes();
        if (gds != null) {
            totalGribSize += gds.getNumberBytes();
        }
        if (bms != null) {
            totalGribSize += bms.getNumberBytes();
        }
        totalGribSize += bds.getNumberBytes();
        totalGribSize += GRIB_ENDING.length();
        is.setTotalGribSize(totalGribSize);
    }

    /**
     * Validates that all required sections within the Grib File have been
     * initialized. The existence of the {@link IndicatorSection} will
     * optionally be validated based on the specified include indicator boolean
     * flag.
     * 
     * 
     * @param includeIndicator
     *            the specified include indicator boolean flag. When
     *            {@code true}, the existence of the Indicator Section will not
     *            be validated.
     * @throws XmrgToGribConversionException
     */
    private void validateFileStructure(boolean includeIndicator)
            throws XmrgToGribConversionException {
        if (includeIndicator && is == null) {
            throw new XmrgToGribConversionException(
                    "The Indicator Section has not been initialized.");
        }
        if (pds == null) {
            throw new XmrgToGribConversionException(
                    "The Product Definition Section has not been initialized.");
        }
        if (Boolean.TRUE.equals(pds.getIncludeGDS()) && gds == null) {
            throw new XmrgToGribConversionException(
                    "The Grid Definition Section has not been initialized.");
        }
        if (Boolean.TRUE.equals(pds.getIncludeBMS()) && bms == null) {
            throw new XmrgToGribConversionException(
                    "The Bit Map Section has not been initialized.");
        }
        if (bds == null) {
            throw new XmrgToGribConversionException(
                    "The Bit Map Section has not been initialized.");
        }
    }

    private void readHeader(final ByteBuffer byteBuffer) {
        final byte[] headerBytes = new byte[GRIB_HEADER_LENGTH];
        byteBuffer.get(headerBytes);
        header = new String(headerBytes);
    }

    private void writeHeader(final OutputStream os) throws IOException {
        try {
            os.write(header.getBytes());
        } catch (IOException e) {
            throw new IOException("Failed to write the grib header.", e);
        }
    }

    private void readEnding(final ByteBuffer byteBuffer)
            throws InvalidGribException {
        final byte[] endingBytes = new byte[GRIB_ENDING.length()];
        byteBuffer.get(endingBytes);
        ending = new String(endingBytes);
        if (!GRIB_ENDING.equals(ending)) {
            throw new InvalidGribException("Read unexpected grib ending: "
                    + ending + ". Expected: " + GRIB_ENDING + ".");
        }
    }

    private void writeEnding(final OutputStream os) throws IOException {
        try {
            os.write(GRIB_ENDING.getBytes());
        } catch (IOException e) {
            throw new IOException("Failed to write the grib ending.", e);
        }
    }

    private void readIndicatorSection(final ByteBuffer byteBuffer)
            throws InvalidGribException {
        is = new IndicatorSection();
        is.readSection(byteBuffer);
    }

    private void readProductDefinitionSection(final ByteBuffer byteBuffer) {
        pds = new ProductDefinitionSection();
        pds.readSection(byteBuffer);
    }

    private void readGridDefinitionSection(final ByteBuffer byteBuffer)
            throws GridDefinitionLookupException {
        gds = GridDefinitionSectionFactory.getInstance().readDefinitionSection(
                byteBuffer);
    }

    private void readBitMapSection(final ByteBuffer byteBuffer) {
        bms = new BitMapSection();
        bms.readSection(byteBuffer);
    }

    private void readBinaryDataSection(final ByteBuffer byteBuffer) {
        bds = new BinaryDataSection();
        bds.readSection(byteBuffer);
    }

    public String getHeader() {
        return header;
    }

    public void setHeader(String header) {
        this.header = header;
    }

    public String getEnding() {
        return ending;
    }

    public void setEnding(String ending) {
        this.ending = ending;
    }

    public IndicatorSection getIs() {
        return is;
    }

    public void setIs(IndicatorSection is) {
        this.is = is;
    }

    public ProductDefinitionSection getPds() {
        return pds;
    }

    public void setPds(ProductDefinitionSection pds) {
        this.pds = pds;
    }

    public GridDefinitionSection<?> getGds() {
        return gds;
    }

    public void setGds(GridDefinitionSection<?> gds) {
        this.gds = gds;
    }

    public BitMapSection getBms() {
        return bms;
    }

    public void setBms(BitMapSection bms) {
        this.bms = bms;
    }

    public BinaryDataSection getBds() {
        return bds;
    }

    public void setBds(BinaryDataSection bds) {
        this.bds = bds;
    }
}