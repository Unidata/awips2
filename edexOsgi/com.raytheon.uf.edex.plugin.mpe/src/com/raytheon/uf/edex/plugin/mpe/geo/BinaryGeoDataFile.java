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
package com.raytheon.uf.edex.plugin.mpe.geo;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;

/**
 * POJO representative of the structure of a binary hydroapps geo data file. The
 * standard location for these files is:
 * /awips2/edex/data/share/hydroapps/geo_data/host/binary. Based on:
 * /rary.ohd.pproc/inc/save_gif.h.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2016            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BinaryGeoDataFile {

    // sizeof(float) * 2.
    private static final int TWO_FLOATS_SIZE = 8;

    // sizeof(int)
    private static final int INT_SIZE = 4;

    private static final int ID_SIZE = 9;

    private static final int NAME_SIZE = 21;

    private String id;

    private String name;

    private int order;

    private int numberPoints;

    private List<Coordinate> hrap;

    /**
     * Identifies the file that was read recently or written. Will be
     * {@code null} when no I/O operations have occurred.
     */
    private Path filePath;

    public static BinaryGeoDataFile loadFile(final Path filePath)
            throws IOException {
        BinaryGeoDataFile binaryGeoData = new BinaryGeoDataFile();
        final byte[] geoDataBytes = Files.readAllBytes(filePath);
        final ByteBuffer byteBuffer = ByteBuffer.wrap(geoDataBytes);
        try {
            /*
             * file contents are little endian
             */
            byteBuffer.order(ByteOrder.LITTLE_ENDIAN);

            binaryGeoData.readFile(byteBuffer);
        } catch (BinGeoDataInconsistentException e) {
            throw new IOException("Failed to read binary geo data file: "
                    + filePath.toString() + ".", e);
        }

        binaryGeoData.filePath = filePath;
        return binaryGeoData;
    }

    private void readFile(final ByteBuffer byteBuffer)
            throws BinGeoDataInconsistentException {
        verifyStructure(ID_SIZE, BinGeoDataInconsistentException.SECTION_ID,
                byteBuffer);
        /*
         * 9 characters must be read initially. Remember a char in C/C++ is only
         * one byte compared to the two byte characters in Java. So, the
         * characters will be read as 9 bytes.
         */
        byte[] idBytes = new byte[ID_SIZE];
        byteBuffer.get(idBytes);
        id = new String(idBytes);

        verifyStructure(NAME_SIZE,
                BinGeoDataInconsistentException.SECTION_NAME, byteBuffer);
        /*
         * Same as above. 21 characters will be read as 21 bytes.
         */
        byte[] nameBytes = new byte[NAME_SIZE];
        byteBuffer.get(nameBytes);
        name = new String(nameBytes);

        verifyStructure(INT_SIZE,
                BinGeoDataInconsistentException.SECTION_ORDER, byteBuffer);
        order = byteBuffer.getInt();

        verifyStructure(INT_SIZE,
                BinGeoDataInconsistentException.SECTION_NUMBER_POINTS,
                byteBuffer);
        numberPoints = byteBuffer.getInt();

        verifyStructure(TWO_FLOATS_SIZE * numberPoints,
                BinGeoDataInconsistentException.SECTION_HRAP, byteBuffer);
        hrap = new ArrayList<>(numberPoints);
        for (int i = 0; i < numberPoints; i++) {
            float x = byteBuffer.getFloat();
            float y = byteBuffer.getFloat();
            hrap.add(new Coordinate(x, y));
        }
    }

    private void verifyStructure(final int bytesRequired,
            final String sectionName, final ByteBuffer byteBuffer)
            throws BinGeoDataInconsistentException {
        if (byteBuffer.remaining() < bytesRequired) {
            throw new BinGeoDataInconsistentException(sectionName,
                    bytesRequired, byteBuffer.remaining());
        }
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getOrder() {
        return order;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public int getNumberPoints() {
        return numberPoints;
    }

    public void setNumberPoints(int numberPoints) {
        this.numberPoints = numberPoints;
    }

    public List<Coordinate> getHrap() {
        return hrap;
    }

    public void setHrap(List<Coordinate> hrap) {
        this.hrap = hrap;
    }

    public Path getFilePath() {
        return filePath;
    }
}