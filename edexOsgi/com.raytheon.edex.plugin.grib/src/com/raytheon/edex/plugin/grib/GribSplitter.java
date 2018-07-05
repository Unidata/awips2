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
package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.uf.common.gridcoverage.GridCoverage;

import ucar.nc2.grib.GribNumbers;
import ucar.unidata.io.KMPMatch;
import ucar.unidata.io.RandomAccessFile;

/**
 * 
 * Split a single grib file into one or more {@link GribDecodeMessage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 07, 2013  2402     bsteffen    Rewritten to output GribDecodeMessage.
 * Sep 11, 2017  6406     bsteffen    Upgrade ucar
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribSplitter {

    private static final KMPMatch matcher = new KMPMatch("GRIB".getBytes());

    public List<GribDecodeMessage> split(File file) throws GribException {
        List<GribDecodeMessage> messages = new ArrayList<>();
        try (RandomAccessFile raf = new RandomAccessFile(file.getAbsolutePath(),
                "r")) {
            raf.order(RandomAccessFile.BIG_ENDIAN);
            while (raf.searchForward(matcher, Integer.MAX_VALUE)) {
                GribDecodeMessage message = new GribDecodeMessage();
                message.setFileName(file.getAbsolutePath());
                long startPosition = raf.getFilePointer();
                message.setStartPosition(startPosition);
                raf.skipBytes(4);
                raf.skipBytes(3);
                byte edition = raf.readByte();
                message.setGribEdition(edition);
                long length;
                switch (edition) {
                case 1:
                    message.setGridPointCount(
                            getGrib1GridPointCount(raf, startPosition));
                    raf.seek(startPosition + 4);
                    length = GribNumbers.uint3(raf);
                    raf.skipBytes(1);
                    break;
                case 2:
                    length = GribNumbers.int8(raf);
                    message.setGridPointCount(
                            getGrib2GridPointCount(raf, startPosition, length));
                    break;
                default:
                    /*
                     * This is not a grid we can handle. Let the message proceed
                     * to the decoder to throw errors.
                     */
                    message.setGridPointCount(0);
                    length = 8;
                    break;
                }
                message.setMessageLength(length);
                messages.add(message);
                /*
                 * A significant amount of files contain one grib record with
                 * several bytes of gibberish on the end. This prevents us from
                 * reading the gibberish if it is too small to be a grib record
                 * anyway.
                 */
                if (raf.length() < startPosition + length + 24) {
                    break;
                }
                raf.seek(startPosition + length);
            }
        } catch (IOException e) {
            throw new GribException(
                    "Unable to split file: " + file.getAbsolutePath(), e);
        }
        return messages;
    }

    private long getGrib2GridPointCount(RandomAccessFile raf,
            long startPosition, long messageLength) throws IOException {
        long gridPointCount = 0;
        long totalGridPointCount = 0;
        long start = startPosition + 16;
        while (start < startPosition + messageLength - 4) {
            raf.seek(start);
            int length = raf.readInt();
            int section = raf.readByte();
            switch (section) {
            case 3:
                raf.seek(start + 30);
                int nx = raf.readInt();
                int ny = raf.readInt();
                gridPointCount = nx * ny;
                break;
            case 7:
                totalGridPointCount += gridPointCount;
                break;
            }
            start += length;
        }
        return totalGridPointCount;
    }

    private long getGrib1GridPointCount(RandomAccessFile raf,
            long startPosition) throws IOException {
        raf.seek(startPosition + 8);
        int pdsLength = (raf.readUnsignedShort() << 8) + raf.readUnsignedByte();
        raf.skipBytes(3);
        int grid = raf.readUnsignedByte();
        int gdsPresent = raf.readUnsignedByte() & 0x80;
        if (grid != 255) {
            GridCoverage coverage = GribSpatialCache.getInstance()
                    .getGridByName(String.valueOf(grid));
            if (coverage != null) {
                return coverage.getNx() * coverage.getNy();
            }
        }
        if (gdsPresent != 0) {
            raf.seek(startPosition + 8 + pdsLength + 6);
            /*
             * Note: for Quasi-regular grids nx or ny may be coded as 0xFFFF
             * which will result in a dramatic overestimate of the number of
             * grid points. All such known grids are identified by grid number
             * above.
             */
            int nx = raf.readUnsignedShort();
            int ny = raf.readUnsignedShort();
            return nx * ny;
        }
        /*
         * This is not a grid we can handle. Let the message proceed to the
         * decoder to throw errors.
         */
        return 0;
    }

}
