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
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
 * Mar 15, 2018  6353     rjpeter     Use iterator to return messages as found.
 * Jun 22, 2018  7324     tjensen     Integrate NCEP performance enhancements
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GribSplitter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    // KPMMatch is thread safe
    private static final KMPMatch matcher = new KMPMatch("GRIB".getBytes());

    public Iterator<GribDecodeMessage> split(File file) throws GribException {
        /*
         * Utilize an iterator for a message is returned after each seek. Mainly
         * needed for NCEP use case of GB grib files
         */
        return new GribIterator(file);
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

    private class GribIterator implements Iterator<GribDecodeMessage> {
        private final String absolutePath;

        private final RandomAccessFile raf;

        private GribDecodeMessage next = null;

        private boolean done = false;

        public GribIterator(File file) throws GribException {
            absolutePath = file.getAbsolutePath();
            try {
                raf = new RandomAccessFile(absolutePath, "r");
                raf.order(RandomAccessFile.BIG_ENDIAN);
            } catch (IOException e) {
                throw new GribException("Unable to split file: " + absolutePath,
                        e);
            }
        }

        @Override
        public boolean hasNext() {
            if (next == null) {
                if (done) {
                    return false;
                }

                try {
                    if (raf.searchForward(matcher, Integer.MAX_VALUE)) {
                        next = new GribDecodeMessage();
                        next.setFileName(absolutePath);
                        long startPosition = raf.getFilePointer();
                        next.setStartPosition(startPosition);
                        raf.skipBytes(4);
                        raf.skipBytes(3);
                        byte edition = raf.readByte();
                        next.setGribEdition(edition);
                        long length;
                        switch (edition) {
                        case 1:
                            next.setGridPointCount(
                                    getGrib1GridPointCount(raf, startPosition));
                            raf.seek(startPosition + 4);
                            length = GribNumbers.uint3(raf);
                            raf.skipBytes(1);
                            break;
                        case 2:
                            length = GribNumbers.int8(raf);
                            next.setGridPointCount(getGrib2GridPointCount(raf,
                                    startPosition, length));
                            break;
                        default:
                            /*
                             * This is not a grid we can handle. Let the message
                             * proceed to the decoder to throw errors.
                             */
                            next.setGridPointCount(0);
                            length = 8;
                            break;
                        }
                        next.setMessageLength(length);

                        /*
                         * A significant amount of files contain one grib record
                         * with several bytes of gibberish on the end. This
                         * prevents us from reading the gibberish if it is too
                         * small to be a grib record anyway.
                         */
                        if (raf.length() < startPosition + length + 24) {
                            done = true;
                            ;
                        } else {
                            raf.seek(startPosition + length);
                        }
                    } else {
                        done = true;
                    }
                } catch (IOException e) {
                    done = true;
                    next = null;
                    logger.error("Unable to split file: " + absolutePath, e);
                }
                if (done) {
                    try {
                        raf.close();
                    } catch (Throwable e) {
                        logger.warn("Cannot close grib file: " + absolutePath,
                                e);
                    }
                }
            }
            return next != null;
        }

        @Override
        public GribDecodeMessage next() {
            /*
             * ensure hasNext() was called as this implementation does all the
             * work in hasNext()
             */
            if (hasNext()) {
                GribDecodeMessage rval = next;
                next = null;
                return rval;
            }

            throw new NoSuchElementException(
                    "No remaining grib records in file: " + absolutePath);
        }

    }

}
