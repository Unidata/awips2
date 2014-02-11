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
package com.raytheon.uf.topo.utilities;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pypies.PyPiesDataStore;
import com.raytheon.uf.common.pypies.PypiesProperties;

/**
 * Import topo data into HDF5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2009     #3280  randerso    Initial creation
 * Feb 11, 2014     #2788  randerso    Changed to use PyPiesDataStore
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoImporter {

    private static class TopoHdr implements Comparable<TopoHdr> {
        File file;

        String byteOrder;

        int nRows;

        int nCols;

        int nBits;

        int totalRowBytes;

        long noData;

        double ulXmap;

        double ulYmap;

        double xDim;

        double yDim;

        public TopoHdr(File file) {
            if (file.getName().endsWith(".HDR")) {
                readHdrFile(file);
            } else if (file.getName().endsWith(".ers")) {
                readErsFile(file);
            } else {
                throw new IllegalArgumentException(
                        "Unrecognized header file format: "
                                + file.getAbsolutePath());
            }
        }

        private void readHdrFile(File file) {
            BufferedReader in = null;
            try {
                this.file = file;
                in = new BufferedReader(new FileReader(file));
                String line;
                while ((line = in.readLine()) != null) {
                    String[] s = line.split("\\s+");

                    if ("BYTEORDER".equals(s[0])) {
                        byteOrder = s[1];
                    } else if ("NROWS".equals(s[0])) {
                        nRows = Integer.parseInt(s[1]);
                    } else if ("NCOLS".equals(s[0])) {
                        nCols = Integer.parseInt(s[1]);
                    } else if ("NBITS".equals(s[0])) {
                        nBits = Integer.parseInt(s[1]);
                    } else if ("TOTALROWBYTES".equals(s[0])) {
                        totalRowBytes = Integer.parseInt(s[1]);
                    } else if ("NODATA".equals(s[0])) {
                        noData = Long.parseLong(s[1]);
                    } else if ("ULXMAP".equals(s[0])) {
                        ulXmap = Double.parseDouble(s[1]);
                    } else if ("ULYMAP".equals(s[0])) {
                        ulYmap = Double.parseDouble(s[1]);
                    } else if ("XDIM".equals(s[0])) {
                        xDim = Double.parseDouble(s[1]);
                    } else if ("YDIM".equals(s[0])) {
                        yDim = Double.parseDouble(s[1]);
                    } else {
                        System.out.println("Unrecognized line in file "
                                + file.getAbsolutePath() + "\n" + line);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (in != null) {
                    try {
                        in.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        private void readErsFile(File file) {
            BufferedReader in = null;
            try {
                this.file = file;
                in = new BufferedReader(new FileReader(file));
                String line;
                while ((line = in.readLine()) != null) {
                    line = line.replace(" = ", "   ");
                    String[] s = line.split("\\s+");

                    if ("ByteOrder".equals(s[0])) {
                        byteOrder = s[1];
                    } else if ("NrOfLines".equals(s[0])) {
                        nRows = Integer.parseInt(s[1]);
                    } else if ("NrOfCellsPerLine".equals(s[0])) {
                        nCols = Integer.parseInt(s[1]);
                    } else if ("CellType".equals(s[0])) {
                        if (s[1].equals("Signed16BitInteger")) {
                            nBits = 16;
                        } else if (s[1].equals("Signed32BitInteger")) {
                            nBits = 32;
                        } else if (s[1].equals("Signed64BitInteger")) {
                            nBits = 64;
                        } else {
                            throw new IllegalArgumentException(
                                    "Unrecognized data type: " + s[1]);
                        }
                    } else if ("NullCellValue".equals(s[0])) {
                        noData = Long.parseLong(s[1]);
                    } else if ("Longitude".equals(s[0])) {
                        ulXmap = parseDMS(s[1]);
                    } else if ("Latitude".equals(s[0])) {
                        ulYmap = parseDMS(s[1]);
                    } else if ("Xdimension".equals(s[0])) {
                        xDim = Double.parseDouble(s[1]);
                    } else if ("Ydimension".equals(s[0])) {
                        yDim = Double.parseDouble(s[1]);
                    } else {
                        // TODO: fully recognize the ers header file
                        System.out.println("Unrecognized line in file "
                                + file.getAbsolutePath() + "\n" + line);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (in != null) {
                    try {
                        in.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    // convert upper left coordinate from corner to
                    // center of cell
                    ulXmap += xDim / 2;
                    ulYmap -= yDim / 2;

                    totalRowBytes = (nCols * nBits) / 8;
                }
            }
        }

        private double parseDMS(String dms) {
            // parse a string in degrees:minutes:seconds format into decimal
            // degrees
            double degrees = Double.NaN;
            String[] s = dms.split(":");
            if (s.length == 3) {
                try {
                    int deg = Integer.parseInt(s[0]);
                    int min = Integer.parseInt(s[1]);
                    double sec = Double.parseDouble(s[2]);
                    degrees = deg + (min - ((sec / 60.0) / 60.0));
                } catch (NumberFormatException e) {
                    throw new IllegalArgumentException("Unable to parse DMS: "
                            + dms, e);
                }
            } else {
                throw new IllegalArgumentException("Unable to parse DMS: "
                        + dms);
            }
            return degrees;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(TopoHdr o) {
            // sort descending in Y, ascending in X
            int retVal = Double.compare(o.ulYmap, ulYmap);
            if (retVal == 0) {
                retVal = Double.compare(ulXmap, o.ulXmap);
            }
            return retVal;
        }
    }

    private static final double TOL = 0.0000001;

    /**
     * @param args
     */
    public static void main(String[] args) {
        long t0 = System.currentTimeMillis();
        FilenameFilter filter = new FilenameFilter() {

            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".HDR") || name.endsWith(".ers");
            }

        };

        if (args.length < 1) {
            System.out
                    .println("Usage: TopoImporter <topoDir>\n"
                            + "   Where <topoDir> is a directory path containing topo data in *.DEM and *.HDR file pairs.");
        }

        // read in all the hdr files
        File dir = new File(args[0]);
        System.out.println("Importing topo data from " + dir.getAbsolutePath());
        List<TopoHdr> hdrList = new ArrayList<TopoHdr>();
        for (File file : dir.listFiles(filter)) {
            hdrList.add(new TopoHdr(file));
        }

        if (hdrList.isEmpty()) {
            throw new IllegalArgumentException(
                    "No recognized header files found");
        }

        // sort the hdr files by descending lat/ascending lon
        Collections.sort(hdrList);

        // verify the files are in the expected order and
        // determine the total dataset dimensions
        double startLat = hdrList.get(0).ulYmap;
        double startLon = hdrList.get(0).ulXmap;
        double xDim = hdrList.get(0).xDim;
        double yDim = hdrList.get(0).yDim;

        int last = hdrList.size() - 1;
        double endLat = hdrList.get(last).ulYmap
                - ((hdrList.get(last).nRows - 1) * hdrList.get(last).yDim);
        double endLon = hdrList.get(last).ulXmap
                + ((hdrList.get(last).nCols - 1) * hdrList.get(last).xDim);
        int bits = hdrList.get(0).nBits;

        double expectedLat = startLat;
        double expectedLon = startLon;
        int maxRows = 0;
        int maxCols = 0;
        int rows = 0;
        int cols = 0;
        for (TopoHdr hdr : hdrList) {
            // is this the expected file ??
            if (Math.abs(expectedLat - hdr.ulYmap) > TOL) {
                System.out.println("Bad latitude. Expected: " + expectedLat
                        + "  got: " + hdr.ulYmap);
            }

            if (Math.abs(expectedLon - hdr.ulXmap) > TOL) {
                System.out.println("Bad longtude. Expected: " + expectedLat
                        + "  got: " + hdr.ulXmap);
            }

            // determine expected origin of next file
            cols += hdr.nCols;
            maxCols = Math.max(cols, maxCols);

            expectedLat = hdr.ulYmap;
            expectedLon = hdr.ulXmap + (hdr.nCols * hdr.xDim);
            if ((expectedLon - startLon) > (360.0 - TOL)) {
                expectedLon = startLon;
                expectedLat -= hdr.nRows * hdr.yDim;
                rows += hdr.nRows;
                maxRows = Math.max(rows, maxRows);
                cols = 0;
            }
        }

        // create the hdf5 file
        File hdf = new File("topo" + File.separatorChar
                + dir.getName().toLowerCase() + "_unpacked.h5");
        if (hdf.exists()) {
            System.out
                    .println(hdf.getAbsolutePath()
                            + " exists\nIf you wish to recreate, delete this file and try again.");
            System.exit(-1);
        }

        PypiesProperties pypiesProps = new PypiesProperties();
        pypiesProps.setAddress("http://localhost:9582");
        IDataStore store = new PyPiesDataStore(hdf, true, pypiesProps);

        String dataset = "full";
        long[] sizes = new long[] { maxCols, maxRows };

        StorageProperties properties = new StorageProperties();
        properties.setCompression(Compression.NONE);
        properties.setChunked(true);

        IDataRecord record = null;
        if (bits <= Byte.SIZE) {
            record = new ByteDataRecord(dataset, null, null, 2, sizes);
            record.setFillValue(Byte.MIN_VALUE);
        } else if (bits <= Short.SIZE) {
            record = new ShortDataRecord(dataset, null, null, 2, sizes);
            record.setFillValue(Short.MIN_VALUE);
        } else if (bits <= Integer.SIZE) {
            record = new IntegerDataRecord(dataset, null, null, 2, sizes);
            record.setFillValue(Integer.MIN_VALUE);
        } else if (bits <= Long.SIZE) {
            record = new LongDataRecord(dataset, null, null, 2, sizes);
            record.setFillValue(Long.MIN_VALUE);
        } else {
            System.out.println("NBITS > 64");
            System.exit(-1);
        }

        double centralMeridian = 0.0;
        double latOfOrigin = 0.0;

        double cm = (startLon + endLon) / 2;
        centralMeridian = Math.round(cm);

        ProjectedCRS crs = MapUtil.constructEquidistantCylindrical(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                centralMeridian, latOfOrigin);

        Map<String, Object> attributes = new LinkedHashMap<String, Object>();
        attributes.put("Width", maxCols);
        attributes.put("Height", maxRows);

        // Using arrays to work around serialization "feature" that returns
        // floats when doubles are stored
        // H5DataStore created these arrays automatically, Pypies does not
        attributes.put("xDim", new double[] { xDim });
        attributes.put("yDim", new double[] { yDim });
        attributes.put("ulLat", new double[] { startLat });
        attributes.put("ulLon", new double[] { startLon });
        attributes.put("lrLat", new double[] { endLat });
        attributes.put("lrLon", new double[] { endLon });
        attributes.put("CRS", crs.toWKT());

        record.setProperties(properties);
        record.setDataAttributes(attributes);

        try {
            store.createDataset(record);
        } catch (Exception e) {
            e.printStackTrace();
        }

        rows = 0;
        cols = 0;
        for (TopoHdr hdr : hdrList) {
            String demPath = hdr.file.getAbsolutePath();
            if (demPath.endsWith(".HDR")) {
                demPath = demPath.replace(".HDR", ".DEM");
            } else if (demPath.endsWith(".ers")) {
                demPath = demPath.replace(".ers", "");
            }

            File dem = new File(demPath);

            System.out.println(cols + ", " + rows + "  " + dem.getName());

            DataInputStream in = null;
            try {
                in = new DataInputStream(new FileInputStream(dem));
                ByteBuffer buffer = ByteBuffer.allocate(hdr.totalRowBytes);

                if (hdr.byteOrder.startsWith("M")) {
                    buffer.order(ByteOrder.BIG_ENDIAN);
                } else {
                    buffer.order(ByteOrder.LITTLE_ENDIAN);
                }

                sizes[0] = hdr.nCols;
                sizes[1] = 1;
                record = null;
                long writeSize = 0;
                for (int i = 0; i < hdr.nRows; i++) {
                    in.read(buffer.array());
                    buffer.rewind();
                    if (bits <= Byte.SIZE) {
                        byte[] byteData = new byte[hdr.nCols];
                        for (int j = 0; j < byteData.length; j++) {
                            byteData[j] = buffer.get();

                            // replace no data flag with 0
                            if (byteData[j] == (byte) hdr.noData) {
                                byteData[j] = Byte.MIN_VALUE;
                            }
                        }
                        record = new ByteDataRecord(dataset, null, null, 2,
                                sizes);
                        record.setMinIndex(new long[] { cols, rows + i });
                        record.setFillValue(Byte.MIN_VALUE);
                        ((ByteDataRecord) record).setByteData(buffer.array());
                    } else if (bits <= Short.SIZE) {
                        short[] shortData = new short[hdr.nCols];
                        for (int j = 0; j < shortData.length; j++) {
                            shortData[j] = buffer.getShort();

                            // replace no data flag with 0
                            if (shortData[j] == (short) hdr.noData) {
                                shortData[j] = Short.MIN_VALUE;
                            }
                        }
                        record = new ShortDataRecord(dataset, null, null, 2,
                                sizes);
                        record.setMinIndex(new long[] { cols, rows + i });
                        record.setFillValue(Short.MIN_VALUE);
                        ((ShortDataRecord) record).setShortData(shortData);
                    } else if (bits <= Integer.SIZE) {
                        int[] intData = new int[hdr.nCols];
                        for (int j = 0; j < intData.length; j++) {
                            intData[j] = buffer.getInt();

                            // replace no data flag with 0
                            if (intData[j] == (int) hdr.noData) {
                                intData[j] = Integer.MIN_VALUE;
                            }
                        }
                        record = new IntegerDataRecord(dataset, null, null, 2,
                                sizes);
                        record.setMinIndex(new long[] { cols, rows + i });
                        record.setFillValue(Integer.MIN_VALUE);
                        ((IntegerDataRecord) record).setIntData(intData);
                    } else if (bits <= Long.SIZE) {
                        long[] longData = new long[hdr.nCols];
                        for (int j = 0; j < longData.length; j++) {
                            longData[j] = buffer.getLong();

                            // replace no data flag with 0
                            if (longData[j] == hdr.noData) {
                                longData[j] = Long.MIN_VALUE;
                            }
                        }
                        record = new LongDataRecord(dataset, null, null, 2,
                                sizes);
                        record.setMinIndex(new long[] { cols, rows + i });
                        record.setFillValue(Long.MIN_VALUE);
                        ((LongDataRecord) record).setLongData(longData);
                    }

                    store.addDataRecord(record, properties);
                    writeSize += hdr.totalRowBytes;
                    if (writeSize > (64 * 1024 * 1024)) {
                        store.store();
                        writeSize = 0;
                    }
                }
                store.store();
            } catch (Throwable e) {
                e.printStackTrace();
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            // determine origin of next file
            cols += hdr.nCols;

            if (cols >= maxCols) {
                rows += hdr.nRows;
                cols = 0;
            }
        }
        System.out.println("took " + ((System.currentTimeMillis() - t0) / 1000)
                + " seconds");
    }
}
