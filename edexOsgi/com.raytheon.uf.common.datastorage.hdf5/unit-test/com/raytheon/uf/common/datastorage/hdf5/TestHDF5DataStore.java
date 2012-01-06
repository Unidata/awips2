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
package com.raytheon.uf.common.datastorage.hdf5;

import java.awt.Point;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;

/**
 * TestHDF5DataStore
 * 
 * Use case test to validate hdf5 datastore capabilities
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 24, 2007             chammack    Initial Creation.
 *    Apr 01, 2008 #1041       chammack    Added additional test cases
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TestHDF5DataStore {

    @Test
    public void partialWritesTest() {
        File file = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (file.exists()) {
            file.delete();
        }

        HDF5DataStore store = (HDF5DataStore) DataStoreFactory
                .getDataStore(file);

        String dataset = "test";
        int dim = 2;
        long[] sizes = new long[] { 16, 16 };

        int fillValue = 9;
        IntegerDataRecord record = new IntegerDataRecord(dataset, "/a/b/c",
                null, dim, sizes);
        record.setFillValue(fillValue);

        try {
            // store.addDataRecord(record);
            // store.store();
            store.createDataset(record);
        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        int[] data = new int[16];
        for (int i = 0; i < 16; ++i) {
            data[i] = 1;
        }

        sizes = new long[] { 1, 16 };

        IntegerDataRecord record1 = new IntegerDataRecord(dataset, "/a/b/c",
                data, dim, sizes);
        record1.setMinIndex(new long[] { 7, 0 });

        data = new int[16];
        for (int i = 0; i < 16; ++i) {
            data[i] = 2;
        }

        sizes = new long[] { 16, 1 };

        IntegerDataRecord record2 = new IntegerDataRecord(dataset, "/a/b/c",
                data, dim, sizes);
        record2.setMinIndex(new long[] { 0, 7 });

        data = new int[16];
        for (int i = 0; i < 16; ++i) {
            data[i] = 3;
        }

        sizes = new long[] { 4, 4 };

        IntegerDataRecord record3 = new IntegerDataRecord(dataset, "/a/b/c",
                data, dim, sizes);
        record3.setMinIndex(new long[] { 6, 6 });

        try {
            store.addDataRecord(record1);
            store.addDataRecord(record2);
            store.addDataRecord(record3);
            store.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        try {
            IntegerDataRecord retrieve = (IntegerDataRecord) store.retrieve(
                    "/a/b/c", dataset, Request.ALL);
            int[] retData = retrieve.getIntData();
            for (int y = 0; y < 16; ++y) {
                for (int x = 0; x < 16; ++x) {
                    int i = y * 16 + x;
                    int value = retData[i];
                    if (x >= 6 && x < 10 && y >= 6 && y < 10) {
                        Assert.assertEquals(3, value);
                    } else if (x == 7) {
                        Assert.assertEquals(1, value);
                    } else if (y == 7) {
                        Assert.assertEquals(2, value);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());

        }

    }

    @Test
    public void extensiveRoundtripTest() {

        // Extensive roundtrip test behavior:
        // - Create a simple integer based hdf5 record
        // - Retrieve the whole dataset
        // - Retrieve a rectangular subset of the dataset
        // - Retrieve specific points from the dataset
        // - Delete the dataset
        // - Delete the group that the dataset was in

        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        int[] d = new int[16];
        for (int i = 0; i < 16; i++) {
            d[i] = i;
        }

        IntegerDataRecord idr = new IntegerDataRecord("ds", "/a/b/c", d, 2,
                new long[] { 4, 4 });

        IDataStore ds = DataStoreFactory.getDataStore(f);

        try {
            ds.addDataRecord(idr);
            ds.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
            return;
        }

        try {
            IDataRecord dr = ds.retrieve("/a/b/c", "ds", Request.ALL);
            Assert.assertNotNull(dr);
            int[] data = ((IntegerDataRecord) dr).getIntData();
            Assert.assertNotNull(data);
            Assert.assertTrue(data.length == 16);
            for (int i = 0; i < 16; i++) {
                Assert.assertEquals(i, data[i]);
            }

            // Test slab case
            Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] {
                    2, 2 });
            dr = ds.retrieve("/a/b/c", "ds", request);
            Assert.assertNotNull(dr);
            data = ((IntegerDataRecord) dr).getIntData();
            Assert.assertNotNull(data);
            Assert.assertEquals(4, data.length);
            Assert.assertEquals(data[0], 0);
            Assert.assertEquals(data[1], 1);
            Assert.assertEquals(data[2], 4);
            Assert.assertEquals(data[3], 5);

            // Test points retrieval case
            request = Request.buildPointRequest(new Point[] { new Point(0, 0),
                    new Point(3, 3) });
            dr = ds.retrieveGroups(new String[] { "/a/b/c" }, request)[0];
            Assert.assertNotNull(dr);
            data = ((IntegerDataRecord) dr).getIntData();
            Assert.assertNotNull(data);
            Assert.assertEquals(2, data.length);
            Assert.assertEquals(0, data[0]);
            Assert.assertEquals(15, data[1]);

            // Test deletion
            ds.delete("/a/b/c/ds");
            ds.delete("/a/b/c");

            try {
                ds.retrieve("/a/b/c", "ds", Request.ALL);
                Assert.fail("Dataset was not deleted");
            } catch (StorageException e) {
                // this is desired behavior
            }

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

    }

    private static float[] dTest = new float[337 * 451];
    static {

        Random r = new Random();
        for (int i = 0; i < dTest.length; i++) {
            dTest[i] = r.nextInt(254);
        }
    }

    private static Point[] p = new Point[500];

    private static final String[] s = new String[10];

    private static final float[] f1 = new float[10];
    static {
        Random r = new Random();
        for (int i = 0; i < p.length; i++) {
            p[i] = new Point(r.nextInt(261), 0);
        }

        Set<Point> pSet = new HashSet<Point>(Arrays.asList(p));
        p = pSet.toArray(new Point[pSet.size()]);

        for (int i = 0; i < s.length; i++) {
            s[i] = "HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO HELLO";
            f1[i] = i;
        }
    }

    @Test
    public void timingTests() {
        File f = new File("/tmp/testc" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }
        IDataStore ds = DataStoreFactory.getDataStore(f);

        FloatDataRecord idr = new FloatDataRecord("ds", "/a/b/c", dTest, 2,
                new long[] { 337, 451 });

        try {
            ds.addDataRecord(idr);
            StorageStatus ss = ds.store(StoreOp.STORE_ONLY);
            if (ss.getExceptions().length > 0) {
                ss.getExceptions()[0].printStackTrace();
            }

        } catch (StorageException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        long t0 = System.currentTimeMillis();
        for (int z = 0; z < 100; z++) {
            if (f.exists()) {
                f.delete();
            }

            try {
                IDataRecord dr = ds.retrieve("/a/b/z", "ds", Request.ALL);
            } catch (Throwable e) {
                // ignore
            }

            idr = new FloatDataRecord("ds", "/a/b/c", dTest, 2, new long[] {
                    337, 451 });

            StringDataRecord sdr = new StringDataRecord("sdr", "/a/b/e", s);

            try {
                StorageProperties sp = new StorageProperties();
                sp.setChunked(true);
                sp.setCompression(Compression.LZF);
                ds.addDataRecord(idr, sp);
                ds.addDataRecord(sdr, sp);
                ds.store();
            } catch (StorageException e) {
                e.printStackTrace();
                Assert.assertNull(e);
                return;
            }

            try {
                IDataRecord dr = ds.retrieve("/a/b/c", "ds", Request.ALL);
                Assert.assertNotNull(dr);
                float[] data = ((FloatDataRecord) dr).getFloatData();
                Assert.assertNotNull(data);
                Assert.assertEquals(data.length, dTest.length);

                for (int i = 0; i < data.length; i++) {
                    Assert.assertEquals(dTest[i], data[i], 0.00001);
                }

                dr = ds.retrieve("/a/b/e", "sdr", Request.ALL);

                try {
                    dr = ds.retrieve("/a/b/z", "ds", Request.ALL);
                } catch (Exception e) {
                    // ignore
                }

            } catch (Exception e) {
                e.printStackTrace();
                Assert.fail(e.getMessage());
            }
        }

        System.out.println("Took: " + (System.currentTimeMillis() - t0));

        // System.out.println("Starting large string array benchmark:");

        f = new File("/tmp/testc" + Thread.currentThread().getName() + ".h5");
        if (f.exists()) {
            f.delete();
        }
        ds = DataStoreFactory.getDataStore(f);

        for (int k = 0; k < 100; k++) {
            StringDataRecord sdr = new StringDataRecord("sdr", "/a/b/e", s);
            FloatDataRecord fdr = new FloatDataRecord("fdr", "/a/b/e", f1);
            FloatDataRecord fdr2 = new FloatDataRecord("fdr2", "/a/b/e", f1);
            FloatDataRecord fdr3 = new FloatDataRecord("fdr3", "/a/b/e", f1);

            try {
                StorageProperties sp = new StorageProperties();
                sp.setChunked(true);
                // sp.setCompression(Compression.LZF);
                ds.addDataRecord(sdr, sp);
                ds.addDataRecord(fdr, sp);
                ds.addDataRecord(fdr2, sp);
                ds.addDataRecord(fdr3, sp);
                StorageStatus ss = ds.store(StoreOp.APPEND);
                if (ss.getExceptions() != null) {
                    for (StorageException e : ss.getExceptions()) {
                        e.printStackTrace();
                    }
                }
            } catch (StorageException e) {
                e.printStackTrace();
                Assert.assertNull(e);
                return;
            }
        }

        System.out.println("Retrieving");
        Request request = Request.buildPointRequest(p);
        t0 = System.currentTimeMillis();
        try {
            IDataRecord[] dr = ds.retrieveDatasets(
                    new String[] { "/a/b/e/sdr" }, request);

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        // System.out.println("Took: " + (System.currentTimeMillis() - t0));

    }

    @Test
    public void validReplaceTest() {
        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        IDataStore ds = DataStoreFactory.getDataStore(f);

        float[] d1 = new float[16];
        FloatDataRecord fdr1 = new FloatDataRecord("ds", "/a/b/c", d1);

        try {
            ds.addDataRecord(fdr1);
            ds.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
            return;
        }

        float[] d2 = new float[16];
        FloatDataRecord fdr2 = new FloatDataRecord("ds", "/a/b/c", d2);

        // try {
        // ds.addDataRecord(fdr2);
        // StorageStatus status = ds.store(StoreOp.REPLACE);
        // StorageException[] exceptions = status.getExceptions();
        // if (exceptions != null) {
        // for (StorageException e : exceptions) {
        // e.printStackTrace();
        // }
        // }
        // Assert.assertFalse(exceptions != null && exceptions.length > 0);
        // } catch (StorageException e) {
        // e.printStackTrace();
        // Assert.assertNull(e);
        // return;
        // }

        // Now retrieve and make sure we get back the FloatDataRecord, not the
        // IntegerDataRecord

        // try {
        // IDataRecord rec = ds.retrieve("/a/b/c", "ds", Request.ALL);
        // Assert.assertNotNull(rec);
        // Assert.assertTrue(rec instanceof FloatDataRecord);
        // Assert.assertArrayEquals(new long[] { 16 }, rec.getSizes());
        // } catch (Exception e) {
        // e.printStackTrace();
        // Assert.assertNull(e);
        // }

    }

    @Test
    public void appendDataSet2DTest() {
        File f = new File("/tmp/testAppend" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        int maxRandSize = 3 * 1000;

        Random r = new Random();
        int[] i = new int[maxRandSize];
        for (int k = 0; k < maxRandSize; k++) {
            i[k] = r.nextInt();
        }

        IntegerDataRecord idr = new IntegerDataRecord("x", "", i, 2,
                new long[] { 3, 1000 });
        idr.setMaxSizes(new long[] { 3, 0 });
        IDataStore ds = DataStoreFactory.getDataStore(f);

        try {
            ds.addDataRecord(idr);
            ds.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }
        ds = null;

        int[] i2 = new int[] { 6, 7, 8 };
        IntegerDataRecord idr2 = new IntegerDataRecord("x", "", i2, 2,
                new long[] { 3, 1 });
        idr.setMaxSizes(new long[] { 0, 3 });
        IDataStore ds2 = DataStoreFactory.getDataStore(f);

        try {
            ds2.addDataRecord(idr2);
            ds2.store(StoreOp.APPEND);
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        ds2 = null;

        IDataStore ds3 = DataStoreFactory.getDataStore(f);

        try {
            IDataRecord dr = ds3.retrieve("", "x", Request.ALL);
            Assert.assertTrue(dr instanceof IntegerDataRecord);
            Assert.assertEquals(dr.getSizes()[0], 3);
            Assert.assertEquals(dr.getSizes()[1], 3);
            int[] data = ((IntegerDataRecord) dr).getIntData();

            int[] expected = new int[] { 0, 2, 4, 1, 3, 5, 6, 7, 8 };
            for (int z = 0; z < expected.length; z++) {
                Assert.assertEquals(expected[z], data[z]);
            }

        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        long t0 = System.currentTimeMillis();
        for (int b = 0; b < 100; b++) {
            try {
                Point p1 = new Point(0, 0);
                Point p2 = new Point(2, 1);
                Request request = Request.buildPointRequest(new Point[] { p1,
                        p2 });
                IDataRecord[] dr = ds3.retrieveDatasets(new String[] { "x" },
                        request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof IntegerDataRecord);
                int[] data = ((IntegerDataRecord) dr[0]).getIntData();

                int[] expected = new int[] { 0, 5 };
                for (int z = 0; z < expected.length; z++) {
                    Assert.assertEquals(expected[z], data[z]);
                }

            } catch (Exception e) {
                e.printStackTrace();
                Assert.assertNull(e);
            }
        }
        for (int b = 0; b < 100; b++) {
            try {
                int[] p1 = new int[2];
                p1[0] = 0;
                p1[1] = 2;
                Request request = Request.buildYLineRequest(p1);
                IDataRecord[] dr = ds3.retrieveDatasets(new String[] { "x" },
                        request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof IntegerDataRecord);
                Assert.assertEquals(dr[0].getDimension(), 2);
                long[] sz = dr[0].getSizes();
                Assert.assertEquals(sz.length, 2);
                Assert.assertEquals(sz[0], 3);
                Assert.assertEquals(sz[1], 2);

                int[] data = ((IntegerDataRecord) dr[0]).getIntData();

                int[] expected = new int[] { 0, 2, 4, 6, 7, 8 };
                for (int z = 0; z < expected.length; z++) {
                    Assert.assertEquals(expected[z], data[z]);
                }

                p1 = new int[] { 0, 2 };
                request = Request.buildXLineRequest(p1);
                dr = ds3.retrieveDatasets(new String[] { "x" }, request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof IntegerDataRecord);
                Assert.assertEquals(dr[0].getDimension(), 2);
                sz = dr[0].getSizes();
                Assert.assertEquals(sz.length, 2);
                Assert.assertEquals(sz[0], 2);
                Assert.assertEquals(sz[1], 3);

                data = ((IntegerDataRecord) dr[0]).getIntData();

                // Results are still in column major order
                expected = new int[] { 0, 4, 1, 5, 6, 8 };
                for (int z = 0; z < expected.length; z++) {
                    Assert.assertEquals(expected[z], data[z]);
                }

            } catch (Exception e) {
                e.printStackTrace();
                Assert.assertNull(e);
            }
        }
        long t1 = System.currentTimeMillis();
        System.out.println("Append2D tests timing: " + (t1 - t0));

    }

    @Test
    public void appendDataSet2DStringsTest() {
        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        String[] i = new String[] { "abcdefg", "hijk", "lmnop", "q", "rs",
                "tuvw" };

        StringDataRecord idr = new StringDataRecord("x", "", i, 2, new long[] {
                3, 2 });
        IDataStore ds = DataStoreFactory.getDataStore(f);

        try {
            ds.addDataRecord(idr);
            ds.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }
        ds = null;

        String[] i2 = new String[] { "1234", "5678", "9" };
        StringDataRecord idr2 = new StringDataRecord("x", "", i2, 2,
                new long[] { 3, 1 });
        IDataStore ds2 = DataStoreFactory.getDataStore(f);

        try {
            ds2.addDataRecord(idr2);
            ds2.store(StoreOp.APPEND);
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        ds2 = null;

        IDataStore ds3 = DataStoreFactory.getDataStore(f);

        try {
            IDataRecord dr = ds3.retrieve("", "x", Request.ALL);
            Assert.assertTrue(dr instanceof StringDataRecord);
            Assert.assertEquals(dr.getSizes()[0], 3);
            Assert.assertEquals(dr.getSizes()[1], 3);
            String[] data = ((StringDataRecord) dr).getStringData();

        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        for (int b = 0; b < 100; b++) {
            try {
                Point p1 = new Point(0, 0);
                Point p2 = new Point(2, 1);
                Request request = Request.buildPointRequest(new Point[] { p1,
                        p2 });
                IDataRecord[] dr = ds3.retrieveDatasets(new String[] { "x" },
                        request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof StringDataRecord);

            } catch (Exception e) {
                e.printStackTrace();
                Assert.assertNull(e);
            }
        }
        for (int b = 0; b < 100; b++) {
            try {
                int[] p1 = new int[2];
                p1[0] = 0;
                p1[1] = 2;
                Request request = Request.buildYLineRequest(p1);
                IDataRecord[] dr = ds3.retrieveDatasets(new String[] { "x" },
                        request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof StringDataRecord);
                Assert.assertEquals(dr[0].getDimension(), 2);
                long[] sz = dr[0].getSizes();
                Assert.assertEquals(sz.length, 2);
                Assert.assertEquals(sz[0], 3);
                Assert.assertEquals(sz[1], 2);

                p1 = new int[] { 0, 2 };
                request = Request.buildXLineRequest(p1);
                dr = ds3.retrieveDatasets(new String[] { "x" }, request);
                Assert.assertTrue(dr.length == 1);
                Assert.assertTrue(dr[0] instanceof StringDataRecord);
                Assert.assertEquals(dr[0].getDimension(), 2);
                sz = dr[0].getSizes();
                Assert.assertEquals(sz.length, 2);
                Assert.assertEquals(sz[0], 2);
                Assert.assertEquals(sz[1], 3);

            } catch (Exception e) {
                e.printStackTrace();
                Assert.assertNull(e);
            }
        }

    }

    @Test
    public void appendDataSetTest() {
        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        int[] i = new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        IntegerDataRecord idr = new IntegerDataRecord("x", "", i);
        IDataStore ds = DataStoreFactory.getDataStore(f);

        try {
            ds.addDataRecord(idr);
            ds.store();
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }
        ds = null;

        int[] i2 = new int[] { 10, 11, 12, 13, 14, 15 };
        IntegerDataRecord idr2 = new IntegerDataRecord("x", "", i2);
        IDataStore ds2 = DataStoreFactory.getDataStore(f);

        try {
            ds2.addDataRecord(idr2);
            ds2.store(StoreOp.APPEND);
        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        ds2 = null;

        IDataStore ds3 = DataStoreFactory.getDataStore(f);

        try {
            IDataRecord dr = ds3.retrieve("", "x", Request.ALL);
            Assert.assertTrue(dr instanceof IntegerDataRecord);
            Assert.assertEquals(dr.getSizes()[0], 16);
            int[] data = ((IntegerDataRecord) dr).getIntData();
            for (int z = 0; z < 16; z++) {
                Assert.assertEquals(z, data[z]);
            }

        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }
    }

    @Test
    public void propertyDataSetTest() {
        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        int[] i = new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        IntegerDataRecord idr = new IntegerDataRecord("x", "", i);
        IDataStore ds = DataStoreFactory.getDataStore(f);

        Map<String, Object> attribs = new HashMap<String, Object>();
        try {
            attribs.put("a", new Integer(8));
            attribs.put("b", new String("k"));
            attribs.put("c", new Float(2.35));
            attribs.put("d", new String(""));
            attribs.put("e", new String("seconds since 1-1-1970"));
            idr.setDataAttributes(attribs);
            ds.addDataRecord(idr);
            ds.store();

        } catch (StorageException e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

        for (int z = 0; z < 100; z++) {
            try {
                IDataRecord dr = ds.retrieve("x", "", Request.ALL);
                Assert.assertNotNull(dr);
                Assert.assertTrue(dr instanceof IntegerDataRecord);
                Map<String, Object> props = dr.getDataAttributes();
                Assert.assertNotNull(props);
                Assert.assertNotNull(props.get("a"));
                Assert.assertEquals(props.get("a"), new Integer(8));
                Assert.assertNotNull(props.get("b"));
                String k = (String) props.get("b");
                Assert.assertEquals(k, ("k"));
                Assert.assertNotNull(props.get("c"));
                Assert.assertEquals(props.get("c"), new Float(2.35));
                Assert.assertEquals("seconds since 1-1-1970", props.get("e"));
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
                Assert.assertNull(e);
            }
        }
    }

    @Test
    public void simulateSatellite() {
        final byte[] f1 = new byte[2048 * 2048];
        Random r = new Random();
        int fl = r.nextInt();
        for (int i = 0; i < f1.length; i++) {
            f1[i] = (byte) (fl * i);
        }

        File f = new File("/tmp/test" + Thread.currentThread().getName()
                + ".h5");
        if (f.exists()) {
            f.delete();
        }

        ByteDataRecord idr = new ByteDataRecord("x", "", f1, 2, new long[] {
                2048, 2048 });
        IDataStore ds = DataStoreFactory.getDataStore(f);

        try {
            StorageProperties sp = new StorageProperties();
            sp.setChunked(true);
            sp.setDownscaled(false);
            ds.addDataRecord(idr, sp);
            ds.store();

            ds.retrieve("/");

            ds.delete("x");
        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertNull(e);
        }

    }

    public static void main(String[] args) {

        HDF5DataStore ds = new HDF5DataStore(new File("/tmp/metar.h5"), true);

        String[] data = new String[25 * 10000];
        for (int i = 0; i < data.length; i++) {
            data[i] = "hello";
        }

        String[] data2 = new String[25 * 1000];
        for (int i = 0; i < data2.length; i++) {
            data2[i] = "this is a really long string intended to overflow the max length";
        }

        int[] iData2 = new int[data2.length];
        for (int i = 0; i < iData2.length; i++) {
            iData2[i] = i;
        }

        StringDataRecord sdr = new StringDataRecord("presWeather", "", data, 2,
                new long[] { 25, 10000 });
        try {
            StorageProperties sp = new StorageProperties();
            ds.addDataRecord(sdr, sp);
            sdr.setMaxSizes(new long[] { 0, 25 });
            sdr.setMaxLength(16);
            sdr.setMaxChunkSize(4096);
            // sdr.setMaxLength(16);
            StorageStatus ss = ds.store();
            if (ss.getExceptions() != null) {
                for (StorageException e : ss.getExceptions()) {
                    e.printStackTrace();
                }
            }

            IntegerDataRecord idr2 = new IntegerDataRecord("int", "", iData2,
                    2, new long[] { 25, 1000 });
            idr2.setMaxSizes(new long[] { 0, 25 });
            idr2.setMaxChunkSize(1024);
            ds.addDataRecord(idr2, sp);

            long tW0 = System.currentTimeMillis();
            for (int i = 0; i < 10; i++) {
                StringDataRecord sdr2 = new StringDataRecord("presWeather", "",
                        data2, 2, new long[] { 25, 1000 });
                sdr2.setMaxLength(16);
                ds.addDataRecord(sdr2, sp);

                idr2 = new IntegerDataRecord("int", "", iData2, 2, new long[] {
                        25, 1000 });
                // idr2.setMaxSizes(new long[] { 0, 25 });
                // idr2.setMaxChunkSize(64);
                // ds.addDataRecord(idr2, sp);

                ds.store(StoreOp.APPEND);
            }
            System.out.println("Many small writes took: "
                    + (System.currentTimeMillis() - tW0));

        } catch (StorageException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        Random r = new Random();

        Set<Integer> intSet = new HashSet<Integer>();
        for (int i = 0; i < 200; i++) {
            intSet.add(r.nextInt(10000));
        }

        int[] pts = new int[intSet.size()];
        Iterator<Integer> intSetIterator = intSet.iterator();
        int i = 0;
        while (intSetIterator.hasNext()) {
            pts[i] = intSetIterator.next();
            i++;
        }

        Request req = Request.buildYLineRequest(pts);

        try {
            IDataRecord[] rec = ds
                    .retrieveDatasets(new String[] { "int" }, req);
            System.out.println("Start retrieval");
            long t0 = System.currentTimeMillis();
            for (int z = 0; z < 1; z++) {
                rec = ds.retrieveDatasets(new String[] { "presWeather" }, req);
            }
            System.out.println("Took: " + (System.currentTimeMillis() - t0));
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (StorageException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public static void main2(String[] args) {

        final TestHDF5DataStore t = new TestHDF5DataStore();

        System.out.println("Starting unit tests:");

        // try {
        // for (int i = 0; i < 1000000000000000000L; i++) {
        // File f = new File("/tmp/foo123");
        // if (f.exists())
        // f.delete();
        // int file_id = H5.H5Fcreate("/tmp/foo123",
        // HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
        // HDF5Constants.H5P_DEFAULT);
        // H5.H5Fclose(file_id);
        // System.gc();
        // }
        // } catch (HDF5LibraryException e) {
        // // TODO Auto-generated catch block
        // e.printStackTrace();
        // }

        System.out.println("Done");
        // below code is for testing threading issues
        final Runnable run = new Runnable() {

            @Override
            public void run() {
                Random r = new Random();
                for (int i = 0; i < 1000000000; i++) {
                    try {

                        // if (r.nextBoolean()) {
                        // System.out.println(Thread.currentThread().getName()
                        // + ":: executing extensive rt tests");
                        // t.extensiveRoundtripTest();
                        // }
                        if (r.nextBoolean()) {
                            // System.out.println(Thread.currentThread().getName()
                            // + ":: executing valid replace tests");
                            t.validReplaceTest();
                        }
                        // if (r.nextBoolean()) {
                        // System.out.println(Thread.currentThread().getName()
                        // + ":: executing appendDataSet2DTest tests");
                        // t.appendDataSet2DTest();
                        // }
                        // if (r.nextBoolean()) {
                        // System.out.println(Thread.currentThread().getName()
                        // + ":: executing appendDataSetTest tests");
                        // t.appendDataSetTest();
                        // }
                        // if (r.nextBoolean()) {
                        // System.out.println(Thread.currentThread().getName()
                        // + ":: executing propertyDataSetTest tests");
                        // t.propertyDataSetTest();
                        // }
                        // if (r.nextBoolean()) {
                        // System.out.println(Thread.currentThread().getName()
                        // + ":: executing simulateSatellite tests");
                        // t.simulateSatellite();
                        // }

                        System.gc();
                    } catch (Throwable e) {
                        e.printStackTrace();
                    }

                }
            }

        };

        Thread t1 = new Thread(run);
        Thread t2 = new Thread(run);
        Thread t3 = new Thread(run);
        Thread t4 = new Thread(run);
        t1.start();
        // t2.start();
        // t3.start();
        // t4.start();
        try {
            t1.join();
            t2.join();
            t3.join();
            t4.join();
        } catch (InterruptedException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
