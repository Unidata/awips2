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

import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferFloat;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferShort;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.io.File;
import java.util.Arrays;
import java.util.Map;

import javax.media.jai.BorderExtender;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.ParameterBlockJAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.pypies.PyPiesDataStore;
import com.raytheon.uf.common.pypies.PypiesProperties;

/**
 * Interpolator for topo data files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            randerso    Initial creation
 * Feb 12, 2013     #1608  randerso    Remove exlicit references to HDF5DataStore
 *                                     Added explicit calls to deleteGroups
 * Feb 11, 2014     #2788  randerso    Changed to use PyPiesDataStore
 *                                     Fixed corner points for interpolated levels
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoInterpolator {
    private static final int BLOCK_SIZE = 2400;

    private IDataStore dataStore;

    /**
     * Create a TopoInterpolator instance for the specified hdf file
     * 
     * @param file
     */
    public TopoInterpolator(File hdf) {
        PypiesProperties pypiesProps = new PypiesProperties();
        pypiesProps.setAddress("http://localhost:9582");
        dataStore = new PyPiesDataStore(hdf, true, pypiesProps);
    }

    public void interpolate(String group, String dataSet) {

        Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                1 });
        try {
            IDataRecord record = dataStore.retrieve(group, dataSet, request);
            Map<String, Object> attributes = record.getDataAttributes();
            int width = (Integer) attributes.get("Width");
            int height = (Integer) attributes.get("Height");

            // These attributes are stored as 1x1 arrays in the file to match
            // what H5DataStore did
            // Pypies automatically returns them as scalars
            double xDim = (Double) attributes.get("xDim");
            double yDim = (Double) attributes.get("yDim");
            double startLat = (Double) attributes.get("ulLat");
            double startLon = (Double) attributes.get("ulLon");
            double endLat = (Double) attributes.get("lrLat");
            double endLon = (Double) attributes.get("lrLon");

            int level = 1;
            String srcGroup = group;
            String srcDataSet = dataSet;
            String dstGroup = group
                    + (group.endsWith("/") ? "interpolated" : "/interpolated");

            // remove existing interpolated datasets
            if (Arrays.asList(dataStore.getDatasets(srcGroup)).contains(
                    "interpolated")) {
                dataStore.deleteGroups(dstGroup);
            }

            StorageProperties properties = new StorageProperties();
            properties.setCompression(Compression.NONE);
            properties.setChunked(true);

            long t0 = System.currentTimeMillis();
            long t1 = t0;
            while (level < 6) {
                System.out.print("\nInterpolating " + srcGroup + srcDataSet
                        + " (" + width + ", " + height + ")");

                int srcWidth = width;
                int srcHeight = height;

                // compute attributes for new level
                width /= 2;
                height /= 2;
                startLon += xDim / 2;
                startLat -= yDim / 2;
                endLon -= xDim / 2;
                endLat += yDim / 2;
                xDim *= 2;
                yDim *= 2;
                String dstDataSet = "" + level;
                record.setName(dstDataSet);
                record.setGroup(dstGroup);
                record.setSizes(new long[] { width, height });
                record.setProperties(properties);
                attributes.put("Width", width);
                attributes.put("Height", height);
                attributes.put("xDim", new double[] { xDim });
                attributes.put("yDim", new double[] { yDim });
                attributes.put("ulLat", new double[] { startLat });
                attributes.put("ulLon", new double[] { startLon });
                attributes.put("lrLat", new double[] { endLat });
                attributes.put("lrLon", new double[] { endLon });
                record.setDataAttributes(attributes);

                dataStore.createDataset(record);

                int y = 0;
                while (y < srcHeight) {
                    System.out.print(".");
                    int h = Math.min(BLOCK_SIZE, srcHeight - y);

                    int x = 0;
                    while (x < srcWidth) {
                        int w = Math.min(BLOCK_SIZE, srcWidth - x);
                        request = Request.buildSlab(new int[] { x, y },
                                new int[] { x + w, y + h });
                        record = dataStore.retrieve(srcGroup, srcDataSet,
                                request);

                        resize(record, 2);
                        record.setName(dstDataSet);
                        record.setGroup(dstGroup);
                        record.setMinIndex(new long[] { x / 2, y / 2 });

                        dataStore.addDataRecord(record);
                        dataStore.store();

                        x += BLOCK_SIZE;
                    }
                    y += BLOCK_SIZE;
                }

                long t2 = System.currentTimeMillis();
                System.out.print(" took " + ((t2 - t1) / 1000) + " s");
                t1 = t2;

                srcGroup = dstGroup;
                srcDataSet = dstDataSet;
                level++;
            }
            System.out.print("\nTotal "
                    + ((System.currentTimeMillis() - t0) / 1000) + " s");
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    protected static void resize(IDataRecord rec, int scale) {
        int w = (int) rec.getSizes()[0];
        int h = (int) rec.getSizes()[1];

        int len = w * h;
        java.awt.Point origin = new Point(0, 0);

        // create a sample model
        DataBuffer dataBuffer = null;
        int type = 0;

        Object in = rec.getDataObject();

        if (rec instanceof ByteDataRecord) {
            dataBuffer = new java.awt.image.DataBufferByte((byte[]) in, len);
            type = DataBuffer.TYPE_BYTE;
        } else if (rec instanceof ShortDataRecord) {
            dataBuffer = new java.awt.image.DataBufferShort((short[]) in, len);
            type = DataBuffer.TYPE_SHORT;
        } else if (rec instanceof IntegerDataRecord) {
            dataBuffer = new java.awt.image.DataBufferInt((int[]) in, len);
            type = DataBuffer.TYPE_INT;
        } else if (rec instanceof FloatDataRecord) {
            dataBuffer = new java.awt.image.DataBufferFloat((float[]) in, len);
            type = DataBuffer.TYPE_FLOAT;
        } else {
            throw new UnsupportedOperationException("["
                    + rec.getClass().getName() + "]"
                    + " not supported by resize.");
        }

        SampleModel sampleModel = RasterFactory.createBandedSampleModel(type,
                w, h, 1);

        // create a TiledImage using the float SampleModel
        TiledImage tiledImage = new TiledImage(0, 0, w, h, 0, 0, sampleModel,
                null);

        // create a Raster
        Raster raster = RasterFactory.createWritableRaster(sampleModel,
                dataBuffer, origin);

        // set the TiledImage data to that of the Raster
        tiledImage.setData(raster);

        PlanarImage scaledImg;

        // Interpolate the image using a scale factor and
        // the copy border extender
        ParameterBlockJAI param = new ParameterBlockJAI("Scale");
        param.addSource(tiledImage);
        param.setParameter("xScale", 1.0f / scale);
        param.setParameter("yScale", 1.0f / scale);
        Interpolation interpol = Interpolation
                .getInstance(Interpolation.INTERP_NEAREST);
        param.setParameter("interpolation", interpol);
        RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                BorderExtender.createInstance(BorderExtender.BORDER_COPY));
        scaledImg = JAI.create("Scale", param, hint);

        // Get the floats back out
        DataBuffer newDb = scaledImg.getData().getDataBuffer();

        if (rec instanceof ByteDataRecord) {
            ((ByteDataRecord) rec).setByteData(((DataBufferByte) newDb)
                    .getData());
        } else if (rec instanceof ShortDataRecord) {
            ((ShortDataRecord) rec).setShortData(((DataBufferShort) newDb)
                    .getData());
        } else if (rec instanceof IntegerDataRecord) {
            ((IntegerDataRecord) rec).setIntData(((DataBufferInt) newDb)
                    .getData());
        } else if (rec instanceof FloatDataRecord) {
            ((FloatDataRecord) rec).setFloatData(((DataBufferFloat) newDb)
                    .getData());
        }

        rec.setSizes(new long[] { w / 2, h / 2 });

    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        TopoInterpolator ti;
        if (args.length < 1) {
            System.out.println("usage: TopoInterpolator topofile");
        } else {
            String fileName = args[0];
            System.out.println("Interpolating " + fileName);
            ti = new TopoInterpolator(new File(fileName));

            ti.interpolate("/", "full");
        }
    }

}
