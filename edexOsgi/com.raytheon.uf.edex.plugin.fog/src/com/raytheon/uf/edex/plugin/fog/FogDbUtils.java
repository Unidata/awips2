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
package com.raytheon.uf.edex.plugin.fog;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.plugin.satellite.dao.SatelliteDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

public class FogDbUtils {

    /**
     * Extracts the float[] from the super grid
     * 
     * @param uri
     * @return DataRecord
     */
    public static int[] getSatGridData(String uri, int[] minIndex, int[] maxIndex)
    throws PluginException {

        IDataRecord dataRec = null;
        int[] shorts = null;

        SatelliteRecord sr = new SatelliteRecord(uri);
        SatelliteDao sd = (SatelliteDao) PluginFactory.getInstance().getPluginDao(
                sr.getPluginName());
        sr = (SatelliteRecord) sd.getMetadata(uri);
        IDataStore dataStore = sd.getDataStore(sr);

        // retrieves an area trimmed Sat/Grib record
        try {
            dataRec = dataStore.retrieve(uri, "Data", Request.buildSlab(minIndex, maxIndex));

            if (dataRec instanceof ByteDataRecord) {
                shorts = FogDbUtils.convertBytes(((ByteDataRecord) dataRec).getByteData());
            } else {
                return null;
            }
        } catch (Exception se) {
            se.printStackTrace();
        }

        return shorts;
    }

    /**
     * gets the meta data for a satellite record
     * 
     * @param uri
     * @return SatelliteRecord
     */
    public static SatelliteRecord getSatRecord(String uri) throws PluginException {

        SatelliteRecord sr = new SatelliteRecord(uri);
        SatelliteDao sd = (SatelliteDao) PluginFactory.getInstance().getPluginDao(
                sr.getPluginName());
        sr = (SatelliteRecord) sd.getMetadata(uri);

        return sr;
    }

    /**
     * Construct a 2D GridGeometry to use for display
     * 
     * @return
     */
    public static GridGeometry2D getGridGeometry(SatelliteRecord rec) {
        return rec.getGridGeometry();
    }

    /**
     * Used to re size raster VIS sat grids to IR size
     * 
     * @param inputGrid
     * @param outputGridSize
     * @param nx
     * @param ny
     * @return
     */
    public static int[] reSizeIntGrid(int[] inputGrid, int outnx, int outny, int innx, int inny) {

        int[] outputGrid = new int[outny * outnx];

        // decimate
        // (SK) i => x j => y
        for (int j = 0; j < outny - 1; j++) {
            for (int i = 0; i < outnx; i++) {
                int avValue = 0;
                for (int y = 0; y < 4; y++) {
                    for (int x = 0; x < 4; x++) {
                        // average the grid values
                        avValue += inputGrid[innx * (j * 4 + y) + i * 4 + x];
                    }
                }
                outputGrid[outnx * j + i] = avValue / 16;
            }
        }
        return outputGrid;
    }

    /**
     * convert the byte records to int
     * 
     * @param bytes
     * @return
     */
    public static int[] convertBytes(byte[] bytes) {
        int[] ints = new int[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            ints[i] = bytes[i] & 0xff;
        }
        return ints;
    }

}
