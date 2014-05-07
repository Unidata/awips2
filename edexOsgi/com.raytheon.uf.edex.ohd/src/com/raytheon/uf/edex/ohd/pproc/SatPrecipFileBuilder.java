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
package com.raytheon.uf.edex.ohd.pproc;

import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.mpe.util.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Builder for xmrg files that contain satellite precip data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 08, 2009            snaples     Initial creation
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * Mar 19, 2014 17109      snaples     Removed code that adds 1 hour to grid reftime, was not needed.
 * May 05, 2014 2060       njensen     Major cleanup and remove dependency on grid dataURI
 * 
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SatPrecipFileBuilder {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SatPrecipFileBuilder.class);

    private static final String FILEPRE = "SATPRE";

    private static final String PROC_FLAG = "MPA01   ";

    private static final float VERNUM = 1.0f;

    private static final int MAXVAL = 32767;

    private static final String OS = "LX";

    private static final String USER = "SANmdY";

    private Rectangle extent = null;

    private int height;

    private int width;

    private int minX;

    private int minY;

    private int maxY;

    private SimpleDateFormat sdf;

    private String outputPath;

    public SatPrecipFileBuilder() throws Exception {
        extent = HRAPCoordinates.getHRAPCoordinates();
        maxY = (int) (HRAP.getInstance().getNy() - extent.getMinY());
        minY = (int) (maxY - extent.getHeight());
        minX = (int) extent.getMinX();
        width = (int) (extent.getWidth());
        height = (int) (extent.getHeight());

        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        outputPath = appsDefaults.getToken("mpe_satpre_dir");
        File outputDir = new File(outputPath);
        if (!outputDir.exists()) {
            outputDir.mkdirs();
        }
    }

    /**
     * Retrieves a grid record, converts the data to millimeters, and stores it
     * to an xmrg file.
     * 
     * @param uri
     *            the data URI of the grid record
     */
    public void createSatPre(String uri) {
        try {
            GridRecord gr = getGridRecord(uri);
            float[] fa = (float[]) gr.getMessageData();
            String gribUnit = gr.getParameter().getUnitString();
            UnitConverter cv = null;
            Unit<?> gi = UnitConv.deserializer(gribUnit);
            Unit<?> xOut = SI.MILLIMETER;
            cv = gi.getConverterTo(xOut);
            short[] data = new short[fa.length];
            int k = 0;
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    if (fa[k] < 0) {
                        fa[k] = 0;
                    }
                    data[(i * width) + j] = (short) (cv.convert(fa[k]) * 100.0);
                    k++;
                }
            }
            fa = null;
            createXmrgFile(data, gr.getDataTime().getRefTime());
        } catch (Exception e) {
            logger.error("Error creating sat precip xmrg file for URI " + uri,
                    e);
        }
    }

    private void createXmrgFile(short[] data, Date grReftime)
            throws IOException {
        String fname = outputPath + File.separatorChar + FILEPRE
                + sdf.format(grReftime) + "z";
        XmrgFile xmfile = new XmrgFile(fname);
        XmrgHeader xmhead = new XmrgHeader();
        xmhead.setValidDate(grReftime);
        xmhead.setSaveDate(grReftime);
        xmhead.setOperatingSystem(OS);
        xmhead.setVersionNumber(VERNUM);
        xmhead.setUserId(USER);
        xmhead.setMaxValue(MAXVAL);
        xmhead.setProcessFlag(PROC_FLAG);
        xmfile.setHeader(xmhead);
        xmfile.setHrapExtent(extent);
        xmfile.setData(data);

        xmfile.save(fname);
    }

    /**
     * Get Populated grid record
     * 
     * @param uri
     *            the uri of the grid record
     * @return
     */
    private GridRecord getGridRecord(String uri) throws Exception {
        GridRecord gr = new GridRecord(uri);
        PluginDao gd = PluginFactory.getInstance().getPluginDao(
                gr.getPluginName());
        IDataStore dataStore = gd.getDataStore(gr);

        int[] minIndex = { minX, minY };
        int[] maxIndex = { minX + width, minY + height };
        IDataRecord dataRec = dataStore.retrieve(uri, "Data",
                Request.buildSlab(minIndex, maxIndex));
        if (dataRec instanceof FloatDataRecord) {
            gr.setMessageData(((FloatDataRecord) dataRec).getFloatData());
        } else {
            gr.setMessageData(dataRec);
        }

        return gr;
    }

}
