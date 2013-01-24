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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.edex.util.UnitConv;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.mpe.util.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SatPrecipFileBuilder {

    Rectangle extent = null;

    XmrgFile xmfile = null;

    GridRecord gr = null;

    private IDataRecord dataRec;

    private String uri = "";

    private String outputPath = "";

    private short[] data;

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyyMMddHH");
    static {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private static final String FILEPRE = "SATPRE";

    private static final String PROC_FLAG = "MPA01   ";

    private static final float VERNUM = 1.0f;

    private static final int MAXVAL = 32767;

    private static final String OS = "LX";

    private static final String USER = "SANmdY";

    private Date grReftime = null;

    private int height;

    private int width;

    private static int minX;

    private static int minY;

    private static int maxX;

    private static int maxY;

    public SatPrecipFileBuilder(String datauri) {
        uri = datauri;
    }

    public void createSatPre() {
        try {
            getGridRecord();
        } catch (PluginException e) {
            e.printStackTrace();
        }
        float[] fa = null;
        fa = new float[((float[]) gr.getMessageData()).length];
        fa = (float[]) gr.getMessageData();
        String gribUnit = gr.getParameter().getUnitString();
        UnitConverter cv = null;
        Unit<?> gi = Unit.ONE;
        try {
            gi = UnitConv.deserializer(gribUnit);
        } catch (ParseException e) {
            e.printStackTrace();
        }
        Unit<?> xOut = SI.MILLIMETER;
        cv = gi.getConverterTo(xOut);
        data = new short[fa.length];
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
        createXmrgFile();
    }

    private void createXmrgFile() {

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        outputPath = appsDefaults.getToken("mpe_satpre_dir");
        File op = new File(outputPath);
        if (op.exists() == false) {
            op.mkdir();
        }
        op = null;
        String fname = outputPath + File.separatorChar + FILEPRE
                + sdf.format(grReftime) + "z";
        xmfile = new XmrgFile(fname);
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
        try {
            xmfile.save(fname);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    /**
     * get Populated grib record
     * 
     * @param uri
     * @return
     */
    public void getGridRecord() throws PluginException {
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        HRAP hr = HRAP.getInstance();

        maxY = (int) (hr.getNy() - extent.getMinY());
        minY = (int) (maxY - extent.getHeight());
        minX = (int) extent.getMinX();
        maxX = (int) extent.getMaxX();
        width = (int) (extent.getWidth());
        height = (int) (extent.getHeight());

        int[] minIndex = { minX, minY };
        int[] maxIndex = { minX + width, minY + height };

        gr = new GridRecord(uri);
        PluginDao gd = PluginFactory.getInstance().getPluginDao(
                gr.getPluginName());
        gr = (GridRecord) gd.getMetadata(uri);
        IDataStore dataStore = gd.getDataStore(gr);

        try {
            dataRec = dataStore.retrieve(uri, "Data",
                    Request.buildSlab(minIndex, maxIndex));
        } catch (Exception se) {
            se.printStackTrace();
        }
        if (dataRec instanceof FloatDataRecord) {
            gr.setMessageData(((FloatDataRecord) dataRec).getFloatData());
        } else if (dataRec instanceof ShortDataRecord) {
            gr.setMessageData(((ShortDataRecord) dataRec).getShortData());
        } else {
            gr.setMessageData(dataRec);
        }
        // this get the reftime of the record
        grReftime = gr.getDataTime().getRefTime();
        long millis = grReftime.getTime();
        // convert 1 hour to milliseconds to add to time.
        millis += 60 * 60 * 1000;
        grReftime.setTime(millis);
    }

}
