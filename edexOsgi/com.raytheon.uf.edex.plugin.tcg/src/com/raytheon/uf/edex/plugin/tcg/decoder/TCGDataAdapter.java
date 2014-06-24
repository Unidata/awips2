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
package com.raytheon.uf.edex.plugin.tcg.decoder;

import java.io.File;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.tcg.TCGStormType;
import com.raytheon.uf.common.dataplugin.tcg.TropicalCycloneGuidance;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.tcg.TropicalCycloneGuidanceDao;

/**
 * Base class for Tropical Cyclone Guidance (TCG) products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez     Initial creation
 * Jun 28, 2012  #826      dgilling     Ensure getDataTime properly
 *                                      handles time zones.
 * May 14, 2014 2536       bclement     moved WMO Header to common, removed constructDataURI() call
 * Jun 24, 2014 3235       nabowle      Switch to slf4j.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class TCGDataAdapter {

    protected static Logger logger = LoggerFactory
            .getLogger(TCGDataAdapter.class);

    protected PointDataDescription pointDataDescription;

    protected TropicalCycloneGuidanceDao tcgDao;

    protected Map<File, PointDataContainer> containerMap;

    protected String pluginName;

    protected String productType;

    protected WMOHeader wmoHeader;

    protected int currentReport = -1;

    protected String traceId;

    protected String stormName;

    protected String stationId;

    protected TCGStormType stormType;

    protected DataTime refTime;

    private HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    private List<TropicalCycloneGuidance> reports;

    protected static final HashMap<String, Integer> MONTH_MAP = new HashMap<String, Integer>();

    static {
        MONTH_MAP.put("JAN", 1);
        MONTH_MAP.put("FEB", 2);
        MONTH_MAP.put("MAR", 3);
        MONTH_MAP.put("APR", 4);
        MONTH_MAP.put("MAY", 5);
        MONTH_MAP.put("JUN", 6);
        MONTH_MAP.put("JUL", 7);
        MONTH_MAP.put("AUG", 8);
        MONTH_MAP.put("SEP", 9);
        MONTH_MAP.put("OCT", 10);
        MONTH_MAP.put("NOV", 11);
        MONTH_MAP.put("DEC", 12);
    }

    public TCGDataAdapter(PointDataDescription pdd,
            TropicalCycloneGuidanceDao dao, String pluginName) {
        this.pointDataDescription = pdd;
        this.tcgDao = dao;
        this.pluginName = pluginName;
        containerMap = new HashMap<File, PointDataContainer>();
    }

    public void setData(byte[] message, String traceId, Headers headers) {
        currentReport = -1;
        this.traceId = traceId;
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        wmoHeader = new WMOHeader(message, fileName);
        if (wmoHeader != null) {
            reports = findReports(message);
        } else {
            logger.error(traceId + "- Missing or invalid WMOHeader");
        }
        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        }
    }

    abstract public List<TropicalCycloneGuidance> findReports(byte[] message);

    abstract public void clearData();

    /**
     * Does this parser contain any more reports.
     *
     * @return Does this parser contain any more reports.
     */
    public boolean hasNext() {
        boolean next = (reports != null);
        if (next) {
            next = ((currentReport >= 0) && (currentReport < reports.size()));
        }
        if (!next) {
            reports = null;
            currentReport = -1;
        }
        return next;
    }

    /**
     * Get the next available report. Returns a null reference if no more
     * reports are available.
     *
     * @return The next available report.
     */
    public TropicalCycloneGuidance next() {

        TropicalCycloneGuidance report = null;
        if (currentReport < 0) {
            return report;
        }
        if (currentReport >= reports.size()) {
            reports = null;
            currentReport = -1;
        } else {
            report = reports.get(currentReport++);
            logger.debug("Getting report " + report);

            if (URI_MAP.containsKey(report.getDataURI())) {
                report = null;
            } else {
                URI_MAP.put(report.getDataURI(), Boolean.TRUE);
            }
            if (report != null) {

                PointDataContainer pdc = getContainer(report);

                // Populate the point data.
                PointDataView view = pdc.append();
                view.setLong("validTime", report.getDataTime().getValidTime()
                        .getTimeInMillis());
                view.setFloat("latitude", (float) report.getLatitude());
                view.setFloat("longitude", (float) report.getLongitude());
                view.setString("wmoHeader", report.getWmoHeader());
                view.setString("dataURI", report.getDataURI());

                view.setString("stormType", report.getType().toString());
                view.setString("stormName", report.getStormName());
                view.setString("modelName", report.getModelName());
                view.setString("productType", report.getProductType());

                report.setPointDataView(view);
            }
        }
        return report;
    }

    /**
     *
     * @param obsData
     * @return
     */
    private PointDataContainer getContainer(TropicalCycloneGuidance obsData) {

        File file = tcgDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    public static TCGDataAdapter getAdapter(PointDataDescription pdd,
            TropicalCycloneGuidanceDao dao, String pluginName,
            WMOHeader wmoHeader) {
        TCGDataAdapter adapter = null;

        String ttaaii = wmoHeader.getTtaaii();

        if ("WHXX01".equals(ttaaii)) {
            adapter = new HURData(pdd, dao, pluginName);
        } else if ("WHXX04".equals(ttaaii)) {
            adapter = new QLMData(pdd, dao, pluginName);
        } else if (ttaaii.startsWith("WTNT5") || ttaaii.startsWith("WTPZ5")
                || ttaaii.startsWith("WTPA5")) {
            adapter = new TCEData(pdd, dao, pluginName);
        } else {
            adapter = new NullData(pdd, dao, pluginName);
            logger.error("No decoder adapter for file "
                    + wmoHeader.getWmoHeader());
        }
        return adapter;
    }

    protected int setStormType(String dataLine) {
        int index = -1;
        if (dataLine.startsWith("TROPICAL STORM")) {
            index = 14;
            stormType = TCGStormType.STORM;
        } else if (dataLine.startsWith("TROPICAL DEPRESSION")) {
            index = 19;
            stormType = TCGStormType.STORM;
        } else if (dataLine.startsWith("TROPICAL CYCLONE")) {
            index = 16;
            stormType = TCGStormType.STORM;
        } else if (dataLine.startsWith("HURRICANE")) {
            index = 9;
            stormType = TCGStormType.HURRICANE;
        } else if (dataLine.startsWith("TYPHOON")) {
            index = 7;
            stormType = TCGStormType.TYPHOON;
        } else if (dataLine.startsWith("EXTRATROPICAL")) {
            index = 13;
            stormType = TCGStormType.EXTRA;
        } else if (dataLine.startsWith("DISTURBANCE")) {
            index = 11;
            stormType = TCGStormType.STORM;
        } else {
            stormType = TCGStormType.UNKNOWN;
        }
        return index;
    }

    protected DataTime getDataTime(int year, int month, int day, int hour,
            int minute, String timeZone) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(timeZone));
        cal.set(Calendar.YEAR, year);
        cal.set(Calendar.MONTH, month - 1);
        cal.set(Calendar.DAY_OF_MONTH, day);
        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, minute);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return new DataTime(cal);
    }

    protected String[] getParts(String str, int n) {
        String parts[] = str.split(" ");
        String data[] = new String[n];
        int index = 0;
        for (int i = 0; i < parts.length && index < n; i++) {
            if (parts[i] != null && parts[i].length() > 0) {
                data[index] = parts[i].trim();
                index++;
            }
        }

        return data;
    }
}
