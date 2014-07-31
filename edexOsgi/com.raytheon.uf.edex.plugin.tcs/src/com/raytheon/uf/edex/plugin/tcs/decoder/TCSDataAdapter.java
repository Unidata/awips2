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
package com.raytheon.uf.edex.plugin.tcs.decoder;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.exception.UnrecognizedDataException;
import com.raytheon.uf.common.dataplugin.tcs.Radius;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.dataplugin.tcs.util.TCSConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.tcs.TropicalCycloneSummaryDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez     Initial creation
 * Apr 19, 2012  #457      dgilling     Create headers field so 
 *                                      subclasses can use TimeTools
 *                                      for time calculations.
 * May 14, 2014 2536       bclement     moved WMO Header to common, removed constructDataURI() call
 * Jun 23, 2014 3272       nabowle      Throw UnrecognizedDataException in
 *                                      {@link #getDecodedData()} if there were
 *                                      no reports. Switch to slf4j.
 * Jul 30, 2014 3410       bclement     data uri moved to database point data desc
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class TCSDataAdapter implements TCSConstants {

    protected static Logger logger = LoggerFactory
            .getLogger(TCSDataAdapter.class);

    protected PointDataDescription pointDataDescription;

    protected TropicalCycloneSummaryDao tcgDao;

    protected Map<File, PointDataContainer> containerMap;

    protected String pluginName;

    protected WMOHeader wmoHeader;

    protected int currentReport = -1;

    protected String traceId;

    protected HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    protected List<TropicalCycloneSummary> reports;

    protected Headers headers;

    public TCSDataAdapter(PointDataDescription pdd,
            TropicalCycloneSummaryDao dao, String pluginName) {
        this.pointDataDescription = pdd;
        this.tcgDao = dao;
        this.pluginName = pluginName;
        containerMap = new HashMap<File, PointDataContainer>();
    }

    public void setData(byte[] message, String traceId, Headers headers) {
        currentReport = -1;
        this.traceId = traceId;
        this.headers = headers;
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

    abstract public List<TropicalCycloneSummary> findReports(byte[] message);

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

    public TropicalCycloneSummary getDecodedData()
            throws UnrecognizedDataException {
        boolean isFirstReport = true;
        int COLUMN_WIDTH = 4;
        TropicalCycloneSummary headReport = null;
        TropicalCycloneSummary report = null;
        PointDataView view = null;
        int row = 0;
        while (hasNext()) {
            report = next();
            if (report != null) {
                if (isFirstReport) {
                    isFirstReport = false;
                    headReport = report;
                    view = getContainer(report).append();
                    view.setString(WMO_HEADER, report.getWmoHeader());
                    view.setString(TYPE, report.getProductType());
                    view.setString(NAME, report.getName());
                    view.setInt(PRESSURE, report.getPressure());
                }
                // Populate the point data.
                view.setString(DISPLAY_TIME, report.getDisplayTime(), row);
                view.setFloat(LAT, (float) report.getLatitude(), row);
                view.setFloat(LON, (float) report.getLongitude(), row);
                view.setInt(WIND_SPEED, report.getWindSpeed(), row);
                view.setInt(TROPICAL, report.isTropical() ? 1 : 0, row);

                if (report.getRadiusList() != null) {
                    for (Radius radius : report.getRadiusList()) {
                        String radiusName = "";
                        switch (radius.getKT_FT()) {
                        case 64:
                            radiusName = RAD_64;
                            break;
                        case 50:
                            radiusName = RAD_50;
                            break;
                        case 34:
                            radiusName = RAD_34;
                            break;
                        case 12:
                            radiusName = RAD_12;
                            break;
                        }
                        int column = 0;
                        view.setInt(radiusName, radius.getNE(),
                                (row * COLUMN_WIDTH) + column);

                        column++;
                        view.setInt(radiusName, radius.getSE(),
                                (row * COLUMN_WIDTH) + column);

                        column++;
                        view.setInt(radiusName, radius.getSW(),
                                (row * COLUMN_WIDTH) + column);

                        column++;
                        view.setInt(radiusName, radius.getNW(),
                                (row * COLUMN_WIDTH) + column);
                    }
                }
                row++;
            }
        }

        if (view == null || headReport == null) {
            throw new UnrecognizedDataException("No reports were found.");
        }

        view.setInt("size", row);
        headReport.setPointDataView(view);
        return headReport;
    }

    /**
     * Get the next available report. Returns a null reference if no more
     * reports are available.
     * 
     * @return The next available report.
     */
    public TropicalCycloneSummary next() {

        TropicalCycloneSummary report = null;
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
        }
        return report;
    }

    /**
     * 
     * @param obsData
     * @return
     */
    protected PointDataContainer getContainer(TropicalCycloneSummary obsData) {

        File file = tcgDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    public static TCSDataAdapter getAdapter(PointDataDescription pdd,
            TropicalCycloneSummaryDao dao, String pluginName,
            WMOHeader wmoHeader) {
        TCSDataAdapter adapter = null;

        String ttaaii = wmoHeader.getTtaaii();

        if (ttaaii.startsWith("WTNT2") || ttaaii.startsWith("WTPZ2")
                || ttaaii.startsWith("WTPA2") || ttaaii.startsWith("WTPN3")) {
            adapter = new TCMData(pdd, dao, pluginName);
        } else {
            adapter = new NullData(pdd, dao, pluginName);
            logger.error("No decoder adapter for file "
                    + wmoHeader.getWmoHeader());
        }

        return adapter;
    }
}
