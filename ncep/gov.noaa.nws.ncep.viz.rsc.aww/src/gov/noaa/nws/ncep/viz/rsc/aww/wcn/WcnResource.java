package gov.noaa.nws.ncep.viz.rsc.aww.wcn;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord.AwwReportType;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.rsc.aww.query.WcnCountyQueryResult;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         Uma Josyula  Initial creation.
 * 10/01/10       #307     Greg Hull    implement processRecords and change to 
 *                                      process WcnData as the IRscDataObj
 * 01/10/11					Uma Josyula	 Made changes to preprocess update and event date
 * 07/28/11       #450      Greg Hull    NcPathManager    
 * 02/16/2012     #555      S. Gurung    Added call to setAllFramesAsPopulated() in queryRecords().                                  
 * 05/23/2012     785       Q. Zhou     Added getName for legend.
 * 08/17/2012     655       B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 09/30/2012     857       Q. Zhou     Displayed watch number. Modified time string and alignment in drawTimeLabelWatchNumber().
 *                                      Added label/time for union.  Modified fill alpha to 0.5.
 * 										Remove constraint & metamap in initResource().
 * 08/14/2013     1028    G. Hull      Move to aww project. Use AwwReportType enum.
 * 9/15/2014      4637      J. Huber    Fixed a duplicate key issue in the display hash map as well as as logic error in
 *                                      parsing the ugc line where it started with a string that need to be re-parsed. Also
 *                                      incorporating changes made by Dimitry for actually handling the > in UGC as well as 
 *                                      modifying times as appropriate.
 * 10/16/2014     4637      J. Huber    Fixed display label issue. Changed labeling mechanism from each WCN message to each county.
 *                                      Cleaned up deprecated methods as well. Changed gathering of reportType to read phenomena and 
 *                                      significance from VTEC line.
 * </pre>
 * 
 * @author ujosyula
 * @version 1.0
 */

public class WcnResource extends
        AbstractNatlCntrsResource<WcnResourceData, NCMapDescriptor> implements
        INatlCntrsResource, IStationField {
    private IFont font;

    private StationTable stationTable;

    private WcnResourceData wcnRscData;

    private List<WcnRscDataObj> modifyList;

    public class WcnRscDataObj implements IRscDataObject,
            Comparable<WcnRscDataObj> {
        String datauri; // used as a key string

        DataTime issueTime; // issue time from bulletin

        DataTime eventTime;

        AwwReportType reportType;

        int countyNumPoints;

        float[] countyLat;

        float[] countyLon;

        List<LatLonPoint> countyPoints;

        List<String> countyUgc, countyNames, stateNames;// T456: fips //To get
                                                        // all the counties

        public List<String> countyFips = new ArrayList<String>();

        String eventType;

        String watchNumber; // watch number to be displayed

        String evTrack;

        DataTime evEndTime;

        DataTime origEndTime;

        DataTime displayStart;

        DataTime displayEnd;

        String evOfficeId;

        String evPhenomena;

        String evProductClass;

        String evSignificance;

        public boolean isCounty;// T456

        RGB color = new RGB(155, 155, 155);// T456

        public String getKey() {
            return getWcnRscDataObjKey(this);
        }

        @Override
        public DataTime getDataTime() {
            return eventTime;
        }

        @Override
        public int compareTo(WcnRscDataObj o) {
            return (int) (this.issueTime.getRefTime().getTime() - o.issueTime
                    .getRefTime().getTime());
        }
    }

    protected class FrameData extends AbstractFrameData {
        HashMap<String, WcnRscDataObj> wcnDataMap;

        public FrameData(DataTime frameTime, int timeInt) {
            super(frameTime, timeInt);
            wcnDataMap = new HashMap<String, WcnRscDataObj>();
        }

        public boolean updateFrameData(IRscDataObject rscDataObj) {
            if (!(rscDataObj instanceof WcnRscDataObj)) {
                logger.warning("WcnResource.updateFrameData: expecting objects "
                        + " of type WcnRscDataObj???");
                return false;
            }

            WcnRscDataObj wcnRscDataObj = (WcnRscDataObj) rscDataObj;
            WcnRscDataObj existingWcnData = wcnDataMap
                    .get(wcnRscDataObj.datauri);

            // RM 4697 make unique keys so that all ugc entries are mapped.

            if (existingWcnData == null
                    || wcnRscDataObj.issueTime
                            .greaterThan(existingWcnData.issueTime)
                    || wcnDataMap.containsKey(wcnRscDataObj.datauri)) {
                if (wcnDataMap.containsKey(wcnRscDataObj.datauri)) {
                    String s = "";
                    String r = "";

                    // Put all counties in the WCN into the hash map key to
                    // avoid iterator clean up issues

                    for (int i = 0; i < wcnRscDataObj.countyFips.size(); i++) {
                        s = wcnRscDataObj.countyFips.get(i);
                        r = r + " " + s;
                    }
                    wcnDataMap.put(wcnRscDataObj.datauri + " " + r,
                            wcnRscDataObj);
                } else {
                    wcnDataMap.put(wcnRscDataObj.datauri, wcnRscDataObj);
                }
            }
            return true;
        }
    }

    public WcnResource(WcnResourceData rscData, LoadProperties loadProperties)
            throws VizException {
        super(rscData, loadProperties);
        wcnRscData = (WcnResourceData) resourceData;
        modifyList = new ArrayList<WcnRscDataObj>();
    }

    protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
        return (AbstractFrameData) new FrameData(frameTime, timeInt);
    }

    // turn the db record into an WarnRscDataObj which will be timeMatched and
    // added to one or more of the FrameData's.

    @Override
    protected IRscDataObject[] processRecord(Object pdo) {
        AwwRecord awwRecord = (AwwRecord) pdo;
        ArrayList<WcnRscDataObj> wcnDataList = getAwwtData(awwRecord);
        if (wcnDataList == null) {
            return new IRscDataObject[] {};
        } else {
            return wcnDataList.toArray(new WcnRscDataObj[0]);
        }
    }

    private void fipsRangeReparse(String inUgcPart, String countyname,
            List<String> outList) {
        final String inclusiveDelim = ">";
        String county = countyname;
        String countyFips;

        if (inUgcPart.length() == 10) { // "([A-Z]{3}[0-9]{3}[>][0-9]{3})"
            String intervalToken = inUgcPart.substring(3, 10);
            county = inUgcPart.substring(0, 3);

            // Format in NAMDDD1>DDD2
            StringTokenizer twoTokens = new StringTokenizer(intervalToken,
                    inclusiveDelim);
            String firstToken = twoTokens.nextToken();
            String secondToken = twoTokens.nextToken();

            Integer countyBegin = Integer.parseInt(firstToken);
            Integer countyEnd = Integer.parseInt(secondToken);

            for (int counter = countyBegin; counter <= countyEnd; counter++) {

                String inclusiveToken = Integer.toString(counter);

                // set "1" to "001" ...etc
                if (counter < 10) {
                    inclusiveToken = "00".concat(inclusiveToken);
                }

                // set "10" to "010" ...etc
                else if (counter < 100) {
                    inclusiveToken = "0".concat(inclusiveToken);
                }
                countyFips = county.concat(inclusiveToken);

                outList.add(countyFips);
                // UGC.addAwwFIPS(currentFips);
            }
        } else if (inUgcPart.length() == 7) { // "([0-9]{3}[>][0-9]{3})"
            // A continuation of previous county FIPS
            // with format DDD1>DDD2
            StringTokenizer twoTokens = new StringTokenizer(inUgcPart,
                    inclusiveDelim);
            String firstToken = twoTokens.nextToken();
            String secondToken = twoTokens.nextToken();

            Integer countyBegin = Integer.parseInt(firstToken);
            Integer countyEnd = Integer.parseInt(secondToken);

            for (int counter = countyBegin; counter <= countyEnd; counter++) {

                String inclusiveToken = Integer.toString(counter);

                // set "1" to "001" ...etc
                if (counter < 10) {
                    inclusiveToken = "00".concat(inclusiveToken);
                }

                // set "10" to "010" ...etc
                else if (counter < 100) {
                    inclusiveToken = "0".concat(inclusiveToken);
                }
                countyFips = county.concat(inclusiveToken);

                outList.add(countyFips);
            }
        } else {
            outList.add(inUgcPart);// risky?
        }
    }

    private ArrayList<WcnRscDataObj> getAwwtData(AwwRecord awwRecord) {
        WcnRscDataObj wcnStatusData = null;
        List<WcnRscDataObj> wcnDataList = new ArrayList<WcnRscDataObj>();

        final String ENDTIME_REGEX = "([0-9]{6})";
        final Pattern endTimePattern = Pattern.compile(ENDTIME_REGEX);

        try {
            Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();
            for (AwwUgc awwugcs : awwUgc) {
                wcnStatusData = new WcnRscDataObj();

                wcnStatusData.issueTime = new DataTime(awwRecord.getIssueTime());
                wcnStatusData.datauri = awwRecord.getDataURI();
                wcnStatusData.isCounty = isCountyUgs(awwugcs);

                String ugcline = awwugcs.getUgc();// get the ugc line to find
                                                  // the counties
                if (ugcline != null && ugcline != "") {

                    wcnStatusData.watchNumber = awwugcs
                            .getEventTrackingNumber();
                    wcnStatusData.countyUgc = new ArrayList<String>();

                    int i = 0;
                    String temp;
                    String countyname = ugcline.substring(0, 3);
                    StringTokenizer strugcs = new StringTokenizer(ugcline);
                    while (strugcs.hasMoreTokens()) {
                        temp = strugcs.nextToken("-");

                        boolean dontSkip = true;

                        Matcher endTimeMatcher = endTimePattern.matcher(temp);
                        if (endTimeMatcher.find()) {
                            dontSkip = false;
                        }

                        if (temp != null && dontSkip) {
                            if (temp.startsWith("\r\r\n")) {
                                String temp1 = temp.substring(3);
                                temp = temp1;
                            }

                            if (temp.contains(countyname)) {

                                if (temp.length() == 6) {
                                    if ((0 == Character.getNumericValue(temp
                                            .toCharArray()[3]))
                                            && (0 == Character
                                                    .getNumericValue(temp
                                                            .toCharArray()[4]))
                                            && (0 == Character
                                                    .getNumericValue(temp
                                                            .toCharArray()[5]))) {
                                    } else {
                                        (wcnStatusData.countyUgc).add(temp);
                                    }
                                } else {
                                    fipsRangeReparse(temp, countyname,
                                            (wcnStatusData.countyUgc));
                                }

                            } else if (temp.length() == 7) {
                                fipsRangeReparse(temp, countyname,
                                        (wcnStatusData.countyUgc));
                            } else {
                                if (!"".equalsIgnoreCase(temp)
                                        && Character.isLetter(temp
                                                .toCharArray()[0])) {
                                    countyname = temp.substring(0, 3);
                                    String temp2 = countyname.substring(0, 3)
                                            + temp.substring(3);
                                    (wcnStatusData.countyUgc).add(temp2);
                                }

                                String temp2 = countyname.substring(0, 3)
                                        + temp;
                                if (temp2.length() > 6) {
                                    if (9 == temp2.length()) {
                                        (wcnStatusData.countyUgc).add(temp2
                                                .substring(3, 9));
                                    } else {
                                        fipsRangeReparse(temp2.substring(0, 6),
                                                countyname,
                                                (wcnStatusData.countyUgc));
                                    }
                                } else {
                                    if (!(6 > temp2.length())
                                            && (0 == Character
                                                    .getNumericValue(temp2
                                                            .toCharArray()[3]))
                                            && (0 == Character
                                                    .getNumericValue(temp2
                                                            .toCharArray()[4]))
                                            && (0 == Character
                                                    .getNumericValue(temp2
                                                            .toCharArray()[5]))) {
                                        System.err.println(temp2);
                                    } else {
                                        (wcnStatusData.countyUgc).add(temp2);
                                    }
                                }
                            }
                            i++;
                        }
                    }
                    if (i > 1) {
                        wcnStatusData.countyUgc.remove(wcnStatusData.countyUgc
                                .size() - 1);// cleanup
                    }

                    wcnStatusData = getCountyNameLatLon(wcnStatusData);
                }

                int vtechNumber = awwugcs.getAwwVtecLine().size();
                if (vtechNumber > 0) {
                    for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) {
                        wcnStatusData.eventType = awwVtech.getAction();
                        wcnStatusData.evTrack = awwVtech
                                .getEventTrackingNumber();
                        wcnStatusData.evEndTime = new DataTime(
                                awwVtech.getEventEndTime());
                        wcnStatusData.origEndTime = wcnStatusData.evEndTime;
                        wcnStatusData.displayStart = wcnStatusData.issueTime;
                        wcnStatusData.displayEnd = wcnStatusData.origEndTime;
                        wcnStatusData.evOfficeId = awwVtech.getOfficeID();
                        wcnStatusData.evPhenomena = awwVtech.getPhenomena();
                        wcnStatusData.evProductClass = awwVtech
                                .getProductClass();
                        wcnStatusData.evSignificance = awwVtech
                                .getSignificance();
                        // RM4637 Create the report type from the VTEC phenomena
                        // and significance
                        String buildReportType = wcnStatusData.evPhenomena
                                + "." + wcnStatusData.evSignificance;
                        if (buildReportType.equalsIgnoreCase("TO.A")) {
                            wcnStatusData.reportType = AwwReportType.TORNADO_WATCH;
                        } else if (buildReportType.equalsIgnoreCase("SV.A")) {
                            wcnStatusData.reportType = AwwReportType.SEVERE_THUNDERSTORM_WATCH;
                        } else {
                            wcnStatusData.reportType = AwwReportType
                                    .getReportType(awwRecord.getReportType());
                            if (awwRecord.getBullMessage().contains(
                                    "THUNDERSTORM")) {
                                wcnStatusData.reportType = AwwReportType.SEVERE_THUNDERSTORM_WATCH;// "THUNDERSTORM";
                            } else if (awwRecord.getBullMessage().contains(
                                    "TORNADO")) {
                                wcnStatusData.reportType = AwwReportType.TORNADO_WATCH;// "TORNADO";
                            }
                        }

                        if ((awwVtech.getAction().equalsIgnoreCase("COR"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("CAN"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("NEW"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("EXA"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("EXB"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("EXT"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("EXP"))
                                || (awwVtech.getAction()
                                        .equalsIgnoreCase("CON"))) {
                            modifyList.add(wcnStatusData);
                        }

                        if (awwVtech.getEventStartTime() != null
                                && awwVtech.getEventEndTime() != null) {
                            wcnStatusData.eventTime = new DataTime(
                                    awwVtech.getEventStartTime(),
                                    new TimeRange(awwVtech.getEventStartTime(),
                                            awwVtech.getEventEndTime()));

                        } else if (awwVtech.getEventEndTime() != null) {
                            wcnStatusData.eventTime = new DataTime(
                                    wcnStatusData.issueTime
                                            .getRefTimeAsCalendar(),
                                    new TimeRange(wcnStatusData.issueTime
                                            .getRefTimeAsCalendar(), awwVtech
                                            .getEventEndTime()));

                        } else if (awwVtech.getEventStartTime() != null) {
                            wcnStatusData.eventTime = new DataTime(
                                    awwVtech.getEventStartTime(),
                                    new TimeRange(awwVtech.getEventStartTime(),
                                            wcnStatusData.issueTime
                                                    .getRefTimeAsCalendar()));
                        } else {
                            wcnStatusData.eventTime = wcnStatusData.issueTime;

                        }

                    }
                }

                wcnDataList.add(wcnStatusData);
            }

        } catch (NumberFormatException nfe) {
            // may need to handle..
            System.err
                    .println("****** NumberFormatException **** ****** *** *** ****** *** *** ****** *** *** ******"
                            + "\n" + nfe.getLocalizedMessage());
        } catch (Exception e) {
            logger.warning("In getAwwtData(AwwRecord awwRecord)\n"
                    + e.getClass().getCanonicalName() + ":"
                    + e.getLocalizedMessage());
            e.printStackTrace();
        }

        preProcessFrameUpdate();

        return (ArrayList<WcnRscDataObj>) wcnDataList;
    }

    private static String queryPrefixMZ_LatLons = "select wfo,name,id, lat, lon from mapdata.marinezones";

    private WcnRscDataObj getCountyNameLatLon(WcnRscDataObj wdata) {
        wdata.countyPoints = new ArrayList<LatLonPoint>();
        wdata.countyNames = new ArrayList<String>();
        wdata.stateNames = new ArrayList<String>();
        wdata.countyLat = new float[wdata.countyUgc.size()];
        wdata.countyLon = new float[wdata.countyUgc.size()];

        try {
            int i = 0;

            for (Iterator<String> iterator = wdata.countyUgc.iterator(); iterator
                    .hasNext();) {
                String theKey = iterator.next();

                Station station = stationTable.getStation(StationField.STID,
                        theKey);
                if (station != null) {
                    LatLonPoint point = new LatLonPoint(station.getLatitude(),
                            station.getLongitude(), LatLonPoint.INDEGREES);
                    wdata.countyPoints.add(point);
                    wdata.countyNames.add(station.getStnname());
                    wdata.stateNames.add(station.getState());
                    wdata.countyLat[i] = station.getLatitude();
                    wdata.countyLon[i] = station.getLongitude();

                    if (wdata.isCounty) {
                        String s = station.getStnnum();
                        wdata.countyFips.add(s.length() == 4 ? "0" + s : s);
                    }// T456 AwwUgc.getAwwFIPS

                } else {
                    // ELSE:
                    // <station>
                    // <stid>GUC110</stid> == maps.id
                    // <stnnum>69110</stnnum> ???
                    // <stnname>Saipan</stnname> == maps.name
                    // <state>GU</state> ...
                    // <country>US</country> ...
                    // <latitude>15.19</latitude> == maps.lat
                    // <longitude>145.76</longitude> == maps.lon
                    // <elevation>0</elevation> 0..
                    // <priority>0</priority>
                    // <wfo>GUM</wfo> == maps.wfo
                    // </station>

                    List<Object[]> results = DirectDbQuery.executeQuery(
                            queryPrefixMZ_LatLons.toString() + " where id = '"
                                    + theKey + "'", "maps", QueryLanguage.SQL);

                    LatLonPoint point = new LatLonPoint(
                            Float.parseFloat(results.get(0)[3].toString()),
                            Float.parseFloat(results.get(0)[4].toString()),
                            LatLonPoint.INDEGREES);
                    wdata.countyPoints.add(point);
                    wdata.countyNames.add(results.get(0)[1].toString());
                    wdata.stateNames.add("");
                    wdata.countyLat[i] = Float.parseFloat(results.get(0)[3]
                            .toString());
                    wdata.countyLon[i] = Float.parseFloat(results.get(0)[4]
                            .toString());
                    wdata.countyFips.add(results.get(0)[2].toString());
                }
                i++;
            }
        } catch (IndexOutOfBoundsException idxOobEx) {
            logger.log(Level.FINEST,
                    "In getCountyNameLatLon(WcnRscDataObj wdata)\n" // mute?
                            + idxOobEx.getClass().getCanonicalName()
                            + ":"
                            + idxOobEx.getLocalizedMessage());
        } catch (Exception e) {
            logger.warning("In getCountyNameLatLon(WcnRscDataObj wdata)\n"
                    + e.getClass().getCanonicalName() + ":"
                    + e.getLocalizedMessage());
        }

        wdata.countyNumPoints = wdata.countyNames.size();

        return wdata;
    }

    @Override
    protected boolean preProcessFrameUpdate() {

        modifyQueue();

        return true;
    }

    private void modifyQueue() {
        if (modifyList != null) {
            for (WcnRscDataObj modify : modifyList) {
                for (IRscDataObject rscDataObj : newRscDataObjsQueue) {
                    WcnRscDataObj candidate = (WcnRscDataObj) rscDataObj;

                    if (modify.evTrack.equalsIgnoreCase(candidate.evTrack)
                            && modify.evOfficeId
                                    .equalsIgnoreCase(candidate.evOfficeId)
                            && modify.evPhenomena
                                    .equalsIgnoreCase(candidate.evPhenomena)
                            && modify.evProductClass
                                    .equalsIgnoreCase(candidate.evProductClass)
                            && modify.evSignificance
                                    .equalsIgnoreCase(candidate.evSignificance)) {
                        if (candidate.eventType.equalsIgnoreCase("CAN")) {
                            candidate.evEndTime = candidate.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));// ?
                        } else if (candidate.eventType.equalsIgnoreCase("COR")) {
                            candidate.evEndTime = modify.evEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));// ?
                        } else if (candidate.eventType.equalsIgnoreCase("EXP")) {
                            candidate.evEndTime = candidate.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else if (candidate.eventType.equalsIgnoreCase("EXA")) {
                            candidate.evEndTime = candidate.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else if (candidate.eventType.equalsIgnoreCase("EXT")) {
                            candidate.evEndTime = candidate.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else if (candidate.eventType.equalsIgnoreCase("NEW")) {
                            candidate.eventTime = modify.eventTime;
                            candidate.evEndTime = modify.evEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else if (candidate.eventType.equalsIgnoreCase("EXB")) {
                            candidate.evEndTime = candidate.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else if (candidate.eventType.equalsIgnoreCase("CON")) {
                            candidate.evEndTime = modify.origEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        } else {
                            candidate.evEndTime = modify.evEndTime;
                            candidate.eventTime = new DataTime(
                                    candidate.eventTime.getRefTimeAsCalendar(),
                                    new TimeRange(candidate.eventTime
                                            .getRefTimeAsCalendar(),
                                            candidate.evEndTime
                                                    .getRefTimeAsCalendar()));
                        }
                    }

                }
            }
        }
    }

    public void initResource(IGraphicsTarget grphTarget) throws VizException {
        font = grphTarget.initializeFont("Monospace", 14,
                new IFont.Style[] { IFont.Style.BOLD });
        stationTable = new StationTable(NcPathManager.getInstance()
                .getStaticFile(NcPathConstants.COUNTY_STN_TBL)// FFG_ZONES_STN_TBL)//COUNTY_STN_TBL)
                .getAbsolutePath());
        // HashMap<String, RequestConstraint> metadataMap =new HashMap<String,
        // RequestConstraint>(resourceData.getMetadataMap());
        // metadataMap.put("reportType",new
        // RequestConstraint("WATCH_COUNTY_NOTIFICATION"));
        // resourceData.setMetadataMap(metadataMap);
        queryRecords();
    }

    @Override
    public void disposeInternal() {
    }

    public void paintFrame(AbstractFrameData frameData, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (paintProps == null) {
            return;
        }

        if (areaChangeFlag) {
            areaChangeFlag = false;
            postProcessFrameUpdate();
        }// T456: dispose old outlineShape? TODO

        FrameData currFrameData = (FrameData) frameData;

        // RGB color = new RGB (155, 155, 155);
        RGB symbolColor = new RGB(155, 155, 155);
        LineStyle lineStyle = LineStyle.SOLID;
        int symbolWidth = 2;
        int symbolSize = 2;

        // RM 4637: Need to sort the WCNs by issue time prior to passing in to
        // be chopped into individual counties.
        List<WcnRscDataObj> sortedWcns = new ArrayList<WcnRscDataObj>();

        for (Map.Entry<String, WcnRscDataObj> entry : currFrameData.wcnDataMap
                .entrySet()) {
            sortedWcns.add(entry.getValue());
        }
        Collections.sort(sortedWcns);

        List<PreProcessDisplayObj> displayObjs = PreProcessDisplay(sortedWcns);

        for (PreProcessDisplayObj wcnData : displayObjs) {
            Boolean draw = false, drawLabel = true;

            if (wcnRscData.getColorCodeEnable()) {
                int watchNumberchoice = Integer.parseInt(wcnData.watchNumber) % 10;

                switch (watchNumberchoice) {

                case 0:
                    if (wcnRscData.Watchxxx0Enable) {
                        color = wcnRscData.Watchxxx0Color;
                        symbolWidth = wcnRscData.Watchxxx0SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx0SymbolSize;
                        symbolColor = wcnRscData.Watchxxx0SymbolColor;
                    }
                    break;
                case 1:
                    if (wcnRscData.Watchxxx1Enable) {
                        color = wcnRscData.Watchxxx1Color;
                        symbolWidth = wcnRscData.Watchxxx1SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx1SymbolSize;
                        symbolColor = wcnRscData.Watchxxx1SymbolColor;
                    }
                    break;
                case 2:
                    if (wcnRscData.Watchxxx2Enable) {
                        color = wcnRscData.Watchxxx2Color;
                        symbolWidth = wcnRscData.Watchxxx2SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx2SymbolSize;
                        symbolColor = wcnRscData.Watchxxx2SymbolColor;
                    }
                    break;
                case 3:
                    if (wcnRscData.Watchxxx3Enable) {
                        color = wcnRscData.Watchxxx3Color;
                        symbolWidth = wcnRscData.Watchxxx3SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx3SymbolSize;
                        symbolColor = wcnRscData.Watchxxx3SymbolColor;
                    }
                    break;
                case 4:
                    if (wcnRscData.Watchxxx4Enable) {
                        color = wcnRscData.Watchxxx4Color;
                        symbolWidth = wcnRscData.Watchxxx4SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx4SymbolSize;
                        symbolColor = wcnRscData.Watchxxx4SymbolColor;
                    }
                    break;
                case 5:
                    if (wcnRscData.Watchxxx5Enable) {
                        color = wcnRscData.Watchxxx5Color;
                        symbolWidth = wcnRscData.Watchxxx5SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx5SymbolSize;
                        symbolColor = wcnRscData.Watchxxx5SymbolColor;
                    }
                    break;
                case 6:
                    if (wcnRscData.Watchxxx6Enable) {
                        color = wcnRscData.Watchxxx6Color;
                        symbolWidth = wcnRscData.Watchxxx6SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx6SymbolSize;
                        symbolColor = wcnRscData.Watchxxx6SymbolColor;
                    }
                    break;
                case 7:
                    if (wcnRscData.Watchxxx7Enable) {
                        color = wcnRscData.Watchxxx7Color;
                        symbolWidth = wcnRscData.Watchxxx7SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx7SymbolSize;
                        symbolColor = wcnRscData.Watchxxx7SymbolColor;
                    }
                    break;
                case 8:
                    if (wcnRscData.Watchxxx8Enable) {
                        color = wcnRscData.Watchxxx8Color;
                        symbolWidth = wcnRscData.Watchxxx8SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx8SymbolSize;
                        symbolColor = wcnRscData.Watchxxx8SymbolColor;
                    }
                    break;
                case 9:
                    if (wcnRscData.Watchxxx9Enable) {
                        color = wcnRscData.Watchxxx9Color;
                        symbolWidth = wcnRscData.Watchxxx9SymbolWidth;
                        symbolSize = wcnRscData.Watchxxx9SymbolSize;
                        symbolColor = wcnRscData.Watchxxx9SymbolColor;
                    }
                    break;
                }
            } else {
                if (wcnData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH) {
                    color = wcnRscData.thunderstormColor;
                    symbolWidth = wcnRscData.thunderstormSymbolWidth;
                    symbolSize = wcnRscData.thunderstormSymbolSize;
                    symbolColor = wcnRscData.thunderstormSymbolColor;
                } else if (wcnData.reportType == AwwReportType.TORNADO_WATCH) {
                    color = wcnRscData.tornadoColor;
                    symbolWidth = wcnRscData.tornadoSymbolWidth;
                    symbolSize = wcnRscData.tornadoSymbolSize;
                    symbolColor = wcnRscData.tornadoSymbolColor;
                }
            }

            if ((wcnData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH && wcnRscData.thunderstormEnable)
                    || (wcnData.reportType == AwwReportType.TORNADO_WATCH && wcnRscData.tornadoEnable)) {

                draw = true;
            }
            if (getCurrentFrameTime().getValidTimeAsDate().getTime() < wcnData.displayEnd
                    .getValidPeriod().getEnd().getTime()
                    || getCurrentFrameTime().getValidTimeAsDate().getTime() >= wcnData.displayStart
                            .getTime()) {
                draw = true;
            }

            if (getCurrentFrameTime().getValidTimeAsDate().getTime() > wcnData.displayEnd
                    .getValidPeriod().getEnd().getTime()) {
                draw = false;
            }

            if (getCurrentFrameTime().getValidTimeAsDate().getTime() < wcnData.displayStart
                    .getTime()) {
                draw = false;
            }

            if (getCurrentFrameTime().getValidTimeAsDate().getTime() == (wcnData.displayEnd
                    .getValidPeriod().getEnd().getTime())) {
                // do not draw endtime frame, that's what nmap2 does
                draw = false;
            }
            // }
            // draw the polygon
            if (true == draw) {

                if (wcnRscData.getWatchBoxMarkerEnable()) {
                    try {
                        Color[] colors = new Color[] { new Color(color.red,
                                color.green, color.blue) };

                        // for (int i = 0; i < wcnData.countyNumPoints; i++) {

                        Coordinate coord = new Coordinate(wcnData.countyLon,
                                wcnData.countyLat);
                        Symbol pointSymbol = new Symbol(null, colors,
                                symbolWidth, symbolSize * 0.4, false, coord,
                                "Symbol", "FILLED_DIAMOND");
                        DisplayElementFactory df = new DisplayElementFactory(
                                target, getNcMapDescriptor());
                        ArrayList<IDisplayable> displayEls = df
                                .createDisplayElements(pointSymbol, paintProps);
                        for (IDisplayable each : displayEls) {
                            each.draw(target, paintProps);
                            each.dispose();
                        }
                        // }
                    } catch (Exception e) {
                        logger.warning("In paintFrame(AbstractFrameData frameData, IGraphicsTarget target,PaintProperties paintProps)\n"
                                + e.getClass().getCanonicalName()
                                + ":"
                                + e.getLocalizedMessage());
                    }
                }
                if (wcnRscData.getWatchBoxFillEnable()) {
                    if (!color.equals(wcnData.color)) {
                        postProcessFrameUpdate();
                        wcnData.color = color;
                    }
                    // drawCountyOutline(wcnData,target,symbolColor,symbolWidth,lineStyle,paintProps,1);
                    drawCountyOutline2(wcnData, target, symbolColor,
                            symbolWidth, lineStyle, paintProps, 1);
                    if (!wcnRscData.getWatchBoxUnionEnable()) {
                        drawTimeLabelWatchNumber(wcnData, target, color);
                    }// only if watchbox union enable is false
                }

                if (wcnRscData.getWatchBoxUnionEnable()) {

                    drawLabel = false;
                    double allX = wcnData.countyLon;
                    double allY = wcnData.countyLat;
                    /*
                     * for (int i = 1; i < wcnData.countyNumPoints; i++) { allX
                     * += wcnData.countyLon[i]; allY += wcnData.countyLat[i]; }
                     */
                    double[] labelLatLon = { allX, allY };
                    // double[] labelLatLon = {
                    // (wcnData.countyLon[0]+wcnData.countyLon[(wcnData.countyNumPoints)/2])/2,
                    // (wcnData.countyLat[0]+wcnData.countyLat[(wcnData.countyNumPoints)/2])/2
                    // };

                    double[] labelPix = descriptor.worldToPixel(labelLatLon);
                    if (labelPix != null) {
                        String[] text = new String[2];
                        List<String> enabledText = new ArrayList<String>();

                        if (wcnRscData.getWatchBoxNumberEnable()) {
                            enabledText.add(wcnData.watchNumber);
                        }

                        if (wcnRscData.getWatchBoxTimeEnable()) { // change from
                                                                  // box-time?
                            DataTime startTime = new DataTime(wcnData.eventTime
                                    .getValidPeriod().getStart());
                            DataTime endTime = new DataTime(wcnData.origEndTime
                                    .getValidPeriod().getEnd());
                            String temp = startTime.toString()
                                    .substring(11, 13)
                                    + startTime.toString().substring(14, 16)
                                    + "-"
                                    + endTime.toString().substring(11, 13)
                                    + endTime.toString().substring(14, 16);
                            enabledText.add(temp);
                        }

                        for (int i = enabledText.size(); i < 2; i++)
                            enabledText.add("");

                        text = enabledText.toArray(text);
                        DrawableString wcnTime = new DrawableString(text, color);
                        wcnTime.font = font;
                        wcnTime.setCoordinates(labelPix[0], labelPix[1], 0.0);
                        wcnTime.textStyle = TextStyle.NORMAL;
                        wcnTime.horizontalAlignment = HorizontalAlignment.LEFT;
                        wcnTime.verticallAlignment = VerticalAlignment.BOTTOM;
                        target.drawStrings(wcnTime);
                    }

                    drawCountyOutline2(wcnData, target, color, symbolWidth,
                            lineStyle, paintProps, 2);
                    // drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,2);
                }

                else if (wcnRscData.getWatchBoxOutlineEnable()) {
                    drawCountyOutline2(wcnData, target, color, symbolWidth,
                            lineStyle, paintProps, 0);// T456
                    // drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,0);
                }

                if (((wcnRscData.getWatchBoxTimeEnable()
                        || wcnRscData.getWatchBoxLabelEnable() || wcnRscData
                            .getWatchBoxNumberEnable()) && drawLabel)
                        && !wcnRscData.watchBoxFillEnable) {
                    drawTimeLabelWatchNumber(wcnData, target, color);
                }
            }// if draw = true

        }// end-foreach in wcndata..
    }

    /*
     * 0 for drawoutline 1 for shaded shape 2 for union
     */
    public void drawCountyOutline(WcnRscDataObj wcnData,
            IGraphicsTarget target, RGB color, int symbolWidth,
            LineStyle lineStyle, PaintProperties paintProps,
            int drawOutlineOrShadedshapeorUnion) throws VizException {
        String countyName, stateName;
        Envelope env = null;
        try {
            PixelExtent extent = (PixelExtent) paintProps.getView().getExtent();
            Envelope e = getNcMapDescriptor().pixelToWorld(extent,
                    descriptor.getCRS());
            ReferencedEnvelope ref = new ReferencedEnvelope(e,
                    descriptor.getCRS());
            env = ref.transform(MapUtil.LATLON_PROJECTION, true);
        } catch (Exception e) {
            throw new VizException("Error transforming extent", e);
        }
        Collection<Geometry> gCollection = new ArrayList<Geometry>();

        String geoConstraint = String
                .format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                        env.getMinX(), env.getMinY(), env.getMaxX(),
                        env.getMaxY());
        for (int i = 0; i < wcnData.countyNames.size(); i++) {
            countyName = wcnData.countyNames.get(i);
            stateName = wcnData.stateNames.get(i);
            if (countyName.contains("_")) {
                countyName = countyName.replace("_", " ");
            }
            StringBuilder query = new StringBuilder(
                    "select AsBinary(the_geom), AsBinary(the_geom_0_001) from mapdata.county where countyname ='");
            query.append(countyName);
            query.append("' AND  state ='");
            query.append(stateName);
            query.append("' AND ");
            query.append(geoConstraint);
            query.append(";");
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), "maps", QueryLanguage.SQL);
            IWireframeShape newOutlineShape = target.createWireframeShape(
                    false, descriptor, 0.0f);
            IShadedShape newShadedShape = target.createShadedShape(false,
                    descriptor.getGridGeometry(), true);
            JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,
                    newOutlineShape, descriptor, PointStyle.CROSS);

            WKBReader wkbReader = new WKBReader();
            for (Object[] result : results) {
                int k = 0;
                byte[] wkb = (byte[]) result[k + 1];
                byte[] wkb1 = (byte[]) result[k];
                Geometry g;
                MultiPolygon countyGeo = null;
                try {
                    g = wkbReader.read(wkb);
                    if (!(g instanceof Point)) {
                        jtsCompiler.handle(g, color);
                    }
                    countyGeo = (MultiPolygon) wkbReader.read(wkb1);
                    if (countyGeo != null) {
                        gCollection.add(countyGeo);
                    }
                } catch (VizException e) {
                    logger.warning("Error reprojecting map outline:"
                            + e.getMessage());
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            newOutlineShape.compile();
            newShadedShape.compile();
            float alpha = paintProps.getAlpha();
            if (drawOutlineOrShadedshapeorUnion == 1) {
                if (newShadedShape != null && newShadedShape.isDrawable()) {
                    target.drawShadedShape(newShadedShape, alpha);
                }
            } else if (drawOutlineOrShadedshapeorUnion == 0) {

                if (newOutlineShape != null && newOutlineShape.isDrawable()) {
                    target.drawWireframeShape(newOutlineShape, color,
                            symbolWidth, lineStyle);
                }
            }
        }
        if (drawOutlineOrShadedshapeorUnion == 2) {
            IWireframeShape newUnionShape = target.createWireframeShape(false,
                    descriptor, 0.0f);
            JTSCompiler jtsCompile = new JTSCompiler(null, newUnionShape,
                    descriptor, PointStyle.CROSS);
            GeometryFactory gf = new GeometryFactory();

            GeometryCollection geometryCollection = (GeometryCollection) gf
                    .buildGeometry(gCollection);
            try {
                jtsCompile.handle(geometryCollection.union(), color);
                newUnionShape.compile();

                if (newUnionShape != null && newUnionShape.isDrawable()) {
                    target.drawWireframeShape(newUnionShape, color,
                            symbolWidth, lineStyle);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void drawTimeLabelWatchNumber(PreProcessDisplayObj wcnData,
            IGraphicsTarget target, RGB color) {
        try {
            double[] labelLatLon = { wcnData.countyLon, wcnData.countyLat };
            double[] labelPix = descriptor.worldToPixel(labelLatLon);

            if (labelPix != null) {
                String[] text = new String[3];
                List<String> enabledText = new ArrayList<String>();

                if (wcnRscData.getWatchBoxNumberEnable()) {
                    enabledText.add(wcnData.watchNumber);
                }

                if (wcnRscData.getWatchBoxLabelEnable()) {
                    enabledText.add(wcnData.countyNames);
                }

                if (wcnRscData.getWatchBoxTimeEnable()) {
                    DataTime startTime = new DataTime(wcnData.eventTime
                            .getValidPeriod().getStart());
                    DataTime endTime = new DataTime(wcnData.origEndTime
                            .getValidPeriod().getEnd());
                    String temp = startTime.toString().substring(11, 13)
                            + startTime.toString().substring(14, 16) + "-"
                            + endTime.toString().substring(11, 13)
                            + endTime.toString().substring(14, 16);
                    enabledText.add(temp);
                }

                for (int j = enabledText.size(); j < 3; j++) {
                    enabledText.add("");
                }
                text = enabledText.toArray(text);
                DrawableString wcnTime = new DrawableString(text, color);
                wcnTime.font = font;
                wcnTime.setCoordinates(labelPix[0], labelPix[1], 0.0);
                wcnTime.textStyle = TextStyle.NORMAL;
                wcnTime.horizontalAlignment = HorizontalAlignment.LEFT;
                wcnTime.verticallAlignment = VerticalAlignment.BOTTOM;
                target.drawStrings(wcnTime);
            }
        } catch (Exception e) {
            System.out.println("wcnResource.java at Line 427" + e);// TODO fix
        }
    }

    // RM 4637 county by county display. Creates a subset of the master
    // WcnRscDataObj and boils it down
    // to only what is needed for display.
    public class PreProcessDisplayObj implements
            Comparable<PreProcessDisplayObj> {
        DataTime issueTime; // issue time from bulletin

        DataTime eventTime;

        AwwReportType reportType;

        float countyLat;

        float countyLon;

        String countyNames, stateNames;

        public String countyFips;

        String eventType;

        String watchNumber; // watch number to be displayed

        DataTime origEndTime;

        Date displayStart;

        DataTime displayEnd;

        String evTrack;

        String evPhenomena;

        String evSignificance;

        String evOfficeId;

        DataTime start;

        RGB color = new RGB(155, 155, 155);

        public String getKey() {
            return getpreProcessDisplayObjKey(this);
        }

        // Used to sort finished collection of single county objects.
        @Override
        public int compareTo(PreProcessDisplayObj o) {

            return (int) (this.issueTime.getRefTime().getTime() - o.issueTime
                    .getRefTime().getTime());
        }
    }

    // RM 4637 time labels. The original implementation of this resource was to
    // take each WCN message and
    // use that as a basis for display. This had two flaws. One it caused
    // multiple labels to be created for the
    // same county. Two, if an EXT was issued it would cause a collision of
    // labels. To fix this we pre-process
    // all valid messages for the current frame data and make sure the county
    // only has one label displayed at all
    // times. For every message except EXT and EXB the first instance of the
    // county in any WCN will set the time
    // label. Since when the message is decoded if the VTEC start time is
    // 000000T0000Z it will point back to NEW
    // product, it does not matter which eventType we used to create the initial
    // start time label. For EXT the
    // display needs to be updated to reflect that. In those cases it resets the
    // displayEnd variable time of the previous
    // label so that there is only one label displayed at all times and the end
    // time is correct based on the frame
    // time. Once all the county labels are determined, the array is sorted and
    // returned to have the labels drawn.

    private List<PreProcessDisplayObj> PreProcessDisplay(
            List<WcnRscDataObj> wcnCollection) {
        HashMap<String, PreProcessDisplayObj> displayMap;
        HashMap<String, DataTime> extMap;
        HashMap<String, String> splitWatch;
        displayMap = new HashMap<String, PreProcessDisplayObj>();
        extMap = new HashMap<String, DataTime>();
        splitWatch = new HashMap<String, String>();
        List<PreProcessDisplayObj> displayObj = new ArrayList<PreProcessDisplayObj>();
        for (WcnRscDataObj processWCN : wcnCollection) {
            for (int i = 0; i < processWCN.countyNumPoints; i++) {
                PreProcessDisplayObj singleCounty = new PreProcessDisplayObj();
                singleCounty.countyFips = processWCN.countyFips.get(i);
                singleCounty.countyLat = processWCN.countyLat[i];
                singleCounty.countyLon = processWCN.countyLon[i];
                singleCounty.countyNames = processWCN.countyNames.get(i);
                if (!processWCN.eventType.equalsIgnoreCase("NEW")
                        || !processWCN.eventType.equalsIgnoreCase("CON")) {
                    singleCounty.displayStart = processWCN.issueTime
                            .getValidPeriod().getStart();
                } else {
                    singleCounty.displayStart = processWCN.eventTime
                            .getValidPeriod().getStart();
                }
                singleCounty.displayEnd = processWCN.origEndTime;
                singleCounty.eventTime = processWCN.eventTime;
                singleCounty.eventType = processWCN.eventType;
                singleCounty.issueTime = processWCN.issueTime;
                singleCounty.origEndTime = processWCN.origEndTime;
                singleCounty.reportType = processWCN.reportType;
                singleCounty.stateNames = processWCN.stateNames.get(i);
                singleCounty.watchNumber = processWCN.watchNumber;
                // Some watches beginn when a previous is ending. The watch
                // number in the WCN then is 001/002.
                // Need to track this to avoid multiple labels being displayed.
                if (singleCounty.watchNumber.contains("/")) {
                    splitWatch.put(singleCounty.countyFips,
                            singleCounty.watchNumber);
                }
                singleCounty.evTrack = processWCN.evTrack;
                singleCounty.evPhenomena = processWCN.evPhenomena;
                singleCounty.evSignificance = processWCN.evSignificance;
                singleCounty.evOfficeId = processWCN.evOfficeId;
                if (singleCounty.eventType.equalsIgnoreCase("EXT")
                        || singleCounty.eventType.equalsIgnoreCase("EXB")
                        || singleCounty.eventType.equalsIgnoreCase("EXA")) {
                    if (extMap.containsKey(singleCounty.countyFips + " "
                            + singleCounty.evTrack)) {
                        DataTime retrievePreviousEXTTime = extMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack);
                        PreProcessDisplayObj retrievePreviousEXT = displayMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack + " " + "EXT"
                                        + retrievePreviousEXTTime);
                        if (retrievePreviousEXT == null) {
                            retrievePreviousEXT = displayMap
                                    .get(singleCounty.countyFips + " "
                                            + singleCounty.evTrack + " "
                                            + "EXT");
                            retrievePreviousEXT.displayEnd = singleCounty.issueTime;
                            displayMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack + " " + "EXT",
                                    retrievePreviousEXT);
                            extMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack,
                                    singleCounty.issueTime);
                        } else {
                            retrievePreviousEXT.displayEnd = singleCounty.issueTime;
                            displayMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack + " " + "EXT" + " "
                                    + retrievePreviousEXTTime,
                                    retrievePreviousEXT);
                            displayMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack + " " + "EXT" + " "
                                    + singleCounty.issueTime, singleCounty);
                            extMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack,
                                    singleCounty.issueTime);
                        }
                    } else if (displayMap.containsKey(singleCounty.countyFips
                            + " " + singleCounty.evTrack)) {
                        PreProcessDisplayObj retrieveCurrent = displayMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack);
                        retrieveCurrent.displayEnd = singleCounty.issueTime;
                        displayMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack, retrieveCurrent);
                        displayMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack + " " + "EXT",
                                singleCounty);
                        extMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack, singleCounty.issueTime);
                    } else {
                        displayMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack, singleCounty);
                    }
                } else if (singleCounty.eventType.equalsIgnoreCase("CAN")
                        || singleCounty.eventType.equalsIgnoreCase("EXP")) {
                    if (extMap.containsKey(singleCounty.countyFips + " "
                            + singleCounty.evTrack)) {
                        DataTime retrievePreviousEXTTime = extMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack);
                        PreProcessDisplayObj retrievePreviousEXT = displayMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack + " " + "EXT"
                                        + retrievePreviousEXTTime);
                        if (retrievePreviousEXT == null) {
                            retrievePreviousEXT = displayMap
                                    .get(singleCounty.countyFips + " "
                                            + singleCounty.evTrack + " "
                                            + "EXT");
                            if (singleCounty.eventType.equalsIgnoreCase("CAN")) {
                                retrievePreviousEXT.displayEnd = singleCounty.issueTime;
                            } else {
                                retrievePreviousEXT.displayEnd = singleCounty.origEndTime;
                            }
                            displayMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack + " " + "EXT",
                                    retrievePreviousEXT);
                            extMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack,
                                    singleCounty.origEndTime);
                        } else {
                            retrievePreviousEXT.displayEnd = singleCounty.origEndTime;
                            displayMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack + " " + "EXT" + " "
                                    + retrievePreviousEXTTime,
                                    retrievePreviousEXT);
                            extMap.put(singleCounty.countyFips + " "
                                    + singleCounty.evTrack,
                                    singleCounty.origEndTime);
                        }
                    } else if (displayMap.containsKey(singleCounty.countyFips
                            + " " + singleCounty.evTrack)) {
                        PreProcessDisplayObj retrieveCurrent;
                        retrieveCurrent = displayMap
                                .get(singleCounty.countyFips + " "
                                        + singleCounty.evTrack);
                        if (singleCounty.eventType.equalsIgnoreCase("CAN")) {
                            retrieveCurrent.displayEnd = singleCounty.issueTime;
                        } else {
                            retrieveCurrent.displayEnd = singleCounty.origEndTime;
                        }
                        displayMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack, retrieveCurrent);
                    }
                } else {
                    if (!displayMap.containsKey(singleCounty.countyFips + " "
                            + singleCounty.evTrack)) {
                        displayMap.put(singleCounty.countyFips + " "
                                + singleCounty.evTrack, singleCounty);
                    }
                }
            }
        }

        for (Map.Entry<String, PreProcessDisplayObj> entry : displayMap
                .entrySet()) {
            displayObj.add(entry.getValue());
        }
        Collections.sort(displayObj);

        return (displayObj);
    }

    // ---------------------------------------------------------------T456:

    // for pre-query the database
    private WcnCountyQueryResult queryResult;

    // for storing result of pre-calculation
    private IWireframeShape outlineShape;

    // for storing result of pre-calculation of union
    private IWireframeShape newUnionShape;

    // for storing result of pre-calculation of shade
    private IShadedShape shadedShape;

    // for pre-calculate the IWiredframeShape
    private CountyResultJob crJob = new CountyResultJob("");

    // if it is 1st round in the loop then draw outline since it pre-calculated
    // for all zones
    // private boolean isFirstRound = true;

    // Area change flag
    private boolean areaChangeFlag = false;

    private static java.util.logging.Logger logger = java.util.logging.Logger
            .getLogger(WcnResource.class.getCanonicalName());

    private RGB color = new RGB(155, 155, 155);

    class CountyResultJob extends org.eclipse.core.runtime.jobs.Job {

        private java.util.Map<String, Result> keyResultMap = new java.util.concurrent.ConcurrentHashMap<String, Result>();

        private java.util.Map<String, Result> keyShadeMap = new java.util.concurrent.ConcurrentHashMap<String, Result>();

        private java.util.Map<String, Result> keyUnionMap = new java.util.concurrent.ConcurrentHashMap<String, Result>();

        private IGraphicsTarget target;

        private IMapDescriptor descriptor;

        // private RGB symbolColor = new RGB(155, 155, 155);

        // private RGB color = new RGB (155, 155, 155);

        public class Result {

            public IWireframeShape outlineShape;

            public IWireframeShape newUnionShape;

            public IShadedShape shadedShape;

            public java.util.Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,
                    IWireframeShape nuShape, IShadedShape shadedShape,
                    java.util.Map<Object, RGB> colorMap) {

                this.outlineShape = outlineShape;
                this.shadedShape = shadedShape;
                this.newUnionShape = nuShape;
                this.colorMap = colorMap;
            }
        }

        public CountyResultJob(String name) {
            super(name);
        }

        public void setRequest(IGraphicsTarget target,
                IMapDescriptor descriptor, String query, boolean labeled,
                boolean shaded, java.util.Map<Object, RGB> colorMap) {

            this.target = target;
            this.descriptor = descriptor;
            this.run(null);// this.schedule();

        }

        @Override
        protected org.eclipse.core.runtime.IStatus run(
                org.eclipse.core.runtime.IProgressMonitor monitor) {

            for (AbstractFrameData afd : frameDataMap.values()) {

                FrameData fd = (FrameData) afd;

                for (WcnRscDataObj wrdo : fd.wcnDataMap.values()) {

                    Collection<Geometry> gw = new ArrayList<Geometry>(), gu = new ArrayList<Geometry>();

                    for (int i = 0; i < wrdo.countyFips.size(); i++) {
                        // another loop handles multiple rows in maps
                        // mapdata.county table
                        for (ArrayList<Object[]> results : queryResult
                                .getStateCountyResult2(wrdo.countyFips.get(i))) {

                            if (results == null) {
                                continue;
                            }

                            WKBReader wkbReader = new WKBReader();
                            for (Object[] result : results) {
                                int k = 0;
                                byte[] wkb1 = (byte[]) result[k];

                                MultiPolygon countyGeo = null;
                                try {
                                    countyGeo = (MultiPolygon) wkbReader
                                            .read(wkb1);

                                    if (countyGeo != null
                                            && !countyGeo.isEmpty()) {
                                        gu.add((MultiPolygon) countyGeo.clone());
                                        // gw.add(countyGeo);
                                        gw.add((MultiPolygon) countyGeo.clone());
                                    }
                                } catch (Exception e) {
                                    logger.warning("__Error: " + e.getMessage());
                                }
                            }
                        }
                    }
                    if (gw.size() == 0) {
                        continue;
                    } else {
                        // keyResultMap.put(wrdo.getKey(), new
                        // Result(getEachWrdoShape(gw),null,null,null));
                        setEachWrdoShape(wrdo, gw, gu);
                    }
                }

            }

            return org.eclipse.core.runtime.Status.OK_STATUS;
        }

        public/* IWireframeShape */void setEachWrdoShape(WcnRscDataObj wrdo,
                Collection<Geometry> gw, Collection<Geometry> gu) {

            IWireframeShape newOutlineShape = target.createWireframeShape(
                    false, descriptor, 0.0f);
            IWireframeShape newUnionShape = target.createWireframeShape(false,
                    descriptor, 0.0f);
            IShadedShape newShadedShape = target.createShadedShape(false,
                    descriptor.getGridGeometry(), true);

            JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,
                    newOutlineShape, descriptor, PointStyle.CROSS);
            JTSCompiler jcu = new JTSCompiler(null, newUnionShape, descriptor,
                    PointStyle.CROSS);

            com.vividsolutions.jts.geom.GeometryCollection gColl = (com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory()
                    .buildGeometry(gw);

            com.vividsolutions.jts.geom.GeometryCollection gCollu = (com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory()
                    .buildGeometry(gu);

            try {
                gColl.normalize();
                gCollu.normalize();

                jtsCompiler.handle(gColl, color);
                jcu.handle(gCollu.union(), color);

                newOutlineShape.compile();
                newUnionShape.compile();
                newShadedShape.compile();

            } catch (Exception e) {
                logger.warning("_____Error: " + e.getMessage());
            }

            String key = wrdo.getKey();
            keyResultMap
                    .put(key, new Result(newOutlineShape, null, null, null));
            keyShadeMap.put(key, new Result(null, null, newShadedShape, null));
            keyUnionMap.put(key, new Result(null, newUnionShape, null, null));

            // return newOutlineShape;
        }

    }

    private void drawCountyOutline2(PreProcessDisplayObj wData,
            IGraphicsTarget target, RGB color, int outlineWidth,
            LineStyle lineStyle, PaintProperties paintProps,
            int drawOutlineOrShadedshapeorUnion) {

        String key = wData.getKey();
        CountyResultJob.Result result = crJob.keyResultMap.get(key);
        CountyResultJob.Result resultShaded = crJob.keyShadeMap.get(key);
        CountyResultJob.Result resultU = crJob.keyUnionMap.get(key);

        if (result != null) {
            outlineShape = result.outlineShape;
        } else {
            return;
        }

        if (resultShaded != null) {
            shadedShape = resultShaded.shadedShape;
        } else {
            return;
        }

        if (resultU != null) {
            newUnionShape = resultU.newUnionShape;
        } else {
            return;
        }

        if (getCurrentFrameTime().getValidTimeAsDate().getTime() <= wData.displayEnd
                .getValidPeriod().getEnd().getTime()
                || getCurrentFrameTime().getValidTimeAsDate().getTime() >= wData.displayStart
                        .getTime()) {
            // draw = true;// verify this rule; // // // // // // // // //TODO
        }

        if (shadedShape != null && shadedShape.isDrawable()
                && drawOutlineOrShadedshapeorUnion == 1) {
            try {
                target.drawShadedShape(shadedShape, 0.5f);
            } catch (VizException e) {
                logger.warning("VizException in drawCountyOutline2() of WcnResource");
            }
        }

        if (outlineShape != null && outlineShape.isDrawable()
                && drawOutlineOrShadedshapeorUnion == 0) {
            try {
                target.drawWireframeShape(outlineShape, color, outlineWidth,
                        lineStyle);
            } catch (VizException e) {
                logger.warning("VizException in drawCountyOutline2() of WcnResource");
            }

        } else if (outlineShape == null) {

            // target.setNeedsRefresh(true);
        }

        if (newUnionShape != null && newUnionShape.isDrawable()
                && drawOutlineOrShadedshapeorUnion == 2) {
            try {
                target.drawWireframeShape(newUnionShape, color, outlineWidth,
                        lineStyle);
            } catch (VizException e) {
                logger.warning("VizException in drawCountyOutline2() of WcnResource");
            }
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        areaChangeFlag = true;
    }

    /**
     * avoid null pointers exception in super class
     */
    @Override
    protected long getDataTimeMs(IRscDataObject rscDataObj) {
        // long dataTimeMs =
        // rscDataObj.getDataTime().getValidTime().getTime().getTime();
        if (rscDataObj == null) {
            return 0;
        }

        java.util.Calendar validTimeInCalendar = null;
        DataTime dataTime = rscDataObj.getDataTime();
        if (dataTime != null) {
            validTimeInCalendar = dataTime.getValidTime();

        } else {
            logger.info("===== find IRscDataObject rscDataObj.getDataTime() return NULL!!!");
        }
        long dataTimeInMs = 0;
        if (validTimeInCalendar != null) {
            dataTimeInMs = validTimeInCalendar.getTimeInMillis();
        }
        return dataTimeInMs;
    }

    public String getWcnRscDataObjKey(WcnRscDataObj w) {

        StringBuilder sb = new StringBuilder(w.evOfficeId);

        sb.append(w.evTrack).append(w.evPhenomena).append(w.evSignificance)
                .append(w.eventType);

        for (String s : w.countyFips) {
            sb.append(s);// TODO: is this necessary?
        }
        sb.append(w.countyFips);

        return sb.toString();
    }

    public String getpreProcessDisplayObjKey(PreProcessDisplayObj w) {

        StringBuilder sb = new StringBuilder(w.evOfficeId);

        sb.append(w.evTrack).append(w.evPhenomena).append(w.evSignificance)
                .append(w.eventType);

        sb.append(w.countyFips);

        return sb.toString();
    }

    @Override
    protected boolean postProcessFrameUpdate() {

        AbstractEditor ncme = NcDisplayMngr.getActiveNatlCntrsEditor();

        crJob.setRequest(ncme.getActiveDisplayPane().getTarget(),
                getNcMapDescriptor(), null, false, false, null);

        return true;
    }

    @Override
    public void queryRecords() throws VizException {
        // this method is almost similar to its super class's queryRecords(),
        // may need to be modified later to use the super class's version for
        // the common part

        // RM4637: Changed query to use ThirftClient instead of uengine. Also,
        // limit response to the last twelve hours from frame
        // time requested.
        DbQueryRequest request = new DbQueryRequest();
        DbQueryRequest requestWatchNumber = new DbQueryRequest();
        HashMap<String, RequestConstraint> reqConstraints = new HashMap<String, RequestConstraint>();
        RequestConstraint pluginName = new RequestConstraint(
                wcnRscData.getPluginName());
        IDescriptor.FramesInfo frameTimes = this.descriptor.getFramesInfo();
        long watchcheck = frameTimes.getFrameTimes()[0].getRefTimeAsCalendar()
                .getTimeInMillis();
        watchcheck = watchcheck - 43200000;
        Date queryTime = new Date(watchcheck);
        DataTime setQueryTime = new DataTime(queryTime);
        reqConstraints.put("pluginName", pluginName);
        reqConstraints.put("reportType", new RequestConstraint(
                "WATCH COUNTY NOTIFICATION"));
        reqConstraints.put("dataTime.refTime", new RequestConstraint(
                setQueryTime.toString(), ConstraintType.GREATER_THAN));
        requestWatchNumber.setConstraints(reqConstraints);
        requestWatchNumber.addRequestField("watchNumber");
        requestWatchNumber.setDistinct(true);
        DbQueryResponse responseWatch = (DbQueryResponse) ThriftClient
                .sendRequest(requestWatchNumber);

        // reqConstraints.put("pluginName", pluginName);
        // reqConstraints.put("reportType", new
        // RequestConstraint("WATCH COUNTY NOTIFICATION"));
        reqConstraints.remove("dataTime.refTime");
        List<Object> pdoList = new ArrayList<Object>();
        for (int i = 0; i < responseWatch.getResults().size(); i++) {
            for (Map.Entry<String, Object> watchentry : responseWatch
                    .getResults().get(i).entrySet()) {
                reqConstraints.put("watchNumber", new RequestConstraint(
                        watchentry.getValue().toString()));
                request.setConstraints(reqConstraints);
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                reqConstraints.remove("watchNumber");
                for (int j = 0; j < response.getResults().size(); j++) {
                    for (Map.Entry<String, Object> pdoentry : response
                            .getResults().get(j).entrySet()) {
                        pdoList.add(pdoentry.getValue());
                    }
                }
            }
        }

        // request.setConstraints(reqConstraints);
        // request.setOrderByField("issueTime", OrderMode.ASC);
        // response = (DbQueryResponse) ThriftClient.sendRequest(request);
        queryResult = new WcnCountyQueryResult();
        // List<Object> pdoList = new ArrayList<Object>();
        /*
         * for (int i = 0;i < response.getResults().size();i++) { for
         * (Map.Entry<String, Object> entry :
         * response.getResults().get(i).entrySet()) {
         * pdoList.add(entry.getValue()); } }
         */

        for (Object pdo : pdoList) {
            for (IRscDataObject dataObject : processRecord(pdo)) {
                newRscDataObjsQueue.add(dataObject);

                queryResult.buildQueryPart2(dataObject);
            }
        }

        queryResult.populateMap();
        setAllFramesAsPopulated();
    }

    public boolean isCountyUgs(AwwUgc au) {
        Set<AwwFips> awwFipsSet = au.getAwwFIPS();
        boolean out = false;

        if (awwFipsSet == null) {
            return false;
        } else {

            for (AwwFips eachAwwFips : awwFipsSet) {

                String eachFips = eachAwwFips.getFips();

                if (eachFips == null || eachFips.isEmpty()
                        || eachFips.length() != 6) {
                    return false;
                }

                out = ('C' == eachFips.charAt(2));
            }
        }

        return out;
    }

    @Override
    public String getName() {
        String legendString = super.getName();
        FrameData fd = (FrameData) getCurrentFrame();
        if (fd == null || fd.getFrameTime() == null
                || fd.wcnDataMap.size() == 0) {
            return legendString + "-No Data";
        }
        return legendString + " "
                + NmapCommon.getTimeStringFromDataTime(fd.getFrameTime(), "/");
    }
}
