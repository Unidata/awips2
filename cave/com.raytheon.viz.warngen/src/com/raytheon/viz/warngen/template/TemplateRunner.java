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
package com.raytheon.viz.warngen.template;

import java.awt.geom.Point2D;
import java.io.StringWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.apache.commons.lang.Validate;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.tools.generic.ListTool;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.GetActiveTableRequest;
import com.raytheon.uf.common.activetable.GetActiveTableResponse;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration.AreaType;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warngen.WarngenException;
import com.raytheon.viz.warngen.gis.AffectedAreas;
import com.raytheon.viz.warngen.gis.Area;
import com.raytheon.viz.warngen.gis.ClosestPointComparator;
import com.raytheon.viz.warngen.gis.GisUtil;
import com.raytheon.viz.warngen.gis.PathCast;
import com.raytheon.viz.warngen.gis.Wx;
import com.raytheon.viz.warngen.gui.BackupData;
import com.raytheon.viz.warngen.gui.FollowupData;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.raytheon.viz.warngen.gui.WarngenUIState;
import com.raytheon.viz.warngen.text.WarningTextHandler;
import com.raytheon.viz.warngen.text.WarningTextHandlerFactory;
import com.raytheon.viz.warngen.util.CurrentWarnings;
import com.raytheon.viz.warngen.util.FipsUtil;
import com.raytheon.viz.warngen.util.FollowUpUtil;
import com.raytheon.viz.warngen.util.WarnGenMathTool;
import com.raytheon.viz.warngen.util.WatchUtil;
import com.raytheon.viz.warngen.util.WeatherAdvisoryWatch;
import com.raytheon.viz.warngen.util.WeatherAdvisoryWatch.Portion;
import com.raytheon.viz.warnings.DateUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Sets up and runs the velocity engine for a warngen product. Originally
 * separated from WarngenDialog and cleaned up.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2011            njensen     Initial creation
 * Oct 31, 2011            Qinglu Lin  Call convertAlaskaLons() for eventLocation.
 * May  9, 2012   14887    Qinglu Lin  Changed one argument passed to calculatePortion().
 * May 31, 2012   15047    Qinglu Lin  Added additional logic to canOrExpCal for CAN and EXP.
 * Jun 15, 2012   15043    Qinglu Lin  Added duration to context.
 * Jul 16, 2012   15091    Qinglu Lin  Compute intersection area, which is used for prevent 2nd timezone
 *                                     from appearing in 2nd and 3rd bullets when not necessary.
 * Aug 13, 2012   14493    Qinglu Lin  Handled MND time, event time, and TML time specially for COR to NEW.
 * Aug 29, 2011   15351    jsanchez    Set the timezone for TML time.
 * Sep 10, 2012   15295    snaples     Added property setting for runtime log to createScript.
 * Sep 18, 2012   15332    jsanchez    Used a new warning text handler.
 * Nov  9, 1202   DR 15430 D. Friedman Improve watch inclusion.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TemplateRunner {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TemplateRunner.class);

    private static final String LOGIN_NAME_KEY = "LOGNAME";

    private static final UnitConverter milesToKilometer = NonSI.MILE
            .getConverterTo(SI.KILOMETER);

    private static final double KmToDegrees = 111.12;

    private static final Pattern BBB_PATTERN = Pattern
            .compile(".*\\sCC([A-Z])");

    private static Hashtable<String, DateFormat> dateFormat;

    static {
        dateFormat = new Hashtable<String, DateFormat>();
        dateFormat
                .put("header", new SimpleDateFormat("hmm a z EEE MMM d yyyy"));
        dateFormat.put("plain", new SimpleDateFormat("hmm a z EEEE"));
        dateFormat.put("clock", new SimpleDateFormat("hmm a z"));
        dateFormat.put("ymdthmz", new SimpleDateFormat("yyMMdd'T'HHmm'Z'"));
        dateFormat.put("ddhhmm", new SimpleDateFormat("ddHHmm"));
        dateFormat.put("time", new SimpleDateFormat("HHmm"));
    }

    /**
     * Executes a warngen template given the polygon from the Warngen Layer and
     * the Storm tracking information from StormTrackDisplay
     * 
     * @param warngenLayer
     * @param startTime
     * @param endTime
     * @param selectedBullets
     * @param selectedUpdate
     * @param backupSite
     * @return the generated product
     * @throws Exception
     */
    public static String runTemplate(WarngenLayer warngenLayer, Date startTime,
            Date endTime, String[] selectedBullets, FollowupData followupData,
            BackupData backupData) throws Exception {
        long t0 = System.currentTimeMillis();
        WarngenUIState state = warngenLayer.getWarngenState();
        Geometry warnPolygon = state.getWarningPolygon();
        Geometry warningArea = state.getWarningArea();
        WKTReader wkt = new WKTReader();
        DataTime[] datatimes = warngenLayer.getDescriptor().getFramesInfo()
                .getFrameTimes();
        Date eventTime = datatimes != null && datatimes.length > 0 ? datatimes[datatimes.length - 1]
                .getRefTimeAsCalendar().getTime() : startTime;
        Date simulatedTime = SimulatedTime.getSystemTime().getTime();
        WarngenConfiguration config = warngenLayer.getConfiguration();
        StormTrackState stormTrackState = warngenLayer.getStormTrackState();

        VelocityContext context = new VelocityContext();
        context.put("areaSource", config.getGeospatialConfig().getAreaSource());
        context.put("parentAreaSource", config.getGeospatialConfig()
                .getParentAreaSource());
        context.put("pointSource", config.getGeospatialConfig()
                .getPointSource());

        context.put("user", System.getenv().get(LOGIN_NAME_KEY));

        String threeLetterSiteId = warngenLayer.getLocalizedSite();
        String fourLetterSiteId = SiteMap.getInstance().getSite4LetterId(
                threeLetterSiteId);

        context.put("vtecOffice", fourLetterSiteId);
        context.put("siteId", threeLetterSiteId);
        context.put("WMOId", "TTAAII");

        /** Convenience tools for the template */
        context.put("timeFormat", dateFormat);
        context.put("list", new ListTool());
        context.put("officeShort", warngenLayer.getDialogConfig()
                .getWarngenOfficeShort());
        context.put("officeLoc", warngenLayer.getDialogConfig()
                .getWarngenOfficeLoc());

        if (backupData != null) {
            context.remove("officeLoc");
            context.remove("officeShort");
            context.put("officeLoc", backupData.office);
            context.put("officeShort", backupData.office);
            context.put("backupSite", warngenLayer.getDialogConfig()
                    .getWarngenOfficeShort());
        }

        String stormType = stormTrackState.displayType == DisplayType.POLY ? "line"
                : "single";
        context.put("stormType", stormType);
        context.put("mathUtil", new WarnGenMathTool());
        context.put("dateUtil", new DateUtil());
        context.put("pointComparator", new ClosestPointComparator());

        String action = followupData != null ? followupData.getAct()
                : WarningAction.NEW.toString();
        String phen = followupData != null ? followupData.getPhen() : null;
        String sig = followupData != null ? followupData.getSig() : null;
        String etn = followupData != null ? followupData.getEtn() : null;

        String phenSig = phen + "." + sig;
        WarningAction selectedAction = WarningAction.valueOf(action);
        AffectedAreas[] areas = null;
        AffectedAreas[] cancelareas = null;
        Map<String, Object> intersectAreas = null;
        Wx wx = null;
        long wwaMNDTime = 0l;
        try {
            t0 = System.currentTimeMillis();
            areas = Area.findAffectedAreas(config, warnPolygon, warningArea,
                    threeLetterSiteId);
            System.out.println("Time to get areas = "
                    + (System.currentTimeMillis() - t0));
            context.put(config.getHatchedAreaSource().getVariable(), areas);

            t0 = System.currentTimeMillis();
            intersectAreas = Area.findInsectingAreas(config, warnPolygon,
                    warningArea, threeLetterSiteId, warngenLayer);
            System.out.println("Time to get intersecting areas = "
                    + (System.currentTimeMillis() - t0));
            for (String ia : intersectAreas.keySet()) {
                context.put(ia, intersectAreas.get(ia));
            }

            Map<String, Double> intersectSize = new HashMap<String, Double>();
            String[] oneLetterTZ;
            double minSize = 1.0E-3d;
            if (areas != null && areas.length > 0) {
                Set<String> timeZones = new HashSet<String>();
                for (AffectedAreas area : areas) {
                    if (area.getTimezone() != null) {
                        // Handles counties that span two time zones
                        String oneLetterTimeZones = area.getTimezone().trim();
                        oneLetterTZ = new String[oneLetterTimeZones.length()];
                        if (oneLetterTimeZones.length() == 1) {
                            timeZones.add(String.valueOf(oneLetterTimeZones
                                    .charAt(0)));
                        } else {
                            // Determine if one letter timezone is going to be
                            // put into timeZones.
                            Polygon[] poly1, poly2;
                            int n1, n2;
                            double size, totalSize;
                            for (int i = 0; i < oneLetterTimeZones.length(); i++) {
                                oneLetterTZ[i] = String
                                        .valueOf(oneLetterTimeZones.charAt(i));
                                Geometry timezoneGeom = warngenLayer
                                        .getTimezoneGeom(oneLetterTZ[i]);
                                t0 = System.currentTimeMillis();
                                poly1 = null;
                                poly2 = null;
                                n1 = 0;
                                n2 = 0;
                                size = 0.0d;
                                totalSize = 0.0d;
                                if (timezoneGeom != null && warningArea != null) {
                                    if (intersectSize.get(oneLetterTZ[i]) != null)
                                        continue;
                                    poly1 = new Polygon[warningArea
                                            .getNumGeometries()];
                                    n1 = warningArea.getNumGeometries();
                                    for (int j = 0; j < n1; j++) {
                                        poly1[j] = (Polygon) warningArea
                                                .getGeometryN(j);
                                    }
                                    poly2 = new Polygon[timezoneGeom
                                            .getNumGeometries()];
                                    n2 = timezoneGeom.getNumGeometries();
                                    for (int j = 0; j < n2; j++) {
                                        poly2[j] = (Polygon) timezoneGeom
                                                .getGeometryN(j);
                                    }
                                    // Calculate the total size of intersection
                                    for (Polygon p1 : poly1) {
                                        for (Polygon p2 : poly2) {
                                            size = p1.intersection(p2)
                                                    .getArea();
                                            if (size > 0.0)
                                                totalSize += size;
                                        }
                                        if (totalSize > minSize)
                                            break; // save time when the size of
                                                   // poly1 or poly2 is large
                                    }
                                    intersectSize
                                            .put(oneLetterTZ[i], totalSize);
                                } else
                                    throw new VizException(
                                            "Either timezoneGeom or/and warningArea is null. "
                                                    + "Timezone cannot be determined.");
                                System.out
                                        .println("Time to do size computation = "
                                                + (System.currentTimeMillis() - t0));
                                if (totalSize > minSize) {
                                    timeZones.add(oneLetterTZ[i]);
                                }
                            }
                            // If timeZones has nothing in it when the hatched
                            // area is very small,
                            // use the timezone of larger intersection size.
                            if (timeZones.size() == 0) {
                                if (intersectSize.size() > 1)
                                    if (intersectSize.get(oneLetterTZ[0]) > intersectSize
                                            .get(oneLetterTZ[1])) {
                                        timeZones.add(oneLetterTZ[0]);
                                    } else {
                                        timeZones.add(oneLetterTZ[1]);
                                    }
                                else
                                    throw new VizException(
                                            "The size of intersectSize is less than 1, "
                                                    + "timezone cannot be determined.");
                            }
                        }
                    } else {
                        throw new VizException(
                                "Calling to area.getTimezone() returns null.");
                    }
                }

                Iterator<String> iterator = timeZones.iterator();
                while (iterator.hasNext()) {
                    if (context.get("localtimezone") == null) {
                        context.put("localtimezone", iterator.next());
                    } else if (context.get("secondtimezone") == null) {
                        context.put("secondtimezone", iterator.next());
                    }
                }
            }

            // CAN and EXP products follow different rules as followups
            if (!(selectedAction == WarningAction.CAN || selectedAction == WarningAction.EXP)) {
                Coordinate[] stormLocs = warngenLayer
                        .getStormLocations(stormTrackState);
                wx = new Wx(config, stormTrackState,
                        warngenLayer.getStormLocations(stormTrackState),
                        startTime.getTime(), DateUtil.roundDateTo15(endTime)
                                .getTime(), warnPolygon);
                if (selectedAction == WarningAction.COR) {
                    wwaMNDTime = wx.getStartTime().getTime();
                } else {
                    context.put("now", simulatedTime);
                    context.put("start", wx.getStartTime());
                }
                context.put(
                        "expire",
                        DateUtil.roundDateTo15(selectedAction == WarningAction.EXT ? endTime
                                : wx.getEndTime()));

                // duration: convert millisecond to minute
                long duration = (wx.getEndTime().getTime() - wx.getStartTime()
                        .getTime()) / (1000 * 60);
                context.put("duration", duration);

                context.put("event", eventTime);
                context.put("TMLtime", eventTime);
                context.put("ugcline",
                        FipsUtil.getUgcLine(areas, wx.getEndTime(), 15));
                context.put("areaPoly", GisUtil.convertCoords(warngenLayer
                        .getPolygon().getCoordinates()));

                Map<String, Object> points = wx
                        .getClosetsPoints(threeLetterSiteId);
                for (String variableName : points.keySet()) {
                    context.put(variableName, points.get(variableName));
                }

                PathCast[] pathCast = wx.pathcast(threeLetterSiteId);
                context.put(config.getPathcastConfig().getVariable(), pathCast);

                if (pathCast == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "WarnGen critical error: No PathCast Information");
                }

                // Now create the "other areas

                StormTrackData std = ToolsDataManager.getInstance()
                        .getStormTrackData();
                std.setDate(simulatedTime);
                std.setMotionDirection((int) wx.getMovementDirection());
                std.setMotionSpeed((int) Math.round(wx.getMovementSpeed("kn")));

                context.put("movementSpeed", wx.getMovementSpeed());
                context.put("movementInKnots", wx.getMovementSpeed("kn"));
                double movementDirectionRounded = wx
                        .getMovementDirectionRounded() + 180;
                while (movementDirectionRounded >= 360) {
                    movementDirectionRounded -= 360;
                }
                context.put("movementDirectionRounded",
                        movementDirectionRounded);
                double motionDirection = std.getMotionDirection() + 180;
                while (motionDirection >= 360) {
                    motionDirection -= 360;
                }
                context.put("movementDirection", motionDirection);
                // Convert to Point2D representation as Velocity requires
                // getX() and getY() methods which Coordinate does not have
                Coordinate[] newStormLocs = GisUtil.d2dCoordinates(stormLocs);
                Point2D.Double[] coords = new Point2D.Double[newStormLocs.length];
                for (int i = 0; i < newStormLocs.length; i++) {
                    coords[i] = new Point2D.Double(newStormLocs[i].x,
                            newStormLocs[i].y);
                }
                context.put("eventLocation", coords);
                t0 = System.currentTimeMillis();
                ToolsDataManager.getInstance().setStormTrackData(std);
                System.out.println("save storm track data: "
                        + (System.currentTimeMillis() - t0));
            } else {
                // Retrieve the old Warning
                // Example: s[0-5] = T.CON-KLWX.SV.W.0123
                AbstractWarningRecord oldWarn = CurrentWarnings.getInstance(
                        threeLetterSiteId).getNewestByTracking(etn, phenSig);
                context.put("now", simulatedTime);
                context.put("event", eventTime);
                context.put("start", oldWarn.getStartTime().getTime());
                context.put("expire", oldWarn.getEndTime().getTime());
                Calendar canOrExpCal = Calendar.getInstance();
                canOrExpCal.setTimeZone(TimeZone.getTimeZone("GMT"));
                canOrExpCal.add(Calendar.MINUTE, 10);
                canOrExpCal.add(Calendar.MILLISECOND, 1);
                context.put(
                        "ugcline",
                        FipsUtil.getUgcLine(oldWarn.getUgczones(),
                                canOrExpCal.getTime(), 0));
                String oldGeom = oldWarn.getGeometry().toString();
                context.put("areaPoly", GisUtil.convertCoords(wkt.read(oldGeom)
                        .getCoordinates()));
                // If there is no storm track
                if (oldWarn.getLoc() != null) {
                    Geometry locGeom = wkt.read(oldWarn.getLoc());
                    Coordinate[] locs = locGeom.getCoordinates();
                    // Convert to Point2D representation as Velocity requires
                    // getX() and getY() methods which Coordinate does not have
                    Point2D.Double[] coords = new Point2D.Double[locs.length];
                    for (int i = 0; i < locs.length; i++) {
                        coords[i] = new Point2D.Double(locs[i].x, locs[i].y);
                    }
                    context.put("eventLocation", coords);
                    double motionDirection = oldWarn.getMotdir();
                    while (motionDirection >= 360) {
                        motionDirection -= 360;
                    }
                    context.put("movementDirection", motionDirection);
                    context.put("movementInKnots", oldWarn.getMotspd());

                    StormTrackData std = ToolsDataManager.getInstance()
                            .getStormTrackData();
                    std.setDate(simulatedTime);
                    std.setMotionDirection(oldWarn.getMotdir());
                    std.setMotionSpeed(oldWarn.getMotspd());
                    t0 = System.currentTimeMillis();
                    ToolsDataManager.getInstance().setStormTrackData(std);
                    System.out.println("save storm track data: "
                            + (System.currentTimeMillis() - t0));
                }
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "WarnGen Error", e);
        }

        context.put("BBBId", "");
        context.put("bullets", selectedBullets);

        // Include watches
        context.put("includedWatches", config.getIncludedWatches());

        // Additional Information for Followup Products
        if (etn != null && etn.length() > 0) {
            AbstractWarningRecord oldWarn = null;

            // COR product - What are we correcting?
            boolean allowsNewProduct = false;
            for (String s : config.getFollowUps()) {
                if (s.equals("NEW")) {
                    allowsNewProduct = true;
                }
            }

            CurrentWarnings cw = CurrentWarnings.getInstance(threeLetterSiteId);

            if (selectedAction == WarningAction.COR && !allowsNewProduct) {
                oldWarn = cw.getFollowUpByTracking(etn, phenSig,
                        new WarningAction[] { WarningAction.CON,
                                WarningAction.COR });
            } else {
                oldWarn = cw.getNewestByTracking(etn, phenSig);
            }

            // Hydro product
            if (oldWarn.getFloodSeverity() != null
                    && oldWarn.getFloodSeverity().length() >= 1) {
                context.put("floodseverity", oldWarn.getFloodSeverity());
                context.put("floodic", oldWarn.getImmediateCause());
            }

            if (selectedAction == WarningAction.COR) {
                context.put("action", "COR");
                context.remove("BBBId");
                String oldWmoId = oldWarn.getWmoid();
                Matcher m = BBB_PATTERN.matcher(oldWmoId);
                if (m.matches()) {
                    char c = m.group(1).charAt(0);
                    c++;
                    context.put("BBBId", "CC" + Character.toString(c));
                } else {
                    context.put("BBBId", "CCA");
                }
                context.put("etn", etn);
                context.put("start", oldWarn.getIssueTime().getTime());
                if (oldWarn.getAct().equals("NEW")) {
                    context.put("now", new Date(wwaMNDTime));
                } else
                    context.put("now", simulatedTime);
                context.put("event", oldWarn.getIssueTime().getTime());

                String message = oldWarn.getRawmessage();
                if (!stormTrackState.originalTrack) {
                    context.put("TMLtime", oldWarn.getStartTime().getTime());
                } else {
                    int hour = 0;
                    int minute = 0;
                    int tmlIndex = message.indexOf("TIME...MOT...LOC");
                    int zIndex = -1;
                    if (tmlIndex > 0) {
                        zIndex = message.indexOf("Z", tmlIndex);
                        if (zIndex > 0) {
                            int startIndex = tmlIndex + 16 + 1;
                            String tmlTime = null;
                            tmlTime = message.substring(startIndex,
                                    startIndex + 4);
                            if (tmlTime.length() == 4) {
                                hour = Integer
                                        .parseInt(tmlTime.substring(0, 2));
                                minute = Integer.parseInt(tmlTime.substring(2,
                                        4));
                            } else if (tmlTime.length() == 3) {
                                hour = Integer
                                        .parseInt(tmlTime.substring(0, 1));
                                minute = Integer.parseInt(tmlTime.substring(1,
                                        3));
                            } else {
                                throw new VizException(
                                        "The length of hour and minute for TML time is neither 3 nor 4.");
                            }
                            Calendar c = Calendar.getInstance(TimeZone
                                    .getTimeZone("GMT"));
                            c.set(Calendar.HOUR_OF_DAY, hour);
                            c.set(Calendar.MINUTE, minute);
                            context.put("TMLtime", c.getTime());
                        } else {
                            throw new VizException(
                                    "Z, therefore hour and minute, cannot be found in TIME...MOT...LOC line.");
                        }
                    } else {
                        // To prevent errors resulting from undefined
                        // context("TMLtime")
                        context.put("TMLtime", oldWarn.getIssueTime().getTime());
                    }
                }

                // corEventtime for "COR to NEW", not for
                // "COR to CON, CAN, or CANCON"
                if (oldWarn.getAct().equals("NEW")) {
                    int untilIndex = message.indexOf("UNTIL");
                    int atIndex = -1;
                    int elipsisIndex = -1;
                    if (untilIndex > 0) {
                        atIndex = message.indexOf("AT", untilIndex);
                        if (atIndex > 0) {
                            int hhmmIndex = atIndex + 3;
                            elipsisIndex = message.indexOf("...", hhmmIndex);
                            if (elipsisIndex > 0) {
                                context.put("corToNewMarker", "cortonewmarker");
                                context.put("corEventtime", message.substring(
                                        hhmmIndex, elipsisIndex));
                            }
                        }
                    }
                    if (untilIndex < 0 || atIndex < 0 || elipsisIndex < 0)
                        throw new VizException("Cannot find * AT line.");
                }

                Calendar cal = oldWarn.getEndTime();
                cal.add(Calendar.MILLISECOND, 1);
                context.put("expire", cal.getTime());
                String originalText = FollowUpUtil.originalText(oldWarn);
                m = FollowUpUtil.vtecPtrn.matcher(originalText);
                int totalSegments = 0;
                while (m.find()) {
                    totalSegments++;
                }
                if (stormTrackState.originalTrack) {
                    context.put("originalText", originalText);
                }
                ArrayList<AffectedAreas> al = null;
                if (totalSegments > 1) {
                    al = FollowUpUtil.canceledAreasFromText(originalText);
                }
                context.put("cancel"
                        + config.getHatchedAreaSource().getVariable(), al);
                context.put("ugclinecan",
                        FollowUpUtil.getUgcLineCanFromText(originalText));
            } else if (selectedAction == WarningAction.EXT) {
                context.put("action", WarningAction.EXT.toString());
                context.put("etn", etn);
            } else if (selectedAction == WarningAction.NEW) {
                context.put("action", WarningAction.NEW.toString());
                context.put("etn", "0000");
            } else if (selectedAction == WarningAction.CON) {
                context.put("start", oldWarn.getIssueTime().getTime());
                context.put("expire", oldWarn.getEndTime().getTime());
                context.put("ugcline", FipsUtil.getUgcLine(areas, oldWarn
                        .getEndTime().getTime(), 15));
                Calendar cancelTime = Calendar.getInstance();
                cancelTime.setTime(simulatedTime);
                cancelTime.setTimeZone(TimeZone.getTimeZone("GMT"));
                cancelTime.add(Calendar.MINUTE, 10);
                String[] tmp = compareGeomsForFollowUp(oldWarn, warnPolygon,
                        areas, cancelTime.getTime(), config);
                if ("".equals(tmp[1]) == false) {
                    Geometry oldWarningArea = warngenLayer
                            .getWarningAreaFromPolygon(
                                    (Polygon) oldWarn.getGeometry(), oldWarn);
                    java.util.List<String> oldGids = new ArrayList<String>(
                            Arrays.asList(GeometryUtil.getGID(oldWarningArea)));
                    java.util.List<String> newGids = new ArrayList<String>(
                            Arrays.asList(GeometryUtil.getGID(warngenLayer
                                    .getWarningArea())));
                    oldGids.removeAll(newGids);
                    Geometry removedAreas = warngenLayer.getWarningAreaForGids(
                            oldGids, oldWarningArea);
                    if (removedAreas.isEmpty() == false) {
                        cancelareas = Area.findAffectedAreas(config,
                                oldWarn.getGeometry(), removedAreas,
                                threeLetterSiteId);
                        for (int i = 0; i < cancelareas.length; i++) {
                            for (int j = 0; j < areas.length; j++) {
                                if (cancelareas[i] != null
                                        && cancelareas[i].getFips().equals(
                                                areas[j].getFips())) {
                                    cancelareas[i] = null;
                                }
                            }
                        }
                        ArrayList<AffectedAreas> al = new ArrayList<AffectedAreas>();
                        for (int i = 0; i < cancelareas.length; i++) {
                            if (cancelareas[i] != null) {
                                al.add(cancelareas[i]);
                            }
                        }
                        context.put("cancel"
                                + config.getHatchedAreaSource().getVariable(),
                                al);

                        // This may not be efficient enough. Is it possible that
                        // a removed intersected county be in the affected
                        // intersected county. Need an example to fully test.
                        Map<String, Object> intersectRemovedAreas = Area
                                .findInsectingAreas(config, warnPolygon,
                                        removedAreas, threeLetterSiteId,
                                        warngenLayer);

                        for (String ia : intersectRemovedAreas.keySet()) {
                            context.put("cancel" + ia,
                                    intersectRemovedAreas.get(ia));
                        }

                        if (al.size() < 1) {
                            tmp[0] = WarningAction.CON.toString();
                        }

                    } else {
                        tmp[0] = WarningAction.CON.toString();
                    }
                }
                context.put("action", tmp[0]);
                context.put("ugclinecan", tmp[1]);
                context.put("etn", etn);
            } else {
                context.put("start", oldWarn.getIssueTime().getTime());
                context.put("expire", oldWarn.getEndTime().getTime());
                context.put("action", action);
                context.put("etn", etn);
            }
            context.put("phenomena", phen);
            context.put("ic", oldWarn.getImmediateCause());
            context.put("productClass", oldWarn.getProductClass());
        } else {
            // NOT a followup product
            context.put("action", WarningAction.NEW.toString());
            context.put("etn", "0000");
            context.put("productClass",
                    CAVEMode.getMode().equals(CAVEMode.OPERATIONAL) ? "O" : "T");
        }
        context.put("oldvtec", context.get("etn")); // Depreciated
        context.put("stationary", stormTrackState.timePoints == null);
        // Store Watches
        try {
            t0 = System.currentTimeMillis();
            WatchUtil watches = getWatches(warngenLayer, config, warnPolygon,
                    fourLetterSiteId, simulatedTime);
            System.out.println("getWatches time: "
                    + (System.currentTimeMillis() - t0));
            if (watches != null) {
                context.put("watches", watches);
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.VERBOSE,
                            "WarnGen cannot populate Active Watches. Check your local config.xml",
                            e);
        }

        long tz0 = System.currentTimeMillis();
        String script = createScript(warngenLayer.getTemplateName() + ".vm",
                context, warngenLayer.getLocalizedSite(),
                FileUtil.join(LocalizationManager.getUserDir(), "logs"));
        System.out.println("velocity time: "
                + (System.currentTimeMillis() - tz0));

        String text = script.toString();
        WarningTextHandler handler = WarningTextHandlerFactory.getHandler(
                selectedAction, text, config.getAutoLockText());
        String handledText = handler.handle(text, areas, cancelareas);

        return handledText;
    }

    private static VelocityEngine ENGINE = new VelocityEngine();

    private static synchronized String createScript(String vmFile,
            VelocityContext context, String site, String logDir)
            throws EdexException {
        StringWriter sw = new StringWriter();
        try {
            Properties p = new Properties();
            p.setProperty(LocalizationResourceLoader.SITE_KEY, site);
            p.setProperty("file.resource.loader.class",
                    LocalizationResourceLoader.class.getName());
            p.setProperty("velocimacro.permissions.allowInline", "true");
            p.setProperty(
                    "velocimacro.permissions.allow.inline.to.replace.global",
                    "true");
            p.setProperty("runtime.log", FileUtil.join(logDir, "velocity.log"));
            ENGINE.init(p);
            context.put("scriptLibrary", "VM_global_library.vm");
            Template template = ENGINE.getTemplate(vmFile,
                    Velocity.ENCODING_DEFAULT);
            template.merge(context, sw);
        } catch (Exception e) {
            throw new EdexException("Error generating from template", e);
        }
        return sw.toString();
    }

    /**
     * This method determines whether a CON followup product is reduced in area
     * - which requires that a corresponding CAN segment be issued.
     * 
     * @param oldWarning
     * @param newGeom
     * @param areas
     * @param endTime
     * @return
     * @throws WarngenException
     */
    private static String[] compareGeomsForFollowUp(
            AbstractWarningRecord oldWarning, Geometry newGeom,
            AffectedAreas[] areas, Date endTime, WarngenConfiguration config)
            throws WarngenException {
        String[] rval = { "", "" };
        if (oldWarning == null) {
            return rval;
        }
        if (GisUtil.equivalent(oldWarning.getGeometry(), newGeom)) {
            rval[0] = "CON";
        } else {
            rval[0] = "CANCON";
            rval[1] = FipsUtil.getDifference(oldWarning.getCountyheader(),
                    FipsUtil.getUgcLine(areas, endTime, 0));
        }
        return rval;
    }

    /**
     * This method populates a WatchUtil object with tornado and severe
     * thunderstorm watches from the active table that are contained by the
     * polygon. Furthermore, watches that have not yet expired (current time <
     * end time) are only included.
     * 
     * @param config
     *            WarnGen template configuration settings
     *            ([template_name_site.xml])
     * @param polygon
     *            The Geometry surrounded by the warning polygon.
     * @param simulatedTime
     * @return
     * @throws Exception
     */
    private static WatchUtil getWatches(WarngenLayer warngenLayer,
            WarngenConfiguration config, Geometry polygon,
            String fourLetterSiteId, Date simulatedTime) throws Exception {
        Validate.isTrue(config.getHatchedAreaSource()
                .getIncludedWatchAreaBuffer() >= 0,
                "IncludedWatchAreaBuffer can not be negative");
        WatchUtil rval = null;
        GetActiveTableRequest req = new GetActiveTableRequest();
        String[] includedWatches = config.getIncludedWatches();

        if (includedWatches != null && includedWatches.length > 0) {
            String phensigList = null;
            for (String includedWatch : includedWatches) {
                if (includedWatch.equalsIgnoreCase("torWatches")) {
                    phensigList = phensigList == null ? "TO.A" : phensigList
                            + ",TO.A";
                } else if (includedWatch.equalsIgnoreCase("svrWatches")) {
                    phensigList = phensigList == null ? "SV.A" : phensigList
                            + ",SV.A";
                }
            }

            req.setPhensigList(phensigList);

            if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
                req.setMode(ActiveTableMode.PRACTICE);
            } else {
                req.setMode(ActiveTableMode.OPERATIONAL);
            }

            req.setSiteID(fourLetterSiteId);
            req.setRequestValidTimes(true);

            long t0 = System.currentTimeMillis();
            GetActiveTableResponse resp = (GetActiveTableResponse) ThriftClient
                    .sendRequest(req);
            long t1 = System.currentTimeMillis();
            java.util.List<ActiveTableRecord> respList = resp.getActiveTable();
            ArrayList<ActiveTableRecord> activeTable = new ArrayList<ActiveTableRecord>(
                    respList.size());
            // Filter out entries representing non-active events.
            for (ActiveTableRecord ar : respList) {
                if ("CAN".equals(ar.getAct()) || "EXP".equals(ar.getAct()))
                    continue;
                if (ar.getEndTime() == null) {
                    statusHandler.handle(Priority.ERROR, String.format(
                            "Watch %s has null end time; not included.",
                            ar.getVtecstr()));
                    continue;
                }
                // From A1 SELSparagraphs.C processWOU
                if (simulatedTime.before(new Date(ar.getStartTime().getTime()
                        .getTime() - 180 * 1000))
                        || simulatedTime.after(ar.getEndTime().getTime()))
                    continue;

                activeTable.add(ar);
            }

            System.out.println("getWatches.getActiveTable time: " + (t1 - t0)
                    + ", found "
                    + (activeTable != null ? activeTable.size() : "0")
                    + " watches");
            boolean val = activeTable != null && !activeTable.isEmpty();
            if (val) {
                // Look for ones with valid geometry

                t0 = System.currentTimeMillis();
                Polygon watchArea = (Polygon) polygon.buffer(milesToKilometer
                        .convert(config.getHatchedAreaSource()
                                .getIncludedWatchAreaBuffer())
                        / KmToDegrees);
                t1 = System.currentTimeMillis();
                System.out.println("getWatches.polygonBuffer time: "
                        + (t1 - t0));

                t0 = System.currentTimeMillis();
                warngenLayer.createGeometryForWatches(watchArea, activeTable);
                t1 = System.currentTimeMillis();
                System.out.println("getWatches.createWatchGeometry time: "
                        + (t1 - t0));

                rval = processATEntries(activeTable, warngenLayer);
            }
        }

        return rval;
    }

    private static class WatchWork {
        public WeatherAdvisoryWatch waw;
        public boolean valid;
        public ArrayList<String> ugcZone = new ArrayList<String>();

        public WatchWork(WeatherAdvisoryWatch waw) {
            this.waw = waw;
        }
    }

    /**
     * Create the list of objects representing active watches that will be
     * passed to the template context.
     * 
     * @param activeTable
     *            List of entries for active watches
     * @param warngenLayer
     * @return
     */
    private static WatchUtil processATEntries(
            List<ActiveTableRecord> activeTable, WarngenLayer warngenLayer) {
        WatchUtil rval = new WatchUtil();
        TreeMap<WeatherAdvisoryWatch, WatchWork> map = new TreeMap<WeatherAdvisoryWatch, TemplateRunner.WatchWork>();

        AreaSourceConfiguration asc = null;
        for (AreaSourceConfiguration a : warngenLayer.getConfiguration()
                .getAreaSources()) {
            if (a.getType() == AreaType.HATCHING) {
                asc = a;
                break;
            }
        }
        if (asc == null) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Cannot process watches: missing HATCHING area source configuration");
            return rval;
        }
        GeospatialData[] geoData = warngenLayer.getGeodataFeatures(asc
                .getAreaSource() + "." + warngenLayer.getLocalizedSite());
        if (geoData == null || geoData.length == 0) {
            statusHandler.handle(Priority.ERROR,
                    "Cannot process watches: cannot get geospatial data");
            return rval;
        }

        // For each watch event, get the end time and list of active zones
        for (ActiveTableRecord ar : activeTable) {
            /*
             * Currently reports all zones in the watch even if a given zone is
             * not in the warning polygon. If the logic is changed to only show
             * the portions of the watch near our warning polygon, perform the
             * isEmpty check here.
             */
            WeatherAdvisoryWatch waw = new WeatherAdvisoryWatch();
            waw.setPhensig(ar.getPhensig());
            try {
                waw.setEventId(Integer.parseInt(ar.getEtn()));
            } catch (RuntimeException e) {
                statusHandler.handle(Priority.ERROR, String.format(
                        "Watch %s has null end time; not included.",
                        ar.getVtecstr()));
                continue;
            }

            WatchWork work = map.get(waw);
            if (work == null) {
                waw.setEndTime(ar.getEndTime().getTime());
                work = new WatchWork(waw);
                map.put(waw, work);
            }
            // TODO: Building geometry just to perform this test is probably
            // inefficient with the post-DR-15430 logic...
            if (!ar.getGeometry().isEmpty())
                work.valid = true;

            /* TODO: Currently adding all zones to the list even if they
             * are not in the CWA.  Validation is currently done in
             * determineAffectedPortions to avoid redundant work.
             */
            work.ugcZone.add(ar.getUgcZone()); 
        }

        for (WatchWork work : map.values()) {
            /*
             * If none of the areas in the watch were neer our warning polygon,
             * do not included it.
             */
            if (!work.valid)
                continue;
            if (determineAffectedPortions(work.ugcZone, asc, geoData, work.waw))
                rval.addWaw(work.waw);
        }

        return rval;
    }

    /**
     * Given the list of counties in a watch, fill out the "portions" part of
     * the given WeatherAdvisoryWatch.  Also checks if the given counties are
     * actually in the CWA.
     * 
     * @param ugcs
     * @param asc
     * @param geoData
     * @param waw
     */
    private static boolean determineAffectedPortions(List<String> ugcs,
            AreaSourceConfiguration asc, GeospatialData[] geoData,
            WeatherAdvisoryWatch waw) {

        // Maps state abbreviation to unique fe_area values 
        HashMap<String, Set<String>> map = new HashMap<String, Set<String>>();

        for (String ugc : ugcs) {
            Map<String, String[]> parsed = FipsUtil.parseCountyHeader(ugc);
            Entry<String, String[]> e = null;

            // Either zero or more than one sates/counties would be wrong
            if (parsed.size() != 1
                    || (e = parsed.entrySet().iterator().next()).getValue().length != 1) {
                statusHandler.handle(Priority.ERROR,
                        "Invalid ugczone in active table entry: " + ugc);
                continue;
            }

            String stateAbbrev = e.getKey();
            String feArea = null;
            try {
                feArea = getFeArea(stateAbbrev, e.getValue()[0], asc,
                        geoData);
            } catch (RuntimeException exc) {
                statusHandler.handle(Priority.ERROR, "Error generating included watches.", exc);
                return false;
            }
            if (feArea == NOT_IN_CWA)
                continue;
            
            Set<String> feAreas = map.get(stateAbbrev);
            if (feAreas == null) {
                feAreas = new HashSet<String>();
                map.put(stateAbbrev, feAreas);
            }
            if (feArea != null)
                feAreas.add(feArea);
        }

        ArrayList<Portion> portions = new ArrayList<Portion>(map.size());
        for (Entry<String, Set<String>> e : map.entrySet()) {
            Portion portion = new Portion();
            try {
                portion.parentRegion = getStateName(e.getKey(), asc, geoData)
                        .toUpperCase();
            } catch (RuntimeException exc) {
                statusHandler.handle(Priority.ERROR, "Error generating included watches.", exc);
                return false;
            }
            portion.partOfParentRegion = Area
                    .converFeAreaToPartList(mungeFeAreas(e.getValue()));
            portions.add(portion);
        }
        waw.setPortions(portions);
        // Set legacy values
        if (portions.size() > 0) {
            waw.setParentRegion(portions.get(0).parentRegion);
            waw.setPartOfParentRegion(portions.get(0).partOfParentRegion);
        }
        
        return true;
    }

    // Based on AWIPS 1 SELSparagraphs.C SELSparagraphs::processWOU().
    private static String mungeFeAreas(Set<String> feAreas) {
        String abrev = "";
        // If eight or more portions, don't qualify area of state
        int m = feAreas.size();
        if (m < 8) {
            String partAbrev = "";
            /*
             * TODO: Unused variables should be removed if we are not going to
             * improve this in A2.
             */
            @SuppressWarnings("unused")
            int nw, nc, ne, wc, cc, ec, sw, sc, se, pa;
            int eee, www, nnn, sss, ee, ww, nn, ss;

            // Identify individual sub areas of this state affected
            nw = nc = ne = wc = cc = ec = sw = sc = se = pa = 0;
            eee = www = nnn = sss = ee = ww = nn = ss = 0;
            for (String part : feAreas) {
                if ("pa".equals(part)) {
                    pa = 1;
                    continue;
                } else if ("nn".equals(part))
                    nnn = nn = 1;
                else if ("ss".equals(part))
                    sss = ss = 1;
                else if ("ee".equals(part))
                    eee = ee = 1;
                else if ("ww".equals(part))
                    www = ww = 1;
                else if ("nw".equals(part))
                    nnn = www = nw = 1;
                else if ("nc".equals(part))
                    nnn = nc = 1;
                else if ("ne".equals(part))
                    nnn = eee = ne = 1;
                else if ("wc".equals(part))
                    www = wc = 1;
                else if ("cc".equals(part)) {
                    cc = 1;
                    continue;
                } else if ("ec".equals(part))
                    eee = ec = 1;
                else if ("sw".equals(part))
                    sss = www = sw = 1;
                else if ("sc".equals(part))
                    sss = sc = 1;
                else if ("se".equals(part))
                    sss = eee = se = 1;
                partAbrev = part;
            }
            // decide how to describe these subareas.
            if (ne > 0 && nw > 0)
                nn = 1;
            if (se > 0 && sw > 0)
                ss = 1;
            if (se > 0 && ne > 0)
                ee = 1;
            if (sw > 0 && nw > 0)
                ww = 1;
            if (nnn > 0 && sss > 0 && eee > 0 && www > 0)
                return abrev;
            if (nn > 0 && ss > 0 || ee > 0 && ww > 0)
                return abrev;
            if (nnn + sss + eee + www == 3) {
                if (www == 0) {
                    abrev = "e";
                } else if (eee == 0) {
                    abrev = "w";
                } else if (nnn == 0) {
                    abrev = "s";
                } else if (sss == 0) {
                    abrev = "n";
                }
                return abrev;
            }
            if (nnn == sss && eee == www || cc == m) {
                abrev = "c";
                return abrev;
            }
            if (pa != 0 && cc == 0) {
                abrev = "pa";
                if (--m <= 0) {
                    return abrev;
                }
            }
            if (m == 1 + cc) {
                abrev += partAbrev + " ";
                return abrev;
            }
            if (nnn != sss) {
                abrev += nnn != 0 ? "n" : "s";
            }
            if (eee != www) {
                abrev += eee != 0 ? "e" : "w";
            }
        }
        return abrev;
    }

    private static String getStateName(String key, AreaSourceConfiguration asc,
            GeospatialData[] geoData) {
        for (GeospatialData g : geoData) {
            if (key.equals((String) g.attributes.get("STATE")))
                return (String) g.parent.attributes.get("NAME");
        }
        return null;
    }
    
    private static String NOT_IN_CWA = new String("NOT_IN_CWA");

    /** Determines if the given UGC is in the CWA and if it is, returns
     * the portion of the CWA.
     * @param stateAbbrev
     * @param ugc
     * @param asc
     * @param geoData
     * @return
     */
    private static String getFeArea(String stateAbbrev, String ugc,
            AreaSourceConfiguration asc, GeospatialData[] geoData) {
        for (GeospatialData g : geoData) {
            if (stateAbbrev.equals((String) g.attributes.get("STATE"))
                    && ((String) g.attributes.get(asc.getFipsField()))
                            .endsWith(ugc))
                return (String) g.attributes.get(asc.getFeAreaField());
        }

        // TODO: Is this the correct way to determine if the county is in the CWA?
        return NOT_IN_CWA;
    }

}
