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
import java.io.BufferedReader;
import java.io.File;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.tools.generic.ListTool;

import com.raytheon.uf.common.dataplugin.text.db.MixedCaseProductSupport;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningConstants;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.portions.PortionsUtil;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warngen.WarngenException;
import com.raytheon.viz.warngen.gis.AffectedAreas;
import com.raytheon.viz.warngen.gis.Area;
import com.raytheon.viz.warngen.gis.ClosestPointComparator;
import com.raytheon.viz.warngen.gis.PathCast;
import com.raytheon.viz.warngen.gis.Watch;
import com.raytheon.viz.warngen.gis.WatchUtil;
import com.raytheon.viz.warngen.gis.Wx;
import com.raytheon.viz.warngen.gui.BackupData;
import com.raytheon.viz.warngen.gui.FollowupData;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.raytheon.viz.warngen.gui.WarngenUIState;
import com.raytheon.viz.warngen.text.WarningTextHandler;
import com.raytheon.viz.warngen.text.WarningTextHandlerFactory;
import com.raytheon.viz.warngen.util.AdjustAngle;
import com.raytheon.viz.warngen.util.CurrentWarnings;
import com.raytheon.viz.warngen.util.DateUtil;
import com.raytheon.viz.warngen.util.DurationUtil;
import com.raytheon.viz.warngen.util.FipsUtil;
import com.raytheon.viz.warngen.util.FollowUpUtil;
import com.raytheon.viz.warngen.util.WarnGenMathTool;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.TopologyException;
import org.locationtech.jts.io.WKTReader;

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
 * Nov 26, 2012   15550    Qinglu Lin  For CAN to EXP, added TMLtime to context.
 * Nov 30, 2012   15571    Qinglu Lin  For NEW, assigned simulatedTime to TMLtime; For COR, used stormLocs
 *                                     in oldWarn.
 * Dec 17, 2012   15571    Qinglu Lin  For hydro products, resolved issue caused by calling wkt.read(loc)
 *                                     while loc is null.
 * Jan  8, 2013   15664    Qinglu Lin  Appended selectedAction to handler.handle()'s argument list.
 * Feb 12, 2013   1600     jsanchez    Correctly set the StormTrackData's motion direction for a CAN and EXP.
 * Feb 15, 2013   1607     jsanchez    Added two variables corEventTime and corCreateTime.
 * Feb 15, 2013   15820    Qinglu Lin  Added createOfficeTimezoneMap() and added logic so that localtimezone
 *                                     and secondtimezone can get correct values when warning area covers two time zones.
 * May 10, 2013   1951     rjpeter     Updated ugcZones references
 * May 30, 2013   DR 16237 D. Friedman Fix watch query.
 * Jun 18, 2013   2118     njensen     Only calculate pathcast if it's actually used
 * Aug 19, 2013   2177     jsanchez    Passed PortionsUtil to Area class.
 * Dec  4, 2013   2604     jsanchez    Refactored GisUtil and PortionsUtil.
 * Mar 17, 2014   DR 16309 Qinglu Lin  Updated getWatches(), processATEntries() and determineAffectedPortions(), and
 *                                     added determineAffectedMarinePortions().
 * Apr 28, 2014   3033     jsanchez    Set the site and backup site in Velocity Engine's properties
 * Jul 21, 2014   3419     jsanchez    Refactored WatchUtil.
 * Aug 15, 2014 DR15701 mgamazaychikov Removed static field watchUtil.
 * Aug 28, 2014 ASM #15551 Qinglu Lin  Replaced 1200 PM/1200 AM by NOON/MIDNIGHT, removed days in
 *                                     included tornado/severe thunderstorm watch message.
 * Sep 18, 2014 ASM #15465 Qinglu Lin  For backup, get officeShort and officeLoc from backup WFO's config.xml.
 * May  7, 2015 ASM #17438 D. Friedman Clean up debug and performance logging.
 * May 29, 2015   4440     randerso    Fix resource leak (file not closed)
 * Jul 15, 2015 DR17716 mgamazaychikov Change to Geometry class in total intersection calculations.
 * Oct 21, 2105   5021     randerso    Fix issue with CORs for mixed case
 * Feb  9, 2016 DR18421    D. Friedman Don't call ToolsDataManager.setStormTrackData if there is no storm motion.
 * Feb 17, 2016 DR 17531   Qinglu Lin  Added calStormVelocityAndEventLocation(), updated runTemplate().
 * Mar 10, 2016 5411       randerso    Added productId and mixedCaseEnabled to Velocity context
 * May 25, 2016 DR18789    D. Friedman Extract timezone calculation to method and add short circuit logic.
 * Jul 21, 2016 DR 18159  Qinglu Lin   update runTemplate().
 * Aug 29, 2017  6328      randerso    Fix misspelled method name
 * Oct 31, 2017  6328      randerso    Fix missing CAN segment for partial cancellation
 * Mar 02, 2018  6786      dgilling    Don't allow WMO header time to be
 *                                     different than VTEC start time for some
 *                                     products.
 *
 * </pre>
 *
 * @author njensen
 */

public class TemplateRunner {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TemplateRunner.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("WG:");

    private static final String LOGIN_NAME_KEY = "LOGNAME";

    private static final Pattern BBB_PATTERN = Pattern
            .compile(".*\\sCC([A-Z])");

    private static Map<String, DateFormat> dateFormat;

    static {
        dateFormat = new HashMap<>();
        dateFormat.put("header",
                new SimpleDateFormat("hmm a z EEE MMM d yyyy"));
        dateFormat.put("plain", new SimpleDateFormat("hmm a z EEEE"));
        dateFormat.put("clock", new SimpleDateFormat("hmm a z"));
        dateFormat.put("ymdthmz", new SimpleDateFormat("yyMMdd'T'HHmm'Z'"));
        dateFormat.put("ddhhmm", new SimpleDateFormat("ddHHmm"));
        dateFormat.put("time", new SimpleDateFormat("HHmm"));
    }

    /**
     * Read cwa and timezone info from officeCityTimezone.txt, and put them into
     * map officeCityTimezone.
     *
     * @return officeCityTimezone map
     */
    public static Map<String, String> createOfficeTimezoneMap() {
        Map<String, String> officeCityTimezone = new HashMap<>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String octz = "officeCityTimezone.txt";
        String fileToRetrieve = IPathManager.SEPARATOR
                + WarningConstants.WARNGEN_DIR + IPathManager.SEPARATOR + octz;
        File timezoneFile = pathMgr.getFile(lc, fileToRetrieve);
        String line;
        String[] splitLine;
        try (BufferedReader timezoneReader = Files.newBufferedReader(
                timezoneFile.toPath(), StandardCharsets.UTF_8)) {
            for (line = timezoneReader
                    .readLine(); line != null; line = timezoneReader
                            .readLine()) {
                splitLine = line.trim().split("\\\\");
                officeCityTimezone.put(splitLine[0].trim(),
                        splitLine[1].trim());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "WarnGen Error while processing data in : " + octz, e);
        }
        return officeCityTimezone;
    }

    private static Set<String> determineTimezones(WarngenLayer warngenLayer,
            AffectedAreas[] areas, Geometry warningArea) throws VizException {
        Map<String, Double> intersectSize = new HashMap<>();
        double minSize = 1.0E-3d;
        Set<String> timeZones = new HashSet<>();
        for (AffectedAreas affectedAreas : areas) {
            if (affectedAreas.getTimezone() != null) {
                // Handles counties that span two time zones
                String oneLetterTimeZones = affectedAreas.getTimezone().trim();
                if (oneLetterTimeZones.length() == 1) {
                    timeZones.add(String.valueOf(oneLetterTimeZones.charAt(0)));
                }
            }
        }
        if (timeZones.size() > 1) {
            return timeZones;
        }
        for (AffectedAreas affectedAreas : areas) {
            if (affectedAreas.getTimezone() != null) {
                // Handles counties that span two time zones
                String oneLetterTimeZones = affectedAreas.getTimezone().trim();
                if (oneLetterTimeZones.length() > 1) {
                    // Determine if one letter timezone is going to be
                    // put into timeZones.
                    Geometry[] poly1, poly2;
                    int n1, n2;
                    double size, totalSize;
                    String[] oneLetterTZ = new String[oneLetterTimeZones
                            .length()];
                    for (int i = 0; i < oneLetterTimeZones.length(); i++) {
                        oneLetterTZ[i] = String
                                .valueOf(oneLetterTimeZones.charAt(i));
                        Geometry timezoneGeom = warngenLayer
                                .getTimezoneGeom(oneLetterTZ[i]);
                        long t0 = System.currentTimeMillis();
                        poly1 = null;
                        poly2 = null;
                        n1 = 0;
                        n2 = 0;
                        size = 0.0d;
                        totalSize = 0.0d;
                        if ((timezoneGeom != null) && (warningArea != null)) {
                            if (intersectSize.get(oneLetterTZ[i]) != null) {
                                continue;
                            }
                            poly1 = new Geometry[warningArea
                                    .getNumGeometries()];
                            n1 = warningArea.getNumGeometries();
                            for (int j = 0; j < n1; j++) {
                                poly1[j] = warningArea.getGeometryN(j);
                            }
                            poly2 = new Geometry[timezoneGeom
                                    .getNumGeometries()];
                            n2 = timezoneGeom.getNumGeometries();
                            for (int j = 0; j < n2; j++) {
                                poly2[j] = timezoneGeom.getGeometryN(j);
                            }
                            // Calculate the total size of intersection
                            for (Geometry p1 : poly1) {
                                for (Geometry p2 : poly2) {
                                    try {
                                        size = p1.intersection(p2).getArea();
                                    } catch (TopologyException e) {
                                        statusHandler.handle(Priority.VERBOSE,
                                                "Geometry error calculating the total size of intersection.",
                                                e);
                                    }
                                    if (size > 0.0) {
                                        totalSize += size;
                                    }
                                }
                                if (totalSize > minSize) {
                                    /*
                                     * save time when the size of poly1 or poly2
                                     * is large
                                     */
                                    break;
                                }
                            }
                            intersectSize.put(oneLetterTZ[i], totalSize);
                        } else {
                            throw new VizException(
                                    "Either timezoneGeom or/and warningArea is null. "
                                            + "Timezone cannot be determined.");
                        }
                        perfLog.logDuration("runTemplate size computation",
                                System.currentTimeMillis() - t0);
                        if (totalSize > minSize) {
                            timeZones.add(oneLetterTZ[i]);
                        }
                    }
                    /*
                     * If timeZones has nothing in it when the hatched area is
                     * very small, use the timezone of larger intersection size.
                     */
                    if (timeZones.isEmpty()) {
                        if (intersectSize.size() > 1) {
                            if (intersectSize
                                    .get(oneLetterTZ[0]) > intersectSize
                                            .get(oneLetterTZ[1])) {
                                timeZones.add(oneLetterTZ[0]);
                            } else {
                                timeZones.add(oneLetterTZ[1]);
                            }
                        } else {
                            throw new VizException(
                                    "The size of intersectSize is less than 1, "
                                            + "timezone cannot be determined.");
                        }
                    }
                }
            } else {
                throw new VizException(
                        "Calling to area.getTimezone() returns null.");
            }
        }
        return timeZones;
    }

    /**
     * Executes a warngen template given the polygon from the Warngen Layer and
     * the Storm tracking information from StormTrackDisplay
     *
     * @param warngenLayer
     * @param startTime
     * @param endTime
     * @param selectedBullets
     * @param followupData
     * @param backupData
     * @return the generated product
     * @throws Exception
     */
    public static String runTemplate(WarngenLayer warngenLayer, Date startTime,
            Date endTime, String[] selectedBullets, FollowupData followupData,
            BackupData backupData) throws Exception {
        return runTemplate(warngenLayer,
                SimulatedTime.getSystemTime().getTime(), startTime, endTime,
                selectedBullets, followupData, backupData);
    }

    /**
     * Executes a warngen template given the polygon from the Warngen Layer and
     * the Storm tracking information from StormTrackDisplay
     *
     * @param warngenLayer
     * @param duration
     * @param baseTime
     * @param selectedBullets
     * @param followupData
     * @param backupData
     * @return the generated product
     * @throws Exception
     */
    public static String runTemplate(WarngenLayer warngenLayer, int duration,
            Calendar baseTime, String[] selectedBullets,
            FollowupData followupData, BackupData backupData) throws Exception {
        Calendar currentTime = TimeUtil.newCalendar();
        Calendar endTime = DurationUtil.calcEndTime(
                (baseTime != null) ? baseTime : currentTime, duration);
        return runTemplate(warngenLayer, currentTime.getTime(),
                currentTime.getTime(), endTime.getTime(), selectedBullets,
                followupData, backupData);
    }

    private static String runTemplate(WarngenLayer warngenLayer, Date timeNow,
            Date startTime, Date endTime, String[] selectedBullets,
            FollowupData followupData, BackupData backupData) throws Exception {
        long t0 = System.currentTimeMillis();
        WarngenUIState state = warngenLayer.getWarngenState();
        Geometry warnPolygon = state.getWarningPolygon();
        Geometry warningArea = state.getWarningArea();
        WKTReader wkt = new WKTReader();
        DataTime[] datatimes = warngenLayer.getDescriptor().getFramesInfo()
                .getFrameTimes();
        Date eventTime = (datatimes != null) && (datatimes.length > 0)
                ? datatimes[datatimes.length - 1].getRefTimeAsCalendar()
                        .getTime()
                : startTime;
        WarngenConfiguration config = warngenLayer.getConfiguration();
        StormTrackState stormTrackState = warngenLayer.getStormTrackState();

        VelocityContext context = new VelocityContext();
        context.put("areaSource", config.getGeospatialConfig().getAreaSource());
        context.put("parentAreaSource",
                config.getGeospatialConfig().getParentAreaSource());
        context.put("pointSource",
                config.getGeospatialConfig().getPointSource());

        context.put("user", System.getenv().get(LOGIN_NAME_KEY));

        String threeLetterSiteId = warngenLayer.getLocalizedSite();
        String fourLetterSiteId = SiteMap.getInstance()
                .getSite4LetterId(threeLetterSiteId);

        context.put("vtecOffice", fourLetterSiteId);
        context.put("siteId", threeLetterSiteId);
        context.put("WMOId", "TTAAII");

        /** Convenience tools for the template */
        context.put("timeFormat", dateFormat);
        context.put("list", new ListTool());
        context.put("officeShort",
                warngenLayer.getDialogConfig().getWarngenOfficeShort());
        context.put("officeLoc",
                warngenLayer.getDialogConfig().getWarngenOfficeLoc());

        if (backupData != null) {
            context.remove("officeLoc");
            context.remove("officeShort");
            context.put("officeLoc", warngenLayer.getBackupOfficeLoc());
            context.put("officeShort", warngenLayer.getBackupOfficeShort());
            context.put("backupSite",
                    warngenLayer.getDialogConfig().getWarngenOfficeShort());
        }

        String productId = config.getProductId();
        if (productId == null) {
            statusHandler.warn("Warngen configuration file: "
                    + warngenLayer.getTemplateName() + ".xml"
                    + " does not contain a <productId> tag.");
        }

        String stormType = stormTrackState.displayType == DisplayType.POLY
                ? "line" : "single";
        context.put("stormType", stormType);
        context.put("mathUtil", new WarnGenMathTool());
        context.put("dateUtil", new DateUtil());
        context.put("productId", productId);
        context.put("mixedCaseEnabled",
                MixedCaseProductSupport.isMixedCase(productId));
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
        Area area = new Area(new PortionsUtil(
                LocalizationManager.getInstance().getCurrentSite(),
                warngenLayer.getLocalGridGeometry(),
                warngenLayer.getlocalToLatLon()));
        long wwaMNDTime = 0l;
        try {
            t0 = System.currentTimeMillis();
            areas = area.findAffectedAreas(config, warnPolygon, warningArea,
                    threeLetterSiteId, warngenLayer);
            perfLog.logDuration("runTemplate get areas",
                    System.currentTimeMillis() - t0);
            context.put(config.getHatchedAreaSource().getVariable(), areas);

            t0 = System.currentTimeMillis();
            intersectAreas = area.findIntersectingAreas(config, warnPolygon,
                    warningArea, threeLetterSiteId, warngenLayer);
            perfLog.logDuration("runTemplate get intersecting areas",
                    System.currentTimeMillis() - t0);
            for (Entry<String, Object> entry : intersectAreas.entrySet()) {
                context.put(entry.getKey(), entry.getValue());
            }

            if ((areas != null) && (areas.length > 0)) {
                Set<String> timeZones = determineTimezones(warngenLayer, areas,
                        warningArea);

                Map<String, String> officeCityTimezone = createOfficeTimezoneMap();
                String cityTimezone = null;
                if (officeCityTimezone != null) {
                    cityTimezone = officeCityTimezone
                            .get(warngenLayer.getLocalizedSite());
                }
                Iterator<String> iterator = timeZones.iterator();
                if ((timeZones.size() > 1) && (cityTimezone != null)) {
                    String timezone;
                    while (iterator.hasNext()) {
                        timezone = iterator.next();
                        if (timezone.equals(cityTimezone)
                                && (context.get("localtimezone") == null)) {
                            context.put("localtimezone", timezone);
                        } else if (context.get("secondtimezone") == null) {
                            context.put("secondtimezone", timezone);
                        }
                    }
                } else {
                    while (iterator.hasNext()) {
                        if (context.get("localtimezone") == null) {
                            context.put("localtimezone", iterator.next());
                        } else if (context.get("secondtimezone") == null) {
                            context.put("secondtimezone", iterator.next());
                        }
                    }
                }
            }

            wx = new Wx(config, stormTrackState,
                    warngenLayer.getStormLocations(stormTrackState),
                    startTime.getTime(),
                    DateUtil.roundDateTo15(endTime).getTime(), warnPolygon);

            // duration: convert millisecond to minute
            long duration = (wx.getEndTime().getTime()
                    - wx.getStartTime().getTime()) / (1000 * 60);
            context.put("duration", duration);

            context.put("event", eventTime);

            StormTrackData std = ToolsDataManager.getInstance()
                    .getStormTrackData();
            std.setDate(timeNow);

            // CAN and EXP products follow different rules as followups
            if (!((selectedAction == WarningAction.CAN)
                    || (selectedAction == WarningAction.EXP))) {
                if (selectedAction == WarningAction.COR) {
                    wwaMNDTime = wx.getStartTime().getTime();
                } else {
                    context.put("now", timeNow);
                    context.put("start", wx.getStartTime());
                }
                context.put("expire",
                        DateUtil.roundDateTo15(
                                selectedAction == WarningAction.EXT ? endTime
                                        : wx.getEndTime()));

                if (selectedAction == WarningAction.COR) {
                    context.put("TMLtime", eventTime);
                } else {
                    context.put("TMLtime", timeNow);
                }
                context.put("ugcline",
                        FipsUtil.getUgcLine(areas, wx.getEndTime(), 15));
                context.put("areaPoly", GisUtil.convertCoords(
                        warngenLayer.getPolygon().getCoordinates()));

                Map<String, Object> points = wx
                        .getClosestPoints(threeLetterSiteId);
                for (Entry<String, Object> entry : points.entrySet()) {
                    context.put(entry.getKey(), entry.getValue());
                }

                boolean hasPathCast = false;
                for (String s : selectedBullets) {
                    if (s.indexOf("pathcast") > -1) {
                        hasPathCast = true;
                        break;
                    }
                }
                if (hasPathCast) {
                    PathCast[] pathCast = wx.pathcast(threeLetterSiteId);
                    context.put(config.getPathcastConfig().getVariable(),
                            pathCast);

                    if (pathCast == null) {
                        statusHandler.handle(Priority.PROBLEM,
                                "WarnGen critical error: No PathCast Information");
                    }
                }

                calStormVelocityAndEventLocation(std, timeNow, wx,
                        context, warngenLayer, stormTrackState, selectedAction,
                        wkt, threeLetterSiteId, etn, phenSig);

                if (std.getMotionSpeed() > 0) {
                    t0 = System.currentTimeMillis();
                    ToolsDataManager.getInstance().setStormTrackData(std);
                    perfLog.logDuration("Save storm track data",
                            System.currentTimeMillis() - t0);
                }
            } else {
                // Retrieve the old Warning
                // Example: s[0-5] = T.CON-KLWX.SV.W.0123
                AbstractWarningRecord oldWarn = CurrentWarnings
                        .getInstance(threeLetterSiteId)
                        .getNewestByTracking(etn, phenSig);
                context.put("now", timeNow);
                context.put("start", oldWarn.getStartTime().getTime());
                context.put("expire", oldWarn.getEndTime().getTime());
                Calendar canOrExpCal = Calendar.getInstance();
                canOrExpCal.setTimeZone(TimeZone.getTimeZone("GMT"));
                canOrExpCal.add(Calendar.MINUTE, 10);
                canOrExpCal.add(Calendar.MILLISECOND, 1);
                context.put("ugcline", FipsUtil.getUgcLine(
                        oldWarn.getUgcZones(), canOrExpCal.getTime(), 0));
                String oldGeom = oldWarn.getGeometry().toString();
                context.put("areaPoly", GisUtil
                        .convertCoords(wkt.read(oldGeom).getCoordinates()));
                // If there is no storm track
                if (oldWarn.getLoc() != null) {
                    // Convert to Point2D representation as Velocity requires
                    // getX() and getY() methods which Coordinate does not have
                    if (selectedAction == WarningAction.CAN) {
                        context.put("TMLtime", eventTime);
                        Point2D.Double[] coords;
                        Coordinate[] locs;
                        locs = warngenLayer.getStormLocations(stormTrackState);
                        locs = GisUtil.d2dCoordinates(locs);
                        coords = new Point2D.Double[locs.length];
                        for (int i = 0; i < locs.length; i++) {
                            coords[i] = new Point2D.Double(locs[i].x,
                                    locs[i].y);
                        }
                        context.put("eventLocation", coords);
                        context.put("movementDirection", oldWarn.getMotdir());
                        context.put("movementInKnots", oldWarn.getMotspd());

                        // StormTrackData motion direction is between -180/180,
                        // whereas a WarningRecord motion direction is between
                        // -360/360
                        double motionDirection = AdjustAngle
                                .to180Degrees(oldWarn.getMotdir() - 180);
                        std.setMotionDirection(motionDirection);
                        std.setMotionSpeed(oldWarn.getMotspd());
                    } else {
                        context.put("TMLtime", timeNow);
                        wx = new Wx(config, stormTrackState,
                                warngenLayer.getStormLocations(stormTrackState),
                                startTime.getTime(),
                                DateUtil.roundDateTo15(endTime).getTime(),
                                warnPolygon);
                        calStormVelocityAndEventLocation(std, timeNow, wx,
                                context, warngenLayer, stormTrackState,
                                selectedAction, wkt, threeLetterSiteId, etn,
                                phenSig);
                    }
                    if (std.getMotionSpeed() > 0) {
                        t0 = System.currentTimeMillis();
                        ToolsDataManager.getInstance().setStormTrackData(std);
                        perfLog.logDuration("Save storm track data",
                                System.currentTimeMillis() - t0);
                    }
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
        if ((etn != null) && (etn.length() > 0)) {
            AbstractWarningRecord oldWarn = null;

            // COR product - What are we correcting?
            boolean allowsNewProduct = false;
            for (String s : config.getFollowUps()) {
                if ("NEW".equals(s)) {
                    allowsNewProduct = true;
                }
            }

            CurrentWarnings cw = CurrentWarnings.getInstance(threeLetterSiteId);

            if ((selectedAction == WarningAction.COR) && !allowsNewProduct) {
                oldWarn = cw.getFollowUpByTracking(etn, phenSig,
                        new WarningAction[] { WarningAction.CON,
                                WarningAction.COR });
            } else {
                oldWarn = cw.getNewestByTracking(etn, phenSig);
            }

            // Hydro product
            if ((oldWarn.getFloodSeverity() != null)
                    && (oldWarn.getFloodSeverity().length() >= 1)) {
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
                if ("NEW".equals(oldWarn.getAct())) {
                    context.put("now", new Date(wwaMNDTime));
                    // original warning's 'now' time used in MND header
                    context.put("corCreateTime", new Date(wwaMNDTime));
                } else {
                    context.put("now", timeNow);
                }
                context.put("event", oldWarn.getIssueTime().getTime());
                // original warning's 'event' time, which should match the storm
                // track
                context.put("corEventTime", eventTime);

                String message = oldWarn.getRawmessage().toUpperCase();
                if (!stormTrackState.originalTrack) {
                    context.put("TMLtime", oldWarn.getStartTime().getTime());
                } else {
                    int hour = 0;
                    int minute = 0;
                    int tmlIndex = message.indexOf("TIME...MOT...LOC");
                    int zIndex = -1;
                    if (tmlIndex > 0) {
                        zIndex = message.indexOf('Z', tmlIndex);
                        if (zIndex > 0) {
                            int startIndex = tmlIndex + 16 + 1;
                            String tmlTime = null;
                            tmlTime = message.substring(startIndex,
                                    startIndex + 4);
                            if (tmlTime.length() == 4) {
                                hour = Integer
                                        .parseInt(tmlTime.substring(0, 2));
                                minute = Integer
                                        .parseInt(tmlTime.substring(2, 4));
                            } else if (tmlTime.length() == 3) {
                                hour = Integer
                                        .parseInt(tmlTime.substring(0, 1));
                                minute = Integer
                                        .parseInt(tmlTime.substring(1, 3));
                            } else {
                                throw new VizException(
                                        "The length of hour and minute for TML time is neither 3 nor 4.");
                            }
                            Calendar c = Calendar
                                    .getInstance(TimeZone.getTimeZone("GMT"));
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
                        context.put("TMLtime",
                                oldWarn.getIssueTime().getTime());
                    }
                }

                // corEventtime for "COR to NEW", not for
                // "COR to CON, CAN, or CANCON"
                if ("NEW".equals(oldWarn.getAct())) {
                    int untilIndex = message.indexOf("UNTIL");
                    int atIndex = -1;
                    int hhmmEnd = -1;
                    if (untilIndex > 0) {
                        atIndex = message.indexOf("AT", untilIndex);
                        if (atIndex > 0) {
                            int hhmmStart = atIndex + 3;
                            hhmmEnd = message.indexOf(", ", hhmmStart);
                            if (hhmmEnd < 0) {
                                // check for ellipsis
                                hhmmEnd = message.indexOf("...", hhmmStart);
                            } else {
                                if (hhmmEnd > 0) {
                                    context.put("corToNewMarker",
                                            "cortonewmarker");
                                    context.put("corEventtime", message
                                            .substring(hhmmStart, hhmmEnd));
                                }
                            }
                        }
                    }
                    if ((untilIndex < 0) || (atIndex < 0) || (hhmmEnd < 0)) {
                        throw new VizException("Cannot find * At line.");
                    }
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
                context.put(
                        "cancel" + config.getHatchedAreaSource().getVariable(),
                        al);
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
                context.put("ugcline", FipsUtil.getUgcLine(areas,
                        oldWarn.getEndTime().getTime(), 15));
                Calendar cancelTime = Calendar.getInstance();
                cancelTime.setTime(timeNow);
                cancelTime.setTimeZone(TimeZone.getTimeZone("GMT"));
                cancelTime.add(Calendar.MINUTE, 10);
                String[] tmp = compareGeomsForFollowUp(oldWarn, warnPolygon,
                        areas, cancelTime.getTime(), config);
                if (!tmp[1].isEmpty()) {
                    Geometry oldWarningArea = warngenLayer
                            .getWarningAreaFromPolygon(
                                    (Polygon) oldWarn.getGeometry(), oldWarn);
                    java.util.List<String> oldGids = new ArrayList<>(
                            Arrays.asList(GeometryUtil.getGID(oldWarningArea)));
                    java.util.List<String> newGids = new ArrayList<>(
                            Arrays.asList(GeometryUtil
                                    .getGID(warngenLayer.getWarningArea())));
                    oldGids.removeAll(newGids);
                    Geometry removedAreas = warngenLayer
                            .getWarningAreaForGids(oldGids, oldWarningArea);
                    if (!removedAreas.isEmpty()) {
                        cancelareas = area.findAffectedAreas(config,
                                oldWarn.getGeometry(), removedAreas,
                                threeLetterSiteId, warngenLayer);
                        for (int i = 0; i < cancelareas.length; i++) {
                            for (AffectedAreas affectedAreas : areas) {
                                if ((cancelareas[i] != null)
                                        && cancelareas[i].getFips().equals(
                                                affectedAreas.getFips())) {
                                    cancelareas[i] = null;
                                }
                            }
                        }
                        ArrayList<AffectedAreas> al = new ArrayList<>();
                        for (AffectedAreas cancelarea : cancelareas) {
                            if (cancelarea != null) {
                                al.add(cancelarea);
                            }
                        }
                        context.put("cancel"
                                + config.getHatchedAreaSource().getVariable(),
                                al);

                        // This may not be efficient enough. Is it possible that
                        // a removed intersected county be in the affected
                        // intersected county. Need an example to fully test.
                        Map<String, Object> intersectRemovedAreas = area
                                .findIntersectingAreas(config, warnPolygon,
                                        removedAreas, threeLetterSiteId,
                                        warngenLayer);

                        for (Entry<String, Object> entry : intersectRemovedAreas
                                .entrySet()) {
                            context.put("cancel" + entry.getKey(),
                                    entry.getValue());
                        }

                        if (al.isEmpty()) {
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
                    CAVEMode.getMode().equals(CAVEMode.OPERATIONAL) ? "O"
                            : "T");
        }
        context.put("oldvtec", context.get("etn")); // Depreciated
        context.put("stationary", stormTrackState.timePoints == null);
        // Store Watches
        try {
            t0 = System.currentTimeMillis();
            WatchUtil watchUtil = new WatchUtil(warngenLayer);
            List<Watch> watches = watchUtil.getWatches(config, warnPolygon,
                    timeNow);
            perfLog.logDuration("getWatches", System.currentTimeMillis() - t0);
            if (watches != null && !watches.isEmpty()) {
                context.put("watches", watches);
            }
        } catch (Exception e) {
            e.printStackTrace();
            statusHandler.handle(Priority.VERBOSE,
                    "WarnGen cannot populate Active Watches. Check your local config.xml",
                    e);
        }

        long tz0 = System.currentTimeMillis();
        String text = createScript(warngenLayer.getTemplateName() + ".vm",
                context);
        perfLog.logDuration("velocity", System.currentTimeMillis() - tz0);

        WarningTextHandler handler = WarningTextHandlerFactory
                .getHandler(selectedAction, text, config.getAutoLockText());
        String handledText = handler.handle(text, areas, cancelareas,
                selectedAction);

        return handledText;
    }

    private static VelocityEngine ENGINE;

    /**
     * @param issuingSite
     */
    public static void initialize(String issuingSite) {
        synchronized (TemplateRunner.class) {
            ENGINE = new VelocityEngine();
            Properties p = new Properties();
            p.setProperty("file.resource.loader.class",
                    LocalizationResourceLoader.class.getName());
            p.setProperty("runtime.log",
                    FileUtil.join(FileUtil
                            .join(LocalizationManager.getUserDir(), "logs"),
                            "velocity.log"));
            p.setProperty("velocimacro.permissions.allowInline", "true");
            p.setProperty(
                    "velocimacro.permissions.allow.inline.to.replace.global",
                    "true");

            String site = LocalizationManager.getInstance().getCurrentSite();
            p.setProperty(LocalizationResourceLoader.PROPERTY_SITE, site);

            if (!issuingSite.equalsIgnoreCase(site)) {
                p.setProperty(LocalizationResourceLoader.PROPERTY_BACKUP,
                        issuingSite);
            }

            ENGINE.init(p);
        }
    }

    private static String createScript(String vmFile, VelocityContext context)
            throws VizException {
        synchronized (TemplateRunner.class) {
            StringWriter sw = new StringWriter();
            try {
                // Update site for ENGINE
                context.put("scriptLibrary", "VM_global_library.vm");
                Template template = ENGINE.getTemplate(vmFile,
                        Velocity.ENCODING_DEFAULT);
                template.merge(context, sw);
            } catch (Exception e) {
                throw new VizException("Error generating from template", e);
            }
            return sw.toString();
        }
    }

    /**
     * This method determines whether a CON followup product is reduced in area
     * - which requires that a corresponding CAN segment be issued.
     *
     * @param oldWarning
     * @param newGeom
     * @param areas
     * @param endTime
     * @param config
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

    /*
     * Calculate storm speed, direction, and event coordinates, and add
     * name/value pairs to context.
     */
    private static void calStormVelocityAndEventLocation(StormTrackData std,
            Date simulatedTime, Wx wx, VelocityContext context,
            WarngenLayer warngenLayer, StormTrackState stormTrackState,
            WarningAction selectedAction, WKTReader wkt,
            String threeLetterSiteId, String etn, String phenSig)
            throws Exception {
        std.setMotionDirection((int) wx.getMovementDirection());
        std.setMotionSpeed((int) Math.round(wx.getMovementSpeed("kn")));

        context.put("movementSpeed", wx.getMovementSpeed());
        context.put("movementInKnots", wx.getMovementSpeed("kn"));
        double movementDirectionRounded = wx.getMovementDirectionRounded()
                + 180;
        while (movementDirectionRounded >= 360) {
            movementDirectionRounded -= 360;
        }
        context.put("movementDirectionRounded", movementDirectionRounded);
        double motionDirection = std.getMotionDirection() + 180;
        while (motionDirection >= 360) {
            motionDirection -= 360;
        }
        context.put("movementDirection", motionDirection);
        Coordinate[] stormLocs = warngenLayer
                .getStormLocations(stormTrackState);
        // Convert to Point2D representation as Velocity requires
        // getX() and getY() methods which Coordinate does not have
        if (selectedAction == WarningAction.COR) {
            AbstractWarningRecord oldWarn = CurrentWarnings
                    .getInstance(threeLetterSiteId)
                    .getNewestByTracking(etn, phenSig);
            String loc = oldWarn.getLoc();
            if (loc != null) {
                Geometry locGeom = wkt.read(loc);
                stormLocs = locGeom.getCoordinates();
            }
        } else {
            stormLocs = GisUtil.d2dCoordinates(stormLocs);
        }
        Point2D.Double[] coords = new Point2D.Double[stormLocs.length];
        for (int i = 0; i < stormLocs.length; i++) {
            coords[i] = new Point2D.Double(stormLocs[i].x, stormLocs[i].y);
        }
        context.put("eventLocation", coords);
    }

}
