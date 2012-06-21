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
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
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
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.viz.core.exception.VizException;
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
import com.raytheon.viz.warngen.util.CurrentWarnings;
import com.raytheon.viz.warngen.util.FipsUtil;
import com.raytheon.viz.warngen.util.FollowUpUtil;
import com.raytheon.viz.warngen.util.WarnGenMathTool;
import com.raytheon.viz.warngen.util.WarningTextHandler;
import com.raytheon.viz.warngen.util.WatchUtil;
import com.raytheon.viz.warngen.util.WeatherAdvisoryWatch;
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
 * 
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

        String action = followupData != null ? followupData.getAct() : WarningAction.NEW.toString();
        String phen = followupData != null ? followupData.getPhen() : null;
        String sig = followupData != null ? followupData.getSig() : null;
        String etn = followupData != null ? followupData.getEtn() : null;

        String phenSig = phen + "." + sig;
        WarningAction selectedAction = WarningAction.valueOf(action);
        AffectedAreas[] areas = null;
        AffectedAreas[] cancelareas = null;
        Map<String, Object> intersectAreas = null;
        try {
            t0 = System.currentTimeMillis();
            areas = Area.findAffectedAreas(config, warnPolygon, warningArea,
                    threeLetterSiteId);
            System.out.println("Time to get areas = "
                    + (System.currentTimeMillis() - t0));
            context.put(config.getAreaConfig().getVariable(), areas);

            t0 = System.currentTimeMillis();
            intersectAreas = Area.findInsectingAreas(config, warnPolygon,
                    warningArea, threeLetterSiteId, warngenLayer);
            System.out.println("Time to get intersecting areas = "
                    + (System.currentTimeMillis() - t0));
            for (String ia : intersectAreas.keySet()) {
                context.put(ia, intersectAreas.get(ia));
            }

            if (areas != null && areas.length > 0) {
            	Set<String> timeZones = new HashSet<String>();
                for (AffectedAreas area : areas) {
                    if (area.getTimezone() != null) {
                    	// Handles counties that span two counties
                        String oneLetterTimeZones = area.getTimezone().trim();
                        if (oneLetterTimeZones.length() == 1) {
                            timeZones.add(String.valueOf(oneLetterTimeZones.charAt(0)));
                        } else {
                        	for (int i = 0; i < oneLetterTimeZones.length(); i++) {
                        		String oneLetterTimeZone = String.valueOf(oneLetterTimeZones.charAt(i));
                        		Geometry timezoneGeom = warngenLayer.getTimezoneGeom(oneLetterTimeZone);
                        		if (timezoneGeom != null && GeometryUtil.intersects(warningArea, timezoneGeom)) {
                        		    timeZones.add(oneLetterTimeZone);
                        		}
                        	}
                        }
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
                Wx wx = new Wx(config, stormTrackState,
                        warngenLayer.getStormLocations(stormTrackState),
                        startTime.getTime(), DateUtil.roundDateTo15(endTime)
                                .getTime(), warnPolygon);
                context.put("now", SimulatedTime.getSystemTime().getTime());
                context.put("start", wx.getStartTime());
                context.put(
                        "expire",
                        DateUtil.roundDateTo15(selectedAction == WarningAction.EXT ? endTime
                                : wx.getEndTime()));
                context.put("event", eventTime);
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
                std.setDate(SimulatedTime.getSystemTime().getTime());
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
                Coordinate[] newStormLocs = GisUtil
                        .d2dCoordinates(stormLocs);
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
                context.put("now", SimulatedTime.getSystemTime().getTime());
                context.put("event", eventTime);
                context.put("start", oldWarn.getStartTime().getTime());
                context.put("expire", oldWarn.getEndTime().getTime());
                Calendar canOrExpCal = Calendar.getInstance();
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
                    std.setDate(SimulatedTime.getSystemTime().getTime());
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
                context.put("now", SimulatedTime.getSystemTime().getTime());
                context.put("event", oldWarn.getIssueTime().getTime());
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
                context.put("cancel"+ config.getAreaConfig().getVariable(), al);
                context.put("ugclinecan", FollowUpUtil.getUgcLineCanFromText(originalText));             
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
                Calendar cancelTime = (Calendar) oldWarn.getEndTime().clone();
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
                                + config.getAreaConfig().getVariable(), al);

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
                    fourLetterSiteId);
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
                context, warngenLayer.getLocalizedSite());
        System.out.println("velocity time: "
                + (System.currentTimeMillis() - tz0));

        return WarningTextHandler.handle(script.toString().toUpperCase(),
                areas, cancelareas, selectedAction, 
                WarningAction.valueOf((String) context.get("action")),
                config.getAutoLockText());
    }

    private static VelocityEngine ENGINE = new VelocityEngine();

    private static synchronized String createScript(String vmFile,
            VelocityContext context, String site) throws EdexException {
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
     * @return
     * @throws Exception
     */
    private static WatchUtil getWatches(WarngenLayer warngenLayer,
            WarngenConfiguration config, Geometry polygon,
            String fourLetterSiteId) throws Exception {
        Validate.isTrue(
                config.getAreaConfig().getIncludedWatchAreaBuffer() >= 0,
                "IncludedWatchAreaBuffer can not be negative");
        Polygon watchArea = (Polygon) polygon.buffer(milesToKilometer
                .convert(config.getAreaConfig().getIncludedWatchAreaBuffer())
                / KmToDegrees);
        GetActiveTableRequest req = new GetActiveTableRequest();

        if (config.getIncludedWatches() != null
                && config.getIncludedWatches().length > 0) {
            String phensigList = null;
            for (String includedWatch : config.getIncludedWatches()) {
                if (includedWatch.equalsIgnoreCase("torWatches")) {
                    phensigList = phensigList == null ? "TO.A" : phensigList
                            + ",TO.A";
                } else if (includedWatch.equalsIgnoreCase("svrWatches")) {
                    phensigList = phensigList == null ? "SV.A" : phensigList
                            + ",SV.A";
                }
            }
            req.setPhensigList(phensigList);
        } else {
            return null;
        }

        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            req.setMode(ActiveTableMode.PRACTICE);
        } else {
            req.setMode(ActiveTableMode.OPERATIONAL);
        }

        req.setSiteID(fourLetterSiteId);
        req.setRequestValidTimes(true);

        GetActiveTableResponse resp = (GetActiveTableResponse) ThriftClient
                .sendRequest(req);
        java.util.List<ActiveTableRecord> activeTable = resp.getActiveTable();

        if (activeTable != null && activeTable.isEmpty() == false) {
            warngenLayer.createGeometryForWatches(watchArea, activeTable);
        } else {
            return null;
        }

        SpatialQueryResult[] parentRegionFeatures = null;
        try {
            parentRegionFeatures = SpatialQueryFactory.create().query("States",
                    new String[] { "Name" }, watchArea, null, false,
                    SearchMode.INTERSECTS);
        } catch (Exception e) {
            return null;
        }

        WatchUtil rval = new WatchUtil();
        WeatherAdvisoryWatch watch = null;

        for (ActiveTableRecord atr : activeTable) {
            // For each State in our watchArea...
            for (int j = 0; j < parentRegionFeatures.length; j++) {
                watch = new WeatherAdvisoryWatch();
                watch.setEndTime(atr.getEndTime().getTime());
                watch.setPhensig(atr.getPhensig());

                // Get the intersection of watchArea with State.
                Geometry parentGeom = parentRegionFeatures[j].geometry;

                // If State intersections intersects with out ATR record, add
                // watch
                if (GeometryUtil.intersects(parentGeom, atr.getGeometry())) {
                    watch.setParentRegion(parentRegionFeatures[j].attributes
                            .get("Name").toString());

                    watch.setPartOfParentRegion(GisUtil.asStringList(GisUtil
                            .calculatePortion(parentGeom, atr.getGeometry())));
                    rval.addWaw(watch);
                }
            }
        }

        return rval;
    }

}
