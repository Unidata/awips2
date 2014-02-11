package com.raytheon.edex.plugin.warning.gis;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.warning.config.DialogConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialFactory;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialMetadata;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialTime;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Compares current time in the database against the time of last run
 * generated geometry and if they differ regenerates the files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2014  16090  mgamazaychikov Initial creation
 * </pre>
 * 
 * @author mgamazaychikov
 * @version 1.0
 */

public class GeospatialDataUpdater {
    private final static IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeospatialDataUpdater.class);
    private static final String UPDATER_ENDPOINT = "geospatialUpdateNotify";
    private static Log logger = LogFactory.getLog(Util.class);

    private static Set<GeospatialMetadata>  metaDataSet = null;
    private static Map<GeospatialMetadata, GeospatialTime> map = null;
    
    public static void runCheckUpdate() throws SpatialException {
        StringBuilder sb = new StringBuilder();
        if (metaDataSet == null){
            runInit();
        }
        GeospatialTime curTime = null;
        GeospatialTime lastRunTime = null;
        boolean generate = false;
        sb.append("GeospatialDataUpdater: ");
        for (GeospatialMetadata md : metaDataSet) {
            lastRunTime = map.get(md);
            try {
                curTime = GeospatialDataGenerator.queryForCurrentTimes(md);
            } catch (Exception e) {
                throw new SpatialException(
                        "Unable to look up database version times.",
                        e);
            }
            if (!curTime.equals(lastRunTime)) {
                generate = true;
                break;
            }
            else {
                generate = false;
            }
        }
        if (generate){
            sb.append("Geometry database time differs from current geometry metadata time: regenerating geometries");
        }
        else {
            return;
        }
        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler.info(sb.toString());
        }
        GeospatialDataGenerator.generateUniqueGeospatialMetadataGeometries();

        String updatedTimeStamp = getTimeStamp(curTime, lastRunTime);
        try {
            EDEXUtil.getMessageProducer().sendAsync(UPDATER_ENDPOINT, updatedTimeStamp);
        } catch (EdexException e) {
            logger.error("Could not send message to alarm/alert", e);
        }
        metaDataSet = null;
    }

    private static String getTimeStamp(GeospatialTime curTime,
            GeospatialTime lastRunTime) {
        long tmStampMs = 0;
        if (curTime.getAreaSourceTime() != lastRunTime.getAreaSourceTime()) {
            tmStampMs = curTime.getAreaSourceTime();
        } else if (curTime.getParentSourceTime() != lastRunTime.getParentSourceTime()) {
            tmStampMs = curTime.getParentSourceTime();
        } else if (curTime.getTimeZoneSourceTime() != lastRunTime.getTimeZoneSourceTime()) {
            tmStampMs = curTime.getTimeZoneSourceTime();
        }

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.setTimeInMillis(tmStampMs);
        return sdf.format(calendar.getTime());
    }

    private static void runInit() {
        String mySite = SiteUtil.getSite();
        DialogConfiguration dialogConfig = null;

        try {
            dialogConfig = DialogConfiguration.loadDialogConfig(mySite);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error loading warngen config.xml", e);
            return;
        }
        List<String> sites = GeospatialDataGenerator.getBackupSites(dialogConfig);
        sites.add(0, mySite);
        List<String> templates = GeospatialDataGenerator.getTemplates(dialogConfig);
        metaDataSet = GeospatialDataGenerator.getMetaDataSet(sites, templates);
        map = GeospatialFactory.loadLastRunGeoTimeSet(mySite);
        StringBuilder sb = new StringBuilder();
        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            sb.append("GeospatialDataUpdater has been re-inited");
            statusHandler.info(sb.toString());
        }
    }

}
