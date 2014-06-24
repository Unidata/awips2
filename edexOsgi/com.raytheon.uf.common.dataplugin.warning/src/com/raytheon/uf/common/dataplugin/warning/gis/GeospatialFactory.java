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
package com.raytheon.uf.common.dataplugin.warning.gis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.warning.WarningConstants;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Retrieves geospatial data from disk if present, otherwise forwards request to
 * maps database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            rjpeter     Initial creation
 * Mar 29, 2012  #14691    Qinglu Lin  Added returned value of getFeAreaField() of 
 *                                     AreaSourceConfiguration to areaFields List.
 * Apr 11, 2012  #14691    Qinglu Lin  For marine warnings, getFeAreaField() returns null.
 *                                     So, do not add the returned value of getFeAreaField() 
 *                                     to areaFields.
 * Jan  9, 2013   15600    Qinglu Lin  Execute "timezones = myTimeZones;" even if timezones != null.
 * Oct 22, 2013   2361     njensen     Use JAXBManager for XML
 * Jun 17, 2014  DR 17390  Qinglu Lin  Updated getMetaDataMap() for lonField and latField.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GeospatialFactory {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeospatialFactory.class);

    public static final String GEO_DIR = "warngen/geoms/";

    public static final String METADATA_FILE = "warngen/geomMetaData.xml";

    private static GeospatialData[] timezones;

    private static SingleTypeJAXBManager<GeospatialTimeSet> jaxb = SingleTypeJAXBManager
            .createWithoutException(GeospatialTimeSet.class);

    public static GeospatialData[] getGeoSpatialList(String site,
            GeospatialMetadata metaData) throws SpatialException {
        Map<GeospatialMetadata, GeospatialTime> lastRunTimeMap = loadLastRunGeoTimeSet(site);
        GeospatialTime lastRunTime = lastRunTimeMap.get(metaData);
        GeospatialDataSet dataSet = null;
        boolean generate = true;
        if (lastRunTime != null) {
            System.out.println("Loading areas from disk");
            // load from disk
            try {
                long t0 = System.currentTimeMillis();
                dataSet = loadAreaGeoData(site, lastRunTime);
                System.out.println("Loading areas from disk took "
                        + (System.currentTimeMillis() - t0) + "ms");
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "Failed to load area geometry files from disk", e);
            }

            generate = dataSet == null;
        }

        if (generate) {
            // send request to server
            GenerateGeospatialDataRequest request = new GenerateGeospatialDataRequest();
            request.setMetaData(metaData);
            request.setSite(site);
            try {
                dataSet = (GeospatialDataSet) RequestRouter.route(request);
            } catch (Exception e) {
                throw new SpatialException(
                        "Server failed to generate area geometry files.", e);
            }
        }

        GeospatialData[] areas = dataSet.getAreas();
        GeospatialData[] parentAreas = dataSet.getParentAreas();
        GeospatialData[] myTimeZones = dataSet.getTimezones();
        if (myTimeZones != null && myTimeZones.length > 0) {
            timezones = myTimeZones;

            for (GeospatialData tz : myTimeZones) {
                tz.prepGeom = PreparedGeometryFactory.prepare(tz.geometry);
            }
        }

        // process parent regions
        if (parentAreas != null) {
            Map<String, GeospatialData> parentMap = new HashMap<String, GeospatialData>(
                    (int) (parentAreas.length * 1.25) + 1);
            String areaNotationField = metaData.getAreaNotationField();

            for (GeospatialData pArea : parentAreas) {
                parentMap
                        .put(String.valueOf(pArea.attributes
                                .get(areaNotationField)), pArea);
            }

            for (GeospatialData data : areas) {
                GeospatialData parentArea = parentMap.get(String
                        .valueOf(data.attributes.get(areaNotationField)));
                data.parent = parentArea;
            }
        }

        // Prepare the geometries
        for (GeospatialData data : areas) {
            data.prepGeom = PreparedGeometryFactory.prepare(data.geometry);
        }

        return areas;
    }

    public static GeospatialData[] getTimezones() {
        return timezones;
    }

    /**
     * 
     * @param site
     * @return
     */
    public static Map<GeospatialMetadata, GeospatialTime> loadLastRunGeoTimeSet(
            String site) {
        Map<GeospatialMetadata, GeospatialTime> rval = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        context.setContextName(site);

        LocalizationFile lf = pathMgr.getLocalizationFile(context,
                METADATA_FILE);
        if (lf.exists()) {
            try {
                rval = jaxb.unmarshalFromXmlFile(lf.getFile()).getDataAsMap();
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.WARN,
                                "Error occurred deserializing geometry metadata.  Deleting metadata and recreating.",
                                e);

                try {
                    lf.delete();
                } catch (Exception e1) {
                    statusHandler.handle(Priority.WARN,
                            "Error occurred deleting geometry metadata.", e1);
                }
                rval = new HashMap<GeospatialMetadata, GeospatialTime>();
            }
        } else {
            rval = new HashMap<GeospatialMetadata, GeospatialTime>();
        }

        return rval;
    }

    public static Map<String, GeospatialMetadata> getMetaDataMap(
            WarngenConfiguration template) {
        GeospatialConfiguration geoConfig = template.getGeospatialConfig();
        Map<String, GeospatialMetadata> rval = new HashMap<String, GeospatialMetadata>();
        AreaSourceConfiguration[] ascs = template.getAreaSources();

        for (AreaSourceConfiguration asc : ascs) {
            List<String> areaFields = new ArrayList<String>();
            String feAreaField = asc.getFeAreaField();
            String timeZoneField = asc.getTimeZoneField();
            areaFields.add(WarningConstants.GID);
            areaFields.add(asc.getAreaField());
            if (feAreaField != null) {
                areaFields.add(feAreaField);
            }

            areaFields.add("LON");
            areaFields.add("LAT");

            if (timeZoneField != null) {
                areaFields.add(timeZoneField);
            }
            areaFields.add(asc.getFipsField());
            areaFields.add(asc.getAreaNotationField());

            GeospatialMetadata gmd = new GeospatialMetadata();
            gmd.setAreaSource(asc.getAreaSource());
            gmd.setFipsField(asc.getFipsField());
            gmd.setAreaFields(areaFields);
            gmd.setTimeZoneSource(geoConfig.getTimezoneSource());
            gmd.setTimeZoneField(geoConfig.getTimezoneField());
            gmd.setParentAreaSource(geoConfig.getParentAreaSource());
            gmd.setAreaNotationField(asc.getAreaNotationField());
            gmd.setParentAreaField(asc.getParentAreaField());

            rval.put(asc.getAreaSource(), gmd);
        }

        return rval;
    }

    public static GeospatialDataSet loadAreaGeoData(String site,
            GeospatialTime curTime) throws SerializationException,
            LocalizationException {
        String fileName = curTime.getFileName();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        context.setContextName(site);

        LocalizationFile lf = pathMgr.getLocalizationFile(context, GEO_DIR
                + fileName);

        if (lf.exists()) {
            byte[] data = lf.read();
            return SerializationUtil.transformFromThrift(
                    GeospatialDataSet.class, data);
        } else {
            System.out.println("Attempted to load: " + lf.getName()
                    + " for site " + site + ", but file does not exist.");
        }

        return null;
    }

}
