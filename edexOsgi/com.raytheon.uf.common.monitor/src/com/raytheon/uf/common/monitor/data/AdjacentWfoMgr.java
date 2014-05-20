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
package com.raytheon.uf.common.monitor.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.site.xml.AdjacentWfoXML;
import com.raytheon.uf.common.site.xml.CwaXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.StringUtil;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Data Access Manager for the allAdjacentWOFs.xml file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2009            mpduff      Initial creation
 * Jul 24, 2013   2219     mpduff      Improve error handling.
 * Oct 02, 2013   2361     njensen     Use JAXBManager for XML
 * May 23, 2014   3086     skorolev    Cleaned code.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AdjacentWfoMgr {

    /** Path to Adjacent WFO XML. */
    private static final String fileName = "allAdjacentWFOs.xml";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AdjacentWfoMgr.class);

    private static final SingleTypeJAXBManager<AdjacentWfoXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(AdjacentWfoXML.class);

    /** Configuration Adjacent WFO XML. */
    private AdjacentWfoXML adjXML = null;

    /** Adjacent zones */
    private List<String> adjZones = null;

    /** Current site */
    private final String currentSite;

    /** List of adjacent areas */
    private List<String> idList = null;

    /** Adjacent area geometry */
    private Geometry geoAdjAreas = null;

    /**
     * Constructor.
     * 
     * @param currentSite
     *            The current site
     */
    public AdjacentWfoMgr(String currentSite) {
        statusHandler.debug("****AdjacentWfoMgr instantiated with "
                + currentSite);
        this.currentSite = currentSite;
        readAdjXml();
    }

    /**
     * Read the XML adjacent data for the current XML file name.
     */
    private void readAdjXml() {

        try {
            adjXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();

            String path = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.BASE), fileName)
                    .getAbsolutePath();

            statusHandler.debug("**** path = " + path);

            adjXML = jaxb.unmarshalFromXmlFile(path);

            ArrayList<CwaXML> list = adjXML.getAreaIds();
            for (CwaXML cx : list) {
                if (cx.getId().equals(currentSite)) {
                    idList = cx.getAdjIdList();

                    // Get the areas for each adjacent CWA
                    adjZones = MonitorAreaUtils.getAdjacentZones(idList
                            .toArray(new String[idList.size()]));
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error setting up adjacent WFO data", e);
        }
    }

    /**
     * Gets Adjacent Wfo XML.
     * 
     * @return the adjXML
     */
    public AdjacentWfoXML getAdjXML() {
        return adjXML;
    }

    /**
     * Sets Adjacent Wfo XML.
     * 
     * @param adjXML
     *            the adjXML to set
     */
    public void setAdjXML(AdjacentWfoXML adjXML) {
        this.adjXML = adjXML;
    }

    /**
     * Gets Adjacent Zones.
     * 
     * @return the adjZones
     */
    public ArrayList<String> getAdjZones() {
        ArrayList<String> copiedList = new ArrayList<String>();

        for (String str : adjZones) {
            copiedList.add(str);
        }

        return copiedList;
    }

    /**
     * Gets list of adjacent areas.
     * 
     * @return List of adjacent areas
     */
    public List<String> getAdjIdList() {
        return idList;
    }

    /**
     * Gets geometry of all adjacent CWAs
     * 
     * @param wfo
     *            The WFO
     * @return The adjacent Geometry
     * @throws SpatialException
     *             if problem with wfo geometry
     */
    public static Geometry getAdjacentAreas(String wfo) throws SpatialException {
        boolean invalidGeom = false;
        List<String> areaList = new ArrayList<String>();

        Geometry adjAreaGeometry = getCwaGeomtry(wfo);
        if (adjAreaGeometry == null) {
            throw new SpatialException("CWA Geometry is null for " + wfo);
        }

        AdjacentWfoMgr adjMgr = new AdjacentWfoMgr(wfo);

        for (String area : adjMgr.getAdjIdList()) {
            Geometry areaGeo = getCwaGeomtry(area);
            // verify areaGeo is not null
            if (areaGeo == null) {
                invalidGeom = true;
                areaList.add(area);
                continue;
            }
            adjAreaGeometry = adjAreaGeometry.union(areaGeo);
        }

        if (invalidGeom) {
            StringBuilder sb = new StringBuilder("Missing geometries for:");
            for (String site : areaList) {
                sb.append(StringUtil.NEWLINE).append(site);
            }
            UFStatus.getHandler(AdjacentWfoMgr.class).warn(sb.toString());
        }

        return adjAreaGeometry;
    }

    /**
     * Gets you the CWA geometry
     * 
     * @param cwa
     *            The CWA
     * @return The Geometry for the CWA
     */
    public static Geometry getCwaGeomtry(String cwa) {
        ISpatialQuery sq = null;
        Geometry geo = null;
        WKBReader wkbReader = new WKBReader();
        String sql = "select AsBinary("
                + ScanUtils.getStandardResolutionLevel("cwa")
                + ") from mapdata.cwa where cwa = '" + cwa + "'";
        String msql = "select AsBinary("
                + ScanUtils.getStandardResolutionLevel("marinezones")
                + ") from mapdata.marinezones where wfo = '" + cwa + "'";

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "maps");
            if (results.length > 0) {
                geo = MonitorAreaUtils.readGeometry(results[0], wkbReader);
            }
            // marine zones
            Object[] mresults = sq.dbRequest(msql, "maps");
            if (mresults.length > 0) {
                for (Object res : mresults) {
                    if (res instanceof Object[]) {
                        res = ((Object[]) res)[0];
                    }
                    Geometry mgeo = MonitorAreaUtils.readGeometry(res,
                            wkbReader);
                    geo = geo.union(mgeo);
                }
            }
        } catch (SpatialException e) {
            UFStatus.getHandler(AdjacentWfoMgr.class).error(
                    "Error getting CWA Geometry", e);
        }

        return geo;
    }

    /**
     * Set the geometry for adjacent areas
     * 
     * @param geoAdjAreas
     *            The geometry
     */
    public void setGeoAdjAreas(Geometry geoAdjAreas) {
        this.geoAdjAreas = geoAdjAreas;
    }

    /**
     * Get the geometry for adjacent areas
     * 
     * @return The geometry
     */
    public Geometry getGeoAdjAreas() {
        return geoAdjAreas;
    }
}
