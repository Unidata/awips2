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

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.xml.AdjacentWfoXML;
import com.raytheon.uf.common.site.xml.CwaXML;
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
 * Dec 22, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AdjacentWfoMgr {
    /** Configuration XML. */
    private AdjacentWfoXML adjXML = null;

    private ArrayList<String> adjZones = null;

    private final String currentSite;

    private ArrayList<String> idList = null;

    /** Path to Adjacent WFO XML. */
    private static final String fileName = "allAdjacentWFOs.xml";

	private Geometry geoAdjAreas = null;

    /**
     * Constructor.
     * 
     * @param fullPath
     *            The full path the the configuration XML file.
     */
    public AdjacentWfoMgr(String currentSite) {
        System.out.println("**********************AdjacentWfoMgr instantiated with "
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

            System.out.println("**** path = " + path);

            adjXML = (AdjacentWfoXML) SerializationUtil
            .jaxbUnmarshalFromXmlFile(path.toString());

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
            e.printStackTrace();
        }
    }

    /**
     * Save the XML adjacent data to the current XML file name.
     */
    // public void saveAdjXml() {
    // IPathManager pm = PathManagerFactory.getPathManager();
    // LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
    // LocalizationLevel.SITE);
    // LocalizationFile newXmlFile = pm.getLocalizationFile(lc, fileName);
    //
    // if (newXmlFile.getFile().getParentFile().exists() == false) {
    // System.out.println("Creating new directory");
    //
    // if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
    // System.out.println("Could not create new directory...");
    // }
    // }
    //
    // try {
    // System.out.println("Saving -- "
    // + newXmlFile.getFile().getAbsolutePath());
    // SerializationUtil.jaxbMarshalToXmlFile(adjXML, newXmlFile
    // .getFile().getAbsolutePath());
    // newXmlFile.save();
    // } catch (Exception e) {
    // e.printStackTrace();
    // }
    // }
    /**
     * @return the adjXML
     */
    public AdjacentWfoXML getAdjXML() {
        return adjXML;
    }

    /**
     * @param adjXML
     *            the adjXML to set
     */
    public void setAdjXML(AdjacentWfoXML adjXML) {
        this.adjXML = adjXML;
    }

    /**
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
     * @return List of adjacent areas
     */
    public ArrayList<String> getAdjIdList() {
        return idList;
    }

	/**
	 * Gets geometry of all adjacent CWAs
	 * 
	 * @param wfo
	 * @return
	 */
	public static Geometry getAdjacentAreas(String wfo) {

		Geometry adjAreaGeometry = getCwaGeomtry(wfo);
		AdjacentWfoMgr adjMgr = new AdjacentWfoMgr(wfo);

		for (String area : adjMgr.getAdjIdList()) {
			Geometry areaGeo = getCwaGeomtry(area);
			adjAreaGeometry = adjAreaGeometry.union(areaGeo);
		}

		return adjAreaGeometry;
	}

	/**
	 * Gets you the CWA geometry
	 * 
	 * @param cwa
	 * @return
	 */
	public static Geometry getCwaGeomtry(String cwa) {

		ISpatialQuery sq = null;
		Geometry geo = null;
		WKBReader wkbReader = new WKBReader();
		String sql = "select AsBinary(" + ScanUtils.getStandardResolutionLevel("cwa") + ") from mapdata.cwa where cwa = '"
				+ cwa + "'";
		String msql = "select AsBinary(" + ScanUtils.getStandardResolutionLevel("marinezones") + ") from mapdata.marinezones where wfo = '"
				+ cwa + "'";

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
			e.printStackTrace();
		}
		return geo;
		// return geo.convexHull(); ????
	}

	// /**
	// * extract geometry
	// *
	// * @param object
	// * @return
	// */
	//
	// private static Geometry readGeometry(Object object, WKBReader wkbReader)
	// {
	// Geometry geometry = null;
	// try {
	// geometry = wkbReader.read((byte[]) object);
	// } catch (Exception e) {
	// e.printStackTrace();
	// }
	//
	// return geometry.buffer(0);
	// }

	public void setGeoAdjAreas(Geometry geoAdjAreas) {
		this.geoAdjAreas = geoAdjAreas;
	}

	public Geometry getGeoAdjAreas() {
		return geoAdjAreas;
	}

}
