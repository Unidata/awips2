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
package com.raytheon.edex.util.satellite;

import java.io.File;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.awipstools.GetWfoCenterHandler;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Create the satellite menus for east conus/west conus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2011            mnash     Initial creation
 * Sep 10, 2012 15337      kshresth  Changed sector on OCONUS:Products under 
 * 									 Derived Products Imagery Display
 * Nov 01, 2012 15346      kshresth  Added Satellite Products for OCONUS
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SatelliteMenuUtil extends AbstractMenuUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteMenuUtil.class);

    private static final CoordinateReferenceSystem conusCRS = MapUtil
            .constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 25, 25, -95);

    // The western edge of the west conus satellite image in conusCRS
    private static final double westConusWestEdge = -4226066.525608903;

    // The eastern edge of the west conus satellite image in conusCRS
    private static final double westConusEastEdge = 243783.47439109907;

    // The eastern edge of the east conus satellite image in conusCRS
    private static final double eastConusEastEdge = 3250785.858985792;

    // The western edge of the east conus satellite image in conusCRS
    private static final double eastConusWestEdge = -1950494.1410142092;

    @Override
    public void createMenus() {
        statusHandler.info("Creating satellite menus...");
        // decide on pacific, alaska, or conus
        GetWfoCenterPoint centerPointRequest = new GetWfoCenterPoint(getSite());
        Object[] results = null;
        Number lat = null;
        Number lon = null;
        Coordinate coord = null;
        ISpatialQuery sp = null;
        try {
            coord = new GetWfoCenterHandler().handleRequest(centerPointRequest);
            sp = SpatialQueryFactory.create();
        } catch (Exception e1) {
            statusHandler.error("Unable to send request for lat/lon values");
            return;
        }
        try {
            lat = coord.y;
            lon = coord.x;
            String sql = "select name from mapdata.states where ST_Contains(the_geom, ST_GeomFromText('POINT("
                    + lon.doubleValue()
                    + " "
                    + lat.doubleValue()
                    + ")', 4326))";
            results = sp.dbRequest(sql, "maps");
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        MenuTemplateFile file = (MenuTemplateFile) fromXml("menuTemplate"
                + File.separator + "satellite" + File.separator
                + "baseCompositeTemplate.xml");

        MenuTemplateFile fileOCONUS = (MenuTemplateFile) fromXml("menuTemplate"
                + File.separator + "satellite" + File.separator
                + "baseOCONUSDerivedProductsImageryTemplate.xml");
        
        String state = "";
        if (results != null && results.length > 0) {
            state = results[0].toString();
        } else {
            statusHandler
                    .error("Could not determine state or regional satellites, defaulting to CONUS");
        }

        // exception for the US where Guam uses Puerto Rican satellites
        if ("GUM".equals(getSite())) {
            state = "Puerto Rico";
        }
        ((CommonIncludeMenuContribution) file.contributions[0]).fileName = new File(
                "menus/satellite/baseSatellite.xml");
        ((CommonIncludeMenuContribution) fileOCONUS.contributions[0]).fileName = new File(
        "menus/satellite/baseOCONUSBlendedDerivedProductsImagery.xml");
        if ("Alaska".equals(state) ||"Hawaii".equals(state)
                || "Puerto Rico".equals(state)) {
            ((CommonIncludeMenuContribution) fileOCONUS.contributions[0]).substitutions = new VariableSubstitution[1];
            // sector
            VariableSubstitution sub = new VariableSubstitution();
            sub.key = "sector";
            sub.value = state + " National";
            ((CommonIncludeMenuContribution) fileOCONUS.contributions[0]).substitutions[0] = sub;            
            // ============================================
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions = new VariableSubstitution[5];
            // sector0
            sub = new VariableSubstitution();
            sub.key = "sector0";
            sub.value = state + " Regional";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[0] = sub;           
            // sector1
            sub = new VariableSubstitution();
            sub.key = "sector1";
            sub.value = state + " National";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[1] = sub;
            // sector2
            sub = new VariableSubstitution();
            sub.key = "sector2";
            sub.value = "Supernational";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[2] = sub;
            // sector 3, for these sites copy sector2
            sub = new VariableSubstitution();
            sub.key = "sector3";
            sub.value = "Northern Hemisphere Composite";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[3] = sub;
            // entity
            sub = new VariableSubstitution();
            sub.key = "entity";
            sub.value = "GOES%";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[4] = sub;
            toXml(fileOCONUS, "menus" + File.separator + "satellite" + File.separator
                    + "baseDerivedProductsImagery.xml");
        } else {
            // decide on east conus or west conus
            ((CommonIncludeMenuContribution) file.contributions[0]).fileName = new File(
                    "menus/satellite/baseSatellite.xml");
            double[] conusPoint = new double[2];
            try {
                MapUtil.getTransformFromLatLon(conusCRS).transform(
                        new double[] { lon.doubleValue(), lat.doubleValue() },
                        0, conusPoint, 0, 1);
            } catch (TransformException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            } catch (FactoryException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }

            double eastDistance = Math.abs(conusPoint[0] - eastConusWestEdge);

            double westDistance = Math.abs(conusPoint[0] - westConusEastEdge);

            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions = new VariableSubstitution[6];

            VariableSubstitution sub = new VariableSubstitution();
            if (eastDistance < westDistance) {
                sub.key = "sector0";
                sub.value = "East CONUS";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[0] = sub;
                sub = new VariableSubstitution();
                sub.key = "sector1";
                sub.value = "West CONUS";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[1] = sub;
            } else {
                sub.key = "sector0";
                sub.value = "West CONUS";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[0] = sub;
                sub = new VariableSubstitution();
                sub.key = "sector1";
                sub.value = "East CONUS";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[1] = sub;
            }
            if (conusPoint[0] < westConusEastEdge
                    && conusPoint[0] > eastConusWestEdge) {
                sub = new VariableSubstitution();
                sub.key = "blendedTimeMatchMode";
                sub.value = "ALL_IMAGES";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[5] = sub;
            } else {
                sub = new VariableSubstitution();
                sub.key = "blendedTimeMatchMode";
                sub.value = "FIRST_IMAGE";
                ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[5] = sub;
            }
            sub = new VariableSubstitution();
            sub.key = "sector2";
            sub.value = "Supernational";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[2] = sub;
            sub = new VariableSubstitution();
            sub.key = "sector3";
            sub.value = "Northern Hemisphere Composite";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[3] = sub;
            sub = new VariableSubstitution();
            sub.key = "entity";
            sub.value = "GOES%";
            ((CommonIncludeMenuContribution) file.contributions[0]).substitutions[4] = sub;
        }

        toXml(file, "menus" + File.separator + "satellite" + File.separator
                + "baseComposite.xml");
        
        statusHandler.info("Finished creating satellite menus");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.menus.AbstractMenuUtil#checkCreated()
     */
    @Override
    public boolean checkCreated() {
        MenuTemplateFile file = (MenuTemplateFile) fromXml("menus"
                + File.separator + "satellite" + File.separator
                + "baseComposite.xml", caveConfigured);
        MenuTemplateFile fileO = (MenuTemplateFile) fromXml("menus"
                + File.separator + "satellite" + File.separator
                + "baseDerivedProductsImagery.xml", caveConfigured);        
        if (file == null || file.contributions == null
                || file.contributions.length == 0) {
        	 return false;
        } else {
        	if (getSite().equals("AJK") || getSite().equals("AFC")
                || getSite().equals("AFG") || getSite().equals("HFO") 
                || getSite().equals("GUM") || getSite().equals("SJU")) {
                if( fileO == null || fileO.contributions == null
                    || fileO.contributions.length == 0) 
                  return false;
                }
            statusHandler.info("Menus already created for site " + getSite()
                    + " for satellite");
            return true;
        }
    }
}
