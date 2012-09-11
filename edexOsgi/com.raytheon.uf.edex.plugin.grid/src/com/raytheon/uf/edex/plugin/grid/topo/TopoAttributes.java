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

package com.raytheon.uf.edex.plugin.grid.topo;

import java.util.HashMap;
import java.util.Map;

import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Static class used for holding the attributes of the static topo data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/09/2010   6394        bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class TopoAttributes {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoAttributes.class);

    /** Lower left latitude attribute name */
    public static final String LL_LAT = "llLat";

    /** Lower left longitude attribute name */
    public static final String LL_LON = "llLon";

    /** Upper right latitude attribute name */
    public static final String UR_LAT = "urLat";

    /** Upper right longitude attribute name */
    public static final String UR_LON = "urLon";

    /** Nx attribute name */
    public static final String NX = "nx";

    /** Ny attribute name */
    public static final String NY = "ny";

    /** Central meridian attribute name */
    public static final String CENTRAL_MERIDIAN = "central_meridian";

    /** Latitude of origin attribute name */
    public static final String LAT_OF_ORIGIN = "latOfOrigin";

    /** CRS attribute name */
    public static final String CRS = "CRS";

    /** Map containing the attributes for the static topo data */
    public static Map<String, Map<String, Object>> attributeMap = new HashMap<String, Map<String, Object>>();

    static {
        createAttributeMap("ak", 61.968945f, 150.32922f, 47.7384f, -128.74811f,
                4270, 2800, -150f, 90f);
        createAttributeMap("carib", 26.831362f, -75.716293f, 8.909350f,
                -56.794250f, 2271, 2151, 0f, 0f);
        createAttributeMap("us", 16.114336f, -127.13067f, 51.825356f,
                -49.03707f, 6138, 4610, -100f, 45f);
        createAttributeMap("pac", -34.99583f, 120.004166f, 39.995834f,
                -120.004166f, 14400, 9000, 0f, 0f);
        createAttributeMap("world", -89.979164f, -180.0f, 89.979164f, 180.0f,
                8642, 4320, 0f, 0f);

    }

    /**
     * Retrieves the attributes for a static topo data set
     * 
     * @param name
     *            The name of the static topo data set
     * @return The attribute for the requested data set
     */
    public static Map<String, Object> getAttributes(String name) {
        return attributeMap.get(name);
    }

    /**
     * Creates an attribute map with the provided parameters
     * 
     * @param name
     *            The name of the static topo data set
     * @param llLat
     *            The lower left latitude
     * @param llLon
     *            The lower left longitude
     * @param urLat
     *            The upper right latitude
     * @param urLon
     *            The upper right longitude
     * @param nx
     *            Number of points on the x axis
     * @param ny
     *            Number of points on the y axis
     * @param central_meridian
     *            The central meridian
     * @param latOfOrigin
     *            The latitude of origin
     */
    private static void createAttributeMap(String name, float llLat,
            float llLon, float urLat, float urLon, int nx, int ny,
            float central_meridian, float latOfOrigin) {

        Map<String, Object> attributes = new HashMap<String, Object>();
        attributes.put(LL_LAT, new Float(llLat));
        attributes.put(LL_LON, new Float(llLon));
        attributes.put(UR_LAT, new Float(urLat));
        attributes.put(UR_LON, new Float(urLon));
        attributes.put(NX, new Integer(nx));
        attributes.put(NY, new Integer(ny));
        attributes.put(CENTRAL_MERIDIAN, new Float(central_meridian));
        attributes.put(LAT_OF_ORIGIN, new Float(latOfOrigin));
        if (name.equals("us")) {
            try {
                attributes.put(
                        CRS,
                        initUSProjectionData("US", latOfOrigin,
                                central_meridian).toWKT());
            } catch (Exception e) {
                statusHandler.handle(Priority.INFO, "Error creating US CRS");
            }
        } else if (name.equals("ak")) {
            attributes.put(
                    CRS,
                    MapUtil.constructStereographic(6371200.0, 6371200.0,
                            latOfOrigin, central_meridian).toWKT());
        } else {
            attributes.put(
                    CRS,
                    MapUtil.constructEquidistantCylindrical(6371200.0,
                            6371200.0, 0, 0).toWKT());
        }

        attributeMap.put(name, attributes);

    }

    /**
     * Initializes the CRS for the us data set
     * 
     * @param name
     *            The name
     * @param latCenter
     *            The central latitude
     * @param lonCenter
     *            The central longitude
     * @return The constructed CRS
     * @throws Exception
     */
    private static CoordinateReferenceSystem initUSProjectionData(String name,
            float latCenter, float lonCenter) throws Exception {
        ParameterValueGroup parameters = new DefaultMathTransformFactory()
                .getDefaultParameters("Lambert_Azimuthal_Equal_Area");
        parameters.parameter("semi_major").setValue(6371200.0);
        parameters.parameter("semi_minor").setValue(6371200.0);
        parameters.parameter("latitude_of_center").setValue(latCenter);
        parameters.parameter("longitude_of_center").setValue(lonCenter);
        return MapUtil.constructProjection(name, parameters);
    }
}
