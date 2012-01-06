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
package com.raytheon.edex.plugin.gfe.reference;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.jts.JTS;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IEditAreaNamer;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IMapBackgroundFilter;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Load edit area information for a site from shape files. This is a limited
 * version of the processing that takes place in MapManager, created so that
 * ConfigureTextProductsHandler can rebuild the combinations files, area
 * dictionary file, and city location file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2011            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class EditAreaLoader {

    private transient Log log = LogFactory.getLog(getClass());

    /**
     * Load the shapefiles for site and use them to generate editAreaMap and
     * editAreaAttrs. The output collections are not cleared, so results may
     * accumulate if the same collections are used in multiple calls.
     * 
     * @param site
     * @param editAreaMap
     *            The edit area map (output).
     * @param editAreaAttrs
     *            The edit area attributes (output)
     * @throws IOException
     *             When one or more shape files cannot be read
     */
    public void load(String site, Map<String, List<String>> editAreaMap,
            Map<String, Map<String, String>> editAreaAttrs)
            throws GfeConfigurationException, IOException {

        if (editAreaMap == null) {
            throw new IllegalArgumentException("editAreaMap is null");
        }

        if (editAreaAttrs == null) {
            throw new IllegalArgumentException("editAreaAttrs is null");
        }

        // Get the site's bounding geometry
        IFPServerConfig config = getServerConfig(site);
        GridLocation gloc = config.dbDomain();
        Coordinate c0 = new Coordinate(0, 0);
        Coordinate c1 = new Coordinate(gloc.getNx() - 1, 0);
        Coordinate c2 = new Coordinate(gloc.getNx() - 1, gloc.getNy() - 1);
        Coordinate c3 = new Coordinate(0, gloc.getNy() - 1);
        GeometryFactory gf = new GeometryFactory();
        Polygon p = gf.createPolygon(
                gf.createLinearRing(new Coordinate[] { c0, c1, c2, c3, c0 }),
                null);
        PreparedGeometry boundingGeometry = PreparedGeometryFactory.prepare(p);

        // transform: shape file geometry to boundingGeometry CRS
        MathTransform transform = MapUtil.getTransformFromLatLon(
                PixelOrientation.CENTER, gloc);

        ShapeFile[] shapeFiles = Maps.getMaps(site, Maps.GetMode.UNCONDITIONAL);

        for (ShapeFile shapeFile : shapeFiles) {
            List<String> created = new ArrayList<String>();

            if (shapeFile.getEditAreaName() == null) {
                continue;
            }

            log.debug("creating: " + shapeFile.getDisplayName());
            try {
                shapeFile.open();

                IMapBackgroundFilter filter = shapeFile
                        .getMapBackgroundFilter();

                while (shapeFile.hasNext()) {
                    SimpleFeature f = shapeFile.next();
                    Map<String, String> info = shapeFile.getAttributes(f);

                    if (filter != null && !filter.filter(info)) {
                        continue;
                    }

                    String editAreaName;
                    // functions return a string
                    if (shapeFile.getEditAreaName() instanceof IEditAreaNamer) {
                        editAreaName = ((IEditAreaNamer) shapeFile
                                .getEditAreaName()).getEditAreaName(info);
                        // non-functions allow only a string
                    } else {
                        editAreaName = defaultEditAreaNaming(info,
                                shapeFile.getEditAreaName());
                    }

                    // validate edit area name
                    String ean = validateEAN(editAreaName);
                    if (ean.length() == 0) {
                        continue;
                    }

                    Geometry mp = (Geometry) f.getDefaultGeometry();
                    Geometry mpGrid;
                    try {
                        mpGrid = JTS.transform(mp, transform);
                    } catch (TransformException e) {
                        log.warn("Error transforming geometry for edit area "
                                + ean + " : skipped", e);
                        continue;
                    }
                    if (!boundingGeometry.intersects(mpGrid)) {
                        continue;
                    }

                    Map<String, String> savedInfo = editAreaAttrs.get(ean);
                    if (savedInfo == null) {
                        created.add(ean);
                        editAreaAttrs.put(ean, info);
                    }
                }

            } finally {
                shapeFile.close();
            }
            editAreaMap.put(shapeFile.getDisplayName(), created);
        }
    }

    /**
     * Get the server configuration for site. This is just a wrapper around the
     * static method of the same name in IFPServerConfig.
     * 
     * @param site
     *            The site whose server configuration is to be found
     * @return The server configuration for the site.
     */
    protected IFPServerConfig getServerConfig(String site)
            throws GfeConfigurationException {
        return IFPServerConfigManager.getServerConfig(site);
    }

    /**
     * Validate edit area name. Changes the name as needed to ensure a valid
     * python name.
     * <p>
     * Copied from MapManager
     * 
     * @param editAreaName
     * @return
     */
    protected String validateEAN(String ean) {
        String s = ean;

        // strip out white space and puncuation (except _)
        for (int i = s.length() - 1; i >= 0; i--) {
            if (!Character.isLetterOrDigit(s.charAt(i)) && s.charAt(i) != '_') {
                s = s.substring(0, i) + s.substring(i + 1);
            }
        }

        // ensure 1st character is not a number. If a number, preprend.
        if (s.length() > 0 && Character.isDigit(s.charAt(0))) {
            s = "ea" + s;
        }

        return s;
    }

    /**
     * Generate an edit area name from a collection of attributes and an edit
     * area name definition. If the edit area name definition is not a String or
     * an array of Strings, an empty String is returned.
     * <p>
     * Copied from MapManager
     * 
     * @param info
     *            A Map of shape file attribute names to values
     * @param eanDefinition
     *            The Object returned from a call to getEditAreaName() on a
     *            ShapeFile instance, but NOT an IEditAreaNamer instance.
     * @return An edit area name constructed from the input parameters. The name
     *         may not be a valid Python name.
     * @see validateEAN(String)
     */
    protected String defaultEditAreaNaming(Map<String, String> info,
            Object eanDefinition) {
        // simple case, the edit area name definition is the attribute key
        if (eanDefinition instanceof String) {
            String ean = (String) eanDefinition;
            if (info.containsKey(ean)) {
                return info.get(ean);
            } else {
                return ean;
            }

        } else if (eanDefinition instanceof String[]) {
            String s = "";
            for (String e : (String[]) eanDefinition) {
                // valid attribute
                if (info.containsKey(e)) {
                    if (s.length() == 0) {
                        s = info.get(e);
                    } else {
                        s = s + "_" + info.get(e);
                    }
                    // not valid attribute, so use definition directly
                } else {
                    if (s.length() == 0) {
                        s = e;
                    } else {
                        s = s + "_" + e;
                    }
                }
            }
            return s;

        } else {
            return "";
        }
    }
}
