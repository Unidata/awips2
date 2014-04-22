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

package com.raytheon.edex.uengine.tasks.query;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.geotools.data.Query;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureIterator;
import org.geotools.filter.IllegalFilterException;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.feature.IllegalAttributeException;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.type.AttributeDescriptor;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.geospatial.MapUtil;

/**
 * Derived from original uEngine ShapefileQuery task. Queries within a bounding
 * box on a shape file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2007            njensen     Initial Creation
 * Mar 11, 2014  #2718     randerso    Changes for GeoTools 10.5
 * </PRE>
 * 
 */
public class ShapefileQuery extends ScriptTask {
    private String shapeFile;

    private List<String> attributeNames = new ArrayList<String>();

    private double minLat;

    private double maxLat;

    private double minLon;

    private double maxLon;

    public ShapefileQuery(String aShapeFilePath, double aMinLat,
            double aMaxLat, double aMinLon, double aMaxLon) {
        shapeFile = aShapeFilePath;
        minLat = aMinLat;
        maxLat = aMaxLat;
        minLon = aMinLon;
        maxLon = aMaxLon;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        File theShape = new File(shapeFile);

        logger.warn("Querying shapeFile: " + theShape.getAbsolutePath());
        Map<String, List<Object>> resultMap = new HashMap<String, List<Object>>();
        try {
            ShapefileDataStore shpDS;
            shpDS = new ShapefileDataStore(theShape.toURI().toURL());

            // Load up the schema attributes into memory
            List<AttributeDescriptor> at = new ArrayList<AttributeDescriptor>();

            for (String an : attributeNames) {
                AttributeDescriptor a = shpDS.getSchema().getDescriptor(an);
                if (a == null) {
                    String msg = "shapeFile query failure. shapeFile does not contain desired attribute. ";
                    logger.error(msg);
                    throw new MicroEngineException(msg);
                } else {
                    resultMap.put(an, new ArrayList<Object>());
                    at.add(a);
                }
            }

            // The geometry attribute
            String theShapeField = shpDS.getSchema().getGeometryDescriptor()
                    .getLocalName();

            // Get the types
            String[] types = shpDS.getTypeNames();

            Query query = new Query();
            // Query geom
            query.setTypeName(types[0]);

            // Query all fields
            query.setPropertyNames(attributeNames);

            // Generate a geometry filter using an envelope
            // org.geotools.filter.FilterFactory ff = new FilterFactoryImpl();
            // Envelope e = new Envelope(minLon, maxLon, minLat, maxLat);
            // Expression bbox = ff.createBBoxExpression(e);
            // Expression geometry =
            // ff.createAttributeExpression(theShapeField);
            //
            // GeometryFilter bboxFilter = ff
            // .createGeometryFilter(AbstractFilter.GEOMETRY_BBOX);
            // bboxFilter.addLeftGeometry(geometry);
            // bboxFilter.addRightGeometry(bbox);

            FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                    .getDefaultHints());

            ReferencedEnvelope bbox = new ReferencedEnvelope(minLon, maxLon,
                    minLat, maxLat, MapUtil.LATLON_PROJECTION);
            Filter bboxFilter = ff.bbox(ff.property(theShapeField), bbox);

            query.setFilter(bboxFilter);
            FeatureIterator<SimpleFeature> fr = shpDS.getFeatureSource()
                    .getFeatures(query).features();

            SimpleFeature feature = null;
            if (!fr.hasNext()) {
                String msg = "shapeQuery returned no data. ";
                logger.warn(msg);
                throw new MicroEngineException(msg);
            } else {
                while (fr.hasNext()) {
                    feature = fr.next();
                    String msg = "FOUND";
                    for (String an : attributeNames) {
                        Object attr = feature.getAttribute(an);
                        msg = msg + " " + an + ": " + attr.toString();
                        resultMap.get(an).add(attr);
                    }
                    logger.info(msg);
                }
            }
            fr.close();

        } catch (MalformedURLException e1) {
            String msg = "shapeFile query failure. ShapeFile URL is malformed. ";
            logger.error(msg);
            throw new MicroEngineException(msg);
        } catch (NoSuchElementException e) {
            String msg = "shapeFile query failure. No such element. ";
            logger.error(msg);
            throw new MicroEngineException(msg);
        } catch (IOException e) {
            String msg = "shapeFile query failure. I/O exception. ";
            logger.error(msg);
            throw new MicroEngineException(msg);
        } catch (IllegalFilterException e) {
            String msg = "shapeFile query failure. Illegal filter. ";
            logger.error(msg);
            throw new MicroEngineException(msg);
        } catch (IllegalAttributeException e) {
            String msg = "shapeFile query failure. Illegal attribute. ";
            logger.error(msg);
            throw new MicroEngineException(msg);
        }

        return resultMap;
    }

    public void addAttributeName(String anAttributeName) {
        attributeNames.add(anAttributeName);
    }

    public double getMaxLat() {
        return maxLat;
    }

    public void setMaxLat(double aMaxLat) {
        maxLat = aMaxLat;
    }

    public double getMaxLon() {
        return maxLon;
    }

    public void setMaxLon(double aMaxLon) {
        maxLon = aMaxLon;
    }

    public double getMinLat() {
        return minLat;
    }

    public void setMinLat(double aMinLat) {
        minLat = aMinLat;
    }

    public double getMinLon() {
        return minLon;
    }

    public void setMinLon(double aMinLon) {
        minLon = aMinLon;
    }

    public String getShapeFile() {
        return shapeFile;
    }

    public void setShapeFile(String aShapeFile) {
        shapeFile = aShapeFile;
    }

    public List<String> getAttributeNames() {
        return attributeNames;
    }

    public void setAttributeNames(List<String> aAttributeNames) {
        attributeNames = aAttributeNames;
    }

}
