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
package com.raytheon.uf.viz.damagepath;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.opengis.feature.Property;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.type.Name;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.geo.BasicJsonService;
import com.raytheon.uf.common.json.geo.GeoJsonMapUtil;
import com.raytheon.uf.common.json.geo.IGeoJsonService;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.valid.IsValidOp;

/**
 * Loads a damage path GeoJSON-formatted file from either local disk or the
 * localization file store.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------- 
 * Jun 05, 2015  #4375     dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class DamagePathLoader {

    private static final String UNSUPPORTED_GEOJSON_TYPE = "Damage path file is unsupported GeoJSON object type %s. This tool only supports Feature and Geometry objects.";

    private static final String UNSUPPORTED_GEOM_TYPE = "Damage path file contains invalid geometry type %s at geometry index %d. Must only contain Polygons.";

    private static final String INVALID_POLYGON = "Damage path file contains an invalid Polyon at index %d: %s";

    private final Collection<Polygon> polygons;

    private final Map<String, String> properties;

    public DamagePathLoader(LocalizationFile locFile)
            throws LocalizationException, IOException, JsonException {
        this(locFile, null);
    }

    public DamagePathLoader(String filePath) throws LocalizationException,
            IOException, JsonException {
        this(null, Paths.get(filePath));
    }

    private DamagePathLoader(LocalizationFile locFileSource, Path realFileSource)
            throws LocalizationException, IOException, JsonException {
        this.polygons = new ArrayList<>();
        this.properties = new LinkedHashMap<>();

        if (locFileSource != null) {
            loadFromLocalizationFile(locFileSource);
        } else {
            loadFromFileSystem(realFileSource);
        }
    }

    public Collection<Polygon> getPolygons() {
        return polygons;
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    private void loadFromLocalizationFile(final LocalizationFile locFile)
            throws LocalizationException, IOException, JsonException {
        try (InputStream is = locFile.openInputStream()) {
            loadFromInputStream(is);
        }
    }

    private void loadFromFileSystem(final Path filePath) throws IOException,
            JsonException {
        try (InputStream is = Files.newInputStream(filePath)) {
            loadFromInputStream(is);
        }
    }

    private void loadFromInputStream(final InputStream is) throws JsonException {
        Geometry deserializedGeom = null;
        GeoJsonMapUtil geoJsonUtil = new GeoJsonMapUtil();

        /*
         * For compatibility with any users that may have an autosaved damage
         * path file from build 15.1, we'll support deserializing both Geometry
         * and Feature GeoJSON types.
         * 
         * TODO: remove this code for code that just expects the file to always
         * be a Feature.
         */
        Map<String, Object> jsonObject = (Map<String, Object>) new BasicJsonService()
                .deserialize(is, LinkedHashMap.class);
        String geoJsonType = jsonObject.get(GeoJsonMapUtil.TYPE_KEY).toString();
        if (geoJsonType.equals(GeoJsonMapUtil.FEATURE_TYPE)) {
            SimpleFeature feature = geoJsonUtil.populateFeature(jsonObject);
            deserializedGeom = (Geometry) feature.getDefaultGeometry();

            Name defaultGeomAttrib = feature.getDefaultGeometryProperty()
                    .getName();
            properties.put(GeoJsonMapUtil.ID_KEY, feature.getID());
            for (Property p : feature.getProperties()) {
                if (!defaultGeomAttrib.equals(p.getName())) {
                    properties.put(p.getName().toString(), p.getValue()
                            .toString());
                }
            }
        } else if (isGeometryType(geoJsonType)) {
            deserializedGeom = geoJsonUtil.populateGeometry(jsonObject);
        } else {
            throw new JsonException(String.format(UNSUPPORTED_GEOJSON_TYPE,
                    geoJsonType));
        }

        int numGeometries = deserializedGeom.getNumGeometries();
        for (int i = 0; i < numGeometries; i++) {
            Geometry geomN = deserializedGeom.getGeometryN(i);
            if (geomN instanceof Polygon) {
                Polygon newPolygon = (Polygon) geomN;
                IsValidOp validator = new IsValidOp(newPolygon);
                if (validator.isValid()) {
                    polygons.add(newPolygon);
                } else {
                    throw new JsonException(String.format(INVALID_POLYGON, i,
                            validator.getValidationError()));
                }
            } else {
                throw new JsonException(String.format(UNSUPPORTED_GEOM_TYPE,
                        geomN.getGeometryType(), i));
            }
        }
    }

    private boolean isGeometryType(String geoJsonType) {
        return ((GeoJsonMapUtil.GEOM_COLL_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.LINE_STR_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_LINE_STR_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_POINT_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_POLY_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.POINT_TYPE.equals(geoJsonType)) || (GeoJsonMapUtil.POLY_TYPE
                    .equals(geoJsonType)));
    }

    /*
     * TODO: Replace loadFromInputStream with this method when we no longer
     * desire supporting the version 15.1 GeoJSON formatted damage path files.
     */
    @SuppressWarnings("unused")
    private void loadFromInputStreamFuture(final InputStream is)
            throws JsonException {
        IGeoJsonService json = new SimpleGeoJsonService();
        SimpleFeature feature = json.deserializeFeature(is);
        Geometry featureGeom = (Geometry) feature.getDefaultGeometry();

        int numGeometries = featureGeom.getNumGeometries();
        for (int i = 0; i < numGeometries; i++) {
            Geometry geomN = featureGeom.getGeometryN(i);
            if (geomN instanceof Polygon) {
                Polygon newPolygon = (Polygon) geomN;
                IsValidOp validator = new IsValidOp(newPolygon);
                if (validator.isValid()) {
                    polygons.add(newPolygon);
                } else {
                    throw new JsonException(String.format(INVALID_POLYGON, i,
                            validator.getValidationError()));
                }
            } else {
                throw new JsonException(String.format(UNSUPPORTED_GEOM_TYPE,
                        geomN.getGeometryType(), i));
            }
        }

        Name defaultGeomAttrib = feature.getDefaultGeometryProperty().getName();
        properties.put(GeoJsonMapUtil.ID_KEY, feature.getID());
        for (Property p : feature.getProperties()) {
            if (!defaultGeomAttrib.equals(p.getName())) {
                properties.put(p.getName().toString(), p.getValue().toString());
            }
        }
    }
}
