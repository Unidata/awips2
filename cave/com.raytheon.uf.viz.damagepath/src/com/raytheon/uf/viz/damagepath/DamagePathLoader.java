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

import org.geotools.feature.FeatureCollection;
import org.geotools.feature.FeatureIterator;
import org.opengis.feature.Property;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.Name;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.geo.GeoJsonMapUtil;
import com.raytheon.uf.common.json.geo.IGeoJsonService;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.util.Pair;
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
 * Jun 18, 2015  #4354     dgilling    Support FeatureCollections so each polygon
 *                                     can have its own properties.
 * Jun 30, 2015  #4354     dgilling    Remove unnecessary back compat code for
 *                                     15.1 version of damage path tool.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class DamagePathLoader {

    private static final String UNSUPPORTED_GEOM_TYPE = "Damage path file contains invalid geometry type %s at geometry index %d. Must only contain Polygons.";

    private static final String INVALID_POLYGON = "Damage path file contains an invalid Polyon at index %d: %s";

    private final Collection<Pair<Polygon, Map<String, String>>> damagePathData;

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
        this.damagePathData = new ArrayList<>();

        if (locFileSource != null) {
            loadFromLocalizationFile(locFileSource);
        } else {
            loadFromFileSystem(realFileSource);
        }
    }

    public Collection<Pair<Polygon, Map<String, String>>> getDamagePathData() {
        return damagePathData;
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
        IGeoJsonService json = new SimpleGeoJsonService();
        FeatureCollection<SimpleFeatureType, SimpleFeature> featureCollection = json
                .deserializeFeatureCollection(is);
        populateDataFromFeatureCollection(featureCollection);
    }

    private void populateDataFromFeatureCollection(
            FeatureCollection<SimpleFeatureType, SimpleFeature> featureCollection)
            throws JsonException {
        try (FeatureIterator<SimpleFeature> iter = featureCollection.features()) {
            int featureIdx = 0;
            while (iter.hasNext()) {
                SimpleFeature feature = iter.next();

                Geometry geom = (Geometry) feature.getDefaultGeometry();
                if (geom instanceof Polygon) {
                    Polygon newPolygon = (Polygon) geom;
                    IsValidOp validator = new IsValidOp(newPolygon);
                    if (!validator.isValid()) {
                        throw new JsonException(String.format(INVALID_POLYGON,
                                featureIdx, validator.getValidationError()));
                    }

                    Map<String, String> properties = new LinkedHashMap<>();
                    Name defaultGeomAttrib = feature
                            .getDefaultGeometryProperty().getName();
                    properties.put(GeoJsonMapUtil.ID_KEY, feature.getID());
                    for (Property p : feature.getProperties()) {
                        if (!defaultGeomAttrib.equals(p.getName())) {
                            properties.put(p.getName().toString(), p.getValue()
                                    .toString());
                        }
                    }

                    Pair<Polygon, Map<String, String>> polygonAndProps = new Pair<>(
                            newPolygon, properties);
                    damagePathData.add(polygonAndProps);
                } else {
                    throw new JsonException(String.format(
                            UNSUPPORTED_GEOM_TYPE, geom.getGeometryType(),
                            featureIdx));
                }

                featureIdx++;
            }
        }
    }
}
