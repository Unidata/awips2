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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.Property;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.Name;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.geo.BasicJsonService;
import com.raytheon.uf.common.json.geo.GeoJsonMapUtil;
import com.raytheon.uf.common.json.geo.IGeoJsonService;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFileOutputStream;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.polygon.PolygonLayer;
import com.raytheon.uf.viz.drawing.polygon.PolygonUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * A layer for displaying and customizing a weather event's damage path.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2015  3974      njensen     Initial creation
 * Mar 31, 2015  3977      nabowle     Reset polygon when initializing from the
 *                                     localization file fails.
 * Apr 23, 2015  4354      dgilling    Support GeoJSON Feature properties.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DamagePathLayer<T extends DamagePathResourceData> extends
        PolygonLayer<T> implements ILocalizationFileObserver {

    protected static final String NAME = "Damage Path";

    private static final String DIR = "damagepath";

    private static final String FILE = "damagepath1.json";

    private static final String PATH = DIR + IPathManager.SEPARATOR + FILE;

    private Map<String, String> featureProperties = Collections.emptyMap();

    /**
     * JVM property to specify the localization level to attempt to save/load
     * with. Falls back to USER if not defined.
     */
    private static final LocalizationLevel LEVEL_TO_USE = LocalizationLevel
            .valueOf(System.getProperty("damage.path.localization.level",
                    LocalizationLevel.USER.name()));

    /*
     * TODO: If we support multiple polygons in the future then the jobs will
     * need to be smart enough to load/save different files.
     */
    private final Job loadJob = new Job("Loading Damage Path") {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            LocalizationFile prevFile = getDamagePathFile();
            if (prevFile.exists()) {
                loadDamagePath(prevFile);

                // reset the polygon if the localization file is invalid.
                if (getPolygon() == null) {
                    statusHandler
                            .error("The damage path file was invalid. The polygon has been reset.");
                    polygon = PolygonUtil
                            .makeDefaultPolygon(getResourceContainer()
                                    .getActiveDisplayPane()
                                    .getRenderableDisplay());
                }
            }
            return Status.OK_STATUS;
        }
    };

    private final Job saveJob = new Job("Saving Damage Path") {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            LocalizationFile file = getDamagePathFile();
            saveDamagePath(file);
            return Status.OK_STATUS;
        }
    };

    public DamagePathLayer(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        // listen for changes to the directory
        LocalizationFile dir = PathManagerFactory.getPathManager()
                .getLocalizationFile(getContext(), DIR);
        dir.addFileUpdatedObserver(this);

        loadJob.setSystem(true);
        loadJob.schedule();
        saveJob.setSystem(true);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        LocalizationFile prevFile = getDamagePathFile();
        if (polygon == null && !prevFile.exists()) {
            /*
             * only get here if there is no previous file, otherwise loadJob
             * will load the polygon
             */
            polygon = PolygonUtil.makeDefaultPolygon(getResourceContainer()
                    .getActiveDisplayPane().getRenderableDisplay());
        }
    }

    @Override
    protected void disposeInternal() {
        LocalizationFile dir = PathManagerFactory.getPathManager()
                .getLocalizationFile(getContext(), DIR);
        dir.removeFileUpdatedObserver(this);

        super.disposeInternal();
    }

    @Override
    public String getName() {
        return NAME;
    }

    protected Polygon loadStormTrackPolygon() {
        /*
         * TODO get storm track file, use mathematical algorithm to derive
         * damage track
         */
        return null;
    }

    protected Polygon loadFilesystemPolygon(File file) {
        /*
         * TODO load a polygon that was selected from a file chooser dialog from
         * an import menu
         */
        return null;
    }

    protected Polygon loadMousePolygon() {
        /*
         * TODO create a new InputAdapter that takes highest priority and blocks
         * other inputs. left clicking adds vertices, right click indicates the
         * last point on the polygon, then connect the first and last point.
         * 
         * afterwards, remove and dispose of that input adapter
         */
        return null;
    }

    @Override
    public void resetPolygon(Coordinate[] coords) {
        Polygon prevPolygon = this.getPolygon();
        // this call will alter the polygon unless coords is null
        super.resetPolygon(coords);
        Polygon newPolygon = this.getPolygon();

        /*
         * only bother saving the polygon if they're done dragging
         */
        if ((prevPolygon == null && newPolygon != null)
                || (newPolygon != null && !this.uiInput.isDragging())) {
            saveJob.schedule();
        }
    }

    private LocalizationContext getContext() {
        return PathManagerFactory.getPathManager().getContext(
                LocalizationType.COMMON_STATIC, LEVEL_TO_USE);
    }

    protected LocalizationFile getDamagePathFile() {
        LocalizationContext ctx = getContext();
        return PathManagerFactory.getPathManager().getLocalizationFile(ctx,
                PATH);
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (message.getFileName().equals(PATH)) {
            LocalizationFile file = getDamagePathFile();
            if (file.exists()) {
                loadDamagePath(file);
            }
        }
    }

    protected void loadDamagePath(LocalizationFile file) {
        try (InputStream is = file.openInputStream()) {
            Geometry deserializedGeom = null;
            Map<String, String> deserializedProps = Collections.emptyMap();
            GeoJsonMapUtil geoJsonUtil = new GeoJsonMapUtil();

            /*
             * For compatibility with any users that may have an autosaved
             * damage path file from build 15.1, we'll support deserializing
             * both Geometry and Feature GeoJSON types.
             * 
             * TODO: remove this code for code that just expects the file to
             * always be a Feature.
             */
            Map<String, Object> jsonObject = (Map<String, Object>) new BasicJsonService()
                    .deserialize(is, LinkedHashMap.class);
            String geoJsonType = jsonObject.get(GeoJsonMapUtil.TYPE_KEY)
                    .toString();
            if (geoJsonType.equals(GeoJsonMapUtil.FEATURE_TYPE)) {
                SimpleFeature feature = geoJsonUtil.populateFeature(jsonObject);
                deserializedGeom = (Geometry) feature.getDefaultGeometry();

                Name defaultGeomAttrib = feature.getDefaultGeometryProperty()
                        .getName();
                deserializedProps = new LinkedHashMap<>();
                deserializedProps.put(GeoJsonMapUtil.ID_KEY, feature.getID());
                for (Property p : feature.getProperties()) {
                    if (!defaultGeomAttrib.equals(p.getName())) {
                        deserializedProps.put(p.getName().toString(), p
                                .getValue().toString());
                    }
                }
            } else if (isGeometryType(geoJsonType)) {
                deserializedGeom = geoJsonUtil.populateGeometry(jsonObject);
            } else {
                String message = "Unexpected GeoJSON object type "
                        + geoJsonType
                        + ". This tool only supports Feature and Geometry objects.";
                throw new JsonException(message);
            }

            Polygon geometry = (Polygon) deserializedGeom;
            /*
             * specifically call super.resetPolygon() cause this.resetPolygon()
             * will save the file and we don't want to do that or we could
             * infinite loop of load, save, load, save...
             */
            Polygon current = this.getPolygon();
            if (current == null || !current.equals(geometry)) {
                super.resetPolygon(geometry.getExteriorRing().getCoordinates());
            }

            featureProperties = deserializedProps;
        } catch (Exception e) {
            statusHandler.error(
                    "Error loading damage path file " + file.getName(), e);
        }
    }

    protected void saveDamagePath(LocalizationFile file) {
        LocalizationFileOutputStream fos = null;
        try {
            fos = file.openOutputStream();
            IGeoJsonService json = new SimpleGeoJsonService();
            SimpleFeature feature = buildFeature();
            json.serialize(feature, fos);
            fos.closeAndSave();
        } catch (Throwable t) {
            if (fos != null) {
                try {
                    fos.close();
                } catch (IOException e) {
                    // ignore
                }
            }
            statusHandler.error(
                    "Error saving damage path file " + file.getName(), t);
        }
    }

    public SimpleFeature buildFeature() {
        Map<String, String> jsonProps = getFeatureProperties();

        String id = jsonProps.get(GeoJsonMapUtil.ID_KEY);
        SimpleFeatureTypeBuilder typeBuilder = new SimpleFeatureTypeBuilder();
        typeBuilder.setName("feature");
        Geometry geom = getPolygon();
        if (geom != null) {
            typeBuilder.setDefaultGeometry("the_geom");
            typeBuilder.add("the_geom", geom.getClass());
        }

        Collection<String> keysToIgnore = Arrays.asList(GeoJsonMapUtil.ID_KEY);
        Set<String> keySet = jsonProps.keySet();
        List<Object> values = new ArrayList<Object>(keySet.size());
        for (String key : keySet) {
            if (!keysToIgnore.contains(key)) {
                Object val = jsonProps.get(key);
                typeBuilder.add(key, val.getClass());
                values.add(val);
            }
        }

        SimpleFeatureType type = typeBuilder.buildFeatureType();
        SimpleFeatureBuilder featureBuilder = new SimpleFeatureBuilder(type);
        if (geom != null) {
            featureBuilder.add(geom);
        }
        featureBuilder.addAll(values);
        return featureBuilder.buildFeature(id);
    }

    public Map<String, String> getFeatureProperties() {
        return featureProperties;
    }

    public void setFeatureProperties(Map<String, String> featureProperties) {
        this.featureProperties = featureProperties;
        saveJob.schedule();
    }

    public boolean isGeometryType(String geoJsonType) {
        return ((GeoJsonMapUtil.GEOM_COLL_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.LINE_STR_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_LINE_STR_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_POINT_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.MULT_POLY_TYPE.equals(geoJsonType))
                || (GeoJsonMapUtil.POINT_TYPE.equals(geoJsonType)) || (GeoJsonMapUtil.POLY_TYPE
                    .equals(geoJsonType)));
    }
}
