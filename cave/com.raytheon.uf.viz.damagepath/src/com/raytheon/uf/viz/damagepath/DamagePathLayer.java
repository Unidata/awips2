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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IMenuManager;
import org.geotools.data.DataUtilities;
import org.geotools.data.simple.SimpleFeatureCollection;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.json.geo.GeoJsonMapUtil;
import com.raytheon.uf.common.json.geo.IGeoJsonService;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.drawing.polygon.DrawablePolygon;
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
 * Jun 03, 2015  4375      dgilling    Support changes to PolygonLayer for 
 *                                     multiple polygon support.
 * Jun 08, 2015  4355      dgilling    Fix NullPointerException in loadJob.
 * Jun 12, 2015  4375      dgilling    Fix ConcurrentModificationException in
 *                                     initInternal.
 * Jun 18, 2015  4354      dgilling    Allow each polygon to have their own 
 *                                     properties.
 * Jul 01, 2015  4375      dgilling    Fix setDefaultPolygon.
 * Jul 07, 2015  4375      dgilling    Better error message for loadJob, make it
 *                                     INFO level, fix geotools CRS warning.
 * Aug 05, 2015  4635      dgilling    Default save location for damage path
 *                                     is now at SITE level.
 * Aug 18, 2015  3806      njensen     Use SaveableOutputStream to save
 * Jan 11, 2016  5242      kbisanz     Replaced calls to deprecated LocalizationFile methods
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

    /*
     * TODO: Since we've decided to use only 1 working file for this layer,
     * there's no need to append numbers to the end of the file name. In some
     * future release, remove this constant.
     */
    private static final String OLD_FILE = "damagepath1.json";

    private static final String FILE = "damagepath.json";

    /*
     * TODO: Since we've decided to use only 1 working file for this layer,
     * there's no need to append numbers to the end of the file name. In some
     * future release, remove this constant.
     */
    private static final String OLD_PATH = DIR + IPathManager.SEPARATOR
            + OLD_FILE;

    private static final String PATH = DIR + IPathManager.SEPARATOR + FILE;

    /**
     * JVM property to specify the localization level to attempt to save/load
     * with. Falls back to SITE if not defined.
     */
    private static final LocalizationLevel LEVEL_TO_USE = LocalizationLevel
            .valueOf(System.getProperty("damage.path.localization.level",
                    LocalizationLevel.SITE.name()));

    private final Job loadJob = new Job("Loading Damage Path") {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            ILocalizationFile prevFile = getValidDamagePathFile();
            if (prevFile != null) {
                loadDamagePath(prevFile);

                // reset the polygon if the localization file is invalid.
                if (polygons.isEmpty()) {
                    statusHandler
                            .info("The previously saved damage path file didn't contain any polygons. Resetting to default polygon.");
                    setDefaultPolygon();
                }
            } else {
                setDefaultPolygon();
            }

            return Status.OK_STATUS;
        }
    };

    private final Job saveJob = new Job("Saving Damage Path") {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            ILocalizationFile file = getDamagePathFile();
            saveDamagePath(file);
            return Status.OK_STATUS;
        }
    };

    public DamagePathLayer(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        // listen for changes to the directory
        LocalizationFile dir = getDamagePathFile();
        dir.addFileUpdatedObserver(this);

        loadJob.setSystem(true);
        saveJob.setSystem(true);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        loadJob.schedule();
    }

    private void setDefaultPolygon() {
        Polygon polygon = PolygonUtil.makeDefaultPolygon(getResourceContainer()
                .getActiveDisplayPane().getRenderableDisplay());
        DrawablePolygon drawablePolygon = new DamagePathPolygon(polygon, this);
        super.resetPolygons(Arrays.asList(drawablePolygon));
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

    @Override
    public void resetPolygon(int index, Coordinate[] coords) {
        Polygon prevPolygon = getPolygon(index);
        // this call will alter the polygon unless coords is null
        super.resetPolygon(index, coords);
        Polygon newPolygon = getPolygon(0);

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

    /*
     * TODO: Since we've decided to use only 1 working file for this layer,
     * there's no need to append numbers to the end of the file name. In some
     * future release, remove this function and replace uses with
     * getDamagePathFile.
     */
    private ILocalizationFile getValidDamagePathFile() {
        LocalizationContext ctx = getContext();
        ILocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, PATH);
        ILocalizationFile oldFile = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, OLD_PATH);
        if (file.exists()) {
            return file;
        } else if (oldFile.exists()) {
            return oldFile;
        } else {
            return null;
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (message.getFileName().equals(PATH)) {
            ILocalizationFile file = getDamagePathFile();
            if (file.exists()) {
                loadDamagePath(file);
            }
        }
    }

    protected void loadDamagePath(ILocalizationFile file) {
        try {
            DamagePathLoader loader = new DamagePathLoader(file);
            Collection<Pair<Polygon, Map<String, String>>> newData = loader
                    .getDamagePathData();
            if (!newData.isEmpty()) {
                Collection<DrawablePolygon> newDamagePaths = new ArrayList<>(
                        newData.size());
                for (Pair<Polygon, Map<String, String>> data : newData) {
                    newDamagePaths.add(new DamagePathPolygon(data.getFirst(),
                            data.getSecond(), this));
                }

                /*
                 * specifically call super.resetPolygon() cause
                 * this.resetPolygon() will save the file and we don't want to
                 * do that or we could infinite loop of load, save, load,
                 * save...
                 */
                super.resetPolygons(newDamagePaths);
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error loading damage path file " + file.getPath(), e);
        }
    }

    protected void saveDamagePath(ILocalizationFile file) {
        try (SaveableOutputStream sos = file.openOutputStream()) {
            IGeoJsonService json = new SimpleGeoJsonService();
            SimpleFeatureCollection featureCollection = buildFeatureCollection();
            json.serialize(featureCollection, sos);
            sos.save();
        } catch (Throwable t) {
            statusHandler.error(
                    "Error saving damage path file " + file.getPath(), t);
        }
    }

    private SimpleFeature buildFeature(final DamagePathPolygon damagePath) {
        Map<String, String> jsonProps = damagePath.getProperties();

        String id = jsonProps.get(GeoJsonMapUtil.ID_KEY);
        SimpleFeatureTypeBuilder typeBuilder = new SimpleFeatureTypeBuilder();
        typeBuilder.setName("feature");

        typeBuilder.setCRS(DefaultGeographicCRS.WGS84);
        Geometry polygon = damagePath.getPolygon();
        typeBuilder.setDefaultGeometry("the_geom");
        typeBuilder.add("the_geom", polygon.getClass());

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
        if (polygon != null) {
            featureBuilder.add(polygon);
        }
        featureBuilder.addAll(values);
        return featureBuilder.buildFeature(id);
    }

    public SimpleFeatureCollection buildFeatureCollection() {
        List<SimpleFeature> features = new ArrayList<>(polygons.size());
        for (DrawablePolygon polygon : polygons) {
            features.add(buildFeature((DamagePathPolygon) polygon));
        }

        return DataUtilities.collection(features);
    }

    @Override
    public void addPolygon(Coordinate[] coords) {
        super.addPolygon(new DamagePathPolygon(coords, this));
        saveJob.schedule();
    }

    @Override
    public void deletePolygon(int index) {
        super.deletePolygon(index);
        saveJob.schedule();
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (!getCapability(EditableCapability.class).isEditable()) {
            return;
        }

        super.addContextMenuItems(menuManager, x, y);

        int onPolygonIdx = uiInput.pointOnPolygon(x, y);
        if (onPolygonIdx >= 0) {
            menuManager.add(new OpenGeoJsonPropertiesDlgAction(
                    (DamagePathPolygon) polygons.get(onPolygonIdx)));
        }
    }

    @Override
    protected DrawablePolygon getNewDrawable() {
        return new DamagePathPolygon(this);
    }

    protected void scheduleSaveJob() {
        saveJob.schedule();
    }
}
