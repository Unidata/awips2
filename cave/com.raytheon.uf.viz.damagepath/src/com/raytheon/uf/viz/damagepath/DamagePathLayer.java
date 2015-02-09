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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.json.geo.GeoJsonUtil;
import com.raytheon.uf.common.json.geo.GeoJsonUtilSimpleImpl;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFileInputStream;
import com.raytheon.uf.common.localization.LocalizationFileOutputStream;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.polygon.PolygonLayer;
import com.raytheon.uf.viz.drawing.polygon.PolygonUtil;
import com.vividsolutions.jts.geom.Coordinate;
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
                .getLocalizationFile(getUserContext(), DIR);
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
                .getLocalizationFile(getUserContext(), DIR);
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

    private LocalizationContext getUserContext() {
        return PathManagerFactory.getPathManager().getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
    }

    protected LocalizationFile getDamagePathFile() {
        LocalizationContext ctx = getUserContext();
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
        try (LocalizationFileInputStream fis = file.openInputStream()) {
            GeoJsonUtil json = new GeoJsonUtilSimpleImpl();
            Polygon geometry = (Polygon) json.deserializeGeom(fis);
            /*
             * specifically call super.resetPolygon() cause this.resetPolygon()
             * will save the file and we don't want to do that or we could
             * infinite loop of load, save, load, save...
             */
            Polygon current = this.getPolygon();
            if (current == null || !current.equals(geometry)) {
                super.resetPolygon(geometry.getExteriorRing().getCoordinates());
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error loading damage path file " + file.getName(), e);
        }
    }

    protected void saveDamagePath(LocalizationFile file) {
        LocalizationFileOutputStream fos = null;
        try {
            fos = file.openOutputStream();
            GeoJsonUtil json = new GeoJsonUtilSimpleImpl();
            json.serialize(this.getPolygon(), fos);
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
}
