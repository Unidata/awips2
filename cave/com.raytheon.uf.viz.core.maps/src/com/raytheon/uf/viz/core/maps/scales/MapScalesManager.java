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
package com.raytheon.uf.viz.core.maps.scales;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile;
import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile.AutoUpdatingFileChangedListener;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.maps.scales.MapScales.PartId;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.viz.ui.actions.LoadSerializedXml;

/**
 * Manager for {@link MapScales}. May be constructed from any file or the
 * default instance can be used via {@link #getInstance()}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 08, 2013           mschenke    Initial creation
 * Oct 22, 2013  2491     bsteffen    Change from SerializationUtil to
 *                                    ProcedureXmlManager
 * Mar 24, 2014  2954     mpduff      Check for missing map scale files and handle the situation.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MapScalesManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapScalesManager.class);

    private static final String DEFAULT_SCALES_DIR = "bundles"
            + IPathManager.SEPARATOR + "scales";

    private static final String DEFAULT_SCALES_FILE = "scalesInfo.xml";

    /**
     * Manager class for a single {@link MapScale}. Is able to create a Bundle
     * for the scale. May provide functions for modifying/saving scales
     */
    public static final class ManagedMapScale {

        private final AutoUpdatingFileChangedListener listener = new AutoUpdatingFileChangedListener() {
            @Override
            public void fileChanged(AutoUpdatingLocalizationFile file) {
                loadBundleXml();
            }
        };

        private final String displayName;

        private final PartId[] partIds;

        private final AutoUpdatingLocalizationFile scaleFile;

        private String bundleXml;

        private final boolean isCustom;

        private ManagedMapScale(String baseDir, MapScale scale)
                throws SerializationException {
            this.isCustom = false;
            LocalizationFile file = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(
                            baseDir + IPathManager.SEPARATOR
                                    + scale.getFileName());

            if (file == null || !file.exists()) {
                throw new IllegalStateException(
                        "scalesInfo.xml references missing file "
                                + scale.getFileName());
            }
            this.scaleFile = new AutoUpdatingLocalizationFile(file);
            this.scaleFile.addListener(listener);
            this.partIds = scale.getPartIds();
            this.displayName = scale.getDisplayName();
            loadBundleXml();
        }

        private ManagedMapScale(String displayName, Bundle scaleBundle)
                throws SerializationException {
            this.isCustom = true;
            this.displayName = displayName;
            this.partIds = new PartId[0];
            this.scaleFile = null;
            this.bundleXml = ProcedureXmlManager.getInstance().marshal(
                    scaleBundle);
        }

        private void loadBundleXml() {
            try {
                this.bundleXml = new String(scaleFile.getFile().read());
            } catch (LocalizationException e) {
                // Ignore, error will be reported in getScaleBundle
            }
        }

        /**
         * Gets the
         * 
         * @return
         * @throws SerializationException
         */
        public Bundle getScaleBundle() throws SerializationException {
            if (bundleXml != null) {
                long t0 = System.currentTimeMillis();
                try {
                    return ProcedureXmlManager.getInstance().unmarshal(
                            Bundle.class, bundleXml);
                } finally {
                    System.out.println("Time to create Bundle: "
                            + (System.currentTimeMillis() - t0) + "ms");
                }
            } else {
                throw new SerializationException(
                        "Scale Bundle XML could not be read");
            }
        }

        public String getDisplayName() {
            return displayName;
        }

        public PartId[] getPartIds() {
            return partIds;
        }

        public boolean isCustomScale() {
            return isCustom;
        }

        @Override
        public String toString() {
            return "MapScale [displayName=" + displayName + ", fileName="
                    + scaleFile.getFilePath() + "]";
        }

    }

    private static SingleTypeJAXBManager<MapScales> jaxbManager;

    private static MapScalesManager DEFAULT_MANAGER;

    private final AutoUpdatingFileChangedListener listener = new AutoUpdatingFileChangedListener() {
        @Override
        public void fileChanged(AutoUpdatingLocalizationFile file) {
            try {
                loadMapScales();
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    };

    private final AutoUpdatingLocalizationFile scalesFile;

    private final String scaleBundleDir;

    // TODO: Possibly have a Map<String,ManagedMapScale> to handle duplicates
    private Collection<ManagedMapScale> storedScales = new ArrayList<ManagedMapScale>();

    // TODO: Need to figure out best way to create custom scales (depends on
    // maps loaded so it can't be at the projection dialog level)
    private final Collection<ManagedMapScale> customScales = new ArrayList<ManagedMapScale>();

    /**
     * Construct a MapScalesManager for the given scales file. File must be
     * deserializable into a {@link MapScales} object. IPathManager will be used
     * to lookup file
     * 
     * @param scalesDir
     *            directory which scalesFile and bundles references in
     *            scalesFile are relative to
     * @param scalesFile
     *            XML file that deserializes into {@link MapScales} object
     * @throws SerializationException
     */
    public MapScalesManager(String scalesDir, String scalesFile)
            throws SerializationException {
        this(scalesDir, PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
                        scalesDir + IPathManager.SEPARATOR + scalesFile));
    }

    /**
     * Construct a MapScalesManager for the given scales file. File must be
     * deserializable into a {@link MapScales} object
     * 
     * @param bundleDir
     *            directory bundle files are relative to
     * @param scalesFile
     * @throws SerializationException
     */
    public MapScalesManager(String bundleDir, LocalizationFile scalesFile)
            throws SerializationException {
        this.scaleBundleDir = bundleDir;
        this.scalesFile = new AutoUpdatingLocalizationFile(scalesFile);
        this.scalesFile.addListener(listener);
        loadMapScales();
    }

    private synchronized void loadMapScales() throws SerializationException {
        List<ManagedMapScale> storedScales = new ArrayList<ManagedMapScale>();
        MapScales scales = this.scalesFile.loadObject(getJAXBManager(),
                MapScales.class);
        for (MapScale scale : scales.getScales()) {
            storedScales.add(new ManagedMapScale(scaleBundleDir, scale));
        }
        this.storedScales = storedScales;
    }

    /**
     * Loads the {@link ManagedMapScale}s referenced to an editor part id onto
     * the window
     * 
     * @param window
     * @param scales
     * @throws VizException
     */
    public void loadEditorScales(IWorkbenchWindow window,
            ManagedMapScale... scales) throws VizException {
        if (scales == null || scales.length == 0) {
            scales = getScales();
        }
        Procedure procedure = new Procedure();
        List<Bundle> bundles = new ArrayList<Bundle>();
        for (ManagedMapScale scale : scales) {
            String editorId = null;
            for (PartId partId : scale.getPartIds()) {
                if (partId.isView() == false) {
                    editorId = partId.getId();
                    break;
                }
            }
            if (editorId != null) {
                try {
                    Bundle b = scale.getScaleBundle();
                    b.setEditor(editorId);
                    bundles.add(b);
                } catch (SerializationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error getting bundle for scale: " + scale, e);
                }
            }
        }
        procedure.setBundles(bundles.toArray(new Bundle[bundles.size()]));
        LoadSerializedXml.loadProcedureToScreen(procedure, window);
    }

    /**
     * Gets all {@link ManagedMapScale}
     * 
     * @return
     */
    public ManagedMapScale[] getScales() {
        Collection<ManagedMapScale> stored = storedScales;
        Collection<ManagedMapScale> custom = customScales;
        ManagedMapScale[] scales = new ManagedMapScale[stored.size()
                + custom.size()];
        int i = 0;
        for (ManagedMapScale scale : stored) {
            scales[i++] = scale;
        }
        for (ManagedMapScale scale : custom) {
            scales[i++] = scale;
        }
        return scales;
    }

    /**
     * Gets a {@link ManagedMapScale} by scale name
     * 
     * @param name
     * @return
     */
    public ManagedMapScale getScaleByName(String name) {
        // Search for scales by name, search custom first.
        for (ManagedMapScale scale : customScales) {
            if (scale.getDisplayName().equals(name)) {
                return scale;
            }
        }
        for (ManagedMapScale scale : storedScales) {
            if (scale.getDisplayName().equals(name)) {
                return scale;
            }
        }
        // Scale not found
        return null;
    }

    /**
     * Gets the {@link ManagedMapScale}s defined for the partId
     * 
     * @param partId
     * @return
     */
    public ManagedMapScale[] getScalesForPart(String partId) {
        List<ManagedMapScale> scalesForPart = new ArrayList<ManagedMapScale>();
        for (ManagedMapScale scale : storedScales) {
            for (PartId part : scale.getPartIds()) {
                if (partId.equals(part.getId())) {
                    scalesForPart.add(scale);
                    break;
                }
            }
        }
        return scalesForPart.toArray(new ManagedMapScale[0]);
    }

    /**
     * Adds a custom scale to be managed. Scale is only active while program is
     * running.
     * 
     * TODO: Need to figure out where map resources come from for this to work
     * properly... When using CreateProjectionDialog we just have a
     * GeneralGridGeometry
     * 
     * @param scaleName
     * @param display
     */
    public void addCustomScale(String scaleName,
            MapScaleRenderableDisplay display) {
        Bundle bundle = new Bundle();
        bundle.setDisplays(new AbstractRenderableDisplay[] { display });
        bundle.setName(scaleName);

        try {
            customScales.add(new ManagedMapScale(scaleName, bundle));
        } catch (SerializationException e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error adding custom scale (" + scaleName + "): "
                            + e.getLocalizedMessage(), e);
        }
    }

    public static synchronized MapScalesManager getInstance() {
        if (DEFAULT_MANAGER == null) {
            try {
                DEFAULT_MANAGER = new MapScalesManager(DEFAULT_SCALES_DIR,
                        DEFAULT_SCALES_FILE);
            } catch (SerializationException e) {
                throw new IllegalStateException(
                        "Unable to construct the MapScalesManager using the default file: "
                                + DEFAULT_SCALES_FILE);
            }
        }
        return DEFAULT_MANAGER;
    }

    private static synchronized JAXBManager getJAXBManager()
            throws SerializationException {
        if (jaxbManager == null) {
            try {
                jaxbManager = new SingleTypeJAXBManager<MapScales>(
                        MapScales.class);
            } catch (JAXBException e) {
                throw new SerializationException(
                        "Error constructing JAXBManager for MapScales", e);
            }
        }
        return jaxbManager;
    }
}
