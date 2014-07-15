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
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
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
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
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
 * Mar 24, 2014  2954     mpduff      Log when missing map scale files
 * Jul 15, 2014  2954     njensen     Added fallbacks when missing map scale files
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

    // TODO would be better to fall back to a worldwide display
    private static final String LAST_RESORT_NAME = "Northern Hemisphere";

    private static final String LAST_RESORT_FILENAME = "NHemisphere.xml";

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

        private PartId[] partIds;

        private final AutoUpdatingLocalizationFile scaleFile;

        private String bundleXml;

        private final boolean isCustom;

        private ManagedMapScale(String baseDir, MapScale scale)
                throws IllegalStateException, SerializationException {
            this.isCustom = false;
            this.partIds = scale.getPartIds();
            this.displayName = scale.getDisplayName();

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
            loadBundleXml();

            /*
             * TODO this is inefficient to unmarshal it eagerly for no purpose
             * other than to validate, but it ensures that if the files exist
             * but have bad XML, then the thrown exception from getScaleBundle()
             * will cause the fallback code to be triggered, leading to no blank
             * panes
             */

            // validate the XML is good
            getScaleBundle();
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
                MapScales scales = file.loadObject(getJAXBManager(),
                        MapScales.class);
                loadMapScales(scales);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    };

    private AutoUpdatingLocalizationFile scalesFile;

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
        String filename = scalesDir + IPathManager.SEPARATOR + scalesFile;
        LocalizationFile locFile = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
filename);
        MapScales scales = null;
        try {
            this.scalesFile = new AutoUpdatingLocalizationFile(locFile);
            scales = this.scalesFile.loadObject(getJAXBManager(),
                    MapScales.class);
        } catch (SerializationException e) {
            /*
             * failed to load scalesInfo file, try and fall back to BASE
             */
            if (!locFile.getContext().getLocalizationLevel()
                    .equals(LocalizationLevel.BASE)) {
                locFile = PathManagerFactory.getPathManager()
                        .getLocalizationFile(
                                new LocalizationContext(
                                        LocalizationType.CAVE_STATIC,
                                        LocalizationLevel.BASE), filename);
                this.scalesFile = new AutoUpdatingLocalizationFile(locFile);
                scales = this.scalesFile.loadObject(getJAXBManager(),
                        MapScales.class);
            } else {
                throw e;
            }
        }
        this.scaleBundleDir = scalesDir;
        this.scalesFile.addListener(listener);
        loadMapScales(scales);
    }

    private synchronized void loadMapScales(MapScales scales)
 {
        List<ManagedMapScale> storedScales = new ArrayList<ManagedMapScale>();
        List<PartId> failedParts = new ArrayList<PartId>();
        for (MapScale scale : scales.getScales()) {
            try {
                storedScales.add(new ManagedMapScale(scaleBundleDir, scale));
            } catch (Exception e) {
                StringBuilder sb = new StringBuilder();
                sb.append("Error loading " + scale.getDisplayName()
                        + " scale. ");
                if (scale.getPartIds() != null && scale.getPartIds().length > 0) {
                    sb.append(scale.getDisplayName()
                            + " pane will attempt to revert to working scale. ");
                }
                sb.append(scale.getDisplayName()
                        + " will not appear in the menu. ");
                statusHandler.error(sb.toString(), e);
                if (scale.getPartIds() != null) {
                    for (PartId p : scale.getPartIds()) {
                        failedParts.add(p);
                    }
                }
            }
        }
        this.storedScales = storedScales;

        // storedScales must be set before handleMissingParts()
        if (!failedParts.isEmpty()) {
            handleMissingParts(failedParts);
        }
    }

    /**
     * Handles the parts that referenced scales files that couldn't be found.
     * 
     * @param missingParts
     *            the parts that were missing
     * @param goodScales
     *            the scales that successfully loaded
     */
    protected void handleMissingParts(List<PartId> missingParts) {
        /*
         * if the missing part was a side view, fall back to the main pane
         */
        ManagedMapScale mainPane = findEditorScale();

        if (mainPane == null) {
            /*
             * main pane is missing too, so fall back to a base scale that is
             * guaranteed to be there
             */
            mainPane = getLastResortScale();
            storedScales.add(mainPane);
        }

        /*
         * Set all the missing parts to the scale in the main pane
         */
        List<PartId> combinedParts = new ArrayList<PartId>(
                mainPane.partIds.length + missingParts.size());
        for (PartId p : mainPane.getPartIds()) {
            combinedParts.add(p);
        }
        for (PartId p : missingParts) {
            combinedParts.add(p);
        }
        mainPane.partIds = combinedParts.toArray(new PartId[0]);
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
     * Gets the Bundle defined for the partId.
     * 
     * @param partId
     * @return
     */
    public Bundle getScaleBundleForPart(String partId) {
        List<ManagedMapScale> scalesForPart = new ArrayList<ManagedMapScale>();
        for (ManagedMapScale scale : storedScales) {
            for (PartId part : scale.getPartIds()) {
                if (partId.equals(part.getId())) {
                    scalesForPart.add(scale);
                    break;
                }
            }
        }

        Bundle b = null;
        for (ManagedMapScale scale : scalesForPart) {
            try {
                b = scale.getScaleBundle();
            } catch (SerializationException e) {
                statusHandler.error("Error deserializing bundle for scale: "
                        + scale, e);
            }

            if (b != null) {
                break;
            }
        }

        if (b != null) {
            b.setView(partId);
        }

        return b;
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

    /**
     * Gets a base scale that should always be there and should always work.
     * Used to prevent blank panes if scale overrides are misconfigured.
     * 
     * @return
     */
    protected ManagedMapScale getLastResortScale() {
        ManagedMapScale scale = null;
        PartId fallbackPartId = new PartId();
        fallbackPartId.setId(VizMapEditor.EDITOR_ID);
        fallbackPartId.setView(false);
        MapScale fallback = new MapScale();
        fallback.setPartIds(new PartId[] { fallbackPartId });
        fallback.setDisplayName(LAST_RESORT_NAME);
        fallback.setFileName(LAST_RESORT_FILENAME);
        try {
            scale = new ManagedMapScale(scaleBundleDir, fallback);
        } catch (Exception e) {
            statusHandler.fatal("Error loading the last resort scale "
                    + LAST_RESORT_FILENAME, e);
            // things will null pointer if this ever hits, but come on
        }
        return scale;
    }

    /**
     * Finds the editor pane (ie VizMapEditor.EDITOR_ID) associated with a list
     * of scales
     * 
     * @return the first scale tied to a VizMapEditor, or null if none are found
     */
    public ManagedMapScale findEditorScale() {
        ManagedMapScale editorScale = null;
        for (ManagedMapScale scale : storedScales) {
            PartId[] parts = scale.partIds;
            if (parts != null) {
                for (PartId p : parts) {
                    if (VizMapEditor.EDITOR_ID.equals(p.getId())) {
                        editorScale = scale;
                        break;
                    }
                }
            }
            if (editorScale != null) {
                break;
            }
        }
        return editorScale;
    }
}
