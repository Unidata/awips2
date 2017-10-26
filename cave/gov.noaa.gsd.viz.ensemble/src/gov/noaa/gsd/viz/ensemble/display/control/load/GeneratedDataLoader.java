package gov.noaa.gsd.viz.ensemble.display.control.load;

import gov.noaa.gsd.viz.ensemble.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.TimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResourceData;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResourceData;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResourceData;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Loads the generated ensemble resource(s) into active or specified display
 * editor and panel. The resource type is dependent upon the display type. The
 * resource(s) will be registered in the Resource manager;
 * 
 * @author jing
 * @author polster
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 2014        5056      jing    Initial creation
 * Jan 15 2016     12301     jing    Added distribution feature
 * 
 * </pre>
 */

public class GeneratedDataLoader {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeneratedDataLoader.class);

    public enum GeneratedloadMode {
        SAME_UNIT, SAME_UNIT_AND_LEVEL
    }

    private GeneratedloadMode generatedloadMode = GeneratedloadMode.SAME_UNIT_AND_LEVEL;

    private EnsembleToolLayer toolLayer = null;

    // levels includes: Level + Parameter (e.g. 500MB)
    private List<String> levels;

    private List<String> units;

    public GeneratedDataLoader(EnsembleToolLayer tl, GeneratedloadMode glm) {
        generatedloadMode = glm;
        levels = new ArrayList<String>();
        units = new ArrayList<String>();
        toolLayer = tl;
    }

    /**
     * Load into current active editor and panel of Plan-View
     * 
     * @param calculator
     *            - the calculator of the loading overlay.
     */
    public void loadToMapEditor(final EnsembleCalculator calculator) {

        if (!(toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
            return;
        }

        searchLoadedResourcesMapEditor();
        if (units.isEmpty()) {
            return;
        }

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            for (final String level : levels) {
                if ((level == null) || (level.length() <= 0)) {
                    continue;
                }
                for (final String unit : units) {
                    // load to the map editor
                    if ((unit == null) || (unit.length() <= 0)) {
                        continue;
                    } else {
                        /**
                         * Should search if there is any resource for the level
                         * and unit then load a calculator resource, otherwise
                         * maybe a problem
                         * 
                         **/
                        // Same level and unit case
                        Map<String, List<AbstractResourceHolder>> dataHolders = new ConcurrentHashMap<>();
                        try {
                            dataHolders = EnsembleResourceManager
                                    .getInstance()
                                    .getResourceList(toolLayer)
                                    .getUserLoadedRscs(
                                            (IDescriptor) new MapDescriptor(),
                                            true, level, unit);
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                            continue;
                        }
                        if (!dataHolders.isEmpty()) {
                            LoadGeneratedResourceToMapEditorJob ccj = new LoadGeneratedResourceToMapEditorJob(
                                    "Load Generated Resource To Map Editor",
                                    calculator, level, unit);
                            ccj.setPriority(Job.SHORT);
                            ccj.schedule();
                        }
                    }

                }
            }
        }
    }

    /**
     * Load into current active editor and panel of Time Series
     * 
     * @param calculator
     *            - the calculator of the loading overlay.
     */
    public void loadToTimeSeriesEditor(final EnsembleCalculator calculator) {

        if (!(toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof TimeSeriesDescriptor))
            return;

        searchLoadedResourcesTimeSeriesEditor();

        if (units.isEmpty())
            return;

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            for (final String level : levels) {
                if ((level == null) || (level.length() <= 0)) {
                    continue;
                }
                for (final String unit : units) {
                    // load to the time series editor
                    if ((unit == null) || (unit.length() <= 0)) {
                        continue;
                    } else {
                        LoadGeneratedResourceToTimeSeriesEditorJob ccj = new LoadGeneratedResourceToTimeSeriesEditorJob(
                                "Load Generated Resource To Time Series Editor",
                                calculator, level, unit);
                        ccj.setPriority(Job.SHORT);
                        ccj.schedule();
                    }
                }
            }
        }
    }

    // Load into current active editor and panel
    // of ...

    /**
     * Load into editor
     * 
     * @param calculator
     */
    public void load(final EnsembleCalculator calculator) {
        if (toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof TimeSeriesDescriptor) {
            loadToTimeSeriesEditor(calculator);
        } else if (toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor) {
            loadToMapEditor(calculator);
        }
    }

    /**
     * Load a overlay, such the histogram,,fire weather...
     * 
     * @param overlay
     */
    public void loadOverlay(final Calculation overlay) {

        if (!(toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
            return;
        }
        searchLoadedResourcesMapEditor();
        if (units.isEmpty()) {
            return;
        }

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            for (final String level : levels) {
                if ((level == null) || (level.length() <= 0)) {
                    continue;
                }
                for (final String unit : units) {
                    if ((unit == null) || (unit.length() <= 0)) {
                        continue;
                    } else {
                        /**
                         * Should search if there is any resource for the level
                         * and unit then load a calculator resource, otherwise
                         * maybe a problem
                         * 
                         **/
                        // Same level and unit case
                        Map<String, List<AbstractResourceHolder>> dataHolders = new ConcurrentHashMap<>();
                        try {
                            dataHolders = EnsembleResourceManager
                                    .getInstance()
                                    .getResourceList(toolLayer)
                                    .getUserLoadedRscs(
                                            (IDescriptor) new MapDescriptor(),
                                            true, level, unit);
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                            continue;
                        }
                        if (!dataHolders.isEmpty()) {
                            LoadOverlayJob ccj = new LoadOverlayJob(
                                    "Load Overlay Resource", overlay, level,
                                    unit);
                            ccj.setPriority(Job.SHORT);
                            ccj.schedule();
                        }
                    }

                }
            }
        }

    }

    /**
     * Load into current active panel of all editors in the main window
     * 
     * @param calculator
     */
    public void loadToMultipleEditors(EnsembleCalculator calculator) {
        // TODO
        // searchLoadedResources();
    }

    /**
     * Load into all panels of current active editor. Each panel may with
     * different display type, such as Plan-view and Time-Series. This is for
     * supporting interactive * @param calculator
     */
    public void loadToMultiplePanels(EnsembleCalculator calculator) {

        // TODO
        // searchLoadedResources();
    }

    /**
     * Get the load mode of the generated resource.
     * 
     * @return
     */
    public GeneratedloadMode getGeneratedloadMode() {
        return generatedloadMode;
    }

    /**
     * Set the load mode of the generated resource.
     * 
     * @param generatedloadMode
     *            - load mode
     */
    public void setGeneratedloadMode(GeneratedloadMode generatedloadMode) {
        this.generatedloadMode = generatedloadMode;
    }

    /**
     * Get all levels of the resources.
     * 
     * @return- levels string list
     */
    public List<String> getLevels() {
        return levels;
    }

    /**
     * set levels of the resources.
     * 
     * @param levels
     */
    public void setLevel(List<String> levels) {
        this.levels = levels;
    }

    /**
     * Get units of the resources.
     * 
     * @return
     */
    public List<String> getUnits() {
        return units;
    }

    /**
     * Set units of the resources.
     * 
     * @param units
     */
    public void setUnits(List<String> units) {
        this.units = units;
    }

    /**
     * Based current generated ensemble load mode, search the level and unit in
     * the loaded resource, which are used to decide the generated ensemble
     * resource(s)
     */
    private void searchLoadedResourcesMapEditor() {
        units.clear();
        levels.clear();

        if (EnsembleResourceManager.getInstance().getResourceList(toolLayer) == null) {
            return;
        }

        List<AbstractResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(toolLayer).getUserLoadedRscs();

        if (rscs == null || rscs.isEmpty()) {
            return;
        }

        // Search levels and units in the current loaded resource
        for (AbstractResourceHolder gr : rscs) {

            // TODO: How about resource with other descriptors?
            if (gr.getSpecificName() == null || gr.getSpecificName().equals(""))
                continue;

            // levels.add(..)
            String level = gr.getLevel();
            if (!levels.contains(level)) {
                levels.add(level);
            }

            // units.add(...)
            String unit = gr.getUnits();
            if (!units.contains(unit)) {
                units.add(unit);
            }
        }
    }

    /**
     * Search the levels and units in the loaded resources
     */
    private void searchLoadedResourcesTimeSeriesEditor() {
        levels.clear();
        units.clear();

        if (EnsembleResourceManager.getInstance().getResourceList(toolLayer) == null) {
            return;
        }

        List<AbstractResourceHolder> rscs = EnsembleResourceManager
                .getInstance().getResourceList(toolLayer).getUserLoadedRscs();

        if (rscs == null || rscs.isEmpty()) {
            return;
        }

        // Search levels and units in the current loaded resource
        for (AbstractResourceHolder gr : rscs) {

            if (gr instanceof TimeSeriesResourceHolder) {

                if (gr.getRsc().getName() == null || gr.getRsc().equals(""))
                    continue;

                // levels.add(..)
                String level = gr.getLevel();
                if (!levels.contains(level)) {
                    levels.add(level);
                }

                // units.add(...)
                String unit = gr.getUnits();
                if (!units.contains(unit))
                    units.add(unit);

            }
        }
    }

    /*
     * This loads overlay product like sampling.
     */
    protected class LoadOverlayJob extends Job {

        private Calculation overlay = null;

        private String unit = null;

        private String level = null;

        public LoadOverlayJob(String name, final Calculation ov,
                final String lev, final String un) {
            super(name);
            overlay = ov;
            level = lev;
            unit = un;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = Status.CANCEL_STATUS;

            if (!(toolLayer.getEditor().getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
                return Status.CANCEL_STATUS;
            }

            if (overlay == Calculation.VALUE_SAMPLING) {
                // how to pass the level and unit into the
                // HistogramResource
                HistogramResourceData resourceData = new HistogramResourceData(
                        toolLayer, level, unit,
                        HistogramResource.DisplayMode.POINT_SAMPLING);

                LoadProperties loadProperties = new LoadProperties();

                try {
                    resourceData
                            .construct(loadProperties, toolLayer.getEditor()
                                    .getActiveDisplayPane().getDescriptor());
                    status = Status.OK_STATUS;
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

            } else if (overlay == Calculation.HISTOGRAM_SAMPLING) {
                // how to pass the level and unit into the
                // HistogramResource
                HistogramResourceData resourceData = new HistogramResourceData(
                        toolLayer, level, unit,
                        HistogramResource.DisplayMode.HISTOGRAM_SAMPLING);

                LoadProperties loadProperties = new LoadProperties();

                try {
                    resourceData
                            .construct(loadProperties, toolLayer.getEditor()
                                    .getActiveDisplayPane().getDescriptor());
                    status = Status.OK_STATUS;
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

            } else if (overlay == Calculation.HISTOGRAM_GRAPHICS) {
                // how to pass the level and unit into the
                // HistogramResource
                HistogramResourceData resourceData = new HistogramResourceData(
                        toolLayer, level, unit,
                        HistogramResource.DisplayMode.GRAPHIC_HISTGRAM);

                LoadProperties loadProperties = new LoadProperties();

                try {
                    resourceData
                            .construct(loadProperties, toolLayer.getEditor()
                                    .getActiveDisplayPane().getDescriptor());
                    status = Status.OK_STATUS;
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

            }
            toolLayer.getEditor().refresh();

            return status;
        }
    }

    /*
     * This loads generated calculation products.
     */
    protected class LoadGeneratedResourceToMapEditorJob extends Job {

        EnsembleCalculator calculator = null;

        private String unit = null;

        private String level = null;

        public LoadGeneratedResourceToMapEditorJob(String name,
                final EnsembleCalculator calc, final String lev, final String un) {
            super(name);
            calculator = calc;
            level = lev;
            unit = un;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = Status.CANCEL_STATUS;

            AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                    .getInstance().getActiveEditor();
            if (!(theEditor.getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
                return Status.CANCEL_STATUS;
            }

            for (ResourcePair pair : theEditor.getActiveDisplayPane()
                    .getDescriptor().getResourceList()) {
                if (pair.getResourceData() instanceof GeneratedEnsembleGridResourceData) {
                    GeneratedEnsembleGridResourceData grd = (GeneratedEnsembleGridResourceData) pair
                            .getResourceData();

                    /*
                     * If there is already a generated resource having the same
                     * calculation, level and unit then unload the existing
                     * resource.
                     */
                    if (grd.getLevel().equals(level)
                            && grd.getUnit().equals(unit)) {
                        if (grd.getCalculator().getCalculation() == calculator
                                .getCalculation()) {
                            pair.getResource().unload();
                        }
                    }

                }
            }

            GeneratedEnsembleGridResourceData ensembleData = new GeneratedEnsembleGridResourceData(
                    toolLayer, calculator, theEditor.getActiveDisplayPane()
                            .getDescriptor(), level, unit);

            LoadProperties loadProperties = new LoadProperties();

            try {
                ensembleData.construct(loadProperties, theEditor
                        .getActiveDisplayPane().getDescriptor());
                status = Status.OK_STATUS;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            theEditor.refresh();

            return status;
        }
    }

    /*
     * This loads generated calculation products.
     */
    protected class LoadGeneratedResourceToTimeSeriesEditorJob extends Job {

        EnsembleCalculator calculator = null;

        private String unit = null;

        private String level = null;

        public LoadGeneratedResourceToTimeSeriesEditorJob(String name,
                final EnsembleCalculator calc, final String lev, final String un) {
            super(name);
            calculator = calc;
            level = lev;
            unit = un;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IStatus status = Status.CANCEL_STATUS;
            AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                    .getInstance().getActiveEditor();
            if (!(theEditor.getActiveDisplayPane().getDescriptor() instanceof TimeSeriesDescriptor)) {
                return status;
            }

            for (ResourcePair pair : theEditor.getActiveDisplayPane()
                    .getDescriptor().getResourceList()) {

                /*
                 * If there is already a generated resource having the same
                 * calculation, level and unit then unload the existing
                 * resource.
                 */
                if (pair.getResourceData() instanceof GeneratedTimeSeriesResourceData) {
                    GeneratedTimeSeriesResourceData tsd = (GeneratedTimeSeriesResourceData) pair
                            .getResourceData();
                    if (tsd.getLevel().equals(level)
                            && tsd.getUnit().equals(unit)) {
                        if (tsd.getCalculator().getCalculation() == calculator
                                .getCalculation()) {
                            pair.getResource().unload();
                        }
                    }
                }
            }

            GeneratedTimeSeriesResourceData ensembleData = new GeneratedTimeSeriesResourceData(
                    toolLayer, calculator, level, unit);

            LoadProperties loadProperties = new LoadProperties();

            try {
                ensembleData.construct(loadProperties, theEditor
                        .getActiveDisplayPane().getDescriptor());
                status = Status.OK_STATUS;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            theEditor.refresh();

            return status;
        }
    }

}
