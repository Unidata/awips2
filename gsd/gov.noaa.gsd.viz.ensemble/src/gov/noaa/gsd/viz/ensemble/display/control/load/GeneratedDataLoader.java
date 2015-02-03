package gov.noaa.gsd.viz.ensemble.display.control.load;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;
import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.TimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResourceData;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResourceData;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResourceData;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
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

    private AbstractEditor editor = null;

    // levels includes: Level + Parameter (e.g. 500MB)
    private List<String> levels;

    private List<String> units;

    public GeneratedDataLoader(AbstractEditor e, GeneratedloadMode glm) {
        generatedloadMode = glm;
        levels = new ArrayList<String>();
        units = new ArrayList<String>();
        editor = e;
    }

    /**
     * Load into current active editor and panel of Plan-View
     * 
     * @param calculator
     *            - the calculator of the loading overlay.
     */
    public void loadToMapEditor(final EnsembleCalculator calculator) {

        AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                .getInstance().getActiveEditor();
        if (!(theEditor.getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
            return;
        }

        searchLoadedResourcesMapEditor(theEditor);
        if (units.isEmpty()) {
            return;
        }

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            Thread t = null;
            for (final String level : levels) {
                for (final String unit : units) {
                    // load to the map editor
                    t = new Thread() {
                        public void run() {
                            AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                                    .getInstance().getActiveEditor();
                            if (!(theEditor.getActiveDisplayPane()
                                    .getDescriptor() instanceof IMapDescriptor))
                                return;
                            GeneratedEnsembleGridResourceData ensembleData = new GeneratedEnsembleGridResourceData(
                                    calculator,

                                    theEditor.getActiveDisplayPane()
                                            .getDescriptor(), level, unit);

                            LoadProperties loadProperties = new LoadProperties();

                            try {
                                ensembleData.construct(loadProperties,
                                        theEditor.getActiveDisplayPane()
                                                .getDescriptor());
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }

                            theEditor.refresh();
                        }
                    };
                    t.start();

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

        AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                .getInstance().getActiveEditor();
        if (!(theEditor.getActiveDisplayPane().getDescriptor() instanceof TimeSeriesDescriptor))
            return;

        searchLoadedResourcesTimeSeriesEditor(theEditor);

        if (units.isEmpty())
            return;

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            Thread t = null;
            for (final String level : levels) {
                for (final String unit : units) {
                    // load to the map editor
                    t = new Thread() {
                        public void run() {
                            // Temp solution!!!!
                            AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                                    .getInstance().getActiveEditor();
                            if (!(theEditor.getActiveDisplayPane()
                                    .getDescriptor() instanceof TimeSeriesDescriptor))
                                return;
                            GeneratedTimeSeriesResourceData ensembleData = new GeneratedTimeSeriesResourceData(
                                    calculator, level, unit);

                            LoadProperties loadProperties = new LoadProperties();

                            try {
                                ensembleData.construct(loadProperties,
                                        theEditor.getActiveDisplayPane()
                                                .getDescriptor());
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }

                            theEditor.refresh();
                        }
                    };
                    t.start();
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
        if (editor.getActiveDisplayPane().getDescriptor() instanceof TimeSeriesDescriptor) {
            loadToTimeSeriesEditor(calculator);
        } else if (editor.getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor) {
            loadToMapEditor(calculator);
        }
    }

    /**
     * Load a overlay, such the histogram,,fire weather...
     * 
     * @param overlay
     */
    public void loadOverlay(final Calculation overlay) {

        AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                .getInstance().getActiveEditor();
        if (!(theEditor.getActiveDisplayPane().getDescriptor() instanceof IMapDescriptor)) {
            return;
        }
        searchLoadedResourcesMapEditor(theEditor);
        if (units.isEmpty()) {
            return;
        }

        if (generatedloadMode == GeneratedloadMode.SAME_UNIT_AND_LEVEL
                && !levels.isEmpty()) {

            Thread t = null;
            for (final String level : levels) {
                for (final String unit : units) {
                    // load to the map editor
                    t = new Thread() {
                        public void run() {
                            AbstractEditor theEditor = (AbstractEditor) VizWorkbenchManager
                                    .getInstance().getActiveEditor();
                            if (!(theEditor.getActiveDisplayPane()
                                    .getDescriptor() instanceof IMapDescriptor)) {
                                return;
                            }

                            if (overlay == Calculation.HISTOGRAM_SAMPLING) {
                                // how to pass the level and unit into the
                                // HistogramResource
                                HistogramResourceData resourceData = new HistogramResourceData(
                                        theEditor.getActiveDisplayPane()
                                                .getDescriptor(), level, unit,
                                        HistogramResource.DisplayMode.SAMPLING);

                                LoadProperties loadProperties = new LoadProperties();

                                try {
                                    resourceData.construct(loadProperties,
                                            theEditor.getActiveDisplayPane()
                                                    .getDescriptor());
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e.getLocalizedMessage(), e);
                                }

                            } else if (overlay == Calculation.HISTOGRAM_TEXT) {
                                // how to pass the level and unit into the
                                // HistogramResource
                                HistogramResourceData resourceData = new HistogramResourceData(
                                        theEditor.getActiveDisplayPane()
                                                .getDescriptor(),
                                        level,
                                        unit,
                                        HistogramResource.DisplayMode.TEXT_HISTGRAM);

                                LoadProperties loadProperties = new LoadProperties();

                                try {
                                    resourceData.construct(loadProperties,
                                            theEditor.getActiveDisplayPane()
                                                    .getDescriptor());
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e.getLocalizedMessage(), e);
                                }

                            }

                            theEditor.refresh();
                        }
                    };
                    t.start();
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
    private void searchLoadedResourcesMapEditor(AbstractEditor editor) {
        levels.clear();
        units.clear();
        List<GenericResourceHolder> rscs = EnsembleResourceManager.instance
                .getResourceList(editor).getUserLoadedRscs();

        // Search levels and units in the current loaded resource
        for (GenericResourceHolder gr : rscs) {

            // TODO: How about resource with other descriptors?
            if (gr.getUniqueName() == null || gr.getUniqueName().equals(""))
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
     * 
     * @param editor
     */
    private void searchLoadedResourcesTimeSeriesEditor(AbstractEditor editor) {
        levels.clear();
        units.clear();
        List<GenericResourceHolder> rscs = EnsembleResourceManager.instance
                .getResourceList(editor).getUserLoadedRscs();

        // Search levels and units in the current loaded resource
        for (GenericResourceHolder gr : rscs) {

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

}
