package gov.noaa.gsd.viz.ensemble.display.rsc.histogram;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.display.chart.SingleSampleInfo;

/**
 * D2D ensemble sampling resources, supports all pane sampling and left-click
 * sampling as well. Implement steps: 1) Ensemble text value sampling; 2) Text
 * histogram (no color text yet), simple distribution over main display window;
 * 3) Graphics histogram/distribution view; 4) Interactive display in
 * distribution view for multiple chart styles. Implemented PDF and CDF charts
 * only in this version, as the basic probability forecasting tool. 5) A lot of
 * tools can be developed on this framework in the later release.
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July, 2014     5056       jing       Initial creation
 * Jan 12 2016    12301      jing       Added the distribution view tool
 * Mar 17 2017    19325      jing       Resource group behavior added
 * 
 *          </pre>
 */

public class HistogramResource<HistogramResoureData>
        extends EnsSamplingResource {

    public enum DisplayMode {
        POINT_SAMPLING, HISTOGRAM_SAMPLING, COLOR_TEXT_HISTGRAM, GRAPHIC_HISTOGRAM
    }

    private String level;

    private String unit;

    protected Random rand;

    protected DisplayMode mode;

    /**
     * Constructor of this class
     * 
     * 
     * @param histogramResourceData
     *            : The resource data for the histogram
     * @param loadProperties
     *            : load properties.
     * @param descriptor
     *            : Should MapDescriptor
     * @param level
     *            : a selected products level
     * @param unit
     *            : a selected products unit
     * @param mode
     *            : a selected histogram display mode
     */
    public HistogramResource(HistogramResourceData histogramResourceData,
            LoadProperties loadProperties, IDescriptor descriptor, String level,
            String unit, DisplayMode mode) {
        super(histogramResourceData, loadProperties);
        this.level = level;
        this.unit = unit;
        this.mode = mode;
        this.setSampling(true);
        this.setDescriptor(descriptor);

        ColorableCapability colorable = this
                .getCapability(ColorableCapability.class);
        rand = new Random();
        RGB color = new RGB(rand.nextInt(206) + 50, rand.nextInt(206) + 50,
                rand.nextInt(206) + 50);
        colorable.setColor(color);
    }

    private class D2DMouseAdapter
            extends EnsSamplingInputAdapter<HistogramResource<?>> {

        private static final String INSPECT_PREF_HIST = "com.raytheon.viz.ui.input.inspect.hist";

        protected Job job;

        protected long timeUp;

        private MousePreferenceManager prefManager = MousePreferenceManager
                .getInstance();

        private boolean inspectForced = false;

        D2DMouseAdapter() {
            super(HistogramResource.this);
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            super.handleMouseDown(x, y, mouseButton);
            if (prefManager.handleClick(INSPECT_PREF_HIST, mouseButton)
                    && isSampling() == false) {
                inspectForced = true;
                setSampling(true);
                issueRefresh();
                return false;
            } else if (prefManager.handleLongClick(INSPECT_PREF_HIST,
                    mouseButton) && isSampling() == false) {
                timeUp = 0L;
                if (job == null) {
                    job = new Job("InspectAdapter") {

                        @Override
                        protected IStatus run(IProgressMonitor monitor) {
                            if (timeUp == 0L) {
                                inspectForced = true;
                                setSampling(true);
                                issueRefresh();
                            }
                            return Status.OK_STATUS;
                        }

                    };
                }
                if (job.getState() != Job.RUNNING) {
                    job.schedule(500);
                }
                return false;
            }
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            super.handleMouseUp(x, y, mouseButton);
            if (prefManager.handleLongClick(INSPECT_PREF_HIST, mouseButton)) {
                timeUp = System.currentTimeMillis();
            }
            if (inspectForced) {
                inspectForced = false;
                setSampling(false);
                issueRefresh();
            }

            return false;
        }

    }

    private boolean allPanelSampling = false;

    @Override
    protected IInputHandler getSamplingInputHandler() {
        return new D2DMouseAdapter();
    }

    // @Override
    public void setAllPanelSampling(boolean allPanelSampling) {
        this.allPanelSampling = allPanelSampling;
    }

    public boolean isAllPanelSampling() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container instanceof IMultiPaneEditor) {
            // Only all panel sample if we have 1 displayed pane count
            return (allPanelSampling & (((IMultiPaneEditor) container)
                    .displayedPaneCount() == 1));
        }
        return allPanelSampling;
    }

    /**
     * Generate sample histogram text/graphic with grid resources
     */
    @Override
    protected SampleResult doHover(ReferencedCoordinate coord,
            ResourceList resources) throws VizException {
        SampleResult result = new SampleResult();
        if (mode == DisplayMode.POINT_SAMPLING) {
            result = doHoverSampling(coord, resources);
        } else if (mode == DisplayMode.HISTOGRAM_SAMPLING) {
            result = doHoverText(coord, resources);
        } else if (mode == DisplayMode.GRAPHIC_HISTOGRAM) {
            result = doHoverGraphics(coord, resources);
        }
        return result;
    }

    /**
     * Get the sampling labels and colors of current location
     * 
     * @param coord
     *            - current location
     * @param resources
     *            - member resources of the calculations
     * @return text string of values
     * @throws VizException
     */
    protected SampleResult doHoverSampling(ReferencedCoordinate coord,
            ResourceList resources) throws VizException {
        SampleResult result = new SampleResult();

        List<String> labelList = new ArrayList<String>();
        List<RGB> colorList = new ArrayList<RGB>();

        try {
            int size = resources.size();
            for (int i = size - 1; i >= 0; --i) {
                ResourcePair rp = resources.get(i);
                String retVal = recursiveHoverSearchSampling(rp, coord);
                if (retVal != null && retVal.length() > 0) {
                    RGB color = null;
                    if (rp.getResource()
                            .hasCapability(ColorableCapability.class)) {
                        color = rp.getResource()
                                .getCapability(ColorableCapability.class)
                                .getColor();
                    }
                    int p1, p2;
                    p1 = 0;
                    while ((p2 = retVal.indexOf('\n', p1)) >= 0) {
                        colorList.add(color);
                        labelList.add(retVal.substring(p1, p2));
                        p1 = p2 + 1;
                    }
                    String s = retVal.substring(p1);
                    if (s.length() > 0) {
                        colorList.add(color);
                        labelList.add(retVal.substring(p1));
                    }
                }
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error sampling resources: " + t.getLocalizedMessage(), t);
        }

        result.labels = labelList.toArray(new String[labelList.size()]);
        result.colors = colorList.toArray(new RGB[colorList.size()]);
        return result;

    }

    /**
     * Get the sampling label for one resource.
     * 
     * @param rp
     *            - one resource pair
     * @param coordinate
     *            - current location
     * @return - label string
     * @throws VizException
     */
    private String recursiveHoverSearchSampling(ResourcePair rp,
            ReferencedCoordinate coordinate) throws VizException {
        ResourceProperties props = rp.getProperties();
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (!(rsc instanceof AbstractGridResource))
            return null;

        if (rsc != null && rsc.getStatus() == ResourceStatus.INITIALIZED
                && props.isVisible()) {
            String curVal = null;
            Map<String, Object> result = rsc.interrogate(coordinate);

            if (result == null || result.isEmpty())
                return null;

            Set<String> keys = result.keySet();
            if (keys == null || keys.isEmpty())
                return null;

            for (String key : keys) {
                if (key.contains("unit"))
                    continue;
                if (curVal != null) {

                    curVal = curVal + "/"
                            + String.format("%.2f", result.get(key));
                } else {

                    curVal = String.format("%.2f", result.get(key));
                }
            }

            if (curVal != null && curVal.length() > 0) {
                return curVal;
            }
        }

        return null;
    }

    /**
     * Text histogram display by sampling each member resource and creating a
     * histogram text.
     * 
     * @param coord
     *            - sample location
     * @param resources
     *            -a group grid resources
     * @return sample result for text histogram
     * @throws VizException
     */
    protected SampleResult doHoverText(ReferencedCoordinate coord,
            ResourceList resources) throws VizException {
        SampleResult result = new SampleResult();

        List<String> labelList = new ArrayList<String>();

        List<Float> values = new ArrayList<Float>();
        List<AbstractVizResource<?, ?>> rscs = new ArrayList<AbstractVizResource<?, ?>>();

        try {
            int size = resources.size();
            for (int i = size - 1; i >= 0; --i) {
                ResourcePair rp = resources.get(i);
                float retVal = recursiveHoverSearchText(rp, coord);
                if (retVal != Float.NaN) {
                    values.add(retVal);
                    rscs.add(rp.getResource());
                }
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error sampling resources: " + t.getLocalizedMessage(), t);
        }

        TextHistogram textHistogram = new TextHistogram(false);
        labelList = textHistogram.interrogate(rscs, values, unit);
        result.labels = labelList.toArray(new String[labelList.size()]);

        RGB color = this.getCapability(ColorableCapability.class).getColor();
        RGB[] colorList = new RGB[labelList.size()];
        for (int j = 0; j < colorList.length; j++)
            colorList[j] = color;
        result.colors = colorList;

        return result;

    }

    /**
     * Search for sampling data for the text histogram in one resource pair
     * 
     * @param rp
     *            - a resource pair of a grid data
     * @param coordinate
     *            - location
     * @return sampled value at this location of this grid
     * @throws VizException
     */
    private float recursiveHoverSearchText(ResourcePair rp,
            ReferencedCoordinate coordinate) throws VizException {
        ResourceProperties props = rp.getProperties();
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (!(rsc instanceof AbstractGridResource))
            return Float.NaN;

        float curVal = Float.NaN;
        if (rsc != null && rsc.getStatus() == ResourceStatus.INITIALIZED
                && props.isVisible()) {

            Map<String, Object> result = rsc.interrogate(coordinate);

            if (result == null || result.isEmpty())
                return Float.NaN;

            Set<String> keys = result.keySet();
            if (keys == null || keys.isEmpty())
                return Float.NaN;

            for (String key : keys) {
                if (key.contains("unit")) {
                    this.unit = result.get(key).toString();
                } else if (key.contains("value")) {
                    curVal = Float.parseFloat(result.get(key).toString());
                }
            }
        }

        return curVal;
    }

    /**
     * For displaying distribution view, this method samples each member
     * resource under the cursor, and passes the data set to the distribution
     * view GUI.
     * 
     * @param coord
     *            - location of the cursor
     * @param resources
     *            - a group resources to sample
     * @return null
     * @throws VizException
     */
    protected SampleResult doHoverGraphics(ReferencedCoordinate coord,
            ResourceList resources) throws VizException {

        List<Float> values = new ArrayList<Float>();

        try {
            int size = resources.size();
            for (int i = size - 1; i >= 0; --i) {
                ResourcePair rp = resources.get(i);
                float retVal = recursiveHoverSearchText(rp, coord);
                if (retVal != Float.NaN) {
                    values.add(retVal);

                }
            }
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error sampling resources: " + t.getLocalizedMessage(), t);
        }

        // Create the data information
        // Make legend for the location data
        NumberFormat nf = NumberFormat.getInstance();
        nf.setMaximumFractionDigits(1);
        double x = coord.getObject().x;
        double y = coord.getObject().y;
        String lon = nf.format(Math.abs(x));
        String lat = nf.format(Math.abs(y));
        String location = String.format(" %s%s %s%s", lat, y >= 0 ? "N" : "S",
                lon, x >= 0 ? "E" : "W");

        SingleSampleInfo info = new SingleSampleInfo(" ", location, level, unit,
                values);

        /**
         * Pass sampled data into the distribution view GUI, to update the
         * current chart display. Note: Here is the very important place where
         * the HistogramResource communicates with the distribution view GUI.
         * Currently there is only one-way communication, only from the
         * HistogramResource to the distribution view GUI. Later, more
         * complicated sampling will depend on the chart configuration/feature
         * selection from the distribution view GUI (so there will be two-way
         * communication). For example, in two-way communication, if user
         * selected Slope Chart to watch the trend, the sampled data will be for
         * all frames; if user selected Multiple Distribution Chart, the user
         * selected grid groups will be passed to the sampling side. Many
         * interesting chart style implementations will be possible here.
         */
        EnsembleTool.getInstance().getEnsembleToolViewer()
                .getDistributionViewer().getGhGUI().updateDisplay(info);

        return null;

    }

    /**
     * Remove any resource not existing in the resource manager.
     * 
     * @param resources
     * @return selected resources
     */
    private ResourceList filterResource(IDescriptor descriptor) {

        ResourceList resources = EnsembleTool.getInstance()
                .getActiveResourceList();
        ResourceList filteredlist = new ResourceList();

        try {
            ((HistogramResourceData) getResourceData()).update();
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        List<D2DGridResource> matchRscs = ((HistogramResourceData) getResourceData())
                .getAllMembersResources();

        int size = resources.size();
        for (int i = size - 1; i >= 0; --i) {
            ResourcePair rp = resources.get(i);
            if (matchRscs.contains(rp.getResource())
                    && descriptor.equals(rp.getResource().getDescriptor())) {
                filteredlist.add(rp);
            }
        }

        return filteredlist;
    }

    public void setLevelUnit(String level, String unit) {
        this.level = level;
        this.unit = unit;
    }

    public String getLevel() {
        return level;
    }

    public String getUnit() {
        return unit;
    }

    public DisplayMode getMode() {
        return mode;
    }

    @Override
    public String getName() {
        if (mode == DisplayMode.POINT_SAMPLING) {
            return level + " " + unit + " Ensemble Sampling";
        } else if (mode == DisplayMode.GRAPHIC_HISTOGRAM) {
            return level + " " + unit + " Distribution Viewer";
        } else {
            return level + " " + unit + " Histogram Text";
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        /*
         * only show sampling if the ensemble tool is ready and the tool layer
         * is editable
         */
        if ((sampleCoord == null)
                || !(EnsembleTool.getInstance().isToolEditable())) {
            return;
        }

        if (isAllPanelSampling() == false) {

            hoverFont.setMagnification(
                    getCapability(MagnificationCapability.class)
                            .getMagnification().floatValue());
            SampleResult result = doHover(sampleCoord,
                    filterResource(descriptor));
            paintResult(target, paintProps, sampleCoord, result);

            return;
        }

        if (isSampling() == false) {

            return;
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container == null) {

            return;
        }

        ResourceList rList = new ResourceList();

        List<ResourceList> blendedLists = new ArrayList<ResourceList>();

        List<ResourcePair> invisibleList = new ArrayList<ResourcePair>();

        IDisplayPane[] panes = container.getDisplayPanes();
        if (panes.length == 4) {
            // AWIPS2 puts four panels in the wrong order.
            panes = new IDisplayPane[] { panes[0], panes[1], panes[3],
                    panes[2] };
        }

        for (IDisplayPane pane : panes) {
            ResourceList rscList = filterResource(pane.getDescriptor());
            for (ResourcePair pair : rscList) {
                if (pair.getResource() == null
                        || !pair.getProperties().isVisible()) {
                    continue;
                }

                if (!pair.getResource()
                        .hasCapability(BlendableCapability.class)) {
                    rList.add(pair);
                    continue;
                }

                ResourceList list = pair.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                ResourcePair rp = list.get(0);
                if (!rp.getProperties().isVisible()) {
                    invisibleList.add(rp);
                    rp.getProperties().setVisible(true);
                }

                rList.add(rp);

                blendedLists.add(list);
            }
        }

        int i = 1;
        while (true) {
            boolean done = true;
            for (ResourceList list : blendedLists) {
                if (list.size() <= i) {
                    continue;
                }
                ResourcePair rp = list.get(i);

                if (!rp.getProperties().isVisible()) {
                    invisibleList.add(rp);
                    rp.getProperties().setVisible(true);
                }

                rList.add(rp);

                done = false;
            }
            if (done) {
                break;
            }
            i++;
        }

        // doHover goes in reverse list order
        Collections.reverse(rList);

        paintResult(target, paintProps, sampleCoord,
                doHover(sampleCoord, rList));
        for (ResourcePair pair : invisibleList) {
            pair.getProperties().setVisible(false);
        }

    }

}
