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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * Bandwidth utilization graph image manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012   1269     lvenable     Initial creation
 * Dec 13, 2012   1269     lvenable     Fixes and updates.
 * Jan 25, 2013   1528     djohnson     Subscription priority is now an enum on subscriptions.
 * Jan 28, 2013   1529     djohnson    Add hasSubscriptionNameChecked().
 * Oct 28, 2013   2430     mpduff      Add % of bandwidth utilized graph.
 * Nov 19, 2013   1531     mpduff      Update the settings.
 * Dec 17, 2013   2633     mpduff      Keep data used to regenerate images.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class BandwidthImageMgr implements IGraphOptions {
    /**
     * Image type enumeration.
     */
    public enum CanvasImages {
        X_HEADER, Y_HEADER, GRAPH, X_LABEL, Y_LABEL, UTILIZATION_LABEL, UTILIZATION_GRAPH, UTILIZATION_HEADER;
    };

    /**
     * Sort type enumeration.
     */
    public enum SortBy {
        NAME_ASC("Name Ascending"), NAME_DESC("Name Descending"), CURRENT_TIME(
                "Current Time"), SELECTED_INTERSECT("Intersect Start Time"), SELECTED_START(
                "Upcoming Start Time");

        private String sortBy;

        private SortBy(String sortBy) {
            this.sortBy = sortBy;
        }

        public String getSortByString() {
            return sortBy;
        }
    }

    /**
     * Graph Type
     */
    public enum GraphType {
        BAR, LINE;
    }

    /**
     * Signifies which section of the graph.
     */
    public enum GraphSection {
        LOWER, MIDDLE, UPPER;
    }

    /** Map of CanvasImages type -> image */
    private Map<CanvasImages, AbstractCanvasImage> canvasImgMap;

    /** Map of Subscription name -> check box flag */
    private Map<String, Boolean> checkMap = new HashMap<String, Boolean>();

    /** Map of Rectangle object -> Subscription name */
    private Map<Rectangle, String> checkBoxMap = new HashMap<Rectangle, String>();

    /** Live update flag */
    private boolean liveUpdate = true;

    /** Color by priority flag */
    private boolean colorByPriority = true;

    /** Draw subscription lines flag */
    private boolean showSubscriptionLines;

    /** Sort by name flag */
    private SortBy sortBy = SortBy.CURRENT_TIME;

    /** Current time in millis */
    private long currentTimeMillis;

    /** Sort time in millis */
    private long sortTimeMillis;

    /** Map of priorities to colors. This holds the colors changed by the user. */
    private Map<SubscriptionPriority, RGB> priorityColorMap;

    /** Map of percentage to colors. This holds the colors changed by the user. */
    private Map<GraphSection, RGB> percentageColorMap = new LinkedHashMap<GraphSection, RGB>(
            3);

    /** The bandwidth graph type */
    private GraphType bandwidthGraphType = GraphType.LINE;

    /** The network currently displayed in the graph */
    private Network network = Network.OPSNET;

    private int[] bandwidthThreholdValues = new int[] { 33, 66 };

    private final Composite parentComp;

    private BandwidthGraphData graphData;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param canvasSettingsMap
     *            Canvas settings map
     * @param graphData
     *            Graph data object
     * @param currentTime
     *            Current time in millis
     */
    public BandwidthImageMgr(Composite parentComp,
            Map<CanvasImages, CanvasSettings> canvasSettingsMap,
            BandwidthGraphData graphData, long currentTime) {
        this.currentTimeMillis = currentTime;
        this.parentComp = parentComp;
        this.graphData = graphData;
        init(canvasSettingsMap);
    }

    /**
     * Initialize components.
     * 
     * @param parentComp
     *            Parent composite
     * @param graphData
     *            Graph Data
     */
    private void init(Map<CanvasImages, CanvasSettings> canvasSettingsMap) {
        priorityColorMap = new EnumMap<SubscriptionPriority, RGB>(
                SubscriptionPriority.class);
        priorityColorMap.put(SubscriptionPriority.LOW, new RGB(6, 122, 255));
        priorityColorMap.put(SubscriptionPriority.NORMAL, new RGB(0, 255, 0));
        priorityColorMap.put(SubscriptionPriority.HIGH, new RGB(255, 0, 0));

        percentageColorMap.put(GraphSection.LOWER, new RGB(0, 255, 0));
        percentageColorMap.put(GraphSection.MIDDLE,
                RGBColors.getRGBColor("yellow"));
        percentageColorMap.put(GraphSection.UPPER, new RGB(255, 0, 0));

        canvasImgMap = new HashMap<BandwidthImageMgr.CanvasImages, AbstractCanvasImage>();
        populateCanvasMap(parentComp, graphData, canvasSettingsMap);
    }

    /**
     * Populate the canvas map.
     * 
     * @param parentComp
     *            Parent composite
     * @param graphData
     *            Graph Data
     */
    private void populateCanvasMap(Composite parentComp,
            BandwidthGraphData graphData,
            Map<CanvasImages, CanvasSettings> canvasSettingsMap) {
        // Graph image
        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.GRAPH);
        AbstractCanvasImage aci = new GraphImage(parentComp, cs, graphData,
                this);
        canvasImgMap.put(CanvasImages.GRAPH, aci);

        // X label image
        cs = canvasSettingsMap.get(CanvasImages.X_LABEL);
        aci = new XLabelImage(parentComp, cs, graphData);
        canvasImgMap.put(CanvasImages.X_LABEL, aci);

        // Y label image
        cs = canvasSettingsMap.get(CanvasImages.Y_LABEL);
        aci = new YLabelImage(parentComp, cs, graphData, this);

        canvasImgMap.put(CanvasImages.Y_LABEL, aci);

        // X header image
        cs = canvasSettingsMap.get(CanvasImages.X_HEADER);
        aci = new XHeaderImage(parentComp, cs, graphData, this);
        canvasImgMap.put(CanvasImages.X_HEADER, aci);

        // Y header image
        cs = canvasSettingsMap.get(CanvasImages.Y_HEADER);
        aci = new YHeaderImage(parentComp, cs, graphData);
        canvasImgMap.put(CanvasImages.Y_HEADER, aci);

        // utilization header image
        cs = canvasSettingsMap.get(CanvasImages.UTILIZATION_HEADER);
        aci = new UtilizationHeaderImage(parentComp, cs, this);
        canvasImgMap.put(CanvasImages.UTILIZATION_HEADER, aci);

        // utilization label image
        cs = canvasSettingsMap.get(CanvasImages.UTILIZATION_LABEL);
        aci = new UtilizationLabelImage(parentComp, cs, this);
        canvasImgMap.put(CanvasImages.UTILIZATION_LABEL, aci);

        // utilization graph image
        cs = canvasSettingsMap.get(CanvasImages.UTILIZATION_GRAPH);
        aci = new UtilizationGraphImage(parentComp, cs, graphData, this);
        canvasImgMap.put(CanvasImages.UTILIZATION_GRAPH, aci);

        // Regenerate all of the images
        for (CanvasImages ci : CanvasImages.values()) {
            canvasImgMap.get(ci).regenerateImage();
        }
    }

    /**
     * Generate the graph images.
     * 
     * @param graphData
     *            The graph data object
     */
    public void generateImages(BandwidthGraphData graphData) {
        this.graphData = graphData;
        for (CanvasImages ci : CanvasImages.values()) {
            canvasImgMap.get(ci).regenerateImage(graphData);
        }
    }

    /**
     * Update the canvas settings for the specified canvas.
     * 
     * @param ci
     *            Canvas image.
     * @param cs
     *            Canvas setting.
     */
    public void setCanvasSetting(CanvasImages ci, CanvasSettings cs) {
        canvasImgMap.get(ci).setCanvasSetting(cs);
    }

    /**
     * Regenerate the images.
     * 
     * @param ci
     *            CanvasImage to regenerate
     */
    public void regenerateImage(CanvasImages ci) {
        canvasImgMap.get(ci).regenerateImage();
    }

    /**
     * Get the image.
     * 
     * @param ci
     *            The CanvasImages image to get
     * @return The image
     */
    public Image getImage(CanvasImages ci) {
        return canvasImgMap.get(ci).getImage();
    }

    /**
     * Get all images.
     * 
     * @return Map of CanvasImages -> image
     */
    public Map<CanvasImages, Image> getAllImages() {
        Map<CanvasImages, Image> imgMap = new HashMap<BandwidthImageMgr.CanvasImages, Image>();

        for (CanvasImages ci : CanvasImages.values()) {
            imgMap.put(ci, canvasImgMap.get(ci).getImage());
        }

        return imgMap;
    }

    /**
     * Get the display text for mouseovers
     * 
     * @param mouseCoord
     *            Coordinate of the mouse
     * @param ci
     *            CanvasImages under the mouse
     * @return The tool tip text or null if not available
     */
    public String getToolTipText(Point mouseCoord, CanvasImages ci) {
        return canvasImgMap.get(ci).getToolTipText(mouseCoord);
    }

    /**
     * Get the subscription names.
     * 
     * @return Collection of subscription names
     */
    public Collection<String> getSubscriptionNames() {
        return ((YLabelImage) canvasImgMap.get(CanvasImages.Y_LABEL))
                .getSubscriptionNames();
    }

    /**
     * Set the checked subscription map.
     * 
     * @param checkMap
     *            The checkMap
     */
    public void setCheckMap(Map<String, Boolean> checkMap) {
        this.checkMap = checkMap;
    }

    /**
     * Is the subscription checked?
     * 
     * @param name
     *            Subscription name
     * @return true if checked
     */
    public boolean isChecked(String name) {
        if (checkMap.containsKey(name)) {
            return checkMap.get(name);
        }

        return false;
    }

    /**
     * Set the subscription name to check.
     * 
     * @param name
     *            Subscription name.
     * @param checked
     *            Boolean for the checked state.
     */
    public void setChecked(String name, boolean checked) {
        checkMap.put(name, checked);
    }

    /**
     * Set the check box map.
     * 
     * @param checkBoxMap
     *            the checkBoxMap to set
     */
    public void setCheckBoxMap(Map<Rectangle, String> checkBoxMap) {
        this.checkBoxMap = checkBoxMap;
    }

    /**
     * @return the checkBoxMap
     */
    public Map<Rectangle, String> getCheckBoxMap() {
        return checkBoxMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLiveUpdate() {
        return liveUpdate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLiveUpdate(boolean liveUpdate) {
        this.liveUpdate = liveUpdate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isColorByPriority() {
        return colorByPriority;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColorByPriority(boolean colorByPriority) {
        this.colorByPriority = colorByPriority;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setShowSubscriptionLines(boolean showSubLines) {
        this.showSubscriptionLines = showSubLines;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isShowSubscriptionLines() {
        return this.showSubscriptionLines;
    }

    /**
     * Set the sort method.
     * 
     * @param sortBy
     *            The method to sort by
     */
    public void setSortBy(SortBy sortBy) {
        this.sortBy = sortBy;
    }

    /**
     * Get the sort method.
     * 
     * @return The sort method
     */
    public SortBy getSortBy() {
        return this.sortBy;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getCurrentTimeMillis() {
        return currentTimeMillis;
    }

    /**
     * @param currentTimeMillis
     *            the currentTimeMillis to set
     */
    public void setCurrentTimeMillis(long currentTimeMillis) {
        this.currentTimeMillis = currentTimeMillis;
    }

    /**
     * @param sortTimeMillis
     *            the sortTimeMillis to set
     */
    public void setSortTimeMillis(long sortTimeMillis) {
        this.sortTimeMillis = sortTimeMillis;
    }

    /**
     * @return the sortTimeMillis
     */
    public long getSortTimeMillis() {
        return sortTimeMillis;
    }

    /**
     * Is there a selection for the canvas?
     * 
     * @param ci
     *            Canvas to check for selection
     * 
     * @return true if there is a selection
     */
    public boolean hasSelection(CanvasImages ci) {
        return canvasImgMap.get(ci).hasSelection();
    }

    /**
     * Clear the canvas selection.
     * 
     * @param ci
     *            The canvas to clear
     */
    public void clearCanvasSelection(CanvasImages ci) {
        canvasImgMap.get(ci).clearCanvasSelection();
    }

    /**
     * Perform an action on the specified image with the provided mouse point.
     * 
     * @param ci
     *            Canvas image to perform the action on.
     * @param mousePoint
     *            Mouse coordinates.
     */
    public void performAction(CanvasImages ci, Point mousePoint) {
        canvasImgMap.get(ci).performAction(mousePoint);
    }

    /**
     * Get the RGB color associated with the provided priority.
     * 
     * @param priority
     *            Priority.
     * @return The RGB color.
     */
    @Override
    public RGB getPriorityColor(SubscriptionPriority priority) {
        return priorityColorMap.get(priority);
    }

    /**
     * Set the RGB color to the associated priority.
     * 
     * @param priority
     *            Priority.
     * @param rgb
     *            The RGB color.
     */
    @Override
    public void setPriorityColor(SubscriptionPriority priority, RGB rgb) {
        priorityColorMap.put(priority, rgb);

        // Need to redraw the images that use the priority color
        regenerateImage(CanvasImages.GRAPH);
        regenerateImage(CanvasImages.X_HEADER);

    }

    /**
     * Check whether there is a checked subscription name.
     * 
     * @return true if at least one subscription name is checked
     */
    public boolean hasSubscriptionNameChecked() {
        return checkMap.containsValue(Boolean.TRUE);
    }

    /**
     * @return the percentageColorMap
     */
    public Map<GraphSection, RGB> getPercentageColorMap() {
        return percentageColorMap;
    }

    /**
     * @param percentageColorMap
     *            the percentageColorMap to set
     */
    public void setPercentageColorMap(Map<GraphSection, RGB> percentageColorMap) {
        this.percentageColorMap = percentageColorMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RGB getPercentColor(GraphSection section) {
        return this.percentageColorMap.get(section);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setPercentColor(GraphSection percentString, RGB rgb) {
        this.percentageColorMap.put(percentString, rgb);
    }

    /**
     * Set the bandwidth used graph type.
     * 
     * @param type
     *            The graph type
     */
    public void setBandwidthGraphType(GraphType type) {
        this.bandwidthGraphType = type;
    }

    /**
     * Get the bandwidth graph type.
     * 
     * @return The Bandwidth graph type
     */
    public GraphType getBandwidthGraphType() {
        return this.bandwidthGraphType;
    }

    /**
     * Set the network.
     * 
     * @param network
     *            The network
     */
    public void setGraphNetwork(Network network) {
        this.network = network;
    }

    /**
     * Get the network.
     * 
     * @return The network
     */
    public Network getNetwork() {
        return this.network;
    }

    /**
     * @return the bandwidthThreholdValues
     */
    public int[] getBandwidthThreholdValues() {
        return bandwidthThreholdValues;
    }

    /**
     * @param bandwidthThreholdValues
     *            the bandwidthThreholdValues to set
     */
    public void setBandwidthThreholdValues(int[] bandwidthThreholdValues) {
        this.bandwidthThreholdValues = bandwidthThreholdValues;
    }

    /**
     * Update the image map with new settings.
     * 
     * @param canvasSettingsMap
     */
    public void updateImageMap(
            Map<CanvasImages, CanvasSettings> canvasSettingsMap) {
        // Graph image
        CanvasSettings cs = canvasSettingsMap.get(CanvasImages.GRAPH);
        AbstractCanvasImage aci = new GraphImage(parentComp, cs, graphData,
                this);
        canvasImgMap.get(CanvasImages.GRAPH).disposeImage();
        canvasImgMap.put(CanvasImages.GRAPH, aci);

        // X label image
        cs = canvasSettingsMap.get(CanvasImages.X_LABEL);
        aci = new XLabelImage(parentComp, cs, graphData);
        canvasImgMap.get(CanvasImages.X_LABEL).disposeImage();
        canvasImgMap.put(CanvasImages.X_LABEL, aci);

        // Y label image
        cs = canvasSettingsMap.get(CanvasImages.Y_LABEL);
        aci = new YLabelImage(parentComp, cs, graphData, this);
        canvasImgMap.get(CanvasImages.Y_LABEL).disposeImage();
        canvasImgMap.put(CanvasImages.Y_LABEL, aci);

        // X header image
        cs = canvasSettingsMap.get(CanvasImages.X_HEADER);
        aci = new XHeaderImage(parentComp, cs, graphData, this);
        canvasImgMap.get(CanvasImages.X_HEADER).disposeImage();
        canvasImgMap.put(CanvasImages.X_HEADER, aci);

        // Y header image
        cs = canvasSettingsMap.get(CanvasImages.Y_HEADER);
        aci = new YHeaderImage(parentComp, cs, graphData);
        canvasImgMap.get(CanvasImages.Y_HEADER).disposeImage();
        canvasImgMap.put(CanvasImages.Y_HEADER, aci);

        // Regenerate all of the images
        for (CanvasImages ci : CanvasImages.values()) {
            canvasImgMap.get(ci).regenerateImage();
        }
    }
}
