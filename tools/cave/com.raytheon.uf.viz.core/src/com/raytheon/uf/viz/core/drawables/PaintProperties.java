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

package com.raytheon.uf.viz.core.drawables;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.datastructure.PerspectiveSpecificProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;

/**
 * Properties for painting on the screen. This class can be extended if
 * additional values are needed in a specific implementation of paint().
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2007            njensen     Initial creation
 * Jun 3, 2008             chammack    Added zooming property that indicates whether zoom is in progress
 * Jul 19, 2010 #5952      bkowal      We will now clone PaintProperties in the constructor that accepts
 *                                     PaintProperties as an argument.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class PaintProperties {

    protected float alpha;

    protected float zoomLevel;

    protected IView view;

    protected Rectangle canvasBounds;

    protected LoopProperties loopProperties;

    protected boolean isZooming;

    protected boolean occlusionTest = false;

    protected int level;

    protected IExtent clippingPane;

    protected PerspectiveSpecificProperties perspectiveProps;

    protected DataTime dataTime;

    protected FramesInfo framesInfo;

    /**
     * Constructor
     * 
     * @param alpha
     *            the alpha (transparency) value
     * @param zoomLevel
     *            the zoom level
     * @param view
     *            the pixel extent
     */
    public PaintProperties(float alpha, float zoomLevel, IView view,
            Rectangle canvasBounds, boolean isZooming, FramesInfo frameInfo) {
        this.alpha = alpha;
        this.zoomLevel = zoomLevel;
        this.view = view;
        this.canvasBounds = canvasBounds;
        this.isZooming = isZooming;
        this.framesInfo = frameInfo;
    }

    /**
     * Copy constructor
     * 
     * @param props
     *            the properties to copy
     */
    public PaintProperties(PaintProperties props) {
        this(props, (IView) props.getView().clone());
    }

    /**
     * Copy constructor
     * 
     * @param props
     *            the properties to copy
     * @param view
     */
    public PaintProperties(PaintProperties props, IView view) {
        this.alpha = props.getAlpha();
        this.zoomLevel = props.getZoomLevel();
        this.view = view;
        this.canvasBounds = props.canvasBounds;
        this.loopProperties = props.getLoopProperties();
        this.isZooming = props.isZooming();
        this.level = props.level;
        this.clippingPane = props.clippingPane;
        this.perspectiveProps = props.perspectiveProps;
        this.occlusionTest = props.occlusionTest;
        this.dataTime = props.dataTime;
        this.framesInfo = props.framesInfo;
    }

    /**
     * @return the alpha
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

    /**
     * @return the isZooming
     */
    public boolean isZooming() {
        return isZooming;
    }

    /**
     * @return the zoomLevel
     */
    public float getZoomLevel() {
        return zoomLevel;
    }

    /**
     * @param zoomLevel
     *            the zoomLevel to set
     */
    public void setZoomLevel(float zoomLevel) {
        this.zoomLevel = zoomLevel;
    }

    public Rectangle getCanvasBounds() {
        return canvasBounds;
    }

    public void setCanvasBounds(Rectangle canvasBounds) {
        this.canvasBounds = canvasBounds;
    }

    /**
     * @param isZooming
     *            the isZooming to set
     */
    public void setZooming(boolean isZooming) {
        this.isZooming = isZooming;
    }

    /**
     * @return the loopProperties
     */
    public LoopProperties getLoopProperties() {
        return loopProperties;
    }

    /**
     * @param loopProperties
     *            the loopProperties to set
     */
    public void setLoopProperties(LoopProperties loopProperties) {
        this.loopProperties = loopProperties;
    }

    /**
     * @return the view
     */
    public IView getView() {
        return view;
    }

    /**
     * @param view
     *            the view to set
     */
    public void setView(IView view) {
        this.view = view;
    }

    /**
     * @return the occlusionTest
     */
    public boolean isOcclusionTest() {
        return occlusionTest;
    }

    /**
     * @param occlusionTest
     *            the occlusionTest to set
     */
    public void setOcclusionTest(boolean occlusionTest) {
        this.occlusionTest = occlusionTest;
    }

    /**
     * @return the level
     */
    public int getLevel() {
        return level;
    }

    /**
     * Set the displayed level index
     * 
     * @param level
     *            the level to set
     */
    public void setLevel(int level) {
        this.level = level;
    }

    /**
     * @return the clippingPane
     */
    public IExtent getClippingPane() {
        return clippingPane;
    }

    /**
     * @param clippingPane
     *            the clippingPane to set
     */
    public void setClippingPane(IExtent clippingPane) {
        this.clippingPane = clippingPane;
    }

    /**
     * @return the perspectiveProps
     */
    public PerspectiveSpecificProperties getPerspectiveProps() {
        return perspectiveProps;
    }

    /**
     * @param perspectiveProps
     *            the perspectiveProps to set
     */
    public void setPerspectiveProps(
            PerspectiveSpecificProperties perspectiveProps) {
        this.perspectiveProps = perspectiveProps;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    public FramesInfo getFramesInfo() {
        return framesInfo;
    }

    public void setFramesInfo(FramesInfo framesInfo) {
        this.framesInfo = framesInfo;
    }

}
