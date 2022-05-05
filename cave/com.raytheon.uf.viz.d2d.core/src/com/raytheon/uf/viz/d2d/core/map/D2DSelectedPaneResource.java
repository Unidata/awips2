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
package com.raytheon.uf.viz.d2d.core.map;

import java.awt.Font;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Resource for drawing tags on the panes to show selectedness to the user (load
 * to, control image of, etc)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2010            mschenke     Initial creation
 * Aug 04, 2014 3489       mapeters     Updated deprecated getStringBounds() calls.
 * Nov 05, 2015 5070       randerso     Adjust font sizes for dpi scaling
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DSelectedPaneResource extends
        AbstractVizResource<GenericResourceData, IDescriptor> {

    protected static final int LEFT_OFFSET = 6;

    protected static final int BOTTOM_OFFSET = 4;

    protected static final RGB L_COLOR = new RGB(255, 255, 0);

    protected static final RGB I_COLOR = new RGB(0, 255, 0);

    protected static final RGB PLUS_COLOR = new RGB(111, 225, 249);

    private DrawableString stringL;

    private DrawableString stringI;

    private DrawableString plus;

    protected IFont font;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public D2DSelectedPaneResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null && container instanceof IMultiPaneEditor) {
            IDisplayPane myPane = null;
            for (IDisplayPane pane : container.getDisplayPanes()) {
                if (pane.getDescriptor() == descriptor) {
                    myPane = pane;
                    break;
                }
            }
            if (myPane != null) {
                IMultiPaneEditor editor = (IMultiPaneEditor) container;
                IExtent extent = paintProps.getView().getExtent();

                double ratioX = extent.getWidth()
                        / paintProps.getCanvasBounds().width;
                double ratioY = extent.getHeight()
                        / paintProps.getCanvasBounds().height;

                stringL.basics.x = stringI.basics.x = plus.basics.x = paintProps
                        .getView().getExtent().getMinX()
                        + (LEFT_OFFSET * ratioX);
                stringL.basics.y = stringI.basics.y = plus.basics.y = paintProps
                        .getView().getExtent().getMaxY()
                        - (BOTTOM_OFFSET * ratioY);

                if (editor.getNumberofPanes() > 1
                        && editor.isSelectedPane(IMultiPaneEditor.LOAD_ACTION,
                                myPane)) {
                    target.clearClippingPlane();
                    target.drawStrings(stringL);
                    if (editor.isSelectedPane(IMultiPaneEditor.IMAGE_ACTION,
                            myPane)) {
                        stringI.basics.y -= (target.getStringsBounds(stringL)
                                .getHeight() + BOTTOM_OFFSET)
                                * extent.getHeight()
                                / paintProps.getCanvasBounds().height;
                        target.drawStrings(stringI);
                    }
                    target.setupClippingPlane(extent);
                } else if (editor.getNumberofPanes() > 1
                        && editor.displayedPaneCount() == 1) {
                    target.clearClippingPlane();
                    target.drawStrings(plus);
                    target.setupClippingPlane(extent);
                } else if (editor.getNumberofPanes() > 1
                        && editor.isSelectedPane(IMultiPaneEditor.IMAGE_ACTION,
                                myPane)) {
                    target.clearClippingPlane();
                    target.drawStrings(stringI);
                    target.setupClippingPlane(extent);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        font = target.initializeFont(Font.MONOSPACED, 20,
                new Style[] { Style.BOLD });

        stringL = new DrawableString("L", L_COLOR);
        stringI = new DrawableString("I", I_COLOR);
        plus = new DrawableString("+", PLUS_COLOR);

        stringL.font = stringI.font = plus.font = font;
        stringL.addTextStyle(TextStyle.DROP_SHADOW);
        stringI.addTextStyle(TextStyle.DROP_SHADOW);
        plus.addTextStyle(TextStyle.DROP_SHADOW);
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.HIGHEST;
    }

}
