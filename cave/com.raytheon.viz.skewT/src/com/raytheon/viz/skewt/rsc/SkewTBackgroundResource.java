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
package com.raytheon.viz.skewt.rsc;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.skewt.SkewTDescriptor;
import com.raytheon.viz.skewt.rscdata.SkewTBkgResourceData;
import com.raytheon.viz.skewt.ui.HodoBackground;
import com.raytheon.viz.skewt.ui.SkewTConstants;
import com.raytheon.viz.skewt.ui.SkewtBackground;
import com.raytheon.viz.skewt.ui.TempChangeBackground;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SkewTBackgroundResource extends
        AbstractVizResource<SkewTBkgResourceData, SkewTDescriptor> implements
        IContextMenuContributor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SkewTBackgroundResource.class);

    private static final UnitConverter celciusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private SkewtBackground skewTBackground;

    /**
     * @return the skewTBackground
     */
    public SkewtBackground getSkewTBackground() {
        return skewTBackground;
    }

    /**
     * @return the hodoBackground
     */
    public HodoBackground getHodoBackground() {
        return hodoBackground;
    }

    /**
     * @return the tempChangeBackground
     */
    public TempChangeBackground getTempChangeBackground() {
        return tempChangeBackground;
    }

    private HodoBackground hodoBackground;

    private TempChangeBackground tempChangeBackground;

    public SkewTBackgroundResource(SkewTBkgResourceData rscData,
            LoadProperties loadProperties) {
        super(rscData, loadProperties);
        double mag = getCapability(MagnificationCapability.class)
                .getMagnification();
        skewTBackground = new SkewtBackground();
        skewTBackground.setMagnification(mag);
        hodoBackground = new HodoBackground();
        hodoBackground.setMagnification(mag);
        tempChangeBackground = new TempChangeBackground();
        tempChangeBackground.setMagnification(mag);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.CAPABILITY) {
                    if (object instanceof MagnificationCapability) {
                        double mag = ((MagnificationCapability) object)
                                .getMagnification();
                        skewTBackground.setMagnification(mag);
                        hodoBackground.setMagnification(mag);
                        tempChangeBackground.setMagnification(mag);
                    }
                }

            }

        });
    }

    @Override
    protected void disposeInternal() {
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        skewTBackground.paint(target, paintProps);
        hodoBackground.paint(target, paintProps);
        tempChangeBackground.paint(target, paintProps);
    }

    @Override
    public String inspect(ReferencedCoordinate rCoord) throws VizException {
        String s = "NO DATA";

        Coordinate c;
        try {
            c = rCoord.getObject();
            if (getSkewTBackground().contains(c)) {
                c = WxMath.reverseSkewTXY(getSkewTBackground().getWorld()
                        .unMap(c.x, c.y));
                double p_mb = c.y;
                double t_C = c.x; // Celsius
                double t_F = celciusToFahrenheit.convert(c.x);
                double theta = celciusToKelvin.convert(WxMath.theta(p_mb, t_C,
                        1000));
                double thetaE = celciusToKelvin.convert(WxMath.thetae(p_mb,
                        t_C, t_C));
                double w = WxMath.mixingRatio(p_mb, t_C);
                s = String
                        .format("P=%.0f mb T=%.0f%cC/%.0f%cF Theta=%.0fK Theta-e=%.0fK w=%.1f",
                                p_mb, t_C, SkewTConstants.DEGREE_SYMBOL, t_F,
                                SkewTConstants.DEGREE_SYMBOL, theta, thetaE, w);
            } else if (getHodoBackground().contains(c)) {
                c = getHodoBackground().getWorld().unMap(c.x, c.y);
                c = WxMath.speedDir((float) c.x, (float) c.y);
                s = String.format("%.0f%c at %.0f kts", c.y,
                        SkewTConstants.DEGREE_SYMBOL,
                        metersPerSecondToKnots.convert(c.x));
            } else if (getTempChangeBackground().contains(c)) {
                c = getTempChangeBackground().getWorld().unMap(c.x, c.y);
                c.y = WxMath.reverseSkewTXY(c).y;
                double p_mb = c.y;
                double deltaT_C = c.x;
                double deltaT_F = 1.8 * deltaT_C;
                s = String.format("P=%.0f mb T=%+.0f%cC/%+.0f%cF", p_mb,
                        deltaT_C, SkewTConstants.DEGREE_SYMBOL, deltaT_F,
                        SkewTConstants.DEGREE_SYMBOL);
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception translating coordinate", e);
        }

        return s;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        for (ResourcePair rp : getDescriptor().getResourceList()) {
            if (rp.getResource() instanceof SkewTResource
                    && !(rp.getResource() instanceof InteractiveSkewTResource)) {
                InteractiveSkewTMenuAction action = new InteractiveSkewTMenuAction();
                ResourcePair rscPair = new ResourcePair();
                rscPair.setLoadProperties(getLoadProperties());
                rscPair.setProperties(descriptor.getResourceList()
                        .getProperties(this));
                rscPair.setResource(this);
                rscPair.setResourceData(resourceData);
                action.setSelectedRsc(rscPair);
                menuManager.add(action);
                return;
            }
        }
        return;
    }

    private static class InteractiveSkewTMenuAction extends
            AbstractRightClickAction implements IMenuCreator {

        private Menu menu;

        private InteractiveSkewTMenuAction() {
            super("Interactive Skew-T", IAction.AS_DROP_DOWN_MENU);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getMenuCreator()
         */
        @Override
        public IMenuCreator getMenuCreator() {
            return this;
        }

        @Override
        public void dispose() {
            if (menu != null) {
                menu.dispose();
            }
        }

        @Override
        public Menu getMenu(Control parent) {

            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);

            fillMenu(menu);

            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {

            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);

            fillMenu(menu);

            return menu;
        }

        private void fillMenu(Menu menu) {
            ResourceList list = getSelectedRsc().getDescriptor()
                    .getResourceList();
            synchronized (list) {
                InteractiveSkewTResource activeRsc = null;
                String activeIcao = null;
                for (ResourcePair rp : list) {
                    if (rp.getResource() instanceof InteractiveSkewTResource) {
                        activeRsc = (InteractiveSkewTResource) rp.getResource();
                        activeIcao = activeRsc.getSoundingParameters()
                                .getInterleavedData().getName();
                        break;
                    }
                }
                for (ResourcePair rp : list) {
                    if (rp.getResource() != activeRsc
                            && rp.getResource() instanceof SkewTResource) {
                        SkewTResource rsc = (SkewTResource) rp.getResource();
                        String icao = rsc.getSoundingParameters()
                                .getInterleavedData().getName();
                        ActionContributionItem aci = new ActionContributionItem(
                                new InteractiveSkewTInternalAction(rsc,
                                        activeRsc));

                        aci.getAction().setChecked(icao.equals(activeIcao));
                        aci.fill(menu, -1);
                    }
                }
            }
        }
    }

    private static class InteractiveSkewTInternalAction extends Action {

        private SkewTResource rsc;

        private SkewTResource activeRsc;

        public InteractiveSkewTInternalAction(SkewTResource rsc,
                SkewTResource activeRsc) {
            super(rsc.getSoundingParameters().getInterleavedData().getName(),
                    IAction.AS_RADIO_BUTTON);
            this.rsc = rsc;
            this.activeRsc = activeRsc;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            if (!this.isChecked()) {
                return;
            }
            System.out.println(this.getText() + " selected");

            SkewTDescriptor descriptor = this.rsc.getDescriptor();
            ResourceList list = descriptor.getResourceList();
            synchronized (list) {

                // remove the currently active resource if any
                if (activeRsc != null) {
                    list.removeRsc(activeRsc);
                }

                // hide all but the selected resource
                for (ResourcePair rp : list) {
                    AbstractVizResource<?, ?> r = rp.getResource();
                    if (r instanceof SkewTResource && r != rsc) {
                        rp.getProperties().setVisible(false);
                    }
                }

                ResourceProperties props = new ResourceProperties();
                InteractiveSkewTResource activeRsc = new InteractiveSkewTResource(
                        rsc);
                list.add(activeRsc, props);
                try {
                    descriptor.getTimeMatcher().redoTimeMatching(descriptor);
                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return "SkewT Background";
    }

}
