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
package com.raytheon.uf.viz.auto.transition;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.auto.transition.AverageValueCalculator.CalculatorListener;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.viz.core.rsc.BlendedResource;

/**
 * 
 * Provides the ability to automatically select which resource to render based
 * off the average value of a control resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 09, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AutoTransitionResource extends BlendedResource implements
        CalculatorListener {

    private AverageValueCalculator calc = new AverageValueCalculator();

    private AutoTransitionResourceData resourceData;

    public AutoTransitionResource(AutoTransitionResourceData data,
            LoadProperties props) {
        super(data, props);
        this.resourceData = data;
    }

    public AverageValueCalculator getCalculator() {
        return calc;
    }

    @Override
    public void calculationComplete() {
        issueRefresh();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        calc.removeListener(this);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        calc.addListener(this);

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (resourceData.isAutomaticSelection()) {
            int controlIndex = resourceData.getControlIndex();
            AbstractVizResource<?, ?> controller = getResourceList().get(
                    controlIndex).getResource();

            DataTime time = paintProps.getFramesInfo().getTimeForResource(
                    controller);

            if (calc.calculate(controller, time, paintProps.getView()
                    .getExtent().clone()) == false) {
                updatePaintStatus(PaintStatus.INCOMPLETE);
            }
            double avg = calc.getValue();
            if (!Double.isNaN(avg) && avg >= resourceData.getThresholdValue()) {
                getCapability(BlendableCapability.class).toggle(
                        (controlIndex + 1) % 2);
            } else {
                getCapability(BlendableCapability.class).toggle(controlIndex);
            }
        }
        super.paintInternal(target, paintProps);
    }

    @Override
    public AutoTransitionResourceData getResourceData() {
        return resourceData;
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        super.resourceDataChanged(type, updateObject);
        if (updateObject instanceof BlendableCapability) {
            int alphaStep = getCapability(BlendableCapability.class)
                    .getAlphaStep();
            if (alphaStep > 0 && alphaStep < BlendableCapability.BLEND_MAX) {
                resourceData.setAutomaticSelection(false);
            }
        }
    }

}
