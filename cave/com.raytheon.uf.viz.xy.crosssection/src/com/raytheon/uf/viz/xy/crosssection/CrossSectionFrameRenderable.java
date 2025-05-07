/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.crosssection;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ICapabilityProvider;

/**
 * Abstract class for the renderable data for a single cross section frame. This
 * includes the main frame renderables (e.g. the image or contours), along with
 * any data type-specific renderables that were provided by the cross section
 * adapter when loading this frame's data (in
 * {@link CrossSectionFrameExtraRenderable}).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public abstract class CrossSectionFrameRenderable implements IRenderable {

    protected final ICapabilityProvider capProvider;

    private final CrossSectionFrameExtraRenderable extraRenderable;

    /**
     * Constructor.
     *
     * @param capProvider
     *            provides access to the resource's capabilities
     * @param extraRenderable
     *            extra data type-specific renderable
     */
    public CrossSectionFrameRenderable(ICapabilityProvider capProvider,
            CrossSectionFrameExtraRenderable extraRenderable) {
        this.extraRenderable = extraRenderable;
        this.capProvider = capProvider;
        if (extraRenderable != null) {
            extraRenderable.setCapabilityProvider(capProvider);
        }
    }

    /**
     * Get extra text to include in the resource's legend for this frame.
     *
     * @return extra legend text string
     */
    public String getExtraLegendText() {
        if (extraRenderable != null) {
            return extraRenderable.getExtraLegendText();
        }
        return "";
    }

    @Override
    public final void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        paintInternal(target, paintProps);
        if (extraRenderable != null) {
            extraRenderable.paint(target, paintProps);
        }
    }

    /**
     * Abstract method for subclasses to paint their renderables.
     *
     * @param target
     *            graphics target
     * @param paintProps
     *            paint properties
     * @throws VizException
     */
    protected abstract void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException;

    /**
     * Final method for disposing all resources. Subclasses should override
     * {@link #disposeInternal}.
     */
    public final void dispose() {
        disposeInternal();
        if (extraRenderable != null) {
            extraRenderable.dispose();
        }
    }

    /**
     * Abstract method for subclasses to dispose of their resources.
     */
    protected abstract void disposeInternal();
}
