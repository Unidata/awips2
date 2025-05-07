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

import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.rsc.capabilities.ICapabilityProvider;

/**
 * Abstract class for any custom, data type-specific renderables for a single
 * cross section frame.
 *
 * When a cross section adapter calculates a frame's graph data, it may also
 * include an instance of this class in the returned
 * {@link CrossSectionFrameData}. When renderables are then created from the
 * data as well, the resource will link this to them as well, in
 * {@link CrossSectionFrameRenderable}. This class will then be painted/disposed
 * of along with the main frame renderables.
 *
 * This keeps the data type specific renderables/text fully in sync with the
 * rendered graph data, including image resources' custom behavior of keeping
 * the previous frame image displayed while calculating the new image during a
 * data update.
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
public abstract class CrossSectionFrameExtraRenderable implements IRenderable {

    protected ICapabilityProvider capProvider;

    /**
     * @param capProvider
     *            provides access to the resource's capabilities
     */
    public void setCapabilityProvider(ICapabilityProvider capProvider) {
        this.capProvider = capProvider;
    }

    /**
     * Get extra text to include in the resource's legend for this frame.
     *
     * @return extra legend text string
     */
    public abstract String getExtraLegendText();

    /**
     * Dispose all resources.
     */
    public abstract void dispose();
}
