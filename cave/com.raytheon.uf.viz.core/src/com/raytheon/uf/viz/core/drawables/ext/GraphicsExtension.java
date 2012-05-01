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
package com.raytheon.uf.viz.core.drawables.ext;

import com.raytheon.uf.viz.core.IGraphicsTarget;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class GraphicsExtension<T extends IGraphicsTarget> {

    /**
     * Interface that other interfaces should extend if they want to be used as
     * a graphics extension
     */
    public static interface IGraphicsExtensionInterface {

    }

    public static class Compatibilty {
        public static final int INCOMPATIBLE = -1;

        public static final int GENERIC = 0;

        public static final int TARGET_COMPATIBLE = 1000;

        public static final int ENHANCED_TARGET_COMPATIBLE = 2000;
    }

    protected T target;

    /**
     * Used to prepare the extension for use with the specified target and to
     * determine which extension implementation should be used when multiple are
     * available. The return value represents an arbitrary
     * priority/compatibility value of this extension. If multiple extensions
     * are registered with the target it will use the one with the highest
     * priority. If a target is incompatible with an extension implementation
     * this function should return a negative number and it will not be used.
     * 
     * @param target
     * @return compatibility value
     */
    @SuppressWarnings("unchecked")
    public final int setTarget(IGraphicsTarget target) {
        try {
            this.target = (T) target;
            return getCompatibilityValue(this.target);
        } catch (ClassCastException e) {
            this.target = null;
            return Compatibilty.INCOMPATIBLE;
        }
    }

    /**
     * Get the target compability value.
     * 
     * @param target
     * @return
     */
    public abstract int getCompatibilityValue(T target);

    /**
     * Diposes the extension. Extensions should dispose any graphics objects
     * that need disposing. Default implementation does nothing
     */
    public void dispose() {
        // Default does nothing
    }
}
