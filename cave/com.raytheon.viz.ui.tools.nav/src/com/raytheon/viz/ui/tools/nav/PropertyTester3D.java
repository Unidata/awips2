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
/**
 * 
 */
package com.raytheon.viz.ui.tools.nav;

import org.eclipse.core.expressions.PropertyTester;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.core.preferences.PreferenceConstants;
import com.raytheon.viz.ui.EditorUtil;

/**
 * @author randerso
 * 
 */
public class PropertyTester3D extends PropertyTester {

    /**
	 * 
	 */
    public PropertyTester3D() {
        // TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object,
     * java.lang.String, java.lang.Object[], java.lang.Object)
     */
    @Override
    public boolean test(Object receiver, String property, Object[] args,
            Object expectedValue) {
        Boolean result = false;
        if ("is3DEnabled".equals(property)) {
            result = CorePlugin.getDefault().getPreferenceStore()
                    .getBoolean(PreferenceConstants.P_ENABLE_3D);
        } else if ("in3DMode".equals(property)) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                IDescriptor descriptor = container.getActiveDisplayPane()
                        .getDescriptor();
                result = (descriptor.getGridGeometry()
                        .getCoordinateReferenceSystem().getCoordinateSystem()
                        .getDimension() == 3);
            }
        }
        return result.equals(expectedValue);
    }

}
