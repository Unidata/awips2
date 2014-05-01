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
package com.raytheon.uf.viz.core.preferences;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.IThemeManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColorFactory implements IPropertyChangeListener {

    private static ColorFactory instance;

    private Map<String, RGB> rgbMap = new HashMap<String, RGB>();

    private IThemeManager manager;

    public ColorFactory() {
        if (PlatformUI.isWorkbenchRunning()) {
            manager = PlatformUI.getWorkbench().getThemeManager();
            manager.addPropertyChangeListener(this);
        }
    }

    public synchronized static ColorFactory getInstance() {
        if (instance == null) {
            instance = new ColorFactory();
        }
        return instance;
    }

    /**
     * Get the color for the id. The color only needs to be retrieved once
     * 
     * @param id
     * @return the registered color or white if none
     */
    public RGB getColor(String id) {
        RGB rgb = rgbMap.get(id);
        if (rgb == null) {
            if (manager != null) {
                rgb = manager.getCurrentTheme().getColorRegistry().getRGB(id);
            }
            if (rgb == null) {
                rgb = new RGB(255, 255, 255);
            }
            rgbMap.put(id, rgb);
        }
        return rgb;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org
     * .eclipse.jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        RGB color = rgbMap.get(event.getProperty());
        if (color != null) {
            RGB newVal = (RGB) event.getNewValue();
            color.red = newVal.red;
            color.blue = newVal.blue;
            color.green = newVal.green;
        }
    }

}
