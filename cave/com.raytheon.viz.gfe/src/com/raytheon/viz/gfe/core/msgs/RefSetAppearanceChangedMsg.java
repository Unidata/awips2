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
package com.raytheon.viz.gfe.core.msgs;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RefSetAppearanceChangedMsg extends Message {

    private final RGB color;

    private final int lineWidth;

    static {
        new PreferenceInitializer() {     
            @Override
            public void init() {
                IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        
                String colorStr;
                if ((colorStr = prefs.getString("ReferenceSet_color")).isEmpty()) {
                    colorStr = "Gray80";
                }
                RGB color = RGBColors.getRGBColor(colorStr);
        
                int lineWidth = prefs.getInt("ReferenceSet_width");
                if (lineWidth == 0) {
                    lineWidth = 1;
                }
        
                new RefSetAppearanceChangedMsg(color, lineWidth).send();
            }
        }.run();
    }

    public RefSetAppearanceChangedMsg(RGB color, int lineWidth) {
        super();
        this.color = color;
        this.lineWidth = lineWidth;
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @return the lineWidth
     */
    public int getLineWidth() {
        return lineWidth;
    }
}
