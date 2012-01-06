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
package com.raytheon.viz.mpe.ui;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.viz.mpe.ui.rsc.MPEGageResource;
import com.raytheon.viz.mpe.ui.rsc.MPELegendResource;

/**
 * MPE Font manager, uses preferences to retrieve fonts. Fonts are shared
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEFontManager {

    private static final String DEFAULT_ID = "com.raytheon.viz.mpe.ui.defaultFont";

    private static Map<Class<?>, String> idMap = new HashMap<Class<?>, String>();
    static {
        idMap.put(MPELegendResource.class, MPELegendResource.class.getName());
        idMap.put(MPEGageResource.class, MPEGageResource.class.getName());
    }

    private static Map<String, IFont> fontMap = new HashMap<String, IFont>();

    /**
     * Get the font using preference store values. If no preference value is
     * defined for the object, the default mpe font will be used. WARNING: These
     * fonts are shared, changes to them will result in changes to others using
     * them
     * 
     * @param obj
     * @param name
     * @param target
     * @return
     */
    public static IFont getFont(Object obj, String name, IGraphicsTarget target) {
        String id = null;
        if (obj == null) {
            id = DEFAULT_ID + "." + name;
        } else {
            String clazzId = idMap.get(obj.getClass());
            if (clazzId == null) {
                clazzId = DEFAULT_ID;
            }
            id = clazzId + "." + name;
        }

        IFont font = fontMap.get(id);
        if (font == null) {
            font = target.initializeFont(id);
            fontMap.put(id, font);
        }
        return font;
    }
}
