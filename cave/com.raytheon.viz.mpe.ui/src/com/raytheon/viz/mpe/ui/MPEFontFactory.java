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
 * MPE Font factory, uses preference ids to create fonts for a target
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

public class MPEFontFactory {

    private static final String DEFAULT_ID = "com.raytheon.viz.mpe.ui.defaultFont";

    private static Map<Class<?>, String> idMap = new HashMap<Class<?>, String>();
    static {
        idMap.put(MPELegendResource.class, MPELegendResource.class.getName());
        idMap.put(MPEGageResource.class, MPEGageResource.class.getName());
    }

    private Map<String, IFont> fontMap;

    private IGraphicsTarget target;

    private String baseId = DEFAULT_ID;

    public MPEFontFactory(IGraphicsTarget target, Object referenceObject) {
        this.target = target;
        this.fontMap = new HashMap<String, IFont>();
        if (referenceObject != null) {
            String id = idMap.get(referenceObject.getClass());
            if (id != null) {
                this.baseId = id;
            }
        }
    }

    public void dispose() {
        for (IFont font : fontMap.values()) {
            font.dispose();
        }
        fontMap.clear();
    }

    /**
     * Get the IFont with the font name, fonts are cached and should not be
     * disposed of directly but through {@link #dispose()}
     * 
     * @param fontName
     * @return
     */
    public IFont getMPEFont(String fontName) {
        IFont font = fontMap.get(fontName);
        if (font == null) {
            String fontId = baseId + "." + fontName;
            font = target.initializeFont(fontId);
            fontMap.put(fontName, font);
        }
        return font;
    }

}
