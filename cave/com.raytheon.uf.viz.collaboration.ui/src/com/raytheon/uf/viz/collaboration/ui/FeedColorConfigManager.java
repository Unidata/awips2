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
package com.raytheon.uf.viz.collaboration.ui;

import javax.swing.plaf.synth.ColorType;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.collaboration.ui.ColorInfoMap.ColorInfo;

/**
 * Configuration manager for reading/writing colors for each site to/from a
 * user-localized file
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2014 3708       bclement    Moved color methods from SiteConfigurationManager
 * Nov 26, 2014 3709       mapeters    Abstracted out code to {@link AbstractColorConfigManager}, 
 *                                     renamed from SiteColorConfigManager.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeedColorConfigManager extends AbstractColorConfigManager {

    private static final String FILE_PATH = "collaboration"
            + IPathManager.SEPARATOR + "siteColorInfo.xml";

    private static ColorInfoMap colorInfoMap;

    /**
     * Set and store the color type of the given site to be the given rgb.
     * 
     * @param site
     * @param type
     * @param rgb
     * @param defaultForeground
     */
    @Override
    public synchronized void setColor(String site,
            ColorType type, RGB rgb, RGB defaultForeground) {
        super.setColor(site, type, rgb, defaultForeground, FILE_PATH);
    }

    /**
     * Get the {@link ColorInfo} for the given site from memory.
     * 
     * @param site
     * @return
     */
    @Override
    public synchronized ColorInfo getColor(String site) {
        return super.getColor(site, FILE_PATH);
    }

    @Override
    protected ColorInfoMap getColorInfoMap() {
        return colorInfoMap;
    }

    @Override
    protected void setColorInfoMap(ColorInfoMap colorInfoMap) {
        FeedColorConfigManager.colorInfoMap = colorInfoMap;
    }
}
