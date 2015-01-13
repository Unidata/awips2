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
package com.raytheon.uf.viz.collaboration.ui.colors;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.collaboration.ui.colors.ColorInfoMap.ColorInfo;

/**
 * Interface for color configuration managers to keep track of custom color
 * settings for users. Colors are tracked by a string key which could be the
 * user name or attribute (eg site name).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2015  3709       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IColorConfigManager {

    /**
     * Set display colors
     * 
     * @param key
     * @param foreground
     * @param background
     */
    public void setColors(String key, RGB foreground, RGB background);

    /**
     * Get display colors
     * 
     * @param key
     * @return
     */
    public ColorInfo getColor(String key);

    /**
     * @return human readable description of color management
     */
    public String getDescription(String key);

}
