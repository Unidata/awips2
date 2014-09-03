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
package com.raytheon.viz.ui.colormap;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;

/**
 * Use by {@link ColorMapTreeFactory} to perform an action for each non-Base
 * localization level. May be invoked from a non-UI thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2014 3516       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public interface ILevelMapsCallback {
    /**
     * Method invoked for each non-BASE localization level. When tree is null it
     * is being generated and this method will be called again once the tree is
     * created.
     * 
     * @param level
     * @param tree
     */
    public void treeCreated(LocalizationLevel level, ColorMapTree tree);
}
