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
package com.raytheon.uf.viz.core.drawables;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Factory which can provide cached versions of {@link ColorMapTree} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 18, 2013  2421     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ColorMapTreeFactory {

    private static ColorMapTree baseTree;

    private static Object baseTreeLock = new Object();

    private static final Map<LocalizationLevel, ColorMapTree> treesByLevel = new HashMap<LocalizationLevel, ColorMapTree>();

    private static Object treesByLevelLock = new Object();

    /**
     * Get a tree for the BASE localization context. This tree will be different
     * from the tree returned by getTreeForLevel(LocalizationLevel.BASE) because
     * it will not be for the BASE level but instead is for the BASE context.
     * 
     */
    public static ColorMapTree getBaseTree() {
        synchronized (baseTreeLock) {
            if(baseTree == null){
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationContext baseContext = pm.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
                baseTree = new ColorMapTree(pm, baseContext,
                        ColorMapLoader.DIR_NAME);
            }
            return baseTree;
        }
        
    }

    /**
     * Return a {@link ColorMapTree}Tree for the provided level. The tree will
     * have the same name as the level and will have a subtree for each context
     * that exists at that level.
     */
    public static ColorMapTree getTreeForLevel(LocalizationLevel level) {
        synchronized (treesByLevelLock) {
            ColorMapTree tree = treesByLevel.get(level);
            if (tree == null) {
                IPathManager pm = PathManagerFactory.getPathManager();
                tree = new ColorMapTree(pm, level, ColorMapLoader.DIR_NAME);
                treesByLevel.put(level, tree);
            }
            return tree;
        }
    }

}