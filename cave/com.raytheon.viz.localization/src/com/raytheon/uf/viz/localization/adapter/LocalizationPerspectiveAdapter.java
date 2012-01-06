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
package com.raytheon.uf.viz.localization.adapter;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorRegistry;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;

/**
 * Localization Configuration Adapter. Localization extension point contributors
 * can specify their adapter to use in order to provide context menu items when
 * files in the extension contribution are selected
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LocalizationPerspectiveAdapter {

    /**
     * Given the selected tree data items, add context menu items specific to
     * your type. Default implementation does nothing
     * 
     * @param menuMgr
     * @param selectedData
     *            (It can be assumed that all of selectedData objects correspond
     *            to your type)
     * @return true if items were added false otherwise
     */
    public boolean addContextMenuItems(IMenuManager menuMgr,
            FileTreeEntryData[] selectedData) {
        // Do nothing
        return false;
    }

    /**
     * Get any editors that the LocalizationFile file can be loaded into.
     * Editors should be looked up by id using
     * IEditorRegistry.findEditor(String), IEditorRegistry can be retrieved
     * through LocalziationPerspectiveUtils.getEditorRegistry(). Should not
     * return null and should not return any editors that are already registered
     * for the file type
     * 
     * @param registry
     * @param file
     * @return
     */
    public IEditorDescriptor[] getLoadableEditors(IEditorRegistry registry,
            LocalizationFile file) {
        return new IEditorDescriptor[] {};
    }
}
