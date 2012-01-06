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
package com.raytheon.uf.viz.localization.service;

import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;

/**
 * Localization Service Interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface ILocalizationService extends ILocalizationFileObserver {

    /**
     * Activate the editor in the perspective
     * 
     * @param editor
     */
    public void activateEditor(IEditorPart editor);

    /**
     * Select a file in the File Tree View.
     * 
     * @param file
     *            The file to select
     */
    public void selectFile(LocalizationFile file);

    /**
     * Refresh a file in the tree, could be directory
     * 
     * @param file
     *            The file to refresh
     */
    public void refresh(LocalizationFile file);

    /**
     * Open a file in the editor.
     * 
     * @param file
     *            The LocalizationFile to open
     */
    public void openFile(LocalizationFile file);

}
