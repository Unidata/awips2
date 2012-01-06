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
package com.raytheon.uf.viz.localization.filetreeview;

import org.eclipse.core.resources.IFile;

import com.raytheon.uf.common.localization.LocalizationFile;

/**
 * File Tree Localization File object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationFileEntryData extends FileTreeEntryData {

    private LocalizationFile file;

    /**
     * @param pathData
     * @param name
     */
    public LocalizationFileEntryData(PathData pathData, LocalizationFile file) {
        super(pathData, file.getName(), false);
        this.file = file;
    }

    public LocalizationFile getFile() {
        return file;
    }

    @Override
    public IFile getResource() {
        return (IFile) super.getResource();
    }

}
