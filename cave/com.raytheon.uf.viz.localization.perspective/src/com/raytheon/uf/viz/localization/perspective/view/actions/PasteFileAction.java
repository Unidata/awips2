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
package com.raytheon.uf.viz.localization.perspective.view.actions;

import java.util.List;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileGroupData;
import com.raytheon.uf.viz.localization.service.ILocalizationService;

/**
 * Action class for pasting a file to a specific localization level
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2010  6305       mpduff      Initial creation
 * Oct 13, 2015 4410       bsteffen    Allow localization perspective to mix
 *                                     files for multiple Localization Types.
 * Jan 15, 2016 5242       kbisanz     Replaced LocalizationFile with
 *                                     ILocalizationFile where possible
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PasteFileAction extends CopyToAction {

    private LocalizationFileGroupData dataToCopyTo;

    private LocalizationLevel pasteToProtectedLevel;

    public PasteFileAction(ILocalizationService service, LocalizationFile file,
            LocalizationFileGroupData data) {
        super(file, data.getPathData(), service);
        setText("Paste To");
        this.dataToCopyTo = data;
        // Grab the level this file is protected at (if any)
        for (LocalizationFileEntryData entry : dataToCopyTo.getChildrenData()) {
            pasteToProtectedLevel = entry.getFile().getProtectedLevel();
            break;
        }
    }

    @Override
    protected boolean isLevelEnabled(LocalizationLevel level) {
        return pasteToProtectedLevel == null
                || level.compareTo(pasteToProtectedLevel) <= 0;
    }

    @Override
    protected void run(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();

        List<LocalizationType> types = dataToCopyTo.getPathData().getTypes();
        LocalizationType type = file.getContext().getLocalizationType();
        if (!types.contains(type)) {
            type = types.get(0);
        }
        LocalizationContext ctx = pm.getContext(type, level);

        ILocalizationFile newFile = pm.getLocalizationFile(ctx,
                dataToCopyTo.getPath());
        removeAlternateTypeFiles(level);
        copyFile(newFile);
    }
}
