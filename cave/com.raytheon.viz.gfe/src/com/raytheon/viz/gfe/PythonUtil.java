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

package com.raytheon.viz.gfe;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.localization.perspective.service.ILocalizationService;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;

/**
 * Common utilities for Python
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 * 
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    19Sep08                   askripsk    Initial Creation.
 * 
 * 
 * </pre>
 * 
 * @author askripsk
 */
public class PythonUtil {
    public static void openPythonFile(LocalizationFile file) {
        ILocalizationService service = LocalizationPerspectiveUtils
                .changeToLocalizationPerspective();
        if (service != null) {
            service.refresh(file);
            service.openFile(file);
            service.selectFile(file);
        }
    }
}
