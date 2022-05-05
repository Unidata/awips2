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
package com.raytheon.uf.viz.archive.ui;

import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXB;

import org.apache.commons.io.FileUtils;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class obtains the configurable options for the archive case creation
 * dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 11, 2014  3023     rferrel   Initial creation
 * Dec 18, 2018  7677     randerso  Removed overriding of non-positive values
 *                                  from files. Code cleanup.
 *
 * </pre>
 *
 * @author rferrel
 */

public class CaseCreationManager {
    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(CaseCreationManager.class);

    private CaseCreation caseCreation;

    public CaseCreationManager() {
        initValues();
    }

    private void initValues() {
        String path = "archiver" + IPathManager.SEPARATOR + "gui"
                + IPathManager.SEPARATOR + "CaseCreation.xml";
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pm.getStaticLocalizationFile(path);

        try (InputStream is = lf.openInputStream()) {
            caseCreation = JAXB.unmarshal(is, CaseCreation.class);
        } catch (LocalizationException | IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            caseCreation = new CaseCreation();
        }
    }

    public long getCautionThreshold() {
        return (long) (caseCreation.getCautionThreshold() * FileUtils.ONE_GB);
    }

    public long getDangerThreshold() {
        return (long) (caseCreation.getDangerThreshold() * FileUtils.ONE_GB);
    }

    public long getFatalThreshold() {
        return (long) (caseCreation.getFatalThreshold() * FileUtils.ONE_GB);
    }
}
