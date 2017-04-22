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
package com.raytheon.uf.viz.grib.wizard.save;

import java.io.IOException;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationPermissionDeniedException;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * 
 * Implementation of {@link GribWizardSave} which stores the data to
 * localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 26, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class LocalizationGribWizardSave extends GribWizardSave {

    private final IPathManager pathManager;

    private final LocalizationContext context;

    public LocalizationGribWizardSave(IPathManager pathManager,
            LocalizationContext context) {
        this.pathManager = pathManager;
        this.context = context;
    }

    @Override
    protected void save(String location, Object object) throws Exception {
        LocalizationContext context = this.context;
        if (object instanceof VbSourceList) {
            context = pathManager.getContext(LocalizationType.CAVE_STATIC,
                    this.context.getLocalizationLevel());
        }
        ILocalizationFile file = pathManager.getLocalizationFile(context,
                location);
        if (file.exists()) {
            throw new IOException(location + " already exists.");
        }
        try (SaveableOutputStream os = file.openOutputStream()) {
            JAXB.marshal(object, os);
            os.save();
        } catch (IOException e) {
            Throwable root = e;
            while (root != null) {
                if (root instanceof LocalizationPermissionDeniedException) {
                    throw new IOException(root.getLocalizedMessage(), root);
                }
                root = root.getCause();
            }
            throw e;
        }
    }

}
