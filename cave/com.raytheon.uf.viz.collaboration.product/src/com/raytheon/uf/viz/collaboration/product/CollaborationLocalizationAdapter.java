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
package com.raytheon.uf.viz.collaboration.product;

import java.io.File;

import com.raytheon.uf.common.localization.ILocalizationAdapter;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFile.ModifiableLocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;

/**
 * TODO Mimic CAVELocalizationAdapter!
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationLocalizationAdapter implements ILocalizationAdapter {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#getPath(com.
     * raytheon.uf.common.localization.LocalizationContext, java.lang.String)
     */
    @Override
    public File getPath(LocalizationContext context, String fileName) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.localization.ILocalizationAdapter#
     * getLocalizationMetadata
     * (com.raytheon.uf.common.localization.LocalizationContext[],
     * java.lang.String)
     */
    @Override
    public ListResponse[] getLocalizationMetadata(
            LocalizationContext[] context, String fileName)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#retrieve(com
     * .raytheon.uf.common.localization.LocalizationFile)
     */
    @Override
    public void retrieve(LocalizationFile file)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#save(com.raytheon
     * .uf.common.localization.LocalizationFile. ModifiableLocalizationFile)
     */
    @Override
    public boolean save(ModifiableLocalizationFile file)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#listDirectory
     * (com.raytheon.uf.common.localization.LocalizationContext[],
     * java.lang.String, boolean, boolean)
     */
    @Override
    public ListResponse[] listDirectory(LocalizationContext[] context,
            String path, boolean recursive, boolean filesOnly)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.localization.ILocalizationAdapter#
     * getLocalSearchHierarchy
     * (com.raytheon.uf.common.localization.LocalizationContext
     * .LocalizationType)
     */
    @Override
    public LocalizationContext[] getLocalSearchHierarchy(LocalizationType type) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#getStaticContexts
     * ()
     */
    @Override
    public LocalizationType[] getStaticContexts() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#getContext(com
     * .raytheon.uf.common.localization.LocalizationContext.LocalizationType,
     * com
     * .raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public LocalizationContext getContext(LocalizationType type,
            LocalizationLevel level) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#delete(com.raytheon
     * .uf.common.localization.LocalizationFile. ModifiableLocalizationFile)
     */
    @Override
    public boolean delete(ModifiableLocalizationFile file)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#getContextList
     * (com
     * .raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public String[] getContextList(LocalizationLevel level)
            throws LocalizationOpFailedException {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#getAvailableLevels
     * ()
     */
    @Override
    public LocalizationLevel[] getAvailableLevels() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationAdapter#exists(com.raytheon
     * .uf.common.localization.LocalizationFile)
     */
    @Override
    public boolean exists(LocalizationFile file) {
        // TODO Auto-generated method stub
        return false;
    }

}
