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
package com.raytheon.uf.common.localization;

import java.io.File;
import java.net.URI;

import com.raytheon.uf.common.localization.FileLocker.Type;

/**
 * Internal LocalizationFile file, safely deletes Files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationInternalFile extends File {

    private static final long serialVersionUID = 2710325810993713984L;

    /**
     * @param parent
     * @param child
     */
    public LocalizationInternalFile(File parent, String child) {
        super(parent, child);
    }

    /**
     * @param parent
     * @param child
     */
    public LocalizationInternalFile(String parent, String child) {
        super(parent, child);
    }

    /**
     * @param pathname
     */
    public LocalizationInternalFile(String pathname) {
        super(pathname);
    }

    /**
     * @param uri
     */
    public LocalizationInternalFile(URI uri) {
        super(uri);
    }

    @Override
    public boolean delete() {
        FileLocker.lock(this, this, Type.WRITE);
        boolean rval = super.delete();
        FileLocker.unlock(this, this);
        return rval;
    }

    @Override
    public void deleteOnExit() {
        throw new UnsupportedOperationException(
                "Cannot specify a localization file to deleteOnExit");
    }

}
