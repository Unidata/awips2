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

package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Defines the list command
 * 
 * The list command lists all available files in a context
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack     Initial Creation.
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * Oct 01, 2013   2361     njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public class ListUtilityCommand extends AbstractUtilityCommand {

    @DynamicSerializeElement
    String subDirectory;

    @DynamicSerializeElement
    boolean recursive;

    @DynamicSerializeElement
    private boolean filesOnly;

    @DynamicSerializeElement
    private String localizedSite;

    /**
     * Constructor
     * 
     * @param context
     */
    public ListUtilityCommand(LocalizationContext context, String subDirectory,
            boolean recursive, boolean filesOnly, String localizedSite) {
        super(context);

        // Win32
        this.subDirectory = subDirectory.replace("\\", IPathManager.SEPARATOR);

        this.recursive = recursive;
        this.filesOnly = filesOnly;
        this.localizedSite = localizedSite;
    }

    /**
     * Constructor
     */
    public ListUtilityCommand() {

    }

    public String getLocalizedSite() {
        return localizedSite;
    }

    public void setLocalizedSite(String localizedSite) {
        this.localizedSite = localizedSite;
    }

    public String getSubDirectory() {
        return subDirectory;
    }

    public void setSubDirectory(String subDirectory) {
        // Win32
        this.subDirectory = subDirectory.replace("\\", IPathManager.SEPARATOR);
    }

    public boolean isRecursive() {
        return recursive;
    }

    public void setRecursive(boolean recursive) {
        this.recursive = recursive;
    }

    /**
     * @return the filesOnly
     */
    public boolean isFilesOnly() {
        return filesOnly;
    }

    /**
     * @param filesOnly
     *            the filesOnly to set
     */
    public void setFilesOnly(boolean filesOnly) {
        this.filesOnly = filesOnly;
    }

}
