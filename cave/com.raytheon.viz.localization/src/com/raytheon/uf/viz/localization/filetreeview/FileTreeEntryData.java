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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;

import com.raytheon.uf.common.localization.LocalizationUtil;

/**
 * File Tree data object for the application level node or folder
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

public class FileTreeEntryData {

    private IResource resource;

    private boolean requestedChildren = false;

    private PathData pathData;

    private String name;

    private String path;

    public FileTreeEntryData(PathData pathData, String path) {
        this(pathData, path, true);
    }

    public FileTreeEntryData(PathData pathData, String path, boolean directory) {
        this.pathData = pathData;
        this.path = path;
        this.name = LocalizationUtil.extractName(path);
    }

    public PathData getPathData() {
        return pathData;
    }

    public String getName() {
        return name;
    }

    public String getPath() {
        return path;
    }

    public boolean hasRequestedChildren() {
        return requestedChildren;
    }

    public void setRequestedChildren(boolean requestedChildren) {
        this.requestedChildren = requestedChildren;
    }

    public void setName(String name) {
        this.name = name;
    }

    public IResource getResource() {
        return resource;
    }

    public void setResource(IResource resource) {
        this.resource = resource;
    }

    public boolean isDirectory() {
        return resource instanceof IFolder;
    }

}
