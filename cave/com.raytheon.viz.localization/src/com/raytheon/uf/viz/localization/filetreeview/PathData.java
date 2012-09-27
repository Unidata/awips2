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

import java.util.Arrays;

import org.eclipse.core.runtime.IConfigurationElement;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;

/**
 * Holds the data for each extension contribution including context information
 * to search, filters, recursive
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PathData {
    /**
     * Path name.
     */
    private String name = null;

    /**
     * Localization Type.
     */
    private LocalizationType type = null;

    /**
     * The path of the file.
     */
    private String path = null;

    /**
     * The filter used when reading the files in.
     */
    private String[] filter = null;

    /**
     * The application this file belongs to.
     */
    private String application = null;

    /**
     * IConfigurationElement.
     */
    private IConfigurationElement element = null;

    /**
     * Search recursively for files.
     */
    private boolean recursive = false;

    /**
     * The Localization Adapter for this data object.
     */
    private LocalizationPerspectiveAdapter adapter = null;

    public PathData() {

    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the type
     */
    public LocalizationType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(LocalizationType type) {
        this.type = type;
    }

    /**
     * @return the path
     */
    public String getPath() {
        return path;
    }

    /**
     * @param path
     *            the path to set
     */
    public void setPath(String path) {
        this.path = path;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Name = " + name + "\n");
        sb.append("Path = " + path + "\n");
        sb.append("Type = " + type.name() + "\n");
        return sb.toString();
    }

    /**
     * @return the filter
     */
    public String[] getFilter() {
        return filter;
    }

    /**
     * @param filter
     *            the filter to set
     */
    public void setFilter(String[] filter) {
        this.filter = filter;
    }

    /**
     * Set the filter. Comma separated list of filters
     * 
     * @param filterList
     */
    public void setFilter(String filterList) {
        if (filterList != null) {
            if (filterList.indexOf(",") > -1) {
                filter = filterList.split(",");
            } else {
                filter = new String[1];
                filter[0] = filterList;
            }
        }
    }

    /**
     * Set the application.
     * 
     * @param application
     *            The application name
     */
    public void setApplication(String application) {
        this.application = application;
    }

    /**
     * Get the application name.
     * 
     * @return The application name
     */
    public String getApplication() {
        return application;
    }

    /**
     * @param recursive
     *            the recursive to set
     */
    public void setRecursive(boolean recursive) {
        this.recursive = recursive;
    }

    /**
     * @return the recursive
     */
    public boolean isRecursive() {
        return recursive;
    }

    /**
     * @param adapter
     *            the adapter to set
     */
    public void setAdapter(LocalizationPerspectiveAdapter adapter) {
        this.adapter = adapter;
    }

    /**
     * @return the adapter
     */
    public LocalizationPerspectiveAdapter getAdapter() {
        return adapter;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((application == null) ? 0 : application.hashCode());
        result = prime * result + Arrays.hashCode(filter);
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((path == null) ? 0 : path.hashCode());
        result = prime * result + (recursive ? 1231 : 1237);
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        PathData other = (PathData) obj;
        if (application == null) {
            if (other.application != null)
                return false;
        } else if (!application.equals(other.application))
            return false;
        if (!Arrays.equals(filter, other.filter))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (path == null) {
            if (other.path != null)
                return false;
        } else if (!path.equals(other.path))
            return false;
        if (recursive != other.recursive)
            return false;
        if (type != other.type)
            return false;
        return true;
    }
}
