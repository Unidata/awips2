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
package com.raytheon.uf.common.velocity;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.collections.ExtendedProperties;
import org.apache.velocity.exception.ResourceNotFoundException;
import org.apache.velocity.runtime.resource.Resource;
import org.apache.velocity.runtime.resource.loader.FileResourceLoader;

/**
 * Provides a velocity file loading strategy that allows absolute path-based
 * script loading.
 * 
 * Traditional velocity file loader requires all scripts to be in a single
 * declared directory.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/17/2008   1088       chammack    Initial Creation.
 * 06/01/2012   DR 14555   D. Friedman Support new version of Velocity.
 * 03/12/2013   1638       njensen     Move to new plugin and rename
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VelocityTemplateLoader extends FileResourceLoader {

    private List<String> paths = new ArrayList<String>();

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#
     * getLastModified(org.apache.velocity.runtime.resource.Resource)
     */
    @Override
    public long getLastModified(Resource resource) {
        return resolvePath(resource.getName()).lastModified();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#
     * getResourceStream(java.lang.String)
     */
    @Override
    public InputStream getResourceStream(String resource)
            throws ResourceNotFoundException {
        try {
            return new FileInputStream(resolvePath(resource));
        } catch (FileNotFoundException e) {
            throw new ResourceNotFoundException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.velocity.runtime.resource.loader.FileResourceLoader#init(org
     * .apache.commons.collections.ExtendedProperties)
     */
    @Override
    public void init(ExtendedProperties props) {
        this.paths.addAll(Arrays.asList(props.getStringArray("path")));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#
     * isSourceModified(org.apache.velocity.runtime.resource.Resource)
     */
    @Override
    public boolean isSourceModified(Resource rsc) {
        File file = resolvePath(rsc.getName());
        return (file.lastModified() != rsc.getLastModified());
    }

    @Override
    public boolean resourceExists(String name) {
        return resolvePath(name).exists();
    }

    private File resolvePath(String path) {
        File file = new File(path);
        if (file.isAbsolute()) {
            // Absolute path, always use
            return file;
        }
        for (String p : paths) {
            File tmpFile = new File(p, path);
            if (tmpFile.exists()) {
                return tmpFile;
            }
        }
        return file;
    }

}
