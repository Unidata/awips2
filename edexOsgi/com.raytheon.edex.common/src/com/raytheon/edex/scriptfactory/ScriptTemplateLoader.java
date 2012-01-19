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
package com.raytheon.edex.scriptfactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ScriptTemplateLoader extends FileResourceLoader {

    private String path;

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#getLastModified(org.apache.velocity.runtime.resource.Resource)
     */
    @Override
    public long getLastModified(Resource resource) {
        File file = new File(resource.getName());
        return file.lastModified();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#getResourceStream(java.lang.String)
     */
    @Override
    public InputStream getResourceStream(String arg0)
            throws ResourceNotFoundException {
        try {
            String dir = null;
            if (arg0.startsWith(File.separator) || arg0.charAt(1) == ':' )   //Win32
                dir = arg0;
            else
                dir = this.path + File.separator + arg0;

            FileInputStream fis = new FileInputStream(dir);
            return fis;
        } catch (FileNotFoundException e) {
            throw new ResourceNotFoundException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#init(org.apache.commons.collections.ExtendedProperties)
     */
    @Override
    public void init(ExtendedProperties arg0) {
        this.path = arg0.getString("path");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.velocity.runtime.resource.loader.FileResourceLoader#isSourceModified(org.apache.velocity.runtime.resource.Resource)
     */
    @Override
    public boolean isSourceModified(Resource arg0) {
        File file = new File(arg0.getName());
        return (file.lastModified() != arg0.getLastModified());
    }

}
