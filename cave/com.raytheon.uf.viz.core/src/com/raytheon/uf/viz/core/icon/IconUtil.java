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
package com.raytheon.uf.viz.core.icon;

import java.io.File;
import java.net.URL;

import org.eclipse.core.internal.registry.osgi.OSGIUtils;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.osgi.framework.Bundle;

/**
 * Utilities for using icons in Viz
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class IconUtil {

    /**
     * Creates an ImageDescriptor for an icon
     * 
     * @param bundleName
     *            the name of the bundle to find the icon in
     * @param name
     *            the name of the icon
     * @return the ImageDescriptor corresponding to the icon
     */
    public static ImageDescriptor getImageDescriptor(String bundleName,
            String name) {
        return getImageDescriptor(OSGIUtils.getDefault().getBundle(bundleName),
                name);
    }

    /**
     * Creates an ImageDescriptor for an icon
     * 
     * @param bundle
     *            the bundle to find the icon in
     * @param name
     *            the name of the icon
     * @return the ImageDescriptor corresponding to the icon
     */
    public static ImageDescriptor getImageDescriptor(Bundle bundle, String name) {
        String path = null;
        if (!name.startsWith("icons")) {
            path = "icons" + File.separator + name;
        } else {
            path = name;
        }
        URL url = FileLocator.find(bundle, new Path(path), null);
        return ImageDescriptor.createFromURL(url);
    }

}
