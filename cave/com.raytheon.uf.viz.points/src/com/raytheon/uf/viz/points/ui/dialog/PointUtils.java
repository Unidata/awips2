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
package com.raytheon.uf.viz.points.ui.dialog;

import java.io.File;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.points.Activator;

/**
 * List of useful static methods to support the point dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointUtils {
    public static Image getImage(String name) {
        Image image = null;
        StringBuilder sb = new StringBuilder("icons");
        sb.append(File.separator).append(name).append(".gif");
        ImageDescriptor imageDescriptor = IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), sb.toString());
        if (imageDescriptor != null) {
            image = imageDescriptor.createImage();
        }
        return image;
    }
}
