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
package com.raytheon.openfire.plugin.configuration.collaboration.util;

import java.io.File;

/**
 * Various utility methods that are utilized by the Httpd Collaboration plugin
 * that do not require a class to be instantiated.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2012            bkowal      Initial creation
 * Jan 06, 2013  2563      bclement    removed config preamble
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class HttpdCollaborationUtil {
    
    public static String encodeErrorMessage(Throwable e) {
        String content = e.getMessage();
        if (content == null) {
            // final attempt
            content = e.toString();
        }
        
        return "error : " + content;
    }
    
    public static String endPathIfNecessary(String _path) {
        if (_path.endsWith(File.separator)) {
            return _path;
        }
        
        return _path + File.separator;
    }

}