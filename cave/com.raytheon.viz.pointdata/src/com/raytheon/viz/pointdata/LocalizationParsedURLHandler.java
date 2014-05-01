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
package com.raytheon.viz.pointdata;

import java.io.File;

import org.apache.batik.util.ParsedURL;
import org.apache.batik.util.ParsedURLData;
import org.apache.batik.util.ParsedURLDefaultProtocolHandler;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;

/**
 * parse file URLs to go through localization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationParsedURLHandler extends
        ParsedURLDefaultProtocolHandler {

    private IPathManager pathManager;

    public LocalizationParsedURLHandler() {
        super("file");
        pathManager = PathManagerFactory.getPathManager();
    }

    @Override
    public ParsedURLData parseURL(String urlStr) {
        ParsedURLData data = null;
        if (urlStr != null && urlStr.startsWith("file:") == false
                && urlStr.startsWith("#") == false
                && urlStr.startsWith(IPathManager.SEPARATOR) == false) { // Win32
            String name = urlStr;
            String endName = "";
            int idx = name.indexOf("#");
            if (idx > -1) {
                endName = name.substring(idx);
                name = name.substring(0, idx);
            }
            File file = pathManager.getStaticFile(PlotResourceData.PLOT_DIR
                    + name);
            if (file != null) {
                // Win32: Change to convert both Linux and Win32 paths
                // Win32 path -> URL needs separator changed and "/" pre-pended
                String absPath = file.getAbsolutePath();
                absPath = absPath.replace(File.separator,
                        IPathManager.SEPARATOR);
                if (absPath.startsWith(IPathManager.SEPARATOR) == false)
                    absPath = IPathManager.SEPARATOR + absPath;
                data = super.parseURL("file:" + absPath + endName);
            }
        }

        if (data == null) {
            data = super.parseURL(urlStr);
        }
        return data;
    }

    @Override
    public ParsedURLData parseURL(ParsedURL baseURL, String urlStr) {
        ParsedURLData data = null;
        if (urlStr != null && urlStr.startsWith("file:") == false
                && urlStr.startsWith("#") == false
                && urlStr.startsWith(IPathManager.SEPARATOR) == false) { // Win32
            String name = urlStr;
            String endName = "";
            int idx = name.indexOf("#");
            if (idx > -1) {
                endName = name.substring(idx);
                name = name.substring(0, idx);
            }
            File file = pathManager.getStaticFile(PlotResourceData.PLOT_DIR
                    + name);
            if (file != null) {
                // Win32: Change to convert both Linux and Win32 paths
                // Win32 path -> URL needs separator changed and "/" pre-pended
                String absPath = file.getAbsolutePath();
                absPath = absPath.replace(File.separator,
                        IPathManager.SEPARATOR);
                if (absPath.startsWith(IPathManager.SEPARATOR) == false)
                    absPath = IPathManager.SEPARATOR + absPath;
                data = super.parseURL("file:" + absPath + endName);
            }
        }
        if (data == null) {
            data = super.parseURL(baseURL, urlStr);
        }
        return data;
    }
}
