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
package com.raytheon.viz.gfe.python;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.Activator;

/**
 * A CAVE-specific subclass of <code>GfePyIncludeUtil</code> so we can use
 * Eclipse-only classes to locate files within the viz.gfe plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2013  #2033     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class GfeCavePyIncludeUtil extends GfePyIncludeUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeCavePyIncludeUtil.class);

    public static final String AUTO_TEST = FileUtil.join(PYTHON, "autotest");

    public static final String TESTS = FileUtil.join(PYTHON, "testFormatters");

    /**
     * A dummy default constructor. All methods/constants in this class should
     * be static.
     */
    private GfeCavePyIncludeUtil() {
        throw new AssertionError();
    }

    private static File getBundlePath(String path) {
        try {
            return new File(FileLocator.resolve(
                    FileLocator.find(Activator.getDefault().getBundle(),
                            new Path(path), null)).getPath());
        } catch (IOException e) {
            statusHandler.error("Cannot locate internal bundle path: " + path,
                    e);
            return null;
        }
    }

    public static String getAutotestIncludePath() {
        File includePath = getBundlePath(AUTO_TEST);
        return (includePath != null) ? includePath.getPath() : "";
    }

    public static String getTestsIncludePath() {
        File includePath = getBundlePath(TESTS);
        return (includePath != null) ? includePath.getPath() : "";
    }
}
