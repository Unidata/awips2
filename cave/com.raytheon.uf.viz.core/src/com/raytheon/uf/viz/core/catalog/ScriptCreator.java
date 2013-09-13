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

package com.raytheon.uf.viz.core.catalog;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.velocity.VelocityManager;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Creates uEngine scripts on the fly. DEPRECATED: Requests from viz should go
 * through ThriftClient to the thrift service instead of using ScriptCreator and
 * then going to the uengine service. The thrift service performs faster and is
 * more maintainable. Use ThriftClient.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    11/25/06                  brockwoo    Initial Creation.
 *    12/06/06      102         brockwoo    Fix for contour script generation error.
 *    10/2/2007     459         grichard    Added createScript method.
 *    10/8/2007     459         grichard    Changed createScript to createQueryScript.
 *    10/8/2007     459         grichard    Added createUpdateScript method.
 *    10/12/2007    482         grichard    Reformatted file.
 *    12/17/2007    639         grichard    Added &quot;fxa&quot; parm to scripts.
 *    3/17/2008     933         grichard    Added support for taf plugin.
 *    04/14/2008                chammack    Complete refactor to Velocity
 *    Feb 15, 2013  1638        mschenke    Created common VelocityManager for executing scripts
 *    Sep 12, 2013  2277        mschenke    Remove references to this class and deleted unused
 *                                          functionality
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
@Deprecated
public class ScriptCreator {

    private static String SCRIPT_LIBRARY = "BaseRequest";

    private static File SCRIPT_TEMPLATE;

    static {
        try {
            SCRIPT_TEMPLATE = new File(FileLocator.resolve(
                    FileLocator.find(Activator.getDefault().getBundle(),
                            new Path("scriptTemplates/standardTemplate.vm"),
                            null)).getPath());
        } catch (IOException e) {
            UFStatus.getHandler(ScriptCreator.class)
                    .handle(Priority.CRITICAL,
                            "Unable to load the standard script template.  Requesting products will not work until this is fixed.",
                            e);
        }
    }

    /**
     * Create a script from a LayerProperty class, with mode "select"
     * 
     * This is a convenience method for the more general
     * createScript(LayerProperty, String).
     * 
     * @param layerProperty
     *            the layer property
     * @return the script object
     * @throws VizException
     *             an exception if generation fails.
     */
    public static String createScript(LayerProperty layerProperty)
            throws VizException {
        return createScript(layerProperty.getEntryQueryParameters(true),
                layerProperty.getNumberOfImages(), "select");
    }

    /**
     * Create a script from metadata
     * 
     * 
     * @param queryTerms
     *            the query terms
     * @param maxRecords
     *            the maximum number of records
     * @param mode
     *            the mode for the script
     * @return the script
     * @throws VizException
     * 
     */
    private static String createScript(
            Map<String, RequestConstraint> queryTerms, int maxRecords,
            String mode) throws VizException {

        // long t0 = System.currentTimeMillis();

        if (SCRIPT_TEMPLATE == null) {
            throw new IllegalStateException(
                    "ScriptCreator did not initialize properly");
        }

        Map<String, Object> templateObjects = new HashMap<String, Object>();
        templateObjects.put("scriptMetadata", queryTerms);
        templateObjects.put("maxRecords", maxRecords);
        templateObjects.put("scriptLibrary", SCRIPT_LIBRARY);
        templateObjects.put("mode", mode);

        try {
            String script = VelocityManager.executeTemplate(
                    SCRIPT_TEMPLATE,
                    SCRIPT_TEMPLATE.getParentFile(),
                    new File(FileUtil.join(LocalizationManager.getUserDir(),
                            "logs")), templateObjects);
            // System.out.println("Script gen: "
            // + (System.currentTimeMillis() - t0));
            return script;
        } catch (Exception e) {
            throw new VizException(e);
        }

    }
}
