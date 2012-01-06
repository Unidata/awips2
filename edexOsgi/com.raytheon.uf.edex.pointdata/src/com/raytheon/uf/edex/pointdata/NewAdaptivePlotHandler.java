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
package com.raytheon.uf.edex.pointdata;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.requests.NewAdaptivePlotRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.VariableSubstitutor;
import com.raytheon.uf.edex.core.EdexException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NewAdaptivePlotHandler implements
        IRequestHandler<NewAdaptivePlotRequest> {

    private static final String BUNDLE_DIR = "bundles" + IPathManager.SEPARATOR
            + "maps" + IPathManager.SEPARATOR;

    private static final String PLOT_DIR = "adaptivePlots";

    private static final String DATA_DIR = "basemaps" + IPathManager.SEPARATOR
            + PLOT_DIR + IPathManager.SEPARATOR;

    private static final String TEMPLATE = PLOT_DIR + IPathManager.SEPARATOR
            + "template.xml";

    private String template;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(NewAdaptivePlotRequest request)
            throws Exception {
        IPathManager pm = PathManagerFactory.getPathManager();

        if (template == null) {
            LocalizationFile file = pm.getStaticLocalizationFile(TEMPLATE);
            if (file == null) {
                throw new EdexException(
                        "Could not find adaptive plot template file: "
                                + TEMPLATE);
            }
            template = new String(file.read());
        }
        Map<String, String> variables = new HashMap<String, String>();
        variables.put("filePath", DATA_DIR + request.getFileName());
        variables.put("name", request.getBundleName());
        variables.put("description", request.getDescription());

        // Save the contents of the data file
        LocalizationFile dataFile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED),
                DATA_DIR + request.getFileName());
        dataFile.write(request.getFileContents().getBytes());
        dataFile.save();

        // Create the bundle file that points to the data file
        String bundleXml = VariableSubstitutor.processVariables(template,
                variables);
        LocalizationFile bundleFile = pm.getLocalizationFile(
                dataFile.getContext(), BUNDLE_DIR + request.getBundleName()
                        + ".xml");
        bundleFile.write(bundleXml.getBytes());
        bundleFile.save();
        return null;
    }
}
