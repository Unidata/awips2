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
package com.raytheon.edex.plugin.gfe.textproducts;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.reference.EditAreaLoader;
import com.raytheon.uf.common.dataplugin.gfe.request.ConfigureTextProductsRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handler class for ConfigureTextProductRequest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            wldougher     Initial creation
 * May 4, 2011            wldougher     Add script file creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class ConfigureTextProductsHandler implements
        IRequestHandler<ConfigureTextProductsRequest> {

    private transient Log log;

    protected CombinationsFileMaker combinationsFileMaker;

    protected AreaDictionaryMaker areaDictionaryMaker;

    public ConfigureTextProductsHandler() {
        log = LogFactory.getLog(getClass());
        combinationsFileMaker = new CombinationsFileMaker();
        areaDictionaryMaker = new AreaDictionaryMaker();
    }

    /**
     * This handles a request to configure the text products for a given site.
     * It's the server side of the configureTextProducts.py script. The script
     * allows several command-line options, which are passed in the request.
     * However, <em>this handler only cares about the site ID</em>. It assumes
     * the mode will be "create" and ignores the template and destination
     * directories, using the ones automatically obtained by Configurator for
     * the site.
     * <p>
     * The legacy code supported the extra options, and we may have to in the
     * future. Doing so would require several changes to Configurator.
     * </p>
     * 
     * @see com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest(com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(ConfigureTextProductsRequest request)
            throws Exception {
        String site = request.getSite();
        Configurator configurator = new Configurator(site);
        configurator.execute();
        log.info(String.format("configureTextProducts ran for site %s", site));

        Map<String, List<String>> editAreaMap = new HashMap<String, List<String>>();
        Map<String, Map<String, String>> editAreaAttrs = new HashMap<String, Map<String, String>>();
        new EditAreaLoader().load(site, editAreaMap, editAreaAttrs);
        combinationsFileMaker.genCombinationsFiles(site, editAreaMap);
        areaDictionaryMaker.genAreaDictionary(site, editAreaAttrs);
        return null;
    }
}
