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
package com.raytheon.uf.edex.registry.ebxml.services.query.adhoc;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * This class manages the adhoc queries stored under
 * edex_static/base/ebxml/adhocQueries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 29, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AdhocQueryExpressionManager {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AdhocQueryExpressionManager.class);

    /** The map of available adhocqueries */
    private Map<String, AdhocQueryExpression> expressionMap = new HashMap<String, AdhocQueryExpression>();

    /** The singleton instance */
    private static AdhocQueryExpressionManager instance;

    /**
     * Creates a new manager and initializes the prepared adhoc queries
     */
    private AdhocQueryExpressionManager() {
        loadQueries();
    }

    /**
     * Loads the predefined queries
     */
    private void loadQueries() {
        LocalizationFile[] files = PathManagerFactory.getPathManager()
                .listStaticFiles("ebxml/adhocQueries", new String[] { ".xml" },
                        true, true);
        File[] fileList = new File[files.length];
        for (int i = 0; i < fileList.length; i++) {
            fileList[i] = files[i].getFile();
        }

        for (int i = 0; i < fileList.length; i++) {

            AdhocQueryExpression obj = null;
            try {
                obj = (AdhocQueryExpression) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(fileList[i]);
            } catch (SerializationException e) {
                statusHandler.error("Error getting predefined adhoc queries.",
                        e);
            }
            this.expressionMap.put(obj.getQueryName(), obj);
        }
    }

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static AdhocQueryExpressionManager getInstance() {
        if (instance == null) {
            instance = new AdhocQueryExpressionManager();
        }
        return instance;
    }

    /**
     * Retrieves the adhoc query with the given name
     * 
     * @param name
     *            The name of the adhoc query to get
     * @return The adhoc query, null if not present
     */
    public AdhocQueryExpression getAdhocQueryExpression(String name) {
        return expressionMap.get(name);
    }
}
