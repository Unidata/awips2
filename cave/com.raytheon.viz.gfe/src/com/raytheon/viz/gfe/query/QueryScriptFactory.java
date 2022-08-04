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
package com.raytheon.viz.gfe.query;

import java.io.IOException;

import jep.JepException;

import com.raytheon.uf.common.python.concurrent.PythonInterpreterFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Instantiates a {@link QueryScript} on a separate thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2013            mnash       Initial creation
 * Dec 14, 2015  #4816     dgilling    Rewrite based on PythonInterpreterFactory.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class QueryScriptFactory implements
        PythonInterpreterFactory<QueryScript> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryScriptFactory.class);

    private DataManager manager;

    /**
     * 
     */
    public QueryScriptFactory(DataManager dataMgr) {
        this.manager = dataMgr;
    }

    @Override
    public QueryScript createPythonScript() {
        try {
            return new QueryScript(manager);
        } catch (JepException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to create query script", e);
        } catch (IOException e) {
            statusHandler.error(
                    "Unable to create query script from internal bundle", e);
        }
        return null;
    }
}
