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
package com.raytheon.uf.edex.plugin.level.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.request.GetMasterLevelRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009 2924       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GetMasterLevelHandler implements
        IRequestHandler<GetMasterLevelRequest> {

    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public MasterLevel handleRequest(GetMasterLevelRequest request)
            throws Exception {
        if (request.isCreate()) {
            LevelFactory.getInstance().checkMasterLevel(
                    request.getMasterLevel());
        }

        // use LevelFactory on server to allow for using LevelFactory cache
        return LevelFactory.getInstance().getMasterLevel(
                request.getMasterLevel().getName());
    }
}
