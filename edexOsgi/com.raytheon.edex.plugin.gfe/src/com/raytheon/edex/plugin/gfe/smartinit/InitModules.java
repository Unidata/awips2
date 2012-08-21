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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.VGridDatabase;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;

/**
 * Static methods to determine if ingested grib data corresponds to smart inits
 * that should run.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 23, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InitModules {

    private static final Log logger = LogFactory.getLog(InitModules.class);

    // Counter used to simply force one full model run to complete a smart init
    // before another when many inits started at once
    private static long manualOffset = 0;

    public static boolean isManual(String initName) {
        return !(initName.endsWith(":0"));
    }

    public static Collection<SmartInitRecord> splitManual(String initName) {
        List<SmartInitRecord> rval = new ArrayList<SmartInitRecord>(60);

        try {
            if (initName == null) {
                return rval;
            }

            // OAX_GRID_D2D_RUC13_20100923_0900 or
            // OAX_GRID_D2D_RUC13_20100923_0900:1 or
            // OAX_GRID_D2D_RUC13_20100923_0900:1:myRUC13
            String[] tokens = initName.split("[:]");

            int index = tokens[0].indexOf("_GRID_D2D_");
            if (index < 0) {
                return rval;
            }

            DatabaseID dbId = new DatabaseID(tokens[0]);
            VGridDatabase db = (VGridDatabase) GridParmManager.getDb(dbId);

            boolean calcAll = true;
            if (tokens.length > 1 && tokens[1].equals("0")) {
                calcAll = false;
            }

            List<String> siteInitModules;
            String gfeModel = dbId.getModelName();
            String dbName = dbId.toString();

            if (tokens.length > 2 && tokens[2].length() > 0) {
                siteInitModules = new ArrayList<String>();
                siteInitModules.add(tokens[2]);
            } else {
                IFPServerConfig config = IFPServerConfigManager
                        .getServerConfig(dbId.getSiteId());
                siteInitModules = config.initModels(gfeModel);
            }

            int priority = SmartInitRecord.MANUAL_SMART_INIT_PRIORITY;
            if (tokens.length > 3) {
                priority = Integer.parseInt(tokens[3]);
            }

            SortedSet<Date> validTimes = db.getValidTimes();

            for (String module : siteInitModules) {
                for (Date validTime : validTimes) {
                    SmartInitRecordPK pk = new SmartInitRecordPK(
                            dbName.replace(gfeModel, module), validTime);
                    SmartInitRecord record = new SmartInitRecord();
                    record.setId(pk);
                    record.setInsertTime(new Date(manualOffset++));
                    record.setSmartInit(module);
                    record.setDbName(dbName);
                    record.setManual(calcAll);
                    record.setPriority(priority);
                    rval.add(record);
                }
            }
        } catch (Exception e) {
            logger.error("Failed to parse manual smartInit request", e);
        }

        return rval;
    }
}
