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
package com.raytheon.uf.edex.plugin.ffmp.handler;

import java.io.InputStream;
import java.util.List;

import com.raytheon.uf.common.dataplugin.ffmp.request.FFMPGetDefaultPurgeHourRequest;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.purge.PurgeRule;
import com.raytheon.uf.edex.database.purge.PurgeRuleSet;

/**
 * Finds the default purge rule for FFMP data and returns it as an hour.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2018 6641       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class FFMPGetDefaultPurgeHourHandler
        implements IRequestHandler<FFMPGetDefaultPurgeHourRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPGetDefaultPurgeHourHandler.class);

    private SingleTypeJAXBManager<PurgeRuleSet> jaxbManager = SingleTypeJAXBManager
            .createWithoutException(PurgeRuleSet.class);

    @Override
    public Object handleRequest(FFMPGetDefaultPurgeHourRequest request)
            throws Exception {
        Integer retVal = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        String filename = "purge" + IPathManager.SEPARATOR
                + "ffmpPurgeRules.xml";
        ILocalizationFile lf = pathMgr.getStaticLocalizationFile(filename);
        if (lf.exists()) {
            try (InputStream is = lf.openInputStream()) {
                PurgeRuleSet purgeRS = jaxbManager.unmarshalFromInputStream(is);
                List<PurgeRule> rules = purgeRS.getDefaultRules();
                if (!rules.isEmpty()) {
                    PurgeRule defaultRule = rules.get(0);
                    long millis = defaultRule.getPeriodInMillis();
                    int hrs = (int) (millis / TimeUtil.MILLIS_PER_HOUR);
                    retVal = hrs;

                }
            } catch (Exception e) {
                statusHandler.error("Error determining default FFMP purge rule",
                        e);
            }
        }

        if (retVal == null) {
            // default to 24
            retVal = 24;
        }

        retVal = validateHours(retVal);

        return retVal;
    }

    private int validateHours(int hours) {
        if (hours < 12) {
            return 12;
        }

        if (hours > 36) {
            return 36;
        }

        return hours;
    }

}
