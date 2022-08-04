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
package com.raytheon.uf.viz.monitor.snow.threshold;

import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * This class manages the Snow thresholds for display and monitor.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 27, 2009  3963     lvenable  Initial creation
 * Feb 03, 2014  2757     skorolev  Fixed reInitialize()
 * May 21, 2014  3086     skorolev  Cleaned code.
 * Sep 04, 2014  3220     skorolev  Removed "site".
 * Sep 18, 2015  3873     skorolev  Adjusted to AppName and removed
 *                                  areaConfigMgr.
 * Dec 26, 2015  5114     skorolev  Corrected imports.
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds. Code
 *                                  cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
public class SnowThresholdMgr extends AbstractThresholdMgr {
    private static SnowThresholdMgr classInstance;

    /**
     * Constructor
     */
    private SnowThresholdMgr() {
        super("DefaultSnowDisplayThresholds.xml",
                "DefaultSnowMonitorThresholds.xml", AppName.SNOW);
        init();
    }

    /**
     * @return the class instance
     */
    public static synchronized SnowThresholdMgr getInstance() {
        if (classInstance == null) {
            classInstance = new SnowThresholdMgr();
        }
        return classInstance;
    }

    /**
     * Re-initialization of threshold manager.
     *
     * DR#11279: When monitor area configuration is changed, threshold manager
     * needs to be re-initialized using the new monitor area configuration
     */
    public static synchronized void reInitialize() {
        classInstance = null;
        getInstance();
    }
}
