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
package com.raytheon.uf.viz.pdc.data;

import java.util.List;
import java.util.Optional;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydrocommon.datamanager.PDCDataManager;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Structure used for managing the scrolled lists in the GUI; including the
 * PhysicalElement, TypeSource, and DataSource lists
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2008            mpduff      Initial creation
 * Jun 29, 2018 6839       tgurney     Cleanup, remove dead code
 * Jun 03, 2018 6983       dgilling    Implement checkShefPostLatest.
 * Sep 21, 2018 7379       mduff       Moved for PDC Refactor.
 * </pre>
 *
 * @author mpduff
 */

public class PointControlPeTs {
    private static PointControlPeTs pcPeTsRef = null;

    private int elementCount = PDCConstants.MISSING_VALUE;

    /** Holds the PE followed by the TS */
    private static List<String[]> orgBuf = null;

    /** current unique active ts list */
    private static String[] adhocTypeSourceBuffer = null;

    private static String[] adhocDataSrcBuf = null;

    private static String[] timestepTypeSourceBuffer = null;

    private static volatile Optional<Boolean> isShefProcOn = Optional.empty();

    private PointControlPeTs() {
    }

    public static synchronized PointControlPeTs getInstance() {
        if (pcPeTsRef == null) {
            pcPeTsRef = new PointControlPeTs();
            initialize();
        }

        return pcPeTsRef;
    }

    private static void initialize() {
        PDCDataManager dataManager = PDCDataManager.getInstance();

        List<String> result = dataManager.getTelmType(null);
        adhocDataSrcBuf = new String[result.toArray().length];
        adhocDataSrcBuf = result.toArray(new String[result.size()]);

        orgBuf = dataManager.getPeTs();
    }

    public static boolean checkShefProcObs() {
        if (!isShefProcOn.isPresent()) {
            String tokenValue = AppsDefaults.getInstance()
                    .getToken("shef_procobs");
            Boolean newValue = "ON".equalsIgnoreCase(tokenValue);
            isShefProcOn = Optional.of(newValue);
        }

        PDCOptionData pcOptions = PDCOptionData.getInstance();
        int processMode = isShefProcOn.get() ? 1 : 0;
        pcOptions.setProcessMode(processMode);

        return isShefProcOn.get();
    }

    public int getElementCount() {
        return elementCount;
    }

    public void setElementCount(int elementCount) {
        this.elementCount = elementCount;
    }

    public List<String[]> getOrgBuf() {
        return orgBuf;
    }

    public String[] getAdhocTypeSourceBuffer() {
        return adhocTypeSourceBuffer;
    }

    public void setAdhocTypeSourceBuffer(String[] adhocTypeSourceBuffer) {
        PointControlPeTs.adhocTypeSourceBuffer = adhocTypeSourceBuffer;
    }

    public String[] getAdhocDataSrcBuf() {
        return adhocDataSrcBuf;
    }

    public String[] getTimestepTypeSourceBuffer() {
        return timestepTypeSourceBuffer;
    }

    public void setTimestepTypeSourceBuffer(String[] timestepTypeSourceBuffer) {
        PointControlPeTs.timestepTypeSourceBuffer = timestepTypeSourceBuffer;
    }

}
