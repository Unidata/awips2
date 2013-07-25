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
package com.raytheon.viz.texteditor.dialogs;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.GetActiveTableRequest;
import com.raytheon.uf.common.activetable.GetActiveTableResponse;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * Produces the product message and mode message for the warngen confirmation
 * dialog for emergency warnings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013  2176      jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class EmergencyConfirmationMsg implements IWarnGenConfirmationable {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EmergencyConfirmationMsg.class);

    private String productMessage;

    private static class EmergencyType {

        private static final EmergencyType TORNADO = new EmergencyType(
                "TORNADO EMERGENCY", "TO.W");

        private static final EmergencyType FLASH_FLOOD = new EmergencyType(
                "FLASH FLOOD EMERGENCY", "FF.W");

        private final String value;

        private final String phensig;

        private final static EmergencyType[] values = new EmergencyType[] {
                TORNADO, FLASH_FLOOD };

        private EmergencyType(String type, String phensig) {
            this.value = type;
            this.phensig = phensig;
        }

        public static EmergencyType valueOf(String phensig) {
            EmergencyType type = null;
            for (EmergencyType t : values) {
                if (t.phensig.equals(phensig)) {
                    type = t;
                    break;
                }
            }
            return type;
        }
    };

    /**
     * Orders the ActiveTableRecord based on the issue time (ascending)
     */
    private class ActiveTableRecordComparator implements
            Comparator<ActiveTableRecord> {

        @Override
        public int compare(ActiveTableRecord o1, ActiveTableRecord o2) {
            return o1.getIssueTime().compareTo(o2.getIssueTime());
        }

    }

    @Override
    public boolean checkWarningInfo(String header, String body, String nnn) {
        VtecObject vtec = VtecUtil.parseMessage(body);
        EmergencyType type = null;
        WarningAction action = null;
        if (vtec != null) {
            type = EmergencyType.valueOf(vtec.getPhensig());
            action = WarningAction.valueOf(vtec.getAction());
            if (action == WarningAction.CAN && body.split("\\$\\$").length > 2) {
                // It is possible for a warning products to have two segments: a
                // CAN and a CON.'$$' denotes the end of one segment. VtecUtil
                // only grabs the first VTEC. If there are multiple segments CAN
                // should always be the first VTEC
                action = WarningAction.CANCON;
            }
        }

        // Check if the warning product is a valid EmergencyType.
        if (type != null) {
            boolean currentEmergency = body.contains("EMERGENCY");
            if (action == WarningAction.NEW && currentEmergency) {
                // Only occurs when the warning is first issued and not any
                // other action
                productMessage = "This is a " + type.value;
            } else if (action == WarningAction.CON
                    || action == WarningAction.EXT
                    || action == WarningAction.CANCON) {
                // Check if the warning was an upgrade or downgrade in the
                // emergency warning for continuation, extension (FFW), or a
                // cancel
                // and continuation.
                GetActiveTableRequest request = new GetActiveTableRequest();
                if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
                    request.setMode(ActiveTableMode.PRACTICE);
                } else {
                    request.setMode(ActiveTableMode.OPERATIONAL);
                }
                request.setSiteID(vtec.getOffice());
                request.setPhensigList(vtec.getPhensig());
                request.setEtn(String.format("%04d", vtec.getSequence()));
                try {
                    GetActiveTableResponse response = (GetActiveTableResponse) ThriftClient
                            .sendRequest(request);
                    List<ActiveTableRecord> records = response.getActiveTable();
                    // There should be existing records since this is for follow
                    // ups. This is just a precaution
                    if (records != null && !records.isEmpty()) {
                        // Get latest active table record
                        Collections.sort(records,
                                new ActiveTableRecordComparator());
                        ActiveTableRecord record = records
                                .get(records.size() - 1);
                        boolean wasEmergency = record.getRawmessage().contains(
                                "EMERGENCY");
                        if (!wasEmergency && currentEmergency) {
                            productMessage = "This is an upgrade of a "
                                    + type.value;
                        } else if (wasEmergency && !currentEmergency) {
                            productMessage = "This is a downgrade of a "
                                    + type.value;
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error making request to active table.");
                }
            }
        }

        return productMessage == null;
    }

    @Override
    public String getTitle() {
        return "Severe Weather Product";
    }

    @Override
    public String getProductMessage() {
        return productMessage;
    }

    @Override
    public String getModeMessage() {
        return "Should we proceed?\n";
    }
}
