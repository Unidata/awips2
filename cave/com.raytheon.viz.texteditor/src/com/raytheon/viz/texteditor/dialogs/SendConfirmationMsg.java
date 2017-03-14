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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.qc.QualityControl;

/**
 * Produces the product message and mode message for the warngen confirmation
 * dialog for sending a warning.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013 2176       jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SendConfirmationMsg implements IWarnGenConfirmationable {

    private String title;

    private boolean resend;

    private String afosId;

    public SendConfirmationMsg(boolean resend, String afosId, String nnn) {
        this.resend = resend;
        this.afosId = afosId;
        title = QualityControl.getProductWarningType(nnn);
    }

    @Override
    public boolean checkWarningInfo(String header, String body, String nnn) {
        return true;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public String getProductMessage() {
        StringBuilder productMessage = new StringBuilder();
        if (resend) {
            productMessage.append("You are about to RESEND a " + afosId + "\n");
            productMessage.append(title).append(".\n");
        } else {
            productMessage.append("You are about to SEND a " + afosId + "\n");
            productMessage.append(title).append(".\n");
        }
        return productMessage.toString();
    }

    @Override
    public String getModeMessage() {
        CAVEMode mode = CAVEMode.getMode();
        StringBuilder modeMessage = new StringBuilder();
        modeMessage.append("The workstation is in ").append(mode)
                .append(" mode.");
        if (resend) {
            modeMessage.append("\nThere is no QC check for resend product.");
        }

        Pattern p = Pattern.compile(".\\%[s].");
        Matcher m = p.matcher(TextEditorDialog.STORED_SENT_MSG);
        boolean result = (CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST
                .equals(mode));
        modeMessage.append(result ? m.replaceAll(" ") : m.replaceAll(" not "));

        return modeMessage.toString();
    }

}
