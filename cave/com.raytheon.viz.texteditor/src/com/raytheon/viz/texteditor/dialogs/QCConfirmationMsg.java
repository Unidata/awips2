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

import com.raytheon.viz.texteditor.qc.QualityControl;

/**
 * Produces the product message and mode message for the warngen confirmation
 * dialog for warnings failing QC.
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

public class QCConfirmationMsg implements IWarnGenConfirmationable {

    private QualityControl qcCheck = new QualityControl();

    @Override
    public boolean checkWarningInfo(String header, String body, String nnn) {
        return qcCheck.checkWarningInfo(header, body, nnn);
    }

    @Override
    public String getTitle() {
        return "Problem Detected by QC";
    }

    @Override
    public String getProductMessage() {
        return qcCheck.getErrorMessage();
    }

    @Override
    public String getModeMessage() {
        return "Do you really want to Send?\n";
    }

}
