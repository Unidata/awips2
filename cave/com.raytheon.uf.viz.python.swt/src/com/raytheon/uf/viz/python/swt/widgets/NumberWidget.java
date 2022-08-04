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
package com.raytheon.uf.viz.python.swt.widgets;

import org.eclipse.swt.events.VerifyEvent;

/**
 * This widget provides a text area for users to enter numerical values. The
 * widget ensures only numbers are entered.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jun 04, 2008  1164     jelkins   Initial creation
 * Jan 15, 2018  6684     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author jelkins
 */

public class NumberWidget extends InputWidget {

    /**
     * @param string
     */
    public NumberWidget(String string) {
        super();
        setLabel(string);
    }

    @Override
    public void verifyText(VerifyEvent event) {
        String firstPart = getText().substring(0, event.start);
        String lastPart = getText().substring(event.start, getText().length());

        String s = firstPart + event.text + lastPart;

        try {
            // attempt to parse text for syntax verification
            @SuppressWarnings("unused")
            Number n;
            if (s.indexOf('.') > -1) {
                n = Double.parseDouble(s);
            } else {
                n = Integer.parseInt(s);
            }
            hideHelpMessage();
        } catch (NumberFormatException e) {
            if (!"-".equals(s)) {
                event.doit = false;
                showHelpMessage("This field only accepts numbers.");
                return;
            }
        }

        super.verifyText(event);
    }

    @Override
    public void setValue(Object value) {
        Object newValue = value;
        if (value instanceof String) {
            String s = (String) value;
            try {
                if (s.indexOf('.') > -1) {
                    newValue = Double.parseDouble(s);
                } else {
                    newValue = Integer.parseInt(s);
                }
            } catch (NumberFormatException e) {
                newValue = new Float(0);
            }
        }
        super.setValue(newValue);
    }
}
