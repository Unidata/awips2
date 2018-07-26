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
package com.raytheon.viz.avnconfig;

import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * A simple container to hold SWT Label and Text controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class LabelTextControls {
    /**
     * SWT Label.
     */
    public Label labelControl;

    /**
     * SWT Text control.
     */
    public Text textControl;

    /**
     * Constructor.
     */
    public LabelTextControls() {
    }

    /**
     * Constructor.
     * @param labelControl SWT Label.
     * @param textControl SWT Text control.
     */
    public LabelTextControls(Label labelControl, Text textControl) {
        this.labelControl = labelControl;
        this.textControl = textControl;
    }
}
