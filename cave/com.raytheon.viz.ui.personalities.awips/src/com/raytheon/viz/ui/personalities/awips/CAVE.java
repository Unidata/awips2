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
package com.raytheon.viz.ui.personalities.awips;


/**
 * This is the default component for CAVE that is the standard workbench with
 * all the perspectives.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 09, 2010            mschenke    Initial creation
 * Jul 01, 2013  2139      jsanchez    Loaded map tree at cave start up.
 * Oct 22, 2013  2361      njensen     Undid 2139 fix since 2158 fixes it more efficiently
 * Aug 29, 2014  3500      bclement    added postStartupActions()
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CAVE extends AbstractAWIPSComponent {

    public int getRuntimeModes() {
        return (ALERT_VIZ | WORKBENCH);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {

    }

}
