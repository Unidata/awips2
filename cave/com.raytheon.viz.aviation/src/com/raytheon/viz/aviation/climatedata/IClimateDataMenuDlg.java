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
package com.raytheon.viz.aviation.climatedata;

import java.util.List;

/**
 * Limited interface to ClimageDataDlg to allow messages to the status monitor,
 * set its cursor and enable status of some buttons.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2016 5693       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public interface IClimateDataMenuDlg {
    public void assessBtn(boolean enabled);

    public void checkSite();

    public void commitBtn(boolean enabled);

    public void executeDone();

    public void executeStart();

    public List<String> getSites();

    public void overwriteMonitor(String msg);

    public void populateSiteInfoList(String ident, List<List<String>> list);

    public void processBtn(boolean enabled);

    public void rejectBtn(boolean enabled);

    public void saveLogBtn(boolean enabled);

    public void scriptsBtn(boolean enabled);

    public void updateMonitor(String msg);

    public void validateBtn(boolean enabled);
}