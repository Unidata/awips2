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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.Date;
import java.util.NavigableMap;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 26, 2010           lvenable  Initial creation
 * Jul 16, 2018  6766     randerso  Code cleanup
 *
 * </pre>
 *
 * @author lvenable
 */

public interface IRequestTimeHeightData {
    NavigableMap<Long, DMDTableDataRow> requestTimeHeightData(
            SCANConfigEnums.DMDTable tableCol, String dmdIdent);

    Date getDialogTime();

    DrawSettings getDrawSettings();

    void setDrawSettings(DrawSettings drawSettings);
}
