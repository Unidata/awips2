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
package com.raytheon.uf.viz.archive.data;

import java.util.List;

import com.raytheon.uf.common.archive.config.DisplayData;

/**
 * A listener to update file/directory information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2013 1966       rferrel     Initial creation
 * Jul 29, 2012 #2220      rferrel     Change to get all data sizes only one time.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public interface IUpdateListener {
    /**
     * Table display state entries with updated information.
     * 
     * @param dirInfos
     */
    public void update(List<DisplayData> request);
}
