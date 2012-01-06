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

package com.raytheon.viz.texteditor.msgs;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;

/**
 * The IPythonTextEditorToolCallback interface specifies methods that query for
 * data from a python script and returns the results.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/18/2009   2191        rjpeter     Initial creation.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 
 * </pre>
 * 
 * @author rjpeter
 * 
 */

public interface IPythonTextEditorToolCallback {
    public StdTextProduct[] executeAFOSCommand(String afosCommand);

    public StdTextProduct[] executeAwipsQuery(String womid, String siteid,
            String awipsid, String hdrtime, String bbbid, String lastHours,
            String fullRead);

    public void displayMessage(String msg);
}
