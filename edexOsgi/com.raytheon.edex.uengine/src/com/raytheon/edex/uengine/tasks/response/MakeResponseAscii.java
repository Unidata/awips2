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

package com.raytheon.edex.uengine.tasks.response;

import com.raytheon.edex.msg.ResponseMessageASCII;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Makes an ASCII response.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class MakeResponseAscii extends ScriptTask {

	private PluginDataObject record;

	private String input;

	public MakeResponseAscii(PluginDataObject aRecord, String anInput) {
		record = aRecord;
		input = anInput;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {
		String dataURI = (String) record.getIdentifier();
		String time = record.getDataTime().getValidTime().toString();
		String station = "StationID";
		String type = record.getPluginName();

		return ResponseMessageASCII.generateASCIIResponse(null, input, type,
				station, time, dataURI);
	}

}
