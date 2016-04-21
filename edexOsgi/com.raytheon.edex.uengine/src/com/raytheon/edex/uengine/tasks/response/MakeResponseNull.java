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

import java.util.Calendar;

import com.raytheon.edex.msg.ResponseMessageNull;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.uengine.tasks.query.DbQuery;
import com.raytheon.edex.uengine.tasks.query.TermQuery;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/3/2007    459         grichard    Changed query to DbQuery.
 * 	
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class MakeResponseNull extends ScriptTask {
	private String message;
	private String dataURI;

	/**
	 * 
	 * @param message
	 * @param query
	 */
	public MakeResponseNull(String message, DbQuery query) {
		this.message = message;
		if(query instanceof TermQuery)
		{
			this.dataURI = ((TermQuery) query).getMatchURI();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {
		String time = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", Calendar
				.getInstance());

		return ResponseMessageNull.generateNullResponse(message, dataURI, time);
	}

}
