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

package com.raytheon.edex.utility;

/**
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09August2006	17			Phillippe	Initial Creation	
 * 
 * </pre>
 *
 * @author bphillip
 * @version 1
 */


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Monitors a given data stream.
 * 
 * @author bphillip
 * 
 */
public class StreamHandler extends Thread {

	/** The input stream being monitored */
	InputStream inStream;

	/** A record of the data passing through the stream */
	String output;

	/**
	 * StreamHandler Constructor
	 * 
	 * @param is -
	 *            the input stream to be monitored
	 */
	public StreamHandler(InputStream is) {
		this.inStream = is;
		output = "";
	}

	/**
	 * Continuously samples a stream and records its contents.
	 */
	@Override
	public void run() {
		try {
			InputStreamReader inStreamReader = new InputStreamReader(inStream);
			BufferedReader in = new BufferedReader(inStreamReader);
			String line = "";
			while ((line = in.readLine()) != null) {
				output += line + "\n";
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Retrieves the data that has passed through the stream since monitoring
	 * began.
	 * 
	 * @return - The contents of the stream
	 */
	public String getOutput() {
		return output;
	}
}
