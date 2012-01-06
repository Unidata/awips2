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

import java.net.URI;
import java.util.GregorianCalendar;

import com.raytheon.edex.msg.ResponseMessageURI;
import com.raytheon.edex.uengine.ResponseUtil;
import com.raytheon.edex.uengine.tasks.ScriptTask;

/**
 * Makes a response that contains a URI to the product.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class MakeResponseUri extends ScriptTask {

	private URI productUri;

	private Object time;

	private String dataUri;

	private String format;

	public MakeResponseUri(URI aProductUri, Object aTime, Object aDataUri,
			String aFormat) {
		productUri = aProductUri;
		time = aTime;
		dataUri = (String) aDataUri;
		format = aFormat;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {
		ResponseMessageURI responseMsg = new ResponseMessageURI();
		// get the valid time
		GregorianCalendar validTime = null;
		if (time == null) {
			validTime = new GregorianCalendar();
		} else {
			validTime = ResponseUtil.getValidTime(time);
		}

		// get the Data URI
		if (dataUri == null) {
			dataUri = "";
		}

		// if this is a contours product, get the index of the name
		int index = 0;
		if ((index = format.indexOf("contours-")) != -1) {
			format = format.substring(index);
		}

		URI[] uris = new URI[1];
		uris[0] = productUri;
		responseMsg.setProductURI(uris);
		responseMsg.setFileType(format);
		responseMsg.setValidTime(validTime.getTime());
		responseMsg.setDataURI(dataUri);

		return responseMsg;
	}

}
