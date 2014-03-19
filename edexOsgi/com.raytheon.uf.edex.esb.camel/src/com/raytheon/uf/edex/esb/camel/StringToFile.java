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
package com.raytheon.uf.edex.esb.camel;

import java.io.File;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Performs a transform from Strings to Files
 * 
 * Also adds properties useful for instrumentation
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2008             chammack    Initial creation.
 * 15Jul2010          6624 garmendariz Log error and interrupt if file missing.
 * Mar 19, 2014 2726       rjpeter     Added debug logging of file being processed.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class StringToFile implements Processor {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StringToFile.class);

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    @Override
    public void process(Exchange arg0) throws Exception {
        String bodyString = null;
        Object payload = arg0.getIn().getBody();
        if (payload instanceof byte[]) {
            bodyString = new String((byte[]) payload);
        } else if (payload instanceof String) {
            bodyString = (String) payload;
        }
        File file = new File(bodyString);

        // if file does not exist, set fault to interrupt processing
        if (!file.exists()) {
            statusHandler.error("File does not exist : " + bodyString);
            arg0.getOut().setFault(true);
        } else {
            arg0.getIn().setBody(file);
            arg0.getIn().setHeader("ingestFileName", file.toString());
            arg0.getIn().setHeader("dequeueTime", System.currentTimeMillis());
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Processing file: " + file.toString());
            }
        }
    }
}
