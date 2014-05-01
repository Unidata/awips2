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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Adds properties for instrumentation to ensure log messages contain all
 * information.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2008             chammack    Initial creation
 * 15Jul2010          6624 garmendariz Log error and interrupt if file missing
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SetIngestHeaderFields implements Processor {

    protected transient Log logger = LogFactory.getLog(getClass());

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    @Override
    public void process(Exchange arg0) throws Exception {
        arg0.getIn().setHeader("dequeueTime", System.currentTimeMillis());
        Object payload = arg0.getIn().getBody();

        if (payload instanceof String) {
            String bodyString = (String) payload;
            File file = new File(bodyString);

            // if file does not exist, set fault to interrupt processing
            if (!file.exists()) {
                logger.error("File does not exist : " + bodyString);
                arg0.getOut().setFault(true);
            } else {
                arg0.getIn().setHeader("ingestFileName", file.toString());
            }
        }
    }
}
