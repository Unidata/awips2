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
package com.raytheon.uf.edex.esb.camel.cluster.quartz;

import org.apache.camel.Exchange;
import org.apache.camel.component.quartz.QuartzMessage;
import org.quartz.JobExecutionContext;

/**
 * Class solely exists to get Camel's JMS to stop complaining about sending null
 * messages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ClusteredQuartzMessage extends QuartzMessage {

    public ClusteredQuartzMessage(Exchange exchange,
            JobExecutionContext jobExecutionContext) {
        super(exchange, jobExecutionContext);
        this.setBody("");
    }

}
