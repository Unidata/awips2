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
package com.raytheon.uf.edex.esb.camel.spring;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;

import org.springframework.jms.support.destination.DestinationResolver;
import org.springframework.jms.support.destination.DynamicDestinationResolver;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class QpidDestinationNameResolver extends DynamicDestinationResolver
        implements DestinationResolver {

    private String queueNamePrefix = "";

    private String queueNamePostfix = "";

    private String topicNamePrefix = "";

    private String topicNamePostfix = "";

    /*
     * (non-Javadoc)
     * 
     * @seeorg.springframework.jms.support.destination.DestinationResolver#
     * resolveDestinationName(javax.jms.Session, java.lang.String, boolean)
     */
    @Override
    public Destination resolveDestinationName(Session session,
            String destinationName, boolean pubSubDomain) throws JMSException {
        if (pubSubDomain) {
            destinationName = topicNamePrefix + destinationName
                    + topicNamePostfix;
        } else {
            destinationName = queueNamePrefix + destinationName
                    + queueNamePostfix;
        }

        return super.resolveDestinationName(session, destinationName,
                pubSubDomain);
    }

    public String getQueueNamePrefix() {
        return queueNamePrefix;
    }

    public void setQueueNamePrefix(String queueNamePrefix) {
        this.queueNamePrefix = queueNamePrefix;
    }

    public String getQueueNamePostfix() {
        return queueNamePostfix;
    }

    public void setQueueNamePostfix(String queueNamePostfix) {
        this.queueNamePostfix = queueNamePostfix;
    }

    public String getTopicNamePrefix() {
        return topicNamePrefix;
    }

    public void setTopicNamePrefix(String topicNamePrefix) {
        this.topicNamePrefix = topicNamePrefix;
    }

    public String getTopicNamePostfix() {
        return topicNamePostfix;
    }

    public void setTopicNamePostfix(String topicNamePostfix) {
        this.topicNamePostfix = topicNamePostfix;
    }
}
