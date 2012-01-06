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

import java.net.URI;
import java.util.Date;
import java.util.Map;

import org.apache.camel.component.quartz.QuartzComponent;
import org.apache.camel.util.IntrospectionSupport;
import org.apache.camel.util.ObjectHelper;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ClusteredQuartzComponent extends QuartzComponent {

    @SuppressWarnings("unchecked")
    @Override
    protected ClusteredQuartzEndpoint createEndpoint(String uri,
            String remaining, Map parameters) throws Exception {
        ClusteredQuartzEndpoint answer = new ClusteredQuartzEndpoint(uri, this);

        // lets split the remaining into a group/name
        URI u = new URI(uri);
        String path = ObjectHelper.after(u.getPath(), "/");
        String host = u.getHost();
        String cron = getAndRemoveParameter(parameters, "cron", String.class);
        Boolean fireNow = getAndRemoveParameter(parameters, "fireNow",
                Boolean.class, Boolean.FALSE);

        // group can be optional, if so set it to Camel
        String name;
        String group;
        if (ObjectHelper.isNotEmpty(path) && ObjectHelper.isNotEmpty(host)) {
            group = host;
            name = path;
        } else {
            group = "Camel";
            name = host;
        }

        Map<String, Object> triggerParameters = IntrospectionSupport
                .extractProperties(parameters, "trigger.");
        Map<String, Object> jobParameters = IntrospectionSupport
                .extractProperties(parameters, "job.");

        // create the trigger either cron or simple
        Trigger trigger;
        if (ObjectHelper.isNotEmpty(cron)) {
            trigger = createCronTrigger(cron);
        } else {
            trigger = new SimpleTrigger();
            if (fireNow) {
                String intervalString = (String) triggerParameters
                        .get("repeatInterval");
                if (intervalString != null) {
                    long interval = Long.valueOf(intervalString);
                    trigger.setStartTime(new Date(System.currentTimeMillis()
                            - interval));
                }
            }
        }
        answer.setTrigger(trigger);

        trigger.setName(name);
        trigger.setGroup(group);

        setProperties(trigger, triggerParameters);
        setProperties(answer.getJobDetail(), jobParameters);

        return answer;
    }

}
