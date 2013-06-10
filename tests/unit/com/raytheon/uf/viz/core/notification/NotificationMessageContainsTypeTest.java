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
package com.raytheon.uf.viz.core.notification;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import javax.jms.JMSException;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBException;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.SharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Test {@link NotificationMessageContainsType}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 05, 2013 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class NotificationMessageContainsTypeTest {

    private static final TextMessage message = mock(TextMessage.class);

    private static final NotificationMessage notificationMessage = new NotificationMessage(
            message);

    @BeforeClass
    public static void classSetUp() throws JMSException, JAXBException {
        when(message.getText()).thenReturn(
                SerializationUtil.marshalToXml(new SiteSubscription()));
    }

    @Test
    public void returnsTrueWhenPayloadIsOneOfSeveralExpectedTypes()
            throws JMSException, JAXBException {

        NotificationMessageContainsType condition = new NotificationMessageContainsType(
                String.class, SiteSubscription.class);
        assertThat(
                condition
                        .matchesCondition(new NotificationMessage[] { notificationMessage }),
                is(true));
    }

    @Test
    public void returnsTrueWhenPayloadIsOnlyExpectedType() throws JMSException,
            JAXBException {

        NotificationMessageContainsType condition = new NotificationMessageContainsType(
                SiteSubscription.class);
        assertThat(
                condition
                        .matchesCondition(new NotificationMessage[] { notificationMessage }),
                is(true));
    }

    @Test
    public void returnsFalseWhenPayloadIsNotAnExpectedType()
            throws JMSException, JAXBException {

        NotificationMessageContainsType condition = new NotificationMessageContainsType(
                String.class, SharedSubscription.class);
        assertThat(
                condition
                        .matchesCondition(new NotificationMessage[] { notificationMessage }),
                is(false));
    }

    @Test
    public void returnsFalseWhenPayloadIsNotTheOnlyExpectedType()
            throws JMSException, JAXBException {

        NotificationMessageContainsType condition = new NotificationMessageContainsType(
                String.class);
        assertThat(
                condition
                        .matchesCondition(new NotificationMessage[] { notificationMessage }),
                is(false));
    }

}
