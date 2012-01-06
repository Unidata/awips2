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
package com.raytheon.uf.edex.core;


public interface IMessageProducer {

    /**
     * Sends a message asynchronously.
     * 
     * @param endpoint
     *            the route id to send the message to
     * @param message
     *            the message to send
     * @throws EdexException
     */
    public abstract void sendAsync(String endpoint, Object message)
            throws EdexException;

    /**
     * Sends a message synchronously.
     * 
     * @param endpoint
     *            the route id to send the message to
     * @param message
     *            the message to send
     * @return the result
     * @throws EdexException
     */
    public abstract Object sendSync(String endpoint, Object message)
            throws EdexException;

    /**
     * Sends a message asynchronously.
     * 
     * @param uri
     *            the uri to send the message to
     * @param message
     *            the message to send
     */
    public void sendAsyncUri(String uri, Object message) throws EdexException;

}