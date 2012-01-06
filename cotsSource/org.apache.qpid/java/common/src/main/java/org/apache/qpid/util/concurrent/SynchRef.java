package org.apache.qpid.util.concurrent;
/*
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 */


/**
 * A SynchRef is an interface which is returned from the synchronous take and drain methods of {@link BatchSynchQueue},
 * allowing call-backs to be made against the synchronizing strucutre. It allows the consumer to communicate when it
 * wants producers that have their data taken to be unblocked.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Report number of records returned by a taking operation.
 * <tr><td> Provide call-back to release producers of taken records.
 * </table>
 */
public interface SynchRef
{
    /**
     * Reports the number of records taken by the take or drain operation.
     *
     * @return The number of records taken by the take or drain operation.
     */
    public int getNumRecords();

    /**
     * Any producers that have had their data elements taken from the queue but have not been unblocked are
     * unblocked when this method is called. The exception to this is producers that have had their data put back
     * onto the queue by a consumer. Producers that have had exceptions for their data items registered by consumers
     * will be unblocked but will not return from their put call normally, but with an exception instead.
     */
    public void unblockProducers();
}
