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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// CauseFailure provides a method to cause a failure in a messaging broker, usually used in conjunction with fail-over
    /// or other failure mode testing. In some cases failures may be automated, for example by shutting down an in-vm broker,
    /// or by sending a special control signal to a broker over a network connection. In other cases, it may be preferable
    /// to ask a user interactively to cause a failure scenario, in which case an implementation may display a prompt or
    /// dialog box asking for notification once the failure has been caused. The purpose of this interface is to abstract
    /// the exact cause and nature of a failure out of failure test cases.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Cause messaging broker failure.
    /// </table>
    /// </summary>
    public interface CauseFailure
    {
        /// <summary> Causes the active message broker to fail. </summary>
        void causeFailure();
    }
}