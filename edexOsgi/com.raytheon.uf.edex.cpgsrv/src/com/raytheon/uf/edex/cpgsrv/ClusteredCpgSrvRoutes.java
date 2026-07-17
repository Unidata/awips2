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

package com.raytheon.uf.edex.cpgsrv;

import com.raytheon.uf.edex.routes.EDEXRouteBuilder;

/**
 * Camel routes converted from file "cpgsrv-spring.xml", context
 * "clusteredCpgSrvRoutes"
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2024-08-27   2037701    lisa.singh   Initial creation (from auto-generated)
 *
 * </pre>
 */

// @formatter:off
/* Original XML definition
  <camelContext id="clusteredCpgSrvRoutes"
        xmlns="http://camel.apache.org/schema/spring"
        errorHandlerRef="errorHandler">
        <route id="cpgsrvListenerRoute">
            <!-- Data from plugin notification -->
            <from uri="jms-durable:queue:cpgsrvFiltering?concurrentConsumers=5"/>
            <doTry>
                  <pipeline>
                      <bean ref="serializationUtil" method="transformFromThrift" />
                    <bean ref="cpgSrvDispatcher" method="matchURIs"/>
                  </pipeline>
                  <doCatch>
                     <exception>java.lang.Throwable</exception>
                     <to uri="log:cpgSrv?level=ERROR"/>
                  </doCatch>
            </doTry>
        </route>
    </camelContext>

    <bean factory-bean="contextManager" factory-method="registerClusteredContext">
        <constructor-arg ref="clusteredCpgSrvRoutes" />
    </bean>
 */
// @formatter:on

public class ClusteredCpgSrvRoutes extends EDEXRouteBuilder {

    public ClusteredCpgSrvRoutes() {
    }

    @Override
    public void configure() throws Exception {
        // @formatter:off
        from("jms-durable:queue:cpgsrvFiltering?concurrentConsumers=5")
          .doTry()
              .pipeline()
                  .bean("serializationUtil", "transformFromThrift")
                  .bean("cpgSrvDispatcher", "matchURIs")
          .endDoTry()
          .doCatch(Throwable.class)
              .to("log:cpgSrv?level=ERROR")
          .endDoTry()
          .end()
          .setId("cpgsrvListenerRoute");
        // @formatter:on
    }
}
