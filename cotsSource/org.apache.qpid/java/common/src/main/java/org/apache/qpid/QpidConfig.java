package org.apache.qpid;
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
 * API to configure the Security parameters of the client.
 * The user can choose to pick the config from any source 
 * and set it using this class.
 *
 */
public class QpidConfig
{
    private static QpidConfig _instance = new QpidConfig();
    
    private SecurityMechanism[] securityMechanisms = 
        new SecurityMechanism[]{new SecurityMechanism("PLAIN","org.apache.qpid.security.UsernamePasswordCallbackHandler"),
                                new SecurityMechanism("CRAM_MD5","org.apache.qpid.security.UsernamePasswordCallbackHandler")};

    private SaslClientFactory[] saslClientFactories =
        new SaslClientFactory[]{new SaslClientFactory("AMQPLAIN","org.apache.qpid.security.amqplain.AmqPlainSaslClientFactory")};       
    
   private  QpidConfig(){}
    
   public static QpidConfig get()
   {
       return _instance;
   }
    
   public void setSecurityMechanisms(SecurityMechanism... securityMechanisms)
   {
       this.securityMechanisms = securityMechanisms;
   }   
   
   public SecurityMechanism[] getSecurityMechanisms()
   {
       return securityMechanisms;
   }
       
   public void setSaslClientFactories(SaslClientFactory... saslClientFactories)
   {
       this.saslClientFactories = saslClientFactories;
   }   
   
   public SaslClientFactory[] getSaslClientFactories()
   {
       return saslClientFactories;
   }
   
   public class SecurityMechanism
   {
        String type;
        String handler;
        
        SecurityMechanism(String type,String handler)
        {
            this.type = type;
            this.handler = handler;
        }

        public String getHandler()
        {
            return handler;
        }

        public String getType()
        {
            return type;
        }
   }
   
   public class SaslClientFactory
   {
        String type;
        String factoryClass;
        
        SaslClientFactory(String type,String factoryClass)
        {
            this.type = type;
            this.factoryClass = factoryClass;
        }

        public String getFactoryClass()
        {
            return factoryClass;
        }

        public String getType()
        {
            return type;
        }
   }
}
