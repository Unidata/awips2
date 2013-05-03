#!/bin/bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

# Create example keystore for broker and trust store for client/management console.
#
# Use generated qpid.keystore as the brokers keystore
# Use generated qpid.truststore as client/consoles truststore
# All passwords have value: password

#Create Broker Keystore:
keytool -genkey -alias qpidBroker -keyalg RSA -validity 365 -keystore qpid.keystore \
-storepass password -keypass password -dname "CN=hostname, OU=OrgUnit, O=Org, L=City, C=US"

#Export Self Signed Cert:
keytool -export -alias qpidBroker -keystore qpid.keystore -file qpidBroker.cer -storepass password

#Import Broker Cert Into MC TrustStore:
keytool -import -alias qpidBrokerCert -file qpidBroker.cer -keystore qpid.truststore -storepass password -noprompt

#Delete the cert
rm qpidBroker.cer
