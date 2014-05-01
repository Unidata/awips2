@REM
@REM Licensed to the Apache Software Foundation (ASF) under one
@REM or more contributor license agreements.  See the NOTICE file
@REM distributed with this work for additional information
@REM regarding copyright ownership.  The ASF licenses this file
@REM to you under the Apache License, Version 2.0 (the
@REM "License"); you may not use this file except in compliance
@REM with the License.  You may obtain a copy of the License at
@REM 
@REM   http://www.apache.org/licenses/LICENSE-2.0
@REM 
@REM Unless required by applicable law or agreed to in writing,
@REM software distributed under the License is distributed on an
@REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@REM KIND, either express or implied.  See the License for the
@REM specific language governing permissions and limitations
@REM under the License.
@REM

@REM Create example keystore for broker and trust store for client/management console.
@REM
@REM Use generated qpid.keystore as the brokers keystore
@REM Use generated qpid.truststore as client/consoles truststore
@REM All passwords have value: password

@REM Create Broker Keystore:
keytool -genkey -alias qpidBroker -keyalg RSA -validity 365 -keystore qpid.keystore -storepass password -keypass password -dname "CN=hostname, OU=OrgUnit, O=Org, L=City, C=US"

@REM Export Self Signed Cert:
keytool -export -alias qpidBroker -keystore qpid.keystore -file qpidBroker.cer -storepass password

@REM Import Broker Cert Into MC TrustStore:
keytool -import -alias qpidBrokerCert -file qpidBroker.cer -keystore qpid.truststore -storepass password -noprompt

@REM Delete the cert
del qpidBroker.cer