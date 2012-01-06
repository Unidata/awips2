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

package org.apache.qpid.management;

/**
 * Enumerative interfaces containing all QMan messages.
 * 
 * @author Andrea Gazzarini
 */
public interface Messages 
{	
	// MESSAGES
	String EVENT_SEVERITY_ATTRIBUTE_DESCRIPTION = "Severity level for this event.";
	String EVENT_TIMESTAMP_ATTRIBUTE_DESCRIPTION = "Current timestamp of this event.";
	String ACTION_NOT_SUPPORTED="Action %s not supported by resource %s.";
	
	// INFO
	String QMAN_000001_STARTING_QMAN = "<QMAN-000001> : Starting Q-Man...";
	String QMAN_000002_READING_CONFIGURATION = "<QMAN-000002> : Reading Q-Man configuration...";
	String QMAN_000003_CREATING_MANAGEMENT_CLIENTS = "<QMAN-000003> : Creating management client(s)...";
	String QMAN_000004_MANAGEMENT_CLIENT_CONNECTED = "<QMAN-000004> : Management client for broker %s successfully connected.";
	String QMAN_000005_TYPE_MAPPING_CONFIGURED = "<QMAN-000005> : Type mapping : code = %s associated to %s (validator class is %s)";
	String QMAN_000006_ACCESS_MODE_MAPPING_CONFIGURED = "<QMAN-000006> : Access Mode mapping : code = %s associated to %s";
	String QMAN_000007_MANAGEMENT_HANDLER_MAPPING_CONFIGURED = "<QMAN-000007> : Management Queue Message Handler Mapping : opcode = %s associated with %s";
	String QMAN_000008_METHOD_REPLY_HANDLER_MAPPING_CONFIGURED  = "<QMAN-000008> : Method-Reply Queue Message Handler Mapping : opcode = %s associated with %s";
	String QMAN_000009_BROKER_DATA_CONFIGURED = "<QMAN-000009> : Broker configuration %s: %s";
	String QMAN_000010_INCOMING_SCHEMA = "<QMAN-000010> : Incoming schema for %s::%s.%s";
	String QMAN_000011_SHUTDOWN_INITIATED = "<QMAN-000011> : The shutdown sequence has been initiated for management client connected with broker %s";
	String QMAN_000012_MANAGEMENT_CLIENT_SHUT_DOWN = "<QMAN-000012> : Management client connected with broker %s shut down successfully.";
	String QMAN_000013_METHOD_REPLY_CONSUMER_INSTALLED = "<QMAN-000013> : Method-reply queue consumer has been successfully installed and bound on broker %s.";
	String QMAN_000014_MANAGEMENT_CONSUMER_INSTALLED ="<QMAN-000014> : Management queue consumer has been successfully installed and bound on broker %s.";
	String QMAN_000015_MANAGEMENT_QUEUE_DECLARED = "<QMAN-000015> : Management queue with name %s has been successfully declared and bound on broker %s.";
	String QMAN_000016_METHOD_REPLY_QUEUE_DECLARED = "<QMAN-000016> : Method-reply queue with name %s has been successfully declared and bound on broker %s.";
	String QMAN_000017_CONSUMER_HAS_BEEN_REMOVED = "<QMAN-000017> : Consumer %s has been removed from broker %s.";
	String QMAN_000018_QUEUE_UNDECLARED = "<QMAN-000018> : Queue %s has been removed from broker %s.";
	String QMAN_000019_QMAN_STARTED = "<QMAN-000019> : Q-Man open for e-business.";
	String QMAN_000020_SHUTTING_DOWN_QMAN = "<QMAN-000020> : Shutting down Q-Man...";
	String QMAN_000021_SHUT_DOWN = "<QMAN-000021> : Q-Man shut down.";
	String QMAN_000022_NO_BROKER_CONFIGURED = "<QMAN-000022> : Q-Man has no configured broker : in order to connect with a running one use Q-Man Administration interface.";
	String QMAN_000023_QMAN_REGISTERED_AS_MBEAN = "<QMAN-000023> : Q-Man service is now available on MBeanServer.";
	
	String QMAN_000026_WSDM_ADAPTER_STARTS = "<QMAN-000026> : Initializing WS-DM Adapter Environment...";
	String QMAN_000027_WSDM_ADAPTER_STARTED = "<QMAN-000027> : WS-DM Adapter ready for incoming requests.";
	String QMAN_000028_TEST_MODULE_NOT_FOUND = "<QMAN-000028> : Qpid emulator not found. Test notifications are disabled.";
	String QMAN_000029_DEFAULT_URI = "<QMAN-000029> : Default URI will be set to %s";
	String QMAN_000030_RESOURCE_HAS_BEEN_CREATED =  "<QMAN-000030> : New resource instance has been created and registered. Resource id is %s";
	String QMAN_000031_RESOURCE_HAS_BEEN_REMOVED = "<QMAN-000031> : WS-Resource %s has been removed";
	String QMAN_000032_EVENTS_LIFECYCLE_TOPIC_HAS_BEEN_CREATED = "<QMAN-000032> : Events lifecycle topic has been created with name %s";
	String QMAN_000033_OBJECTS_LIFECYCLE_TOPIC_HAS_BEEN_CREATED = "<QMAN-000033> : Objects lifecycle topic has been created with name %s";
	String QMAN_000034_UNCLASSIFIED_LIFECYCLE_TOPIC_HAS_BEEN_CREATED = "<QMAN-000034> : Unclassified object types lifecycle topic has been created with name %s";
	String QMAN_000035_WORK_MANAGER_POOL_SIZE = "<QMAN-000035> : Work Manager thread pool size : %s";
	String QMAN_000036_WORK_MANAGER_MAX_POOL_SIZE = "<QMAN-000036> : Work Manager thread pool max size : %s";
	String QMAN_000037_WORK_MANAGER_KEEP_ALIVE_TIME = "<QMAN-000035> : Work Manager keep alive time : %s";
		
	 // DEBUG
	String QMAN_200001_INCOMING_MESSAGE_HAS_BEEN_RECEIVED = "<QMAN-200001> : New incoming message has been received. Message content is %s";
	String QMAN_200002_OPCODE_HANDLER_ASSOCIATION = "<QMAN-200002> : \"%s\" opcode is associated to handler %s";
	String QMAN_200003_MESSAGE_FORWARDING = "<QMAN-200003> : Incoming message with \"%s\" as opcode will be forwarded to %s for processing.";
	String QMAN_200004_MANAGEMENT_QUEUE_NAME = "<QMAN-200004> : Management queue name : %s"; 
	String QMAN_200005_METHOD_REPLY_QUEUE_NAME = "<QMAN-200005> : Method-reply queue name : %s";
	String QMAN_200006_QPID_CONNECTION_RELEASED = "<QMAN-200006> : Connection %s returned to the pool.";
	String QMAN_200007_TEST_CONNECTION_ON_RESERVE = "<QMAN-200007> : Test connection on reserve. Is valid? %s";
	String QMAN_200008_CONNECTION_DESTROYED = "<QMAN-200008> : Connection has been destroyed.";
	String QMAN_200009_CONNECTION_DESTROY_FAILURE = "<QMAN-200009> : Unable to destroy a connection object.";
	String QMAN_200010_EVENT_MBEAN_REGISTERED = "<QMAN-200010> : Event instance %s::%s::%s successfully registered with MBean Server with name %s";
	String QMAN_200011_OBJECT_MBEAN_REGISTERED = "<QMAN-200011> : Object instance %s::%s::%s:%s successfully registered with MBean Server with name %s";
	String QMAN_200012_OBJECT_MBEAN_UNREGISTERED = "<QMAN-200012> : Object instance %s::%s::%s:%s successfully unregistered from MBean Server. Name was %s";
	String QMAN_200013_ARGUMENT_VALUE_ENCODED = "<QMAN-200013> : Encoded value %S for argument %s. Type is %s";
	String QMAN_200014_INCOMING_INSTRUMENTATION_DATA = "<QMAN-200014> : Incoming instrumentation data for %s::%s.%s.%s";
	String QMAN_200015_INCOMING_CONFIGURATION_DATA = "<QMAN-200015> : Incoming configuration data for %s::%s.%s.%s";
	String QMAN_200016_PROPERTY_DEFINITION_HAS_BEEN_BUILT = "<QMAN-200016> : Property definition for %s::%s.%s has been built.";
	String QMAN_200017_STATISTIC_DEFINITION_HAS_BEEN_BUILT = "<QMAN-200017> : Statistic definition for %s::%s.%s has been built.";
	String QMAN_200018_OPTIONAL_PROPERTIES_INFO = "<QMAN-200018> : Class %s::%s.%s has %s optional properties.";
	String QMAN_200019_EVENT_ARGUMENT_DEFINITION_HAS_BEEN_BUILT = "<QMAN-200019> : Event argument definition for %s::%s.%s has been built.";
	String QMAN_200020_ENTITY_DEFINITION_HAS_BEEN_BUILT = "<QMAN-200020> : Entity definition has been built (without schema) for %s::%s.%s";
	String QMAN_200021_INCOMING_EVENT_DATA = "<QMAN-200021> : Incoming data for event %s::%s.%s";
	String QMAN_200022_VALIDATOR_INSTALLED = "<QMAN-200022> : Validator %s for type %s successfully installed.";
	String QMAN_200023_VALIDATOR_NOT_FOUND = "<QMAN-200023> : No validator was found for type %s. The default (empty) validator will be used.";
	String QMAN_200024_MANAGEMENT_MESSAGE_HAS_BEEN_SENT = "<QMAN-200024> : Message has been sent to management exchange. Message content : %s";
	String QMAN_200025_SUBSCRIPTION_DECLARED = "<QMAN-200025> : New subscription between queue %s and destination %s has been declared.";
	String QMAN_200026_SUBSCRIPTION_REMOVED = "<QMAN-200026> : Subscription named %s has been removed from remote broker.";
	String QMAN_200027_QUEUE_DECLARED = "<QMAN-200027> : New queue with name %s has been declared.";
	String QMAN_200028_QUEUE_REMOVED= "<QMAN-200028> : New queue with name %s has been undeclared.";
	String QMAN_200029_BINDING_DECLARED = "<QMAN-200029> : New binding with %s as routing key has been declared between queue %s and exchange %s.";
	String QMAN_200030_BINDING_REMOVED = "<QMAN-200030> : Binding with %s as routing key has been removed between queue %s and exchange %s.";
	String QMAN_200031_COMPOUND_MESSAGE_CONTAINS = "<QMAN-200031> : Incoming compound message contains %s message(s).";
	String QMAN_200032_COMMAND_MESSAGE_ROUTING_KEY = "<QMAN-200032> : Command message routing key : %s";
	String QMAN_200033_CAPABILITY_CLASS_HAS_BEEN_ADDED = "<QMAN-200033> : Capability has been added to this resource. Class is %s while URI is %s.";
	String QMAN_200034_RMD_NAME = "<QMAN-200034> : Resource Metadata Descriptor name is %s.";
	String QMAN_200035_RMD_PATH = "<QMAN-200035> : Resource Metadata Descriptor path is %s.";
	String QMAN_200036_ADDITIONAL_RMD_PROPERTY = "<QMAN-200036> : Additional RMD property : %s";
	String QMAN_200037_RMD = "<QMAN-200037> : Resource Metadata Descriptor : %s";
	String QMAN_200038_WSRP = "<QMAN-200038> : WS Resource Properties fragment : %s";
	String QMAN_200039_DEBUG_JMX_NOTIFICATION = "<QMAN-200039> : %s";
	String QMAN_200040_WS_ARTIFACTS_CACHED = "<QMAN-200040> : WS Artifacts has been stored on cache with the following id : %s";
	String QMAN_200041_INCOMING_OBJECT_NAME_AND_DERIVED_KEY = "<QMAN-200041> : Incoming object name : %s, derived search key : %s";
	String QMAN_200042_REMOVING_RESOURCE = "<QMAN-200042> : WS-Resource %s is going to be removed";
	String QMAN_200043_GENERATED_ACCESSOR_METHOD = "<QMAN-200043> : Generated accessor method for %s : %s";
	String QMAN_200044_GENERATED_METHOD = "<QMAN-200044> : Generated method for %s : %s";
	
	// WARNING
    String QMAN_300001_MESSAGE_DISCARDED = "<QMAN-300001> : No handler has been configured for processing messages with \"%s\" as opcode. Message will be discarded.";
	String QMAN_300002_UNKNOWN_SEQUENCE_NUMBER = "<QMAN-300002> : Unable to deal with incoming message because it contains a unknown sequence number (%s).";
    String QMAN_300003_BROKER_ALREADY_CONNECTED = "<QMAN-300003> : Unable to enlist given broker connection data : QMan is already connected with broker %s";
    String QMAN_300004_INVALID_CONFIGURATION_FILE = "<QMAN-300004> : The given configuration file (%s) is not valid (it doesn't exist or cannot be read)";
    String QMAN_300005_QEMU_INITIALIZATION_FAILURE = "<QMAN-300005> : Unable to initialize QEmu module and therefore emulation won't be enabled...";
    
    String QMAN_300006_OS_MBEAN_FAILURE = "<QMAN-300006> : Unable to retrieve Operating System properties. No values will be displayed for underlying Operation System.";
    String QMAN_300007_RUNTIME_MBEAN_FAILURE = "<QMAN-300007> : Unable to retrieve Runtime Environment properties. No values will be displayed.";
	
	// ERROR
	String QMAN_100001_BAD_MAGIC_NUMBER_FAILURE = "<QMAN-100001> : Message processing failure : incoming message contains a bad magic number (%s) and therefore will be discaded.";
	String QMAN_100002_MESSAGE_READ_FAILURE = "<QMAN-100002> : Message I/O failure : unable to read byte message content and therefore it will be discarded.";
	String QMAN_100003_MESSAGE_PROCESS_FAILURE = "<QMAN-100003> : Message processing failure : unknown exception; see logs for more details.";
	String QMAN_100004_HANDLER_INITIALIZATION_FAILURE = "<QMAN-100004> : Message handler configured for opcode %s thrown an exception in initialization and therefore will be discarded.";
	String QMAN_100005_CLASS_SCHEMA_PROCESSING_FAILURE = "<QMAN-100005> : Q-Man was unable to process the schema response message.";
	String QMAN_100006_EVENT_SCHEMA_PROCESSING_FAILURE = "<QMAN-100006> : Q-Man was unable to process the schema response message.";
	String QMAN_100007_UNABLE_TO_CONNECT_WITH_BROKER = "<QMAN-100007> : Unable to connect with broker located on %s. This broker will be ignored.";

	
	String QMAN_100010_METHOD_INVOCATION_RESULT_FAILURE = "<QMAN-100010> : an exception occurred while storing the result of a method invocation. Sequence number was %s";
	String QMAN_100011_UNKNOWN_CLASS_KIND = "<QMAN-100011> : Unknwon class kind : %s).";
	String QMAN_100012_SCHEMA_MESSAGE_PROCESSING_FAILURE = "<QMAN-100012> : Q-Man was unable to process the schema response message.";
	String QMAN_100013_MBEAN_REGISTRATION_FAILURE = "<QMAN-100013> : Unable to unregister object instance %s.";
	String QMAN_100014_ATTRIBUTE_DECODING_FAILURE = "<QMAN-100014> : Unable to decode value for attribute %s";
	String QMAN_100015_UNABLE_TO_SEND_SCHEMA_REQUEST = "<QMAN-100015> : Unable to send a schema request schema for %s.%s";
	String QMAN_100016_UNABLE_TO_DECODE_FEATURE_VALUE = "<QMAN-100016> : Unable to decode value for %s::%s::%s";
	String QMAN_100017_UNABLE_TO_CONNECT = "<QMAN-100017>: Cannot connect to broker %s on %s";
	String QMAN_100018_UNABLE_TO_STARTUP_CORRECTLY = "<QMAN-100018> : Q-Man was unable to startup correctly : see logs for further details.";
	String QMAN_100019_REQ_OR_RES_MALFORMED = "<QMAN-100019> : Unexpected exception occurred on WSDM adapter layer : probably request or response was malformed.";
	String QMAN_100020_ACTION_NOT_SUPPORTED = "<QMAN-100020> : "+ACTION_NOT_SUPPORTED;	
	String QMAN_100021_RMD_BUID_FAILURE = "<QMAN-100021> : Unable to build RDM for resource %s.";

	String QMAN_100023_BUILD_WS_ARTIFACTS_FAILURE = "<QMAN-100023> : Unable to build WS artifacts.";
	String QMAN_100024_CAPABILITY_INSTANTIATION_FAILURE = "<QMAN-100024> : Unable to instantiate generated capability class for %s.";
	String QMAN_100025_WSRF_FAILURE = "<QMAN-100025> : Resource manager raised an exception while creating capability for %s.";	
	String QMAN_100026_SOAP_ADDRESS_REPLACEMENT_FAILURE = "<QMAN-100026> : Exception occurred while replacing the placeholder soap address with resource actual location.";
	String QMAN_100027_RESOURCE_SHUTDOWN_FAILURE = "<QMAN-100027> : Shutdown failure while destroying resource %s.";
	String QMAN_100029_MALFORMED_RESOURCE_URI_FAILURE = "<QMAN-100029> : Unable to define URI for QMan resources using \"%s\". It violates RFC 2396";
	String QMAN_100030_JMX_CORE_STARTUP_FAILURE = "<QMAN-100030> : QMan JMX core Unexpected failure while starting up.";
	String QMAN_100031_WS_RESOURCE_ALREADY_INITIALIZED = "<QMAN-100031> : Bad request has been received on this WS-Resource : Initialization is not possible because the resource has already been initialized.";
	String QMAN_100032_WS_RESOURCE_NOT_YET_INITIALIZED = "<QMAN-100032> : Bad request has been received on this WS-Resource : Shutdown is not possible because the resource hasn't yet been initialized.";
	String QMAN_100033_WS_RESOURCE_ALREADY_SHUTDOWN = "<QMAN-100033> : Bad request has been received on this WS-Resource : Shutdown is not possible because the resource has already been shutdown.";
	String QMAN_100034_WSDL_SCHEMA_SECTION_NOT_FOUND = "<QMAN-100034> : Unable to get via XPath the schema section in WSDL.";
	String QMAN_100035_RESOURCE_CAPABILITY_INVOCATION_FAILURE = "<QMAN-100035> : Resource thrown a failure while invoking a capability operation.";
	String QMAN_100036_TOPIC_DECLARATION_FAILURE = "<QMAN-100036> : WS-DM Adapter was unable to declare events and / or objects lifecycle topic(s). As conseguence of that, QMan won't be able to correctly emit lifecycle notifications.";
	
	// NEW
	String QMAN_100035_GET_ATTRIBUTE_FAILURE = "<QMAN-100035> : Get Attribute invocation failure for attribute %s, resource %s.";
	String QMAN_100036_SET_ATTRIBUTE_FAILURE = "<QMAN-100036> : Set Attribute invocation failure for attribute %s, resource %s.";
	String QMAN_100037_INVOKE_OPERATION_FAILURE = "<QMAN-100037> : Operation Invocation failure for operation.";	
	String QMAN_100038_UNABLE_TO_SEND_WS_NOTIFICATION = "<QMAN-100038> : Unable to send notification.";	
	String QMAN_100039_UNABLE_TO_CONFIGURE_PROPERLY_WORKER_MANAGER = "<QMAN-100039> : Unable to properly configure WorkManager. A malformed property (NaN) was given as input parameter.";	
	String QMAN_100040_UNABLE_TO_LOCATE_WSRP_PROPERTIES = "<QMAN-100040> : Unable to evaluate the WSRP XPath expression on resource WSDL.";	
	
}