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

SUBDIRS = . tests

# The Windows-only sources are not compiled using this Makefile, but
# are listed here to ensure they're included in releases. They are built
# using Visual Studio solutions/projects.
windows_dist = \
  qpid/client/windows/SaslFactory.cpp \
  qpid/log/windows/SinkOptions.cpp \
  qpid/log/windows/SinkOptions.h \
  ../include/qpid/sys/windows/check.h \
  qpid/sys/windows/AsynchIO.cpp \
  qpid/sys/windows/AsynchIoResult.h \
  ../include/qpid/sys/windows/Condition.h \
  qpid/sys/windows/FileSysDir.cpp \
  ../include/qpid/sys/windows/IntegerTypes.h \
  qpid/sys/windows/IocpPoller.cpp \
  qpid/sys/windows/IOHandle.cpp \
  qpid/sys/windows/IoHandlePrivate.h \
  qpid/sys/windows/LockFile.cpp \
  qpid/sys/windows/PollableCondition.cpp \
  qpid/sys/windows/PipeHandle.cpp \
  ../include/qpid/sys/windows/Mutex.h \
  qpid/sys/windows/Shlib.cpp \
  qpid/sys/windows/SocketAddress.cpp \
  qpid/sys/windows/Socket.cpp \
  qpid/sys/windows/StrError.cpp \
  qpid/sys/windows/SystemInfo.cpp \
  qpid/sys/windows/Thread.cpp \
  qpid/sys/windows/Time.cpp \
  ../include/qpid/sys/windows/Time.h \
  qpid/sys/windows/uuid.cpp \
  qpid/sys/windows/uuid.h \
  windows/QpiddBroker.cpp \
  qpid/broker/windows/BrokerDefaults.cpp \
  qpid/broker/windows/SaslAuthenticator.cpp

EXTRA_DIST= $(platform_dist) $(rgen_srcs) $(windows_dist)

# Define variables that are be appended to by this file and included .mk files.
nobase_include_HEADERS =
libqpidcommon_la_SOURCES =

## Generated code

# Note: generated soure and makefiles included in distribution so a
# distribution can be built without code generation tools and XML
# sources.

# This phony target is needed by generated makefile fragments:
force:

if GENERATE

# AMQP_FINAL_XML is defined in ../configure.ac
amqp_0_10_xml=@AMQP_FINAL_XML@
specs=$(amqp_0_10_xml) $(top_srcdir)/xml/cluster.xml

# Ruby generator.
rgen_dir=$(top_srcdir)/rubygen
rgen_cmd=ruby -I $(rgen_dir) $(rgen_dir)/generate . ../include $(specs) all

$(rgen_srcs) $(srcdir)/rubygen.mk: rgen.timestamp
rgen.timestamp: $(rgen_generator) $(specs)
	$(rgen_cmd) $(srcdir)/rubygen.mk; touch $@
$(rgen_generator):

# The CMake version is needed for dist
$(srcdir)/rubygen.cmake: $(rgen_generator) $(specs)
	$(rgen_cmd) $(srcdir)/rubygen.cmake

# Management generator.
mgen_dir=$(top_srcdir)/managementgen
mgen_cmd=$(mgen_dir)/qmf-gen -m $(srcdir)/managementgen.mk \
	-c $(srcdir)/managementgen.cmake -q -b -o qmf \
	$(top_srcdir)/../specs/management-schema.xml \
	$(srcdir)/qpid/acl/management-schema.xml \
	$(srcdir)/qpid/cluster/management-schema.xml

$(srcdir)/managementgen.mk $(mgen_broker_cpp) $(dist_qpid_management_HEADERS): mgen.timestamp
mgen.timestamp: $(mgen_generator)
	$(mgen_cmd); touch $@
$(mgen_generator):

endif # GENERATE

include $(srcdir)/rubygen.mk
include $(srcdir)/managementgen.mk

## Compiler flags
AM_CXXFLAGS = $(WARNING_CFLAGS)
AM_LDFLAGS = -version-info $(LIBTOOL_VERSION_INFO_ARG)
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include -I$(srcdir) -I=$(builddir)

#
# Destination for intalled programs and tests defined here
#
qpidexecdir = $(libexecdir)/qpid
AM_CXXFLAGS += -DQPID_EXEC_DIR=\"$(qpidexecdir)\"
qpidexec_PROGRAMS =
qpidexec_SCRIPTS =
qpidtestdir = $(qpidexecdir)/tests
qpidtest_PROGRAMS =
qpidtest_SCRIPTS =
tmoduledir = $(libdir)/qpid/tests
tmodule_LTLIBRARIES=

## Automake macros to build libraries and executables.
qpidd_CXXFLAGS = $(AM_CXXFLAGS) -DQPIDD_MODULE_DIR=\"$(dmoduledir)\" -DQPIDD_CONF_FILE=\"$(sysconfdir)/qpidd.conf\"
libqpidclient_la_CXXFLAGS = $(AM_CXXFLAGS) -DQPIDC_MODULE_DIR=\"$(cmoduledir)\" -DQPIDC_CONF_FILE=\"$(confdir)/qpidc.conf\"

qpidd_LDADD =					\
  libqpidbroker.la				\
  libqpidcommon.la

posix_qpidd_src = posix/QpiddBroker.cpp

sbin_PROGRAMS = qpidd
qpidd_SOURCES = qpidd.cpp qpidd.h $(posix_qpidd_src)

## Platform specific code.

# Posix-specific code
libqpidcommon_la_SOURCES +=			\
  qpid/log/posix/SinkOptions.cpp		\
  qpid/sys/posix/IOHandle.cpp			\
  qpid/sys/posix/Socket.cpp			\
  qpid/sys/posix/SocketAddress.cpp		\
  qpid/sys/posix/AsynchIO.cpp			\
  qpid/sys/posix/FileSysDir.cpp			\
  qpid/sys/posix/LockFile.cpp			\
  qpid/sys/posix/Time.cpp			\
  qpid/sys/posix/Thread.cpp			\
  qpid/sys/posix/Shlib.cpp			\
  qpid/sys/posix/Mutex.cpp			\
  qpid/sys/posix/Fork.cpp			\
  qpid/sys/posix/StrError.cpp			\
  qpid/sys/posix/PollableCondition.cpp		\
  qpid/sys/posix/PidFile.h			\
  qpid/sys/posix/PipeHandle.cpp			\
  qpid/log/posix/SinkOptions.h 			\
  qpid/sys/posix/Fork.h

nobase_include_HEADERS +=			\
  ../include/qpid/sys/posix/Condition.h		\
  ../include/qpid/sys/posix/IntegerTypes.h	\
  ../include/qpid/sys/posix/Mutex.h		\
  ../include/qpid/sys/posix/PrivatePosix.h	\
  ../include/qpid/sys/posix/Time.h		\
  ../include/qpid/sys/posix/check.h

if HAVE_EPOLL 
  poller = qpid/sys/epoll/EpollPoller.cpp
endif

if HAVE_ECF
  poller = qpid/sys/solaris/ECFPoller.cpp
endif

if SUNOS
  systeminfo = qpid/sys/solaris/SystemInfo.cpp
else
  systeminfo = qpid/sys/posix/SystemInfo.cpp
endif

libqpidcommon_la_SOURCES += $(poller) $(systeminfo)

posix_broker_src = \
  qpid/broker/posix/BrokerDefaults.cpp

lib_LTLIBRARIES = libqpidcommon.la libqpidbroker.la libqpidclient.la

# Definitions for client and daemon plugins
PLUGINLDFLAGS=-no-undefined -module -avoid-version
confdir=$(sysconfdir)/qpid
dmoduledir=$(libdir)/qpid/daemon
cmoduledir=$(libdir)/qpid/client
dmodule_LTLIBRARIES =
cmodule_LTLIBRARIES = 

include cluster.mk
include acl.mk
include qmf.mk
include qmfc.mk
if HAVE_XML
include xml.mk
endif
include replication.mk

if RDMA

# RDMA (Infiniband) protocol code
librdmawrap_la_SOURCES = \
  qpid/sys/rdma/rdma_exception.h \
  qpid/sys/rdma/rdma_factories.cpp \
  qpid/sys/rdma/rdma_factories.h \
  qpid/sys/rdma/RdmaIO.cpp \
  qpid/sys/rdma/RdmaIO.h \
  qpid/sys/rdma/rdma_wrap.cpp \
  qpid/sys/rdma/rdma_wrap.h
librdmawrap_la_LIBADD = \
  libqpidcommon.la \
  -lrdmacm \
  -libverbs
librdmawrap_la_CXXFLAGS = \
  $(AM_CXXFLAGS) -Wno-missing-field-initializers
lib_LTLIBRARIES += \
  librdmawrap.la
librdmawrap_la_LDFLAGS = \
  -no-undefined

rdma_la_SOURCES = \
  qpid/sys/RdmaIOPlugin.cpp
rdma_la_LIBADD = \
  libqpidbroker.la \
  librdmawrap.la
rdma_la_LDFLAGS = $(PLUGINLDFLAGS)
rdma_la_CXXFLAGS = \
  $(AM_CXXFLAGS) -Wno-missing-field-initializers
dmodule_LTLIBRARIES += \
  rdma.la

rdmaconnector_la_SOURCES = \
  qpid/client/RdmaConnector.cpp
rdmaconnector_la_LIBADD = \
  libqpidclient.la \
  librdmawrap.la
rdmaconnector_la_LDFLAGS = $(PLUGINLDFLAGS)
rdmaconnector_la_CXXFLAGS = \
  $(AM_CXXFLAGS) -Wno-missing-field-initializers
cmodule_LTLIBRARIES += \
  rdmaconnector.la

# RDMA test/sample programs
noinst_PROGRAMS = RdmaServer RdmaClient
RdmaServer_SOURCES = qpid/sys/rdma/RdmaServer.cpp
RdmaServer_LDADD = \
  librdmawrap.la libqpidcommon.la
RdmaClient_SOURCES = qpid/sys/rdma/RdmaClient.cpp
RdmaClient_CXXFLAGS = \
  $(AM_CXXFLAGS) -Wno-missing-field-initializers
RdmaClient_LDADD = \
  librdmawrap.la libqpidcommon.la

endif

if SSL
include ssl.mk
endif

# New 0-10 codec, to be integrated in future.
# libqpidamqp_0_10_la_SOURCES= 
EXTRA_DIST +=\
  CMakeLists.txt \
  cluster.cmake \
  config.h.cmake \
  rdma.cmake \
  ssl.cmake \
  managementgen.cmake \
  rubygen.cmake \
  $(rgen_amqp_0_10_srcs) \
  qpid/amqp_0_10/apply.h \
  qpid/amqp_0_10/built_in_types.h \
  qpid/amqp_0_10/complex_types.cpp \
  qpid/amqp_0_10/Array.h \
  qpid/amqp_0_10/Array.cpp \
  qpid/amqp_0_10/Body.h \
  qpid/amqp_0_10/Command.h \
  qpid/amqp_0_10/CommmandPacker.h \
  qpid/amqp_0_10/Control.h \
  qpid/amqp_0_10/Header.h \
  qpid/amqp_0_10/Header.cpp \
  qpid/amqp_0_10/FrameHeader.h \
  qpid/amqp_0_10/FrameHeader.cpp \
  qpid/amqp_0_10/Holder.h \
  qpid/amqp_0_10/Codec.h \
  qpid/amqp_0_10/Packer.h \
  qpid/amqp_0_10/Decimal.h \
  qpid/amqp_0_10/SerializableString.h \
  qpid/amqp_0_10/Map.h \
  qpid/amqp_0_10/Map.cpp \
  qpid/amqp_0_10/Struct.h \
  qpid/amqp_0_10/Struct32.h \
  qpid/amqp_0_10/Struct32.cpp \
  qpid/amqp_0_10/Unit.h \
  qpid/amqp_0_10/Unit.cpp \
  qpid/amqp_0_10/UnitHandler.h \
  qpid/amqp_0_10/UnknownType.h \
  qpid/amqp_0_10/UnknownType.cpp \
  qpid/amqp_0_10/UnknownStruct.h \
  qpid/amqp_0_10/UnknownStruct.cpp \
  qpid/store

libqpidcommon_la_LIBADD = \
  -lboost_program_options \
  -lboost_filesystem \
  -luuid \
  $(LIB_DLOPEN) \
  $(LIB_CLOCK_GETTIME)

libqpidcommon_la_SOURCES +=			\
  $(rgen_framing_srcs)				\
  $(platform_src)				\
  qpid/Address.cpp				\
  qpid/DataDir.cpp				\
  qpid/DataDir.h				\
  qpid/Exception.cpp				\
  qpid/Modules.cpp				\
  qpid/Modules.h				\
  qpid/Options.cpp				\
  qpid/Plugin.cpp				\
  qpid/Plugin.h					\
  qpid/RefCounted.h				\
  qpid/RefCountedBuffer.cpp			\
  qpid/RefCountedBuffer.h			\
  qpid/Serializer.h				\
  qpid/SessionId.cpp				\
  qpid/SessionState.cpp				\
  qpid/SessionState.h				\
  qpid/SessionState.h				\
  qpid/SharedObject.h				\
  qpid/StringUtils.cpp				\
  qpid/StringUtils.h				\
  qpid/Url.cpp					\
  qpid/Version.h				\
  qpid/amqp_0_10/Exception.h			\
  qpid/amqp_0_10/SessionHandler.cpp		\
  qpid/amqp_0_10/SessionHandler.h		\
  qpid/amqp_0_10/apply.h			\
  qpid/assert.cpp qpid/assert.h			\
  qpid/assert.h					\
  qpid/framing/AMQBody.cpp			\
  qpid/framing/AMQBody.h			\
  qpid/framing/AMQCommandControlBody.h		\
  qpid/framing/AMQContentBody.cpp		\
  qpid/framing/AMQContentBody.h			\
  qpid/framing/AMQDataBlock.h			\
  qpid/framing/AMQFrame.cpp			\
  qpid/framing/AMQFrame.h			\
  qpid/framing/AMQHeaderBody.cpp		\
  qpid/framing/AMQHeaderBody.h			\
  qpid/framing/AMQHeartbeatBody.cpp		\
  qpid/framing/AMQHeartbeatBody.h		\
  qpid/framing/AMQMethodBody.cpp		\
  qpid/framing/AMQMethodBody.h			\
  qpid/framing/AMQP_HighestVersion.h		\
  qpid/framing/AMQP_HighestVersion.h		\
  qpid/framing/AccumulatedAck.cpp		\
  qpid/framing/AccumulatedAck.h			\
  qpid/framing/Array.cpp			\
  qpid/framing/BodyFactory.h			\
  qpid/framing/BodyHandler.cpp			\
  qpid/framing/BodyHandler.h			\
  qpid/framing/Buffer.cpp			\
  qpid/framing/ChannelHandler.h			\
  qpid/framing/Endian.cpp			\
  qpid/framing/Endian.h				\
  qpid/framing/FieldTable.cpp			\
  qpid/framing/FieldValue.cpp			\
  qpid/framing/FrameDecoder.cpp			\
  qpid/framing/FrameDecoder.h			\
  qpid/framing/FrameDefaultVisitor.h		\
  qpid/framing/FrameHandler.h			\
  qpid/framing/FrameSet.cpp			\
  qpid/framing/FrameSet.h			\
  qpid/framing/Handler.h			\
  qpid/framing/HeaderProperties.h		\
  qpid/framing/InitiationHandler.h		\
  qpid/framing/InputHandler.h			\
  qpid/framing/Invoker.h			\
  qpid/framing/IsInSequenceSet.h		\
  qpid/framing/List.cpp				\
  qpid/framing/MethodBodyFactory.h		\
  qpid/framing/MethodContent.h			\
  qpid/framing/ModelMethod.h			\
  qpid/framing/OutputHandler.h			\
  qpid/framing/ProtocolInitiation.cpp		\
  qpid/framing/ProtocolInitiation.h		\
  qpid/framing/ProtocolVersion.cpp		\
  qpid/framing/Proxy.cpp			\
  qpid/framing/Proxy.h				\
  qpid/framing/SendContent.cpp			\
  qpid/framing/SendContent.h			\
  qpid/framing/SequenceNumber.cpp		\
  qpid/framing/SequenceNumberSet.cpp		\
  qpid/framing/SequenceNumberSet.h		\
  qpid/framing/SequenceSet.cpp			\
  qpid/framing/TransferContent.cpp		\
  qpid/framing/TransferContent.h		\
  qpid/framing/TypeFilter.h			\
  qpid/framing/Uuid.cpp				\
  qpid/framing/Visitor.h			\
  qpid/framing/amqp_framing.h			\
  qpid/framing/frame_functors.h			\
  qpid/framing/variant.h			\
  qpid/log/Helpers.h				\
  qpid/log/Logger.cpp				\
  qpid/log/Options.cpp				\
  qpid/log/OstreamOutput.cpp			\
  qpid/log/OstreamOutput.h			\
  qpid/log/Selector.cpp				\
  qpid/log/Statement.cpp			\
  qpid/management/Manageable.cpp		\
  qpid/management/ManagementObject.cpp		\
  qpid/memory.h					\
  qpid/pointer_to_other.h			\
  qpid/ptr_map.h				\
  qpid/sys/AggregateOutput.cpp			\
  qpid/sys/AggregateOutput.h			\
  qpid/sys/AsynchIO.h				\
  qpid/sys/AsynchIOHandler.cpp			\
  qpid/sys/AsynchIOHandler.h			\
  qpid/sys/AtomicCount.h			\
  qpid/sys/AtomicValue.h			\
  qpid/sys/AtomicValue_gcc.h			\
  qpid/sys/AtomicValue_mutex.h			\
  qpid/sys/BlockingQueue.h			\
  qpid/sys/Codec.h				\
  qpid/sys/ConnectionCodec.h			\
  qpid/sys/ConnectionInputHandler.h		\
  qpid/sys/ConnectionInputHandlerFactory.h	\
  qpid/sys/ConnectionOutputHandler.h		\
  qpid/sys/ConnectionOutputHandlerPtr.h		\
  qpid/sys/CopyOnWriteArray.h			\
  qpid/sys/DeletionManager.h			\
  qpid/sys/DispatchHandle.cpp			\
  qpid/sys/DispatchHandle.h			\
  qpid/sys/Dispatcher.cpp			\
  qpid/sys/Dispatcher.h				\
  qpid/sys/FileSysDir.h				\
  qpid/sys/Fork.h				\
  qpid/sys/LockFile.h				\
  qpid/sys/LockPtr.h				\
  qpid/sys/OutputControl.h			\
  qpid/sys/OutputTask.h				\
  qpid/sys/PipeHandle.h				\
  qpid/sys/PollableCondition.h			\
  qpid/sys/PollableQueue.h			\
  qpid/sys/Poller.h				\
  qpid/sys/ProtocolFactory.h			\
  qpid/sys/Runnable.cpp				\
  qpid/sys/ScopedIncrement.h			\
  qpid/sys/SecurityLayer.h			\
  qpid/sys/Semaphore.h				\
  qpid/sys/Shlib.cpp				\
  qpid/sys/Shlib.h				\
  qpid/sys/ShutdownHandler.h			\
  qpid/sys/Socket.h				\
  qpid/sys/SocketAddress.h			\
  qpid/sys/StateMonitor.h			\
  qpid/sys/TimeoutHandler.h			\
  qpid/sys/Timer.cpp				\
  qpid/sys/Timer.h				\
  qpid/sys/Waitable.h				\
  qpid/sys/alloca.h				\
  qpid/sys/uuid.h

if HAVE_SASL
libqpidcommon_la_SOURCES += qpid/sys/cyrus/CyrusSecurityLayer.h
libqpidcommon_la_SOURCES += qpid/sys/cyrus/CyrusSecurityLayer.cpp
libqpidcommon_la_LIBADD += -lsasl2
endif

libqpidbroker_la_LIBADD = libqpidcommon.la -luuid

libqpidbroker_la_SOURCES = \
  $(mgen_broker_cpp) \
  $(posix_broker_src) \
  qpid/amqp_0_10/Connection.cpp \
  qpid/amqp_0_10/Connection.h \
  qpid/broker/AclModule.h \
  qpid/broker/Bridge.cpp \
  qpid/broker/Bridge.h \
  qpid/broker/Broker.cpp \
  qpid/broker/Broker.h \
  qpid/broker/BrokerImportExport.h \
  qpid/broker/Connection.cpp \
  qpid/broker/Connection.h \
  qpid/broker/ConnectionFactory.cpp \
  qpid/broker/ConnectionFactory.h \
  qpid/broker/ConnectionHandler.cpp \
  qpid/broker/ConnectionHandler.h \
  qpid/broker/ConnectionState.h \
  qpid/broker/ConnectionToken.h \
  qpid/broker/Consumer.h \
  qpid/broker/Daemon.cpp \
  qpid/broker/Daemon.h \
  qpid/broker/Deliverable.h \
  qpid/broker/DeliverableMessage.cpp \
  qpid/broker/DeliverableMessage.h \
  qpid/broker/DeliveryAdapter.h \
  qpid/broker/DeliveryId.h \
  qpid/broker/DeliveryRecord.cpp \
  qpid/broker/DeliveryRecord.h \
  qpid/broker/DirectExchange.cpp \
  qpid/broker/DirectExchange.h \
  qpid/broker/DtxAck.cpp \
  qpid/broker/DtxAck.h \
  qpid/broker/DtxBuffer.cpp \
  qpid/broker/DtxBuffer.h \
  qpid/broker/DtxManager.cpp \
  qpid/broker/DtxManager.h \
  qpid/broker/DtxTimeout.cpp \
  qpid/broker/DtxTimeout.h \
  qpid/broker/DtxWorkRecord.cpp \
  qpid/broker/DtxWorkRecord.h \
  qpid/broker/Exchange.cpp \
  qpid/broker/Exchange.h \
  qpid/broker/ExchangeRegistry.cpp \
  qpid/broker/ExchangeRegistry.h \
  qpid/broker/ExpiryPolicy.cpp \
  qpid/broker/ExpiryPolicy.h \
  qpid/broker/FanOutExchange.cpp \
  qpid/broker/FanOutExchange.h \
  qpid/broker/HandlerImpl.h \
  qpid/broker/HeadersExchange.cpp \
  qpid/broker/HeadersExchange.h \
  qpid/broker/IncompleteMessageList.cpp \
  qpid/broker/IncompleteMessageList.h \
  qpid/broker/Link.cpp \
  qpid/broker/Link.h \
  qpid/broker/LinkRegistry.cpp \
  qpid/broker/LinkRegistry.h \
  qpid/broker/Message.cpp \
  qpid/broker/Message.h \
  qpid/broker/MessageAdapter.cpp \
  qpid/broker/MessageAdapter.h \
  qpid/broker/MessageBuilder.cpp \
  qpid/broker/MessageBuilder.h \
  qpid/broker/MessageStore.h \
  qpid/broker/MessageStoreModule.cpp \
  qpid/broker/MessageStoreModule.h \
  qpid/broker/NameGenerator.cpp \
  qpid/broker/NameGenerator.h \
  qpid/broker/NullMessageStore.cpp \
  qpid/broker/NullMessageStore.h \
  qpid/broker/OwnershipToken.h \
  qpid/broker/Persistable.h \
  qpid/broker/PersistableConfig.h \
  qpid/broker/PersistableExchange.h \
  qpid/broker/PersistableMessage.cpp \
  qpid/broker/PersistableMessage.h \
  qpid/broker/PersistableQueue.h \
  qpid/broker/Queue.cpp \
  qpid/broker/Queue.h \
  qpid/broker/QueueBindings.cpp \
  qpid/broker/QueueBindings.h \
  qpid/broker/QueueCleaner.cpp \
  qpid/broker/QueueCleaner.h \
  qpid/broker/QueueEvents.cpp \
  qpid/broker/QueueEvents.h \
  qpid/broker/QueueListeners.cpp \
  qpid/broker/QueueListeners.h \
  qpid/broker/QueuePolicy.cpp \
  qpid/broker/QueuePolicy.h \
  qpid/broker/QueueRegistry.cpp \
  qpid/broker/QueueRegistry.h \
  qpid/broker/QueuedMessage.h \
  qpid/broker/RateFlowcontrol.h \
  qpid/broker/RateTracker.cpp \
  qpid/broker/RateTracker.h \
  qpid/broker/RecoverableConfig.h \
  qpid/broker/RecoverableExchange.h \
  qpid/broker/RecoverableMessage.h \
  qpid/broker/RecoverableQueue.h \
  qpid/broker/RecoverableTransaction.h \
  qpid/broker/RecoveredDequeue.cpp \
  qpid/broker/RecoveredDequeue.h \
  qpid/broker/RecoveredEnqueue.cpp \
  qpid/broker/RecoveredEnqueue.h \
  qpid/broker/RecoveryManager.h \
  qpid/broker/RecoveryManagerImpl.cpp \
  qpid/broker/RecoveryManagerImpl.h \
  qpid/broker/RetryList.cpp \
  qpid/broker/RetryList.h \
  qpid/broker/SaslAuthenticator.cpp \
  qpid/broker/SaslAuthenticator.h \
  qpid/broker/SecureConnection.cpp \
  qpid/broker/SecureConnection.h \
  qpid/broker/SecureConnectionFactory.cpp \
  qpid/broker/SecureConnectionFactory.h \
  qpid/broker/SemanticState.cpp \
  qpid/broker/SemanticState.h \
  qpid/broker/SessionAdapter.cpp \
  qpid/broker/SessionAdapter.h \
  qpid/broker/SessionAdapter.h \
  qpid/broker/SessionContext.h \
  qpid/broker/SessionHandler.cpp \
  qpid/broker/SessionHandler.h \
  qpid/broker/SessionManager.cpp \
  qpid/broker/SessionManager.h \
  qpid/broker/SessionManager.h \
  qpid/broker/SessionState.cpp \
  qpid/broker/SessionState.h \
  qpid/broker/SignalHandler.cpp \
  qpid/broker/SignalHandler.h \
  qpid/broker/System.cpp \
  qpid/broker/System.h \
  qpid/broker/TopicExchange.cpp \
  qpid/broker/TopicExchange.h \
  qpid/broker/TransactionalStore.h \
  qpid/broker/TxAccept.cpp \
  qpid/broker/TxAccept.h \
  qpid/broker/TxBuffer.cpp \
  qpid/broker/TxBuffer.h \
  qpid/broker/TxOp.h \
  qpid/broker/TxOpVisitor.h \
  qpid/broker/TxPublish.cpp \
  qpid/broker/TxPublish.h \
  qpid/broker/Vhost.cpp \
  qpid/broker/Vhost.h \
  qpid/management/IdAllocator.h \
  qpid/management/ManagementAgent.cpp \
  qpid/management/ManagementAgent.h \
  qpid/management/ManagementExchange.cpp \
  qpid/management/ManagementExchange.h \
  qpid/sys/TCPIOPlugin.cpp


libqpidclient_la_LIBADD = libqpidcommon.la  -luuid

libqpidclient_la_SOURCES =			\
  $(rgen_client_srcs)				\
  qpid/client/Bounds.cpp			\
  qpid/client/Bounds.h				\
  qpid/client/ChainableFrameHandler.h		\
  qpid/client/Completion.cpp			\
  qpid/client/CompletionImpl.h			\
  qpid/client/Connection.cpp			\
  qpid/client/ConnectionAccess.h		\
  qpid/client/ConnectionHandler.cpp		\
  qpid/client/ConnectionHandler.h		\
  qpid/client/ConnectionImpl.cpp		\
  qpid/client/ConnectionImpl.h			\
  qpid/client/ConnectionSettings.cpp		\
  qpid/client/Connector.cpp			\
  qpid/client/Connector.h			\
  qpid/client/Demux.cpp				\
  qpid/client/Demux.h				\
  qpid/client/Dispatcher.cpp			\
  qpid/client/Dispatcher.h			\
  qpid/client/Execution.h			\
  qpid/client/FailoverListener.cpp		\
  qpid/client/FailoverManager.cpp		\
  qpid/client/Future.cpp			\
  qpid/client/FutureCompletion.cpp		\
  qpid/client/FutureResult.cpp			\
  qpid/client/LoadPlugins.cpp			\
  qpid/client/LocalQueue.cpp			\
  qpid/client/LocalQueueImpl.cpp		\
  qpid/client/LocalQueueImpl.h			\
  qpid/client/Message.cpp			\
  qpid/client/MessageImpl.cpp			\
  qpid/client/MessageImpl.h			\
  qpid/client/MessageListener.cpp		\
  qpid/client/MessageReplayTracker.cpp		\
  qpid/client/PrivateImplRef.h			\
  qpid/client/QueueOptions.cpp			\
  qpid/client/Results.cpp			\
  qpid/client/Results.h				\
  qpid/client/Sasl.h				\
  qpid/client/SaslFactory.cpp			\
  qpid/client/SaslFactory.h			\
  qpid/client/SessionBase_0_10.cpp		\
  qpid/client/SessionBase_0_10Access.h		\
  qpid/client/SessionImpl.cpp			\
  qpid/client/SessionImpl.h 			\
  qpid/client/StateManager.cpp			\
  qpid/client/StateManager.h			\
  qpid/client/Subscription.cpp			\
  qpid/client/SubscriptionImpl.cpp		\
  qpid/client/SubscriptionImpl.h		\
  qpid/client/SubscriptionManager.cpp		\
  qpid/client/SubscriptionManagerImpl.cpp	\
  qpid/client/SubscriptionManagerImpl.h		\
  qpid/client/TCPConnector.cpp			\
  qpid/client/TCPConnector.h			\
  qpid/messaging/Address.cpp			\
  qpid/messaging/Connection.cpp			\
  qpid/messaging/ListContent.cpp		\
  qpid/messaging/ListView.cpp			\
  qpid/messaging/MapContent.cpp			\
  qpid/messaging/MapView.cpp			\
  qpid/messaging/Message.cpp			\
  qpid/messaging/MessageImpl.h			\
  qpid/messaging/MessageImpl.cpp		\
  qpid/messaging/Sender.cpp			\
  qpid/messaging/Receiver.cpp			\
  qpid/messaging/Session.cpp			\
  qpid/messaging/Variant.cpp			\
  qpid/messaging/ConnectionImpl.h 		\
  qpid/messaging/SenderImpl.h			\
  qpid/messaging/ReceiverImpl.h			\
  qpid/messaging/SessionImpl.h			\
  qpid/client/amqp0_10/AcceptTracker.h		\
  qpid/client/amqp0_10/AcceptTracker.cpp	\
  qpid/client/amqp0_10/AddressResolution.h	\
  qpid/client/amqp0_10/AddressResolution.cpp	\
  qpid/client/amqp0_10/Codecs.cpp		\
  qpid/client/amqp0_10/CodecsInternal.h		\
  qpid/client/amqp0_10/ConnectionImpl.h	        \
  qpid/client/amqp0_10/ConnectionImpl.cpp	\
  qpid/client/amqp0_10/IncomingMessages.h	\
  qpid/client/amqp0_10/IncomingMessages.cpp	\
  qpid/client/amqp0_10/MessageSink.h		\
  qpid/client/amqp0_10/MessageSource.h		\
  qpid/client/amqp0_10/OutgoingMessage.h	\
  qpid/client/amqp0_10/OutgoingMessage.cpp	\
  qpid/client/amqp0_10/ReceiverImpl.h		\
  qpid/client/amqp0_10/ReceiverImpl.cpp		\
  qpid/client/amqp0_10/SessionImpl.h		\
  qpid/client/amqp0_10/SessionImpl.cpp		\
  qpid/client/amqp0_10/SenderImpl.h		\
  qpid/client/amqp0_10/SenderImpl.cpp

# NOTE: only public header files (which should be in ../include)
# should go in this list. Private headers should go in the SOURCES
# list for one of the libraries or executables that includes it.

nobase_include_HEADERS +=			\
  ../include/qpid/Address.h			\
  ../include/qpid/CommonImportExport.h		\
  ../include/qpid/Exception.h			\
  ../include/qpid/InlineAllocator.h		\
  ../include/qpid/InlineVector.h		\
  ../include/qpid/Msg.h				\
  ../include/qpid/Options.h			\
  ../include/qpid/RangeSet.h			\
  ../include/qpid/SessionId.h			\
  ../include/qpid/Url.h				\
  ../include/qpid/client/AsyncSession.h		\
  ../include/qpid/client/ClientImportExport.h	\
  ../include/qpid/client/Completion.h		\
  ../include/qpid/client/Connection.h		\
  ../include/qpid/client/ConnectionSettings.h	\
  ../include/qpid/client/FailoverListener.h	\
  ../include/qpid/client/FailoverManager.h	\
  ../include/qpid/client/FlowControl.h		\
  ../include/qpid/client/Future.h		\
  ../include/qpid/client/FutureCompletion.h	\
  ../include/qpid/client/FutureResult.h		\
  ../include/qpid/client/Handle.h		\
  ../include/qpid/client/LocalQueue.h		\
  ../include/qpid/client/Message.h		\
  ../include/qpid/client/MessageListener.h	\
  ../include/qpid/client/MessageReplayTracker.h	\
  ../include/qpid/client/QueueOptions.h		\
  ../include/qpid/client/Session.h		\
  ../include/qpid/client/SessionBase_0_10.h	\
  ../include/qpid/client/Subscription.h		\
  ../include/qpid/client/SubscriptionManager.h	\
  ../include/qpid/client/SubscriptionSettings.h	\
  ../include/qpid/client/TypedResult.h		\
  ../include/qpid/framing/Array.h		\
  ../include/qpid/framing/Buffer.h		\
  ../include/qpid/framing/FieldTable.h		\
  ../include/qpid/framing/FieldValue.h		\
  ../include/qpid/framing/List.h		\
  ../include/qpid/framing/ProtocolVersion.h	\
  ../include/qpid/framing/SequenceNumber.h	\
  ../include/qpid/framing/SequenceSet.h		\
  ../include/qpid/framing/StructHelper.h	\
  ../include/qpid/framing/Uuid.h		\
  ../include/qpid/framing/amqp_types.h		\
  ../include/qpid/framing/amqp_types_full.h	\
  ../include/qpid/log/Logger.h			\
  ../include/qpid/log/Options.h			\
  ../include/qpid/log/Selector.h		\
  ../include/qpid/log/SinkOptions.h		\
  ../include/qpid/log/Statement.h		\
  ../include/qpid/management/Args.h		\
  ../include/qpid/management/Manageable.h	\
  ../include/qpid/management/ManagementEvent.h	\
  ../include/qpid/management/ManagementObject.h	\
  ../include/qpid/sys/Condition.h		\
  ../include/qpid/sys/ExceptionHolder.h		\
  ../include/qpid/sys/IOHandle.h		\
  ../include/qpid/sys/IntegerTypes.h		\
  ../include/qpid/sys/Monitor.h			\
  ../include/qpid/sys/Mutex.h			\
  ../include/qpid/sys/Runnable.h		\
  ../include/qpid/sys/StrError.h		\
  ../include/qpid/sys/SystemInfo.h		\
  ../include/qpid/sys/Thread.h			\
  ../include/qpid/sys/Time.h			\
  ../include/qpid/messaging/Address.h 		\
  ../include/qpid/messaging/Connection.h 	\
  ../include/qpid/messaging/Codec.h 	        \
  ../include/qpid/messaging/ListContent.h 	\
  ../include/qpid/messaging/ListView.h 		\
  ../include/qpid/messaging/MapContent.h 	\
  ../include/qpid/messaging/MapView.h 		\
  ../include/qpid/messaging/Message.h 		\
  ../include/qpid/messaging/Sender.h 		\
  ../include/qpid/messaging/Receiver.h 	        \
  ../include/qpid/messaging/Session.h 		\
  ../include/qpid/messaging/Variant.h 		\
  ../include/qpid/client/amqp0_10/Codecs.h

# Force build of qpidd during dist phase so help2man will work.
dist-hook: $(BUILT_SOURCES)
	$(MAKE) qpidd

# Create the default data directory
install-data-local:
	$(mkinstalldirs) $(DESTDIR)/$(localstatedir)/lib/qpidd

