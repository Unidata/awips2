= Qpid C++ Examples =


This directory contains example C++ programs for Apache Qpid. They are
based on the 0-10 version of the AMQP specification (see www.amqp.org for
details). A short description of each example follows.

Please note that by default these examples attempt to connect to a Qpid
broker running on the local host (127.0.0.1) at the standard AMQP port (5672).
It is possible to instruct the examples to connect to an alternate broker
host and port by specifying the host name/address and port number as arguments
to the programs. For example, to have the declare_queues program connect to a
broker running on host1, port 9999, run the following command:

On Linux: 
  # ./declare_queues host1 9999

On Windows:
  C:\Program Files\qpidc-0.6\examples\direct> declare_queues host1 9999

The qpid C++ broker executable is named qpidd on Linux and qpidd.exe
on Windows. The default install locations are:
- Linux: /usr/sbin
- Windows: C:\Program Files\qpidc-0.6\bin

In a C++ source distribution the broker is located in the src subdirectory
(generally, from this examples directory, ../src).

== Direct ==

This example shows how to create Point-to-Point applications using Qpid. This
example contains three components.

 1. declare_queues
    This will bind a queue to the amq.direct exchange, so that the messages
    sent to the amq.direct exchange with a given routing key (routing_key) are 
    delivered to a specific queue (message_queue).

 2. direct_producer
    Publishes messages to the amq.direct exchange using the given routing key
    (routing_key) discussed above.

 3. listener
    Uses a message listener to listen for messages from a specific queue
    (message_queue) as discussed above.

In order to run this example,

On Linux:
  # ./declare_queues
  # ./direct_producer
  # ./listener

On Windows:
  C:\Program Files\qpidc-0.6\examples\direct> declare_queues
  C:\Program Files\qpidc-0.6\examples\direct> direct_producer
  C:\Program Files\qpidc-0.6\examples\direct> listener

Note that there is no requirement for the listener to be running before the
messages are published. The messages are stored in the queue until consumed
by the listener.

== Fanout ==

This example shows how to create Fanout exchange applications using Qpid.
This example has two components. Unlike the Direct example, the Fanout exchange
does not need a routing key to be specified.

 1. fanout_producer
    Publishes a message to the amq.fanout exchange, without using a routing key.

 2. listener
    Uses a message listener to listen for messages from the amq.fanout exchange.


Note that unlike the Direct example, it is necessary to start the listener
before the messages are published. The fanout exchange does not hold messages
in a queue. Therefore, it is recommended that the two parts of the example be
run in separate windows.

In order to run this example:

On Linux:
  # ./listener

  # ./fanout_producer

On Windows:
  C:\Program Files\qpidc-0.6\examples\fanout> listener

  C:\Program Files\qpidc-0.6\examples\direct> fanout_producer

== Publisher/Subscriber ==

This example demonstrates the ability to create topic Publishers and
Subscribers using Qpid. This example has two components.

 1. topic_publisher
    This application is used to publish messages to the amq.topic exchange
    using multipart routing keys, usa.weather, europe.weather, usa.news and
    europe.news.

 2. topic_listener
    This application is used to subscribe to several private queues, such as
    usa, europe, weather and news. In this program, each private queue created
    is bound to the amq.topic exchange using bindings that match the
    corresponding parts of the multipart routing keys. For example, subscribing
    to #.news will retrieve news irrespective of destination.

This example also shows the use of the 'control' routing key which is used by
control messages.

Due to this example's design, the topic_listener must be running before
starting the topic_publisher. Therefore, it is recommended that the two parts
of the example be run in separate windows.

In order to run this example,
  
On Linux:
  # ./topic_listener

  # ./topic_publisher

On Windows:
  C:\Program Files\qpidc-0.6\examples\pub-sub> topic_listener

  C:\Program Files\qpidc-0.6\examples\pub-sub> topic_publisher

== Request/Response ==

This example shows a simple server that will accept strings from a client,
convert them to upper case, and send them back to the client. This example
has two components.

 1. client
    This sends lines of poetry to the server.

 2. server
    This is a simple service that will convert incoming strings to upper case
    and send the result to amq.direct exchange on which the client listens.
    It uses the request's reply_to property as the response's routing key.

In order to run this example,

On Linux:
  # ./server
  # ./client

On Windows:
  C:\Program Files\qpidc-0.6\examples\request-response> server
  C:\Program Files\qpidc-0.6\examples\request-response> client

== QMF Agent ==

This example demonstrates integration with the Qpid Management Framework (QMF).
The qmf-agent program will connect to a running Qpid broker and advertise a
managed object (org.apache.qpid.agent.example:parent). Using the qpid-tool,
you can monitor the object and also call a method (create_child) to spawn
managed child objects.

To build this example, simply invoke make on Unix or Linux. On Windows, you 
must invoke 
	nmake /f example_gen.mak
before building the sample to generate the supporting model classes 
(e.g., Parent,Child,etc.).
