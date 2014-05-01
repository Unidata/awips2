The messaging API is intended as a high level abstraction for
messaging that avoids some of the protocol specific interactions
forced by the earlier API. This allows applications to concern
themselves with handling incoming messages and producing their own
messages and delegates knowledge of the protocol interactions required
to do so to the Qpid client library.

To send or receive messages you first need to establish a connection
to a broker. You can then create one or more sessions on that
connection. Once you have a session you can create 'senders' or
'receivers'. These are objects through which you can send or receive
messages respectively.

Senders and receivers are both created for a specified 'address'. An
address will often just be a simple name. As far as the API is
concerned, addresses are fairly opaque. This allows different types of
messaging pattern to be selected without affecting the code simply by
changing the address used, or configuring the broker to correctly
handle addresses of a certain name.

At present there are two 'types' of address supported: queues and
topics. Messages sent to a queue are stored on the queue until there
is a subscriber who can receive them. Each message is in general
allocated to one subscriber (i.e. a competing consumer pattern). A
topic on the other hand will not store messages if there are no
subscriptions active and each message will be delivered to all
interested subscribers.

In the current AMQP 0-10 implementation, queues are represented by
AMQP queues and topic are represented by AMQP exchanges to which
subscribers bind their own private subscription queue to receive
messages.

The drain and spout examples in this directory are useful for
exploring the behaviour of the messaging API over AMQP 0-10 and
different uses of addresses. There is also some documentation around
the address syntax and currently supported options in the doxygen
reference material for the Address class itself.

For example, to demonstrate classic message queueing behaviour:

* create a queue e.g. by running: qpid-config add queue my-queue

* use spout to send a message to that queue: ./spout --address my-queue

* now use drain to receive that message: ./drain --address my-queue

You can use the --content option to spout to specify text to put n the
message body. You can also alter the id used to unqieuly identify
each message using the --id option.

To demonstrate the publish-subscribe pattern used for topics:

* create an exchange e.g. by running: qpid-config add exchange topic my-topic

* start up a subscriber using drain: ./drain -f --address my-topic
  (the -f here causes the drain program to wait indefinitely for messages)

* now send a message to the topic using spout: ./spout --address my-topic

If you run spout before drain, the message will not be stored. If you
start multiple instances of drain, they will each receive the message.

For a topic, you can select the messages you wish to receive by
'subject', eg. using the same exchange as above:

* start a subscriber using drain for a specific subject:
  ./drain -f --address my-topic/my-subject

* now if you send a message with that subject you can see the
  subscriber receives it: ./spout --address my-topic/my-subject

* however were you to specify another subject for sent messages, those
  would not be received, e.g: ./spout --address my-topic/another-subject

In AMQP 0-10, the routing key is used to route messages for a given
subject. As my-topic is a topic exchange we can use the special
widlcard selection patterns when creating a subscriber:

E.g. A subscriber reciving from address 'my-topic/#.dog' will receive
messages sent to 'my-topic/big.dog' and 'my-topic/blue.dog', but not
those sent to 'my-topic.blue-cat'.

Though preconfiguring the valid addresses on a broker is a very common
pattern, it is still possible to have them created automatically
'on-demand'. This is done by specifying a create 'policy' for the address.

* run: ./spout --address 'my-new-queue; {create: always}'
* then run: ./drain --address my-new-queue

You can see that the queue was created by spout before it sent the
message to it, no explicit creation of the queue was needed.

We can do the same for a topic, but there we need to specify the type
of address (as there is no existing entity from which to infer that
type and as we do not want the default type to be created, namely a
queue):

* run: ./drain -f --address 'my-new-topic; {create: always, node-properties:{type:topic}}'
* then run: ./spout --address my-new-queue

The value to the create policy is one of always, sender, receiver or
never (which is the default). (Note: You can see the exchange created
using qpid-config exchanges, likewise to see the list of all queues
use qpid-config queues).

In addition to a create policy there are assert and delete
policies. These have the same valid values as the create policy -
always, sender, receiver and never - indicating when they come in to
effect. 

An example using the headers exchange (uses default instance, though
this need not of course be the case. You could create another using
qpid-config or even auto-create one):

* First start a subscriber, e.g.:
  ./drain -f --address 'amq.match; {filter:{x-match:all, colour:blue}}'

* Any message with a property name colour with value blue will be
  received:

  ./spout --address amq.match --property colour=blue --content 'matched!'

* But if the value of the colour property is something else, the
  message will not be received:
  ./spout --address amq.match --property colour=red --content 'not matched'

An example using xquery based filtering with the xml exchange:

* First start a subscriber with an xquery filter specified:
  ./drain -f --address 'xml/my-subject; {filter:{xquery:"declare variable $colour external; $colour = '\''red'\''"}}'

* Then test receipt of messages that match the xquery filter:
  ./spout --address 'xml/my-subject' --property colour=red --content 'matched!'
    and
  ./spout --address 'xml/my-subject' --property colour=blue --content 'not matched'

TODO:

* xml content in the xquery example

* 'durable' and 'reliable' subscriptions

* map content

* client/server example: temp queues and reply-to
