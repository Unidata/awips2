This example shows how to program a simple application
using the XML Exchange. 

[Note: The XML Exchange is not a standard AMQP exchange type. To run
this example you need to have a broker that has support for the xml
exchange. If you are compiling the broker from source please refer to
the INSTALL notes from qpid.]

To run the example, execute the programs in the
following order:

1 ./declare_queues
2 ./listener
3 ./message_producer (in a separate window)

The XML Exchange must be explicitly declared. Bindings
are established using queries in XQuery. These queries
can reference message content, message application 
properties (which are declared as external variables
in the XQuery), or both. 

Once this is done, message producers publish to the
exchange using the exchange name and a routing key,
just as for other exchange types. Message consumers
read from the queues to which messages are routed.
If a message does not have XML content, or is
missing message application properties needed by
the query, the query is not routed.

Queries can use message application headers to
provide functionality similar to JMS selectors.
If a query does not use the content of a message,
the message content is not parsed, and need not
be XML.

The XQuery processor, XQilla,  does path-based 
document projection, so once the portion of
a document needed to evaluate a query has
been read, it stops parsing the document.
Suppose a long document has a header section.
You can indicate in the query that only
one header section needs to be queried,
and there is no need to parse the entire
document to see if there are further header
sections, using a path like this:

./message/header[1]/date

If you used a path like this, all children
of the message element would be read to
see if there are further headers:

./message/header/date
