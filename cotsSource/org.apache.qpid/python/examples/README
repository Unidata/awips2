Running the Python Examples
============================


Running the Direct Examples
----------------------------

To run the direct examples, do the following:

1. Make sure that a qpidd broker is running:

 $ ps -eaf | grep qpidd

  If a broker is running, you should see the qpidd process in the output of the above command. 

2.Declare a message queue and bind it to an exchange by running declare_queues.py, as follows:

 $ python declare_queues.py

 This program has no output. After this program has been run, all messages sent to the amq.direct exchange using the routing key routing_key are sent to the queue named message_queue.

3.Publish a series of messages to the amq.direct exchange by running direct_producer.py, as follows:

 $ python direct_producer.py

This program has no output; the messages are routed to the message queue, as instructed by the binding.

4. Read the messages from the message queue using direct_consumer.py or listener.py, as follows:

 $ python direct_consumer.py

 or

 $ python listener.py

You should see the following output:

message 0
message 1
message 2
message 3
message 4
message 5
message 6
message 7
message 8
message 9
That's all, folks!



Running the Fanout Examples
----------------------------

To run the programs for the Fanout example, do the following:

1. Make sure that a qpidd broker is running:

  $ ps -eaf | grep qpidd

If a broker is running, you should see the qpidd process in the output of the above command.  

2. In separate windows, start two or more fanout consumers or fanout listeners as follows:

  $ python fanout_consumer.py

  or

  $ python listener.py

These programs each create a private queue, bind it to the amq.fanout exchange, and wait for messages to arrive on their queue.

3. In a separate window, publish a series of messages to the amq.fanout exchange by running fanout_producer.py, as follows:

  $ python fanout_producer.py

This program has no output; the messages are routed to the message queue, as instructed by the binding.
 
4. Go to the windows where you are running consumers or listeners. You should see the following output for each listener or consumer:

      message 0
      message 1
      message 2
      message 3
      message 4
      message 5
      message 6
      message 7
      message 8
      message 9
      That's all, folks!



Running the Publish-Subscribe Examples
---------------------------------------

To run the programs for the Publish-Subscribe example, do the following:

1. Make sure that a qpidd broker is running:

  $ ps -eaf | grep qpidd

If a broker is running, you should see the qpidd process in the output of the above command. 

2. In separate windows, start one or more topic subscribers by running topic_subscriber.py, as follows:

  $ python topic_subscriber.py

You will see output similar to this:

  Queues created - please start the topic producer
  Subscribing local queue 'local_news' to news-53408183-fcee-4b92-950b-90abb297e739'
  Subscribing local queue 'local_weather' to weather-53408183-fcee-4b92-950b-90abb297e739'
  Subscribing local queue 'local_usa' to usa-53408183-fcee-4b92-950b-90abb297e739'
  Subscribing local queue 'local_europe' to europe-53408183-fcee-4b92-950b-90abb297e739'
  Messages on 'news' queue:

Each topic consumer creates a set of private queues, and binds each queue to the amq.topic exchange together with a binding that indicates which messages should be routed to the queue.

3.In another window, start the topic publisher, which publishes messages to the amq.topic exchange, as follows:

  $ python topic_publisher.py

This program has no output; the messages are routed to the message queues for each topic_consumer as specified by the bindings the consumer created.

4. Go back to the window for each topic consumer. You should see output like this:

      Messages on 'news' queue:
      usa.news 0
      usa.news 1
      usa.news 2
      usa.news 3
      usa.news 4
      europe.news 0
      europe.news 1
      europe.news 2
      europe.news 3
      europe.news 4
      That's all, folks!
      Messages on 'weather' queue:
      usa.weather 0
      usa.weather 1
      usa.weather 2
      usa.weather 3
      usa.weather 4
      europe.weather 0
      europe.weather 1
      europe.weather 2
      europe.weather 3
      europe.weather 4
      That's all, folks!
      Messages on 'usa' queue:
      usa.news 0
      usa.news 1
      usa.news 2
      usa.news 3
      usa.news 4
      usa.weather 0
      usa.weather 1
      usa.weather 2
      usa.weather 3
      usa.weather 4
      That's all, folks!
      Messages on 'europe' queue:
      europe.news 0
      europe.news 1
      europe.news 2
      europe.news 3
      europe.news 4
      europe.weather 0
      europe.weather 1
      europe.weather 2
      europe.weather 3
      europe.weather 4
      That's all, folks!


Running the Request/Response Examples
--------------------------------------

To run the programs for the Request/Response example, do the following:

1. Make sure that a qpidd broker is running:

  $ ps -eaf | grep qpidd

If a broker is running, you should see the qpidd process in the output of the above command.

2. Run the server.

  $ python server.py

You should see the following output:

  Request server running - run your client now.
  (Times out after 100 seconds ...)

3. In a separate window, start a client:

  $ python client.py

You should see the following output:

      Request: Twas brillig, and the slithy toves
      Request: Did gyre and gimble in the wabe.
      Request: All mimsy were the borogroves,
      Request: And the mome raths outgrabe.
      Messages on queue: reply_to:db0f862e-6b36-4e0f-a4b2-ad049eb435ce
      Response: TWAS BRILLIG, AND THE SLITHY TOVES
      Response: DID GYRE AND GIMBLE IN THE WABE.
      Response: ALL MIMSY WERE THE BOROGROVES,
      Response: AND THE MOME RATHS OUTGRABE.
      No more messages!


Running the XML-based Routing Examples
---------------------------------------

To run the programs for the XML-based Routing example, do the following:

1. Make sure that a qpidd broker is running:

  $ ps -eaf | grep qpidd

If a broker is running, you should see the qpidd process in the output of the above command.

2. Declare an XML exchange and a message queue, then bind the queue to the exchange by running declare_queues.py, as follows:

  $ python declare_queues.py

This program has no output. After this program has been run, all messages sent to the xml exchange using the routing key weather are sent to the queue named message_queue if they satisfy the conditions specified in the following XQuery, which is used in the binding:

 let $w := ./weather
 return $w/station = 'Raleigh-Durham International Airport (KRDU)'
    and $w/temperature_f > 50
    and $w/temperature_f - $w/dewpoint > 5
    and $w/wind_speed_mph > 7
    and $w/wind_speed_mph < 20

3. Publish a series of messages to the xml exchange by running xml_producer.py, as follows:

  $ python xml_producer.py

The messages are routed to the message queue, as prescribed by the binding. Each message represents a weather report, such as this one:

  <weather>
      <station>Raleigh-Durham International Airport (KRDU)</station>
      <wind_speed_mph>16</wind_speed_mph>
      <temperature_f>70</temperature_f>
      <dewpoint>35</dewpoint>
  </weather>

4. Read the messages from the message queue using direct_consumer.py or listener.py, as follows:

  $ python xml_consumer.py

  or

  $ python listener.py

You should see the following output:

<weather><station>Raleigh-Durham International Airport (KRDU)</station>
<wind_speed_mph>16</wind_speed_mph><temperature_f>70</temperature_f>
<dewpoint>35</dewpoint></weather>


Running the Headers Examples
-----------------------------

To run the headers examples, do the following:

1. Make sure that a qpidd broker is running:

 $ ps -eaf | grep qpidd

  If a broker is running, you should see the qpidd process in the output of the above command. 

2.Declare a message queues and bind them to an exchange by running declare_queues.py, as follows:

 $ python declare_queues.py

 This program has no output. After this program has been run, all messages sent to the amq.match exchange with an application-header of {'class': 'first'} will be routed to the queue named "first" and messages with an application-header of {'class': 'second'} will be routed to the queue named "second".

3.Publish a series of messages to the amq.match exchange by running headers_producer.py, as follows:

 $ python headers_producer.py

This program has no output; the messages are routed to the message queues, as instructed by the bindings.

4. Read the messages from the message queues using headers_consumer.py as follows:

 $ python headers_consumer.py

You should see the following output:

message(first) 0
message(first) 1
message(first) 2
message(first) 3
message(first) 4
message(first) 5
message(first) 6
message(first) 7
message(first) 8
message(first) 9
That's all, folks!
message(second) 0
message(second) 1
message(second) 2
message(second) 3
message(second) 4
message(second) 5
message(second) 6
message(second) 7
message(second) 8
message(second) 9
That's all, folks!
