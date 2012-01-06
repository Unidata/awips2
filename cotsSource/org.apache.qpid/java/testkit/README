Introduction
============

The Test kit for the java client consists of 2 components.

1) A Simple Perf Test that can be used to,
   a) Run a predefined perf report consisting of 8 use cases (see below)
   b) Run a producer and a consumer with a number of different options

2) Soak tests that can be run for longer durations (hours or days).

I am planning to add some stress tests to this module as well.
Please note this is not a replacement for the existing perf/systests etc.
But rather a small test kit thats focused on providing a packaged set of tests that can be quickly deployed on an environment to do quick smoke testing or easily setup a soak test.

Table of Contents
=================
1. Perf Kit
2. Soak Kit
3. Perf Test use cases
4. Soak Test use cases
5. Running the sample perf test report
6. Running the sample soak test report

1.0 Perf Kit
------------
1.1 The perf kit can be packaged as an RPM or a tar file and deploy on a target environment and run the perf report.
Or else a perf report can be automated to run every day or so an record numbers to catch perf regressions.

1.2 It calculates the following results in msg/sec.
    
    System throuhgput : no_of_msgs / (time_last_msg_rcvd - time_first_msg_send)

    Producer rate : no_of_msgs / (time_after_sending - time_before_sending)

    Producer rate : no_of_msgs / (time_last_msg_rcvd - time_first_msg_rcvd)

    Latency : time_msg_rcvd - time_msg_sent

The test will print min, max and avg latency.

1.3 The test assume that both producer and consumer are run on the same machine or different machines that are time synced.

1.4 You can also use run_sub.sh and run_pub.sh to run different use cases with several options.
    Please look at TestParams.java for all the configurable options.

1.5 You can also use the test kit to benchmark against any vendor.


2.0 Soak tests
--------------
2.0 This includes a set of soak tests that can be run for a longer duration.

2.1 A typical test will send x-1 messages and the xth message will contain an "End" marker.
    The producer will print the timestamp as soon as it sends the xth message.
    The consumer will reply with an empty message to the replyTo destination given in the xth message.
    The consumer prints the throuhgput for the iteration and the latency for the xth message.
    A typical value for x is 100k

2.2 The feedback loop prevents the producer from overrunning the consumer.
    And the printout for every xth message will let you know how many iterations been completed at any given time.
    (Ex a simple cat log | wc -l will give you the how many iterations have been completed so far).

2.2 The following results can be calculated for these tests.
    
    Memory, CPU for each producer/consumer - look at testkit/bin/run_soak_client.sh for an example
    
    You can find the Avg, Min & Max for throughput, latency, CPU and memory for the entire test run.
    (look at testkit/bin/soak_report.sh) for an example).

    You could also graph throughput, latency, CPU and memory using the comma separated log files.

2.2 If you use different machines for producer and consumer the machines have to be time synced to have meaningful latency samples.
 
3.0 Perf Test report use cases
-------------------------------
3.1 Please check testkit/bin/perf_report.sh for more details

3.2 A typical test run will send 1000 msgs during warmup and 200k msgs for result calculation.

Test 1 Trans Queue

Test 2 Dura Queue

Test 3 Dura Queue Sync

Test 4 Topic

Test 5 Durable Topic

Test 6 Fanout

Test 7 Small TX (about 2 msgs per tx)

Test 8 Large TX (about 1000 msgs per tx)


4.0 Soak tests use cases
-------------------------
4.1 Following are the current tests available in the test kit.

4.2 Please refer to the source to see the javadoc and options


1. SimpleProducer/Consumer sends X messages at a time and will wait for confirmation from producer before proceeding with the next iteration. A no of options can be configured.

2. MultiThreadedProducer/Consumer does the same thing as above but runs each session in a separate thread.
   It can also send messages transactionally. Again a no of options can be configured.

3. ResourceLeakTest will setup consumer/producers sends x messages and then teard down everything and continue again.


5.0 Running the sample perf test report
---------------------------------------
The testkit/bin contains perf_report.sh.
It runs the above 8 use cases against a broker and print the results in a tabular format. 

For example
================================================================================================
|Test           |System throuput|Producer rate|Consumer Rate|Avg Latency|Min Latency|Max Latency|
------------------------------------------------------------------------------------------------
|Trans_Queue    |       xxxxx.xx|     xxxxx.xx|     xxxxx.xx|      xx.xx|          x|         xx|


5.1 running perf_report.sh

5.1.1 set JAVA_HOME to point to Java 1.5 and above
5.1.2 set QPID_TEST_HOME to point to the testkit dir
5.1.3 set VENDOR_LIB to point to the Qpid (or other JMS providers) jar files.
5.1.4 start a broker
5.1.5 update the testkit/etc/jndi.properties to point to the correct broker
5.1.6 execute perf_report.sh


6.0 Running the sample soak test report
---------------------------------------
The testkit/bin contains soak_report.sh
It runs MultiThreadedProducer/Consumer for the duration specified and prints a report for the following stats.
Avg, Min and Max for System Throughput, letency, CPU and memory.

6.1 running soak_report.sh

5.1.1 set JAVA_HOME to point to Java 1.5 and above
5.1.2 set QPID_TEST_HOME to point to the testkit dir
5.1.3 set JAR_PATH to point to the Qpid jars
5.1.4 start a broker
5.1.5 execute soak_report.sh with correct params. 
      Ex sh soak_report.sh 1 36000 will run for 10 hours colllecting CPU, memory every second.

5.1.6 Please note the total duration for the test is log_freq * log_iterations
      So if you want to run the test for 10 hours and collect 10 second samples then do the following
      sh soak_report.sh 10 3600 

