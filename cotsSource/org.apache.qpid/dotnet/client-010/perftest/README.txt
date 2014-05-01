There are two ways to use perftest: 
- single process:
If none of the -Setup, -Publish, -Subscribe or -Control options are given perftest will run a single-process test.
- multi-process: 
For a  multi-process test first run:
Perftest.exe -Setup <other options>
and wait for it to complete. The remaining process should run concurrently:
Run -Pubs times: Perftest.exe -Publish  <other options>
Run -Subs times: Perftest.exe -Subscribe <other options>
Run once:        Perftest.exe -Control <other options>
Note the <other options> must be identical for all processes.

Options:
   -Broker             Specifies the broler name
   -Confirm            Publisher use confirm-mode.
   -Control            Run test, print report.
   -Count              Each publisher sends N messages.
   -Durable            Publish messages as durable.
   -Help               Displays this help text
   -IntervalPub        >=0 delay between msg publish.
   -IntervalSub        >=0 delay between msg consume
   -Iterations         Desired number of iterations of the test.
   -Mode               Test mode: [shared|fanout|topic]
   -Port               Specifies the port name
   -Publish            Publish messages.
   -Pubs               Create N publishers.
   -QueueDurable       Make queue durable (implied if durable set.
   -QueueMaxCount      Queue policy: count to trigger 'flow to disk'
   -QueueMaxSize       Queue policy: accumulated size to trigger 'flow to disk'
   -Queues             Create N queues.
   -Setup              Create shared queues.
   -Size               Size of messages in bytes.
   -SubAck             N>0: Subscriber acks batches of N. N==0: Subscriber uses unconfirmed mode
   -Subs               Create N subscribers.
   -Subscribe          Subscribe for messages.
   -SyncPub            Wait for confirmation of each message before sending the next one.
   -Tx                 If non-zero, the transaction batch size.
   -UniqueData         Make data for each message unique.