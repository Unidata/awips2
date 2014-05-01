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

#include "TestOptions.h"

#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Completion.h"
#include "qpid/client/Message.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/Thread.h"

#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <iostream>
#include <sstream>
#include <numeric>
#include <algorithm>
#include <math.h>


using namespace std;
using namespace qpid;
using namespace client;
using namespace sys;
using boost::lexical_cast;
using boost::bind;

namespace qpid {
namespace tests {

enum Mode { SHARED, FANOUT, TOPIC };
const char* modeNames[] = { "shared", "fanout", "topic" };

// istream/ostream ops so Options can read/display Mode.
istream& operator>>(istream& in, Mode& mode) {
    string s;
    in >> s;
    int i = find(modeNames, modeNames+3, s) - modeNames;
    if (i >= 3)  throw Exception("Invalid mode: "+s);
    mode = Mode(i);
    return in;
}

ostream& operator<<(ostream& out, Mode mode) {
    return out << modeNames[mode];
}


struct Opts : public TestOptions {

    // Actions
    bool setup, control, publish, subscribe;

    // Queue policy
    uint32_t queueMaxCount;
    uint64_t queueMaxSize;
    std::string baseName;
    bool queueDurable;

    // Publisher
    size_t pubs;
    size_t count ;
    size_t size;
    bool confirm;
    bool durable;
    bool uniqueData;
    bool syncPub;

    // Subscriber
    size_t subs;
    size_t ack;

    // General
    size_t qt;
    bool singleConnect;
    size_t iterations;
    Mode mode;
    bool summary;
    uint32_t intervalSub;
    uint32_t intervalPub;
    size_t tx;
    size_t txPub;
    size_t txSub;
    bool commitAsync;

    static const std::string helpText;

    Opts() :
        TestOptions(helpText),
        setup(false), control(false), publish(false), subscribe(false), baseName("perftest"),
        pubs(1), count(500000), size(1024), confirm(true), durable(false), uniqueData(false), syncPub(false),
        subs(1), ack(0),
        qt(1),singleConnect(false), iterations(1), mode(SHARED), summary(false),
        intervalSub(0), intervalPub(0), tx(0), txPub(0), txSub(0), commitAsync(false)
    {
        addOptions()
            ("setup", optValue(setup), "Create shared queues.")
            ("control", optValue(control), "Run test, print report.")
            ("publish", optValue(publish), "Publish messages.")
            ("subscribe", optValue(subscribe), "Subscribe for messages.")

            ("mode", optValue(mode, "shared|fanout|topic"), "Test mode."
             "\nshared: --qt queues, --npubs publishers and --nsubs subscribers per queue.\n"
             "\nfanout: --npubs publishers, --nsubs subscribers, fanout exchange."
             "\ntopic: --qt topics, --npubs publishers and --nsubs subscribers per topic.\n")

            ("npubs", optValue(pubs, "N"), "Create N publishers.")
            ("count", optValue(count, "N"), "Each publisher sends N messages.")
            ("size", optValue(size, "BYTES"), "Size of messages in bytes.")
            ("pub-confirm", optValue(confirm, "yes|no"), "Publisher use confirm-mode.")
            ("durable", optValue(durable, "yes|no"), "Publish messages as durable.")
            ("unique-data", optValue(uniqueData, "yes|no"), "Make data for each message unique.")
            ("sync-publish", optValue(syncPub, "yes|no"), "Wait for confirmation of each message before sending the next one.")

            ("nsubs", optValue(subs, "N"), "Create N subscribers.")
            ("sub-ack", optValue(ack, "N"), "N>0: Subscriber acks batches of N.\n"
             "N==0: Subscriber uses unconfirmed mode")

            ("qt", optValue(qt, "N"), "Create N queues or topics.")
            ("single-connection", optValue(singleConnect, "yes|no"), "Use one connection for multiple sessions.")

            ("iterations", optValue(iterations, "N"), "Desired number of iterations of the test.")
            ("summary,s", optValue(summary), "Summary output: pubs/sec subs/sec transfers/sec Mbytes/sec")

            ("queue-max-count", optValue(queueMaxCount, "N"), "queue policy: count to trigger 'flow to disk'")
            ("queue-max-size", optValue(queueMaxSize, "N"), "queue policy: accumulated size to trigger 'flow to disk'")
            ("base-name", optValue(baseName, "NAME"), "base name used for queues or topics")
            ("queue-durable", optValue(queueDurable, "N"), "Make queue durable (implied if durable set)")

            ("interval_sub", optValue(intervalSub, "ms"), ">=0 delay between msg consume")
            ("interval_pub", optValue(intervalPub, "ms"), ">=0 delay between msg publish")

            ("tx", optValue(tx, "N"), "if non-zero, the transaction batch size for publishing and consuming")
            ("pub-tx", optValue(txPub, "N"), "if non-zero, the transaction batch size for publishing")
            ("async-commit", optValue(commitAsync, "yes|no"), "Don't wait for completion of commit")
            ("sub-tx", optValue(txSub, "N"), "if non-zero, the transaction batch size for consuming");
    }

    // Computed values
    size_t totalPubs;
    size_t totalSubs;
    size_t transfers;
    size_t subQuota;

    void parse(int argc, char** argv) {
        TestOptions::parse(argc, argv);
        switch (mode) {
          case SHARED:
            if (count % subs) {
                count += subs - (count % subs);
                cout << "WARNING: Adjusted --count to " << count
                     << " the nearest multiple of --nsubs" << endl;
            }
            totalPubs = pubs*qt;
            totalSubs = subs*qt;
            subQuota = (pubs*count)/subs;
            break;
          case FANOUT:
            if (qt != 1) cerr << "WARNING: Fanout mode, ignoring --qt="
                              << qt << endl;
            qt=1;
            totalPubs = pubs;
            totalSubs = subs;
            subQuota = totalPubs*count;
            break;
          case TOPIC:
            totalPubs = pubs*qt;
            totalSubs = subs*qt;
            subQuota = pubs*count;
            break;
        }
        transfers=(totalPubs*count) + (totalSubs*subQuota);
        if (tx) {
            if (txPub) {
                cerr << "WARNING: Using overriden tx value for publishers: " << txPub << std::endl;
            } else {
                txPub = tx;
            }
            if (txSub) {
                cerr << "WARNING: Using overriden tx value for subscribers: " << txSub << std::endl;
            } else {
                txSub = tx;
            }
        }
    }
};

const std::string Opts::helpText=
"There are two ways to use perftest: single process or multi-process.\n\n"
"If none of the --setup, --publish, --subscribe or --control options\n"
"are given perftest will run a single-process test.\n"
"For a  multi-process test first run:\n"
"  perftest --setup <other options>\n"
"and wait for it to complete. The remaining process should run concurrently::\n"
"Run --npubs times: perftest --publish <other options>\n"
"Run --nsubs times: perftest --subscribe <other options>\n"
"Run once:          perftest --control <other options>\n"
"Note the <other options> must be identical for all processes.\n";

Opts opts;
Connection globalConnection;

std::string fqn(const std::string& name)
{
    ostringstream fqn;
    fqn << opts.baseName << "_" << name;
    return fqn.str();
}

struct Client : public Runnable {
    Connection* connection;
    Connection localConnection;
    AsyncSession session;
    Thread thread;

    Client() {
        if (opts.singleConnect){
            connection = &globalConnection;
            if (!globalConnection.isOpen()) opts.open(globalConnection);
        }else{
            connection = &localConnection;
            opts.open(localConnection);
        }
        session = connection->newSession();
    }

    ~Client() {
        try {
            if (connection->isOpen()) {
                session.close();
                connection->close();
            }
        } catch (const std::exception& e) {
            std::cerr << "Error in shutdown: " << e.what() << std::endl;
        }
    }
};

struct Setup : public Client {

    void queueInit(string name, bool durable=false, const framing::FieldTable& settings=framing::FieldTable()) {
        session.queueDeclare(arg::queue=name, arg::durable=durable, arg::arguments=settings);
        session.queuePurge(arg::queue=name);
        session.sync();
    }

    void run() {
        queueInit(fqn("pub_start"));
        queueInit(fqn("pub_done"));
        queueInit(fqn("sub_ready"));
        queueInit(fqn("sub_done"));
        if (opts.iterations > 1) queueInit(fqn("sub_iteration"));
        if (opts.mode==SHARED) {
            framing::FieldTable settings;//queue policy settings
            settings.setInt("qpid.max_count", opts.queueMaxCount);
            settings.setInt("qpid.max_size", opts.queueMaxSize);
            for (size_t i = 0; i < opts.qt; ++i) {
                ostringstream qname;
                qname << opts.baseName << i;
                queueInit(qname.str(), opts.durable || opts.queueDurable, settings);
            }
        }
    }
};

void expect(string actual, string expect) {
    if (expect != actual)
        throw Exception("Expecting "+expect+" but received "+actual);

}

double secs(Duration d) { return double(d)/TIME_SEC; }
double secs(AbsTime start, AbsTime finish) {
    return secs(Duration(start,finish));
}


// Collect rates & print stats.
class Stats {
    vector<double> values;
    double sum;

  public:
    Stats() : sum(0) {}

    // Functor to collect rates.
    void operator()(const string& data) {
        try {
            double d=lexical_cast<double>(data);
            values.push_back(d);
            sum += d;
        } catch (const std::exception&) {
            throw Exception("Bad report: "+data);
        }
    }

    double mean() const {
        return sum/values.size();
    }

    double stdev() const {
        if (values.size() <= 1) return 0;
        double avg = mean();
        double ssq = 0;
        for (vector<double>::const_iterator i = values.begin();
             i != values.end(); ++i) {
            double x=*i;
            x -= avg;
            ssq += x*x;
        }
        return sqrt(ssq/(values.size()-1));
    }

    ostream& print(ostream& out) {
        ostream_iterator<double> o(out, "\n");
        copy(values.begin(), values.end(), o);
        out << "Average: " << mean();
        if (values.size() > 1)
            out << " (std.dev. " << stdev() << ")";
        return out << endl;
    }
};


// Manage control queues, collect and print reports.
struct Controller : public Client {

   SubscriptionManager subs;

    Controller() : subs(session) {}

    /** Process messages from queue by applying a functor. */
    void process(size_t n, string queue,
                 boost::function<void (const string&)> msgFn)
    {
        if (!opts.summary)
            cout << "Processing " << n << " messages from "
                 << queue << " " << flush;
        LocalQueue lq;
        subs.setFlowControl(n, SubscriptionManager::UNLIMITED, false);
        subs.subscribe(lq, queue);
        for (size_t i = 0; i < n; ++i) {
            if (!opts.summary) cout << "." << flush;
            msgFn(lq.pop().getData());
        }
        if (!opts.summary) cout << " done." << endl;
    }

    void process(size_t n, LocalQueue lq, string queue,
                 boost::function<void (const string&)> msgFn)
    {
        session.messageFlow(queue, 0, n);
        if (!opts.summary)
            cout << "Processing " << n << " messages from "
                 << queue << " " << flush;
        for (size_t i = 0; i < n; ++i) {
            if (!opts.summary) cout << "." << flush;
            msgFn(lq.pop().getData());
        }
        if (!opts.summary) cout << " done." << endl;
    }

    void send(size_t n, string queue, string data) {
        if (!opts.summary)
            cout << "Sending " << data << " " << n << " times to " << queue
                 << endl;
        Message msg(data, queue);
        for (size_t i = 0; i < n; ++i)
            session.messageTransfer(arg::content=msg, arg::acceptMode=1);
    }

    void run() {                // Controller
        try {
            // Wait for subscribers to be ready.
            process(opts.totalSubs, fqn("sub_ready"), bind(expect, _1, "ready"));

            LocalQueue pubDone;
            LocalQueue subDone;
            subs.setFlowControl(0, SubscriptionManager::UNLIMITED, false);
            subs.subscribe(pubDone, fqn("pub_done"));
            subs.subscribe(subDone, fqn("sub_done"));

            double txrateTotal(0);
            double mbytesTotal(0);
            double pubRateTotal(0);
            double subRateTotal(0);

            for (size_t j = 0; j < opts.iterations; ++j) {
                AbsTime start=now();
                send(opts.totalPubs, fqn("pub_start"), "start"); // Start publishers
                if (j) {
		    send(opts.totalPubs, fqn("sub_iteration"), "next"); // Start subscribers on next iteration
                }

                Stats pubRates;
                Stats subRates;

                process(opts.totalPubs, pubDone, fqn("pub_done"), boost::ref(pubRates));
                process(opts.totalSubs, subDone, fqn("sub_done"), boost::ref(subRates));

                AbsTime end=now();

                double time=secs(start, end);
                double txrate=opts.transfers/time;
                double mbytes=(txrate*opts.size)/(1024*1024);

                if (!opts.summary) {
                    cout << endl << "Total " << opts.transfers << " transfers of "
                         << opts.size << " bytes in "
                         << time << " seconds." << endl;
                    cout << endl << "Publish transfers/sec:    " << endl;
                    pubRates.print(cout);
                    cout << endl << "Subscribe transfers/sec:  " << endl;
                    subRates.print(cout);
                    cout << endl
                         << "Total transfers/sec:      " << txrate << endl
                         << "Total Mbytes/sec: " << mbytes << endl;
                }
                else {
                    cout << pubRates.mean() << "\t"
                         << subRates.mean() << "\t"
                         << txrate << "\t"
                         << mbytes << endl;
                }

                txrateTotal += txrate;
                mbytesTotal += mbytes;
                pubRateTotal += pubRates.mean();
                subRateTotal += subRates.mean();
            }
            if (opts.iterations > 1) {
                cout << "Averages: "<< endl
                     << (pubRateTotal / opts.iterations) << "\t"
                     << (subRateTotal / opts.iterations) << "\t"
                     << (txrateTotal / opts.iterations) << "\t"
                     << (mbytesTotal / opts.iterations) << endl;
            }
        }
        catch (const std::exception& e) {
            cout << "Controller exception: " << e.what() << endl;
        }
    }
};


struct PublishThread : public Client {
    string destination;
    string routingKey;

    PublishThread() {};

    PublishThread(string key, string dest=string()) {
        destination=dest;
        routingKey=key;
    }

    void run() {                // Publisher
        try {
            string data;
            size_t offset(0);
            if (opts.uniqueData) {
                offset = 5;
                data += "data:";//marker (requested for latency testing tool scripts)
                data += string(sizeof(size_t), 'X');//space for seq no
                data += session.getId().str();
                if (opts.size > data.size()) {
                    data += string(opts.size - data.size(), 'X');
                } else if(opts.size < data.size()) {
                    cout << "WARNING: Increased --size to " << data.size()
                         << " to honour --unique-data" << endl;
                }
            } else {
                size_t msgSize=max(opts.size, sizeof(size_t));
                data = string(msgSize, 'X');
            }

            Message msg(data, routingKey);
            if (opts.durable)
                msg.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);


            if (opts.txPub){
                session.txSelect();
            }
            SubscriptionManager subs(session);
            LocalQueue lq;
            subs.setFlowControl(1, SubscriptionManager::UNLIMITED, true);
            subs.subscribe(lq, fqn("pub_start"));

            for (size_t j = 0; j < opts.iterations; ++j) {
                expect(lq.pop().getData(), "start");
                AbsTime start=now();
                for (size_t i=0; i<opts.count; i++) {
                    // Stamp the iteration into the message data, avoid
                    // any heap allocation.
                    const_cast<std::string&>(msg.getData()).replace(offset, sizeof(size_t),
                                          reinterpret_cast<const char*>(&i), sizeof(size_t));
                    if (opts.syncPub) {
                        sync(session).messageTransfer(
                            arg::destination=destination,
                            arg::content=msg,
                            arg::acceptMode=1);
                    } else {
                        session.messageTransfer(
                            arg::destination=destination,
                            arg::content=msg,
                            arg::acceptMode=1);
                    }
                    if (opts.txPub && ((i+1) % opts.txPub == 0)){
                        if (opts.commitAsync){
                            session.txCommit();
                        } else {
                            sync(session).txCommit();
                        }
                    }
                    if (opts.intervalPub)
                        qpid::sys::usleep(opts.intervalPub*1000);
                }
                if (opts.confirm) session.sync();
                AbsTime end=now();
                double time=secs(start,end);

                // Send result to controller.
                Message report(lexical_cast<string>(opts.count/time), fqn("pub_done"));
                session.messageTransfer(arg::content=report, arg::acceptMode=1);
                if (opts.txPub){
                    sync(session).txCommit();
                }
            }
            session.close();
        }
        catch (const std::exception& e) {
            cout << "PublishThread exception: " << e.what() << endl;
        }
    }
};

struct SubscribeThread : public Client {

    string queue;

    SubscribeThread() {}

    SubscribeThread(string q) { queue = q; }

    SubscribeThread(string key, string ex) {
        queue=session.getId().str(); // Unique name.
        session.queueDeclare(arg::queue=queue,
                             arg::exclusive=true,
                             arg::autoDelete=true,
                             arg::durable=opts.durable);
        session.exchangeBind(arg::queue=queue,
                             arg::exchange=ex,
                             arg::bindingKey=key);
    }

    void verify(bool cond, const char* test, uint32_t expect, uint32_t actual) {
        if (!cond) {
            Message error(
                QPID_MSG("Sequence error: expected  n" << test << expect << " but got " << actual),
                "sub_done");
            session.messageTransfer(arg::content=error, arg::acceptMode=1);
            throw Exception(error.getData());
        }
    }

    void run() {                // Subscribe
        try {
            if (opts.txSub) sync(session).txSelect();
            SubscriptionManager subs(session);
            SubscriptionSettings settings;
            settings.autoAck = opts.txSub ? opts.txSub : opts.ack;
            settings.acceptMode = (opts.txSub || opts.ack ? ACCEPT_MODE_EXPLICIT : ACCEPT_MODE_NONE);
            settings.flowControl = FlowControl::messageCredit(opts.subQuota);
            LocalQueue lq;
            Subscription subscription = subs.subscribe(lq, queue, settings);
            // Notify controller we are ready.
            session.messageTransfer(arg::content=Message("ready", fqn("sub_ready")), arg::acceptMode=1);
            if (opts.txSub) {
                if (opts.commitAsync) session.txCommit();
                else sync(session).txCommit();
            }

            LocalQueue iterationControl;
            if (opts.iterations > 1) {
                subs.subscribe(iterationControl, fqn("sub_iteration"), SubscriptionSettings(FlowControl::messageCredit(0)));
            }

            for (size_t j = 0; j < opts.iterations; ++j) {
                if (j > 0) {
                    //need to wait here until all subs are done
                    session.messageFlow(fqn("sub_iteration"), 0, 1);
                    iterationControl.pop();

                    //need to allocate some more credit for subscription
                    session.messageFlow(queue, 0, opts.subQuota);
                }
                Message msg;
                AbsTime start=now();
                size_t expect=0;
                for (size_t i = 0; i < opts.subQuota; ++i) {
                    msg=lq.pop();
                    if (opts.txSub && ((i+1) % opts.txSub == 0)) {
                        if (opts.commitAsync) session.txCommit();
                        else sync(session).txCommit();
                    }
                    if (opts.intervalSub)
                        qpid::sys::usleep(opts.intervalSub*1000);
                    // TODO aconway 2007-11-23: check message order for.
                    // multiple publishers. Need an array of counters,
                    // one per publisher and a publisher ID in the
                    // message. Careful not to introduce a lot of overhead
                    // here, e.g. no std::map, std::string etc.
                    //
                    // For now verify order only for a single publisher.
                    size_t offset = opts.uniqueData ? 5 /*marker is 'data:'*/ : 0;
                    size_t n = *reinterpret_cast<const size_t*>(msg.getData().data() + offset);
                    if (opts.pubs == 1) {
                        if (opts.subs == 1 || opts.mode == FANOUT) verify(n==expect, "==", expect, n);
                        else verify(n>=expect, ">=", expect, n);
                        expect = n+1;
                    }
                }
                if (opts.txSub || opts.ack)
                    subscription.accept(subscription.getUnaccepted());
                if (opts.txSub) {
                    if (opts.commitAsync) session.txCommit();
                    else sync(session).txCommit();
                }
                AbsTime end=now();

                // Report to publisher.
                Message result(lexical_cast<string>(opts.subQuota/secs(start,end)),
                               fqn("sub_done"));
                session.messageTransfer(arg::content=result, arg::acceptMode=1);
                if (opts.txSub) sync(session).txCommit();
            }
            session.close();
        }
        catch (const std::exception& e) {
            cout << "SubscribeThread exception: " << e.what() << endl;
        }
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv) {
    int exitCode = 0;
    boost::ptr_vector<Client> subs(opts.subs);
    boost::ptr_vector<Client> pubs(opts.pubs);

    try {
        opts.parse(argc, argv);

        string exchange;
        switch (opts.mode) {
            case FANOUT: exchange="amq.fanout"; break;
            case TOPIC: exchange="amq.topic"; break;
            case SHARED: break;
        }

        bool singleProcess=
            (!opts.setup && !opts.control && !opts.publish && !opts.subscribe);
        if (singleProcess)
            opts.setup = opts.control = opts.publish = opts.subscribe = true;

        if (opts.setup) Setup().run();          // Set up queues

        // Start pubs/subs for each queue/topic.
        for (size_t i = 0; i < opts.qt; ++i) {
            ostringstream key;
            key << opts.baseName << i; // Queue or topic name.
            if (opts.publish) {
                size_t n = singleProcess ? opts.pubs : 1;
                for (size_t j = 0; j < n; ++j)  {
                    pubs.push_back(new PublishThread(key.str(), exchange));
                    pubs.back().thread=Thread(pubs.back());
                }
            }
            if (opts.subscribe) {
                size_t n = singleProcess ? opts.subs : 1;
                for (size_t j = 0; j < n; ++j)  {
                    if (opts.mode==SHARED)
                        subs.push_back(new SubscribeThread(key.str()));
                    else
                        subs.push_back(new SubscribeThread(key.str(),exchange));
                    subs.back().thread=Thread(subs.back());
                }
            }
        }

        if (opts.control) Controller().run();
    }
    catch (const std::exception& e) {
        cout << endl << e.what() << endl;
        exitCode = 1;
    }

    // Wait for started threads.
    if (opts.publish) {
        for (boost::ptr_vector<Client>::iterator i=pubs.begin();
             i != pubs.end();
             ++i)
            i->thread.join();
    }

    if (opts.subscribe) {
        for (boost::ptr_vector<Client>::iterator i=subs.begin();
             i != subs.end();
             ++i)
            i->thread.join();
    }
    return exitCode;
}
