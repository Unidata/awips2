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

#include <stdlib.h>
#include <string>
#include <windows.h>
#include <qpid/broker/RecoverableQueue.h>
#include <qpid/log/Statement.h>
#include <qpid/store/MessageStorePlugin.h>
#include <qpid/store/StorageProvider.h>
#include "AmqpTransaction.h"
#include "BlobAdapter.h"
#include "BlobRecordset.h"
#include "BindingRecordset.h"
#include "MessageMapRecordset.h"
#include "MessageRecordset.h"
#include "DatabaseConnection.h"
#include "Exception.h"
#include "State.h"
#include "VariantHelper.h"

// Bring in ADO 2.8 (yes, I know it says "15", but that's it...)
#import "C:\Program Files\Common Files\System\ado\msado15.dll" \
        no_namespace rename("EOF", "EndOfFile")
#include <comdef.h>
namespace {
inline void TESTHR(HRESULT x) {if FAILED(x) _com_issue_error(x);};

// Table names
const std::string TblBinding("tblBinding");
const std::string TblConfig("tblConfig");
const std::string TblExchange("tblExchange");
const std::string TblMessage("tblMessage");
const std::string TblMessageMap("tblMessageMap");
const std::string TblQueue("tblQueue");
}

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class MSSqlProvider
 *
 * Implements a qpid::store::StorageProvider that uses Microsoft SQL Server as
 * the backend data store for Qpid.
 */
class MSSqlProvider : public qpid::store::StorageProvider
{
protected:
    void finalizeMe();

    void dump();

public:
    MSSqlProvider();
    ~MSSqlProvider();

    virtual qpid::Options* getOptions() { return &options; }

    virtual void earlyInitialize (Plugin::Target& target);
    virtual void initialize(Plugin::Target& target);

    /**
     * Receive notification that this provider is the one that will actively
     * handle provider storage for the target. If the provider is to be used,
     * this method will be called after earlyInitialize() and before any
     * recovery operations (recovery, in turn, precedes call to initialize()).
     */
    virtual void activate(MessageStorePlugin &store);

    /**
     * @name Methods inherited from qpid::broker::MessageStore
     */
    //@{
    /**
     * If called after init() but before recovery, will discard the database
     * and reinitialize using an empty store dir. If @a pushDownStoreFiles
     * is true, the content of the store dir will be moved to a backup dir
     * inside the store dir. This is used when cluster nodes recover and must
     * get thier content from a cluster sync rather than directly fromt the
     * store.
     *
     * @param pushDownStoreFiles If true, will move content of the store dir
     *                           into a subdir, leaving the store dir
     *                           otherwise empty.
     */
    virtual void truncateInit(const bool pushDownStoreFiles = false);

    /**
     * Record the existence of a durable queue
     */
    virtual void create(PersistableQueue& queue,
                        const qpid::framing::FieldTable& args);
    /**
     * Destroy a durable queue
     */
    virtual void destroy(PersistableQueue& queue);

    /**
     * Record the existence of a durable exchange
     */
    virtual void create(const PersistableExchange& exchange,
                        const qpid::framing::FieldTable& args);
    /**
     * Destroy a durable exchange
     */
    virtual void destroy(const PersistableExchange& exchange);

    /**
     * Record a binding
     */
    virtual void bind(const PersistableExchange& exchange,
                      const PersistableQueue& queue,
                      const std::string& key,
                      const qpid::framing::FieldTable& args);

    /**
     * Forget a binding
     */
    virtual void unbind(const PersistableExchange& exchange,
                        const PersistableQueue& queue,
                        const std::string& key,
                        const qpid::framing::FieldTable& args);

    /**
     * Record generic durable configuration
     */
    virtual void create(const PersistableConfig& config);

    /**
     * Destroy generic durable configuration
     */
    virtual void destroy(const PersistableConfig& config);

    /**
     * Stores a messages before it has been enqueued
     * (enqueueing automatically stores the message so this is
     * only required if storage is required prior to that
     * point). If the message has not yet been stored it will
     * store the headers as well as any content passed in. A
     * persistence id will be set on the message which can be
     * used to load the content or to append to it.
     */
    virtual void stage(const boost::intrusive_ptr<PersistableMessage>& msg);

    /**
     * Destroys a previously staged message. This only needs
     * to be called if the message is never enqueued. (Once
     * enqueued, deletion will be automatic when the message
     * is dequeued from all queues it was enqueued onto).
     */
    virtual void destroy(PersistableMessage& msg);

    /**
     * Appends content to a previously staged message
     */
    virtual void appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg,
                               const std::string& data);

    /**
     * Loads (a section) of content data for the specified
     * message (previously stored through a call to stage or
     * enqueue) into data. The offset refers to the content
     * only (i.e. an offset of 0 implies that the start of the
     * content should be loaded, not the headers or related
     * meta-data).
     */
    virtual void loadContent(const qpid::broker::PersistableQueue& queue,
                             const boost::intrusive_ptr<const PersistableMessage>& msg,
                             std::string& data,
                             uint64_t offset,
                             uint32_t length);

    /**
     * Enqueues a message, storing the message if it has not
     * been previously stored and recording that the given
     * message is on the given queue.
     *
     * Note: that this is async so the return of the function does
     * not mean the opperation is complete.
     *
     * @param msg the message to enqueue
     * @param queue the name of the queue onto which it is to be enqueued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void enqueue(qpid::broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<PersistableMessage>& msg,
                         const PersistableQueue& queue);

    /**
     * Dequeues a message, recording that the given message is
     * no longer on the given queue and deleting the message
     * if it is no longer on any other queue.
     *
     * Note: that this is async so the return of the function does
     * not mean the opperation is complete.
     *
     * @param msg the message to dequeue
     * @param queue the name of the queue from which it is to be dequeued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void dequeue(qpid::broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<PersistableMessage>& msg,
                         const PersistableQueue& queue);

    /**
     * Flushes all async messages to disk for the specified queue
     *
     * Note: this is a no-op for this provider.
     *
     * @param queue the name of the queue from which it is to be dequeued
     */
    virtual void flush(const PersistableQueue& queue) {};

    /**
     * Returns the number of outstanding AIO's for a given queue
     *
     * If 0, than all the enqueue / dequeues have been stored
     * to disk
     *
     * @param queue the name of the queue to check for outstanding AIO
     */
    virtual uint32_t outstandingQueueAIO(const PersistableQueue& queue)
        {return 0;}
    //@}

    /**
     * @name Methods inherited from qpid::broker::TransactionalStore
     */
    //@{
    virtual std::auto_ptr<qpid::broker::TransactionContext> begin();
    virtual std::auto_ptr<qpid::broker::TPCTransactionContext> begin(const std::string& xid);
    virtual void prepare(qpid::broker::TPCTransactionContext& txn);
    virtual void commit(qpid::broker::TransactionContext& txn);
    virtual void abort(qpid::broker::TransactionContext& txn);

    // @TODO This maybe should not be in TransactionalStore
    virtual void collectPreparedXids(std::set<std::string>& xids) {}
    //@}

    virtual void recoverConfigs(qpid::broker::RecoveryManager& recoverer);
    virtual void recoverExchanges(qpid::broker::RecoveryManager& recoverer,
                                  ExchangeMap& exchangeMap);
    virtual void recoverQueues(qpid::broker::RecoveryManager& recoverer,
                               QueueMap& queueMap);
    virtual void recoverBindings(qpid::broker::RecoveryManager& recoverer,
                                 const ExchangeMap& exchangeMap,
                                 const QueueMap& queueMap);
    virtual void recoverMessages(qpid::broker::RecoveryManager& recoverer,
                                 MessageMap& messageMap,
                                 MessageQueueMap& messageQueueMap);

private:
    struct ProviderOptions : public qpid::Options
    {
        std::string connectString;
        std::string catalogName;

        ProviderOptions(const std::string &name)
            : qpid::Options(name),
              catalogName("QpidStore")
        {
            const enum { NAMELEN = MAX_COMPUTERNAME_LENGTH + 1 };
            TCHAR myName[NAMELEN];
            DWORD myNameLen = NAMELEN;
            GetComputerName(myName, &myNameLen);
            connectString = "Data Source=";
            connectString += myName;
            connectString += "\\SQLEXPRESS;Integrated Security=SSPI";
            addOptions()
                ("connect",
                 qpid::optValue(connectString, "STRING"),
                 "Connection string for the database to use. Will prepend "
                 "Provider=SQLOLEDB;")
                ("catalog",
                 qpid::optValue(catalogName, "DB NAME"),
                 "Catalog (database) name")
                ;
        }
    };
    ProviderOptions options;

    // Each thread has a separate connection to the database and also needs
    // to manage its COM initialize/finalize individually. This is done by
    // keeping a thread-specific State.
    boost::thread_specific_ptr<State> dbState;

    State *initState();
    DatabaseConnection *initConnection(void);
    void createDb(_ConnectionPtr conn, const std::string &name);
};

static MSSqlProvider static_instance_registers_plugin;

void
MSSqlProvider::finalizeMe()
{
    dbState.reset();
}

MSSqlProvider::MSSqlProvider()
    : options("MS SQL Provider options")
{
}

MSSqlProvider::~MSSqlProvider()
{
}

void
MSSqlProvider::earlyInitialize(Plugin::Target &target)
{
    MessageStorePlugin *store = dynamic_cast<MessageStorePlugin *>(&target);
    if (store) {
        // If the database init fails, report it and don't register; give
        // the rest of the broker a chance to run.
        //
        // Don't try to initConnection() since that will fail if the
        // database doesn't exist. Instead, try to open a connection without
        // a database name, then search for the database. There's still a
        // chance this provider won't be selected for the store too, so be
        // be sure to close the database connection before return to avoid
        // leaving a connection up that will not be used.
        try {
            initState();     // This initializes COM
            std::auto_ptr<DatabaseConnection> db(new DatabaseConnection());
            db->open(options.connectString, "");
            _ConnectionPtr conn(*db);
            _RecordsetPtr pCatalogs = NULL;
            VariantHelper<std::string> catalogName(options.catalogName);
            pCatalogs = conn->OpenSchema(adSchemaCatalogs, catalogName);
            if (pCatalogs->EndOfFile) {
                // Database doesn't exist; create it
                QPID_LOG(notice,
                         "MSSQL: Creating database " + options.catalogName);
                createDb(conn, options.catalogName);
            }
            else {
                QPID_LOG(notice,
                         "MSSQL: Database located: " + options.catalogName);
            }
            if (pCatalogs) {
                if (pCatalogs->State == adStateOpen)
                    pCatalogs->Close();
                pCatalogs = 0;
            }
            db->close();
            store->providerAvailable("MSSQL", this);
        }
        catch (qpid::Exception &e) {
            QPID_LOG(error, e.what());
            return;
        }
        store->addFinalizer(boost::bind(&MSSqlProvider::finalizeMe, this));
    }
}

void
MSSqlProvider::initialize(Plugin::Target& target)
{
}

void
MSSqlProvider::activate(MessageStorePlugin &store)
{
  QPID_LOG(info, "MS SQL Provider is up");
}

void
MSSqlProvider::truncateInit(const bool pushDownStoreFiles)
{
}

void
MSSqlProvider::create(PersistableQueue& queue,
                      const qpid::framing::FieldTable& /*args needed for jrnl*/)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsQueues;
    try {
        db->beginTransaction();
        rsQueues.open(db, TblQueue);
        rsQueues.add(queue);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error creating queue " + queue.getName(), e);
    }
}

/**
 * Destroy a durable queue
 */
void
MSSqlProvider::destroy(PersistableQueue& queue)
{
    // MessageDeleter class for use with for_each, below.
    class MessageDeleter {
        BlobRecordset& msgs;
    public:
        explicit MessageDeleter(BlobRecordset& _msgs) : msgs(_msgs) {}
        void operator()(uint64_t msgId) { msgs.remove(msgId); }
    };

    DatabaseConnection *db = initConnection();
    BlobRecordset rsQueues;
    BindingRecordset rsBindings;
    MessageRecordset rsMessages;
    MessageMapRecordset rsMessageMaps;
    try {
        db->beginTransaction();
        rsQueues.open(db, TblQueue);
        rsBindings.open(db, TblBinding);
        rsMessages.open(db, TblMessage);
        rsMessageMaps.open(db, TblMessageMap);
        // Remove bindings first; the queue IDs can't be ripped out from
        // under the references in the bindings table. Then remove the
        // message->queue entries for the queue, also because the queue can't
        // be deleted while there are references to it. If there are messages
        // orphaned by removing the queue references, those messages can
        // also be deleted. Lastly, the queue record can be removed.
        rsBindings.removeForQueue(queue.getPersistenceId());
        std::vector<uint64_t> orphans;
        rsMessageMaps.removeForQueue(queue.getPersistenceId(), orphans);
        std::for_each(orphans.begin(), orphans.end(),
                      MessageDeleter(rsMessages));
        rsQueues.remove(queue);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error deleting queue " + queue.getName(), e);
    }
}

/**
 * Record the existence of a durable exchange
 */
void
MSSqlProvider::create(const PersistableExchange& exchange,
                      const qpid::framing::FieldTable& args)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsExchanges;
    try {
        db->beginTransaction();
        rsExchanges.open(db, TblExchange);
        rsExchanges.add(exchange);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error creating exchange " + exchange.getName(), e);
    }
}

/**
 * Destroy a durable exchange
 */
void
MSSqlProvider::destroy(const PersistableExchange& exchange)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsExchanges;
    BindingRecordset rsBindings;
    try {
        db->beginTransaction();
        rsExchanges.open(db, TblExchange);
        rsBindings.open(db, TblBinding);
        // Remove bindings first; the exchange IDs can't be ripped out from
        // under the references in the bindings table.
        rsBindings.removeForExchange(exchange.getPersistenceId());
        rsExchanges.remove(exchange);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error deleting exchange " + exchange.getName(), e);
    }
}

/**
 * Record a binding
 */
void
MSSqlProvider::bind(const PersistableExchange& exchange,
                    const PersistableQueue& queue,
                    const std::string& key,
                    const qpid::framing::FieldTable& args)
{
    DatabaseConnection *db = initConnection();
    BindingRecordset rsBindings;
    try {
        db->beginTransaction();
        rsBindings.open(db, TblBinding);
        rsBindings.add(exchange.getPersistenceId(),
                       queue.getPersistenceId(),
                       key,
                       args);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error binding exchange " + exchange.getName() +
                           " to queue " + queue.getName(), e);
    }
}

/**
 * Forget a binding
 */
void
MSSqlProvider::unbind(const PersistableExchange& exchange,
                      const PersistableQueue& queue,
                      const std::string& key,
                      const qpid::framing::FieldTable& args)
{
    DatabaseConnection *db = initConnection();
    BindingRecordset rsBindings;
    try {
        db->beginTransaction();
        rsBindings.open(db, TblBinding);
        rsBindings.remove(exchange.getPersistenceId(),
                          queue.getPersistenceId(),
                          key,
                          args);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error unbinding exchange " + exchange.getName() +
                           " from queue " + queue.getName(), e);
    }
}

/**
 * Record generic durable configuration
 */
void
MSSqlProvider::create(const PersistableConfig& config)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsConfigs;
    try {
        db->beginTransaction();
        rsConfigs.open(db, TblConfig);
        rsConfigs.add(config);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error creating config " + config.getName(), e);
    }
}

/**
 * Destroy generic durable configuration
 */
void
MSSqlProvider::destroy(const PersistableConfig& config)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsConfigs;
    try {
        db->beginTransaction();
        rsConfigs.open(db, TblConfig);
        rsConfigs.remove(config);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error deleting config " + config.getName(), e);
    }
}

/**
 * Stores a messages before it has been enqueued
 * (enqueueing automatically stores the message so this is
 * only required if storage is required prior to that
 * point). If the message has not yet been stored it will
 * store the headers as well as any content passed in. A
 * persistence id will be set on the message which can be
 * used to load the content or to append to it.
 */
void
MSSqlProvider::stage(const boost::intrusive_ptr<PersistableMessage>& msg)
{
    DatabaseConnection *db = initConnection();
    MessageRecordset rsMessages;
    try {
        db->beginTransaction();
        rsMessages.open(db, TblMessage);
        rsMessages.add(msg);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error staging message", e);
    }  
}

/**
 * Destroys a previously staged message. This only needs
 * to be called if the message is never enqueued. (Once
 * enqueued, deletion will be automatic when the message
 * is dequeued from all queues it was enqueued onto).
 */
void
MSSqlProvider::destroy(PersistableMessage& msg)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsMessages;
    try {
        db->beginTransaction();
        rsMessages.open(db, TblMessage);
        rsMessages.remove(msg);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error deleting message", e);
    }
}

/**
 * Appends content to a previously staged message
 */
void
MSSqlProvider::appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg,
                             const std::string& data)
{
    DatabaseConnection *db = initConnection();
    MessageRecordset rsMessages;
    try {
        db->beginTransaction();
        rsMessages.open(db, TblMessage);
        rsMessages.append(msg, data);
        db->commitTransaction();
    }
    catch(_com_error &e) {
        db->rollbackTransaction();
        throw ADOException("Error appending to message", e);
    }  
}

/**
 * Loads (a section) of content data for the specified
 * message (previously stored through a call to stage or
 * enqueue) into data. The offset refers to the content
 * only (i.e. an offset of 0 implies that the start of the
 * content should be loaded, not the headers or related
 * meta-data).
 */
void
MSSqlProvider::loadContent(const qpid::broker::PersistableQueue& /*queue*/,
                           const boost::intrusive_ptr<const PersistableMessage>& msg,
                           std::string& data,
                           uint64_t offset,
                           uint32_t length)
{
    // SQL store keeps all messages in one table, so we don't need the
    // queue reference.
    DatabaseConnection *db = initConnection();
    MessageRecordset rsMessages;
    try {
        rsMessages.open(db, TblMessage);
        rsMessages.loadContent(msg, data, offset, length);
    }
    catch(_com_error &e) {
        throw ADOException("Error loading message content", e);
    }  
}

/**
 * Enqueues a message, storing the message if it has not
 * been previously stored and recording that the given
 * message is on the given queue.
 *
 * @param ctxt The transaction context under which this enqueue happens.
 * @param msg The message to enqueue
 * @param queue the name of the queue onto which it is to be enqueued
 */
void
MSSqlProvider::enqueue(qpid::broker::TransactionContext* ctxt,
                       const boost::intrusive_ptr<PersistableMessage>& msg,
                       const PersistableQueue& queue)
{
    // If this enqueue is in the context of a transaction, use the specified
    // transaction to nest a new transaction for this operation. However, if
    // this is not in the context of a transaction, then just use the thread's
    // DatabaseConnection with a ADO transaction.
    DatabaseConnection *db = 0;
    AmqpTransaction *atxn = dynamic_cast<AmqpTransaction*> (ctxt);
    if (atxn == 0) {
        db = initConnection();
        db->beginTransaction();
    }
    else {
        (void)initState();     // Ensure this thread is initialized
        db = atxn->dbConn();
        try {
          atxn->begin();
        }
        catch(_com_error &e) {
            throw ADOException("Error queuing message", e);
        }
    }

    MessageRecordset rsMessages;
    MessageMapRecordset rsMap;
    try {
        if (msg->getPersistenceId() == 0) {    // Message itself not yet saved
            rsMessages.open(db, TblMessage);
            rsMessages.add(msg);
        }
        rsMap.open(db, TblMessageMap);
        rsMap.add(msg->getPersistenceId(), queue.getPersistenceId());
        if (atxn)
            atxn->commit();
        else
            db->commitTransaction();
    }
    catch(_com_error &e) {
        if (atxn)
            atxn->abort();
        else
            db->rollbackTransaction();
        throw ADOException("Error queuing message", e);
    }
    msg->enqueueComplete();
}

/**
 * Dequeues a message, recording that the given message is
 * no longer on the given queue and deleting the message
 * if it is no longer on any other queue.
 *
 * @param ctxt The transaction context under which this dequeue happens.
 * @param msg The message to dequeue
 * @param queue The queue from which it is to be dequeued
 */
void
MSSqlProvider::dequeue(qpid::broker::TransactionContext* ctxt,
                       const boost::intrusive_ptr<PersistableMessage>& msg,
                       const PersistableQueue& queue)
{
    // If this dequeue is in the context of a transaction, use the specified
    // transaction to nest a new transaction for this operation. However, if
    // this is not in the context of a transaction, then just use the thread's
    // DatabaseConnection with a ADO transaction.
    DatabaseConnection *db = 0;
    AmqpTransaction *atxn = dynamic_cast<AmqpTransaction*> (ctxt);
    if (atxn == 0) {
        db = initConnection();
        db->beginTransaction();
    }
    else {
        (void)initState();     // Ensure this thread is initialized
        db = atxn->dbConn();
        try {
            atxn->begin();
        }
        catch(_com_error &e) {
            throw ADOException("Error queuing message", e);
        }
    }

    MessageMapRecordset rsMap;
    MessageRecordset rsMessages;
    try {
        rsMap.open(db, TblMessageMap);
        bool more = rsMap.remove(msg->getPersistenceId(),
                                 queue.getPersistenceId());
        if (!more) {
            rsMessages.open(db, TblMessage);
            rsMessages.remove(msg);
        }
        if (atxn)
            atxn->commit();
        else
            db->commitTransaction();
    }
    catch(_com_error &e) {
        if (atxn)
            atxn->abort();
        else
            db->rollbackTransaction();
        throw ADOException("Error dequeuing message", e);
    }  
    msg->dequeueComplete();
}

std::auto_ptr<qpid::broker::TransactionContext>
MSSqlProvider::begin()
{
    (void)initState();     // Ensure this thread is initialized

    // Transactions are associated with the Connection, so this transaction
    // context needs its own connection. At the time of writing, single-phase
    // transactions are dealt with completely on one thread, so we really
    // could just use the thread-specific DatabaseConnection for this.
    // However, that would introduce an ugly, hidden coupling, so play
    // it safe and handle this just like a TPC transaction, which actually
    // can be prepared and committed/aborted from different threads,
    // making it a bad idea to try using the thread-local DatabaseConnection.
    std::auto_ptr<DatabaseConnection> db(new DatabaseConnection);
    db->open(options.connectString, options.catalogName);
    std::auto_ptr<AmqpTransaction> tx(new AmqpTransaction(db));
    tx->begin();
    std::auto_ptr<qpid::broker::TransactionContext> tc(tx);
    return tc;
}

std::auto_ptr<qpid::broker::TPCTransactionContext>
MSSqlProvider::begin(const std::string& xid)
{
    (void)initState();     // Ensure this thread is initialized
    std::auto_ptr<DatabaseConnection> db(new DatabaseConnection);
    db->open(options.connectString, options.catalogName);
    std::auto_ptr<AmqpTPCTransaction> tx(new AmqpTPCTransaction(db, xid));
    tx->begin();
    std::auto_ptr<qpid::broker::TPCTransactionContext> tc(tx);
    return tc;
}

void
MSSqlProvider::prepare(qpid::broker::TPCTransactionContext& txn)
{
    // The inner transactions used for the components of the TPC are done;
    // nothing else to do but wait for the commit.
}

void
MSSqlProvider::commit(qpid::broker::TransactionContext& txn)
{
    (void)initState();     // Ensure this thread is initialized
    AmqpTransaction *atxn = dynamic_cast<AmqpTransaction*> (&txn);
    if (atxn == 0)
        throw qpid::broker::InvalidTransactionContextException();
    atxn->commit();
}

void
MSSqlProvider::abort(qpid::broker::TransactionContext& txn)
{
    (void)initState();     // Ensure this thread is initialized
    AmqpTransaction *atxn = dynamic_cast<AmqpTransaction*> (&txn);
    if (atxn == 0)
        throw qpid::broker::InvalidTransactionContextException();
    atxn->abort();
}

// @TODO Much of this recovery code is way too similar... refactor to
// a recover template method on BlobRecordset.

void
MSSqlProvider::recoverConfigs(qpid::broker::RecoveryManager& recoverer)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsConfigs;
    rsConfigs.open(db, TblConfig);
    _RecordsetPtr p = (_RecordsetPtr)rsConfigs;
    if (p->BOF && p->EndOfFile)
        return;   // Nothing to do
    p->MoveFirst();
    while (!p->EndOfFile) {
        uint64_t id = p->Fields->Item["persistenceId"]->Value;
        long blobSize = p->Fields->Item["fieldTableBlob"]->ActualSize;
        BlobAdapter blob(blobSize);
        blob = p->Fields->Item["fieldTableBlob"]->GetChunk(blobSize);
        // Recreate the Config instance and reset its ID.
        broker::RecoverableConfig::shared_ptr config =
            recoverer.recoverConfig(blob);
        config->setPersistenceId(id);
        p->MoveNext();
    }
}

void
MSSqlProvider::recoverExchanges(qpid::broker::RecoveryManager& recoverer,
                                ExchangeMap& exchangeMap)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsExchanges;
    rsExchanges.open(db, TblExchange);
    _RecordsetPtr p = (_RecordsetPtr)rsExchanges;
    if (p->BOF && p->EndOfFile)
        return;   // Nothing to do
    p->MoveFirst();
    while (!p->EndOfFile) {
        uint64_t id = p->Fields->Item["persistenceId"]->Value;
        long blobSize = p->Fields->Item["fieldTableBlob"]->ActualSize;
        BlobAdapter blob(blobSize);
        blob = p->Fields->Item["fieldTableBlob"]->GetChunk(blobSize);
        // Recreate the Exchange instance, reset its ID, and remember the
        // ones restored for matching up when recovering bindings.
        broker::RecoverableExchange::shared_ptr exchange =
            recoverer.recoverExchange(blob);
        exchange->setPersistenceId(id);
        exchangeMap[id] = exchange;
        p->MoveNext();
    }
}

void
MSSqlProvider::recoverQueues(qpid::broker::RecoveryManager& recoverer,
                             QueueMap& queueMap)
{
    DatabaseConnection *db = initConnection();
    BlobRecordset rsQueues;
    rsQueues.open(db, TblQueue);
    _RecordsetPtr p = (_RecordsetPtr)rsQueues;
    if (p->BOF && p->EndOfFile)
        return;   // Nothing to do
    p->MoveFirst();
    while (!p->EndOfFile) {
        uint64_t id = p->Fields->Item["persistenceId"]->Value;
        long blobSize = p->Fields->Item["fieldTableBlob"]->ActualSize;
        BlobAdapter blob(blobSize);
        blob = p->Fields->Item["fieldTableBlob"]->GetChunk(blobSize);
        // Recreate the Queue instance and reset its ID.
        broker::RecoverableQueue::shared_ptr queue =
            recoverer.recoverQueue(blob);
        queue->setPersistenceId(id);
        queueMap[id] = queue;
        p->MoveNext();
    }
}

void
MSSqlProvider::recoverBindings(qpid::broker::RecoveryManager& recoverer,
                               const ExchangeMap& exchangeMap,
                               const QueueMap& queueMap)
{
    DatabaseConnection *db = initConnection();
    BindingRecordset rsBindings;
    rsBindings.open(db, TblBinding);
    rsBindings.recover(recoverer, exchangeMap, queueMap);
}

void
MSSqlProvider::recoverMessages(qpid::broker::RecoveryManager& recoverer,
                               MessageMap& messageMap,
                               MessageQueueMap& messageQueueMap)
{
    DatabaseConnection *db = initConnection();
    MessageRecordset rsMessages;
    rsMessages.open(db, TblMessage);
    rsMessages.recover(recoverer, messageMap);

    MessageMapRecordset rsMessageMaps;
    rsMessageMaps.open(db, TblMessageMap);
    rsMessageMaps.recover(messageQueueMap);
}

////////////// Internal Methods

State *
MSSqlProvider::initState()
{
    State *state = dbState.get();   // See if thread has initialized
    if (!state) {
        state = new State;
        dbState.reset(state);
    }
    return state;
}
  
DatabaseConnection *
MSSqlProvider::initConnection(void)
{
    State *state = initState();
    if (state->dbConn != 0)
        return state->dbConn;    // And the DatabaseConnection is set up too
    std::auto_ptr<DatabaseConnection> db(new DatabaseConnection);
    db->open(options.connectString, options.catalogName);
    state->dbConn = db.release();
    return state->dbConn;
}

void
MSSqlProvider::createDb(_ConnectionPtr conn, const std::string &name)
{
    const std::string dbCmd = "CREATE DATABASE " + name;
    const std::string useCmd = "USE " + name;
    const std::string tableCmd = "CREATE TABLE ";
    const std::string colSpecs =
        " (persistenceId bigint PRIMARY KEY NOT NULL IDENTITY(1,1),"
        "  fieldTableBlob varbinary(MAX) NOT NULL)";
    const std::string bindingSpecs =
        " (exchangeId bigint REFERENCES tblExchange(persistenceId) NOT NULL,"
        "  queueId bigint REFERENCES tblQueue(persistenceId) NOT NULL,"
        "  routingKey varchar(255),"
        "  fieldTableBlob varbinary(MAX))";
    const std::string messageMapSpecs =
        " (messageId bigint REFERENCES tblMessage(persistenceId) NOT NULL,"
        "  queueId bigint REFERENCES tblQueue(persistenceId) NOT NULL)";
    _variant_t unused;
    _bstr_t dbStr = dbCmd.c_str();
    try {
        conn->Execute(dbStr, &unused, adExecuteNoRecords);
        _bstr_t useStr = useCmd.c_str();
        conn->Execute(useStr, &unused, adExecuteNoRecords);
        std::string makeTable = tableCmd + TblQueue + colSpecs;
        _bstr_t makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
        makeTable = tableCmd + TblExchange + colSpecs;
        makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
        makeTable = tableCmd + TblConfig + colSpecs;
        makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
        makeTable = tableCmd + TblMessage + colSpecs;
        makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
        makeTable = tableCmd + TblBinding + bindingSpecs;
        makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
        makeTable = tableCmd + TblMessageMap + messageMapSpecs;
        makeTableStr = makeTable.c_str();
        conn->Execute(makeTableStr, &unused, adExecuteNoRecords);
    }
    catch(_com_error &e) {
        throw ADOException("MSSQL can't create " + name, e);
    }
}

void
MSSqlProvider::dump()
{
  // dump all db records to qpid_log
  QPID_LOG(notice, "DB Dump: (not dumping anything)");
  //  rsQueues.dump();
}


}}} // namespace qpid::store::ms_sql
