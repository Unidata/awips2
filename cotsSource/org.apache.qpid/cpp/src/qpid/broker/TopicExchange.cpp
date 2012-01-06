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
#include "qpid/broker/TopicExchange.h"
#include <algorithm>


namespace qpid {
namespace broker {

using namespace qpid::framing;
using namespace qpid::sys;
using namespace std;
namespace _qmf = qmf::org::apache::qpid::broker;


// TODO aconway 2006-09-20: More efficient matching algorithm.
// Areas for improvement:
// - excessive string copying: should be 0 copy, match from original buffer.
// - match/lookup: use descision tree or other more efficient structure.

namespace 
{
const std::string qpidFedOp("qpid.fed.op");
const std::string qpidFedTags("qpid.fed.tags");
const std::string qpidFedOrigin("qpid.fed.origin");

const std::string fedOpBind("B");
const std::string fedOpUnbind("U");
const std::string fedOpReorigin("R");
const std::string fedOpHello("H");
}


namespace {
// Iterate over a string of '.'-separated tokens.
struct TokenIterator {
    typedef pair<const char*,const char*> Token;
    
    TokenIterator(const char* b, const char* e) : token(make_pair(b, find(b,e,'.'))), end(e) {}

    bool finished() const { return !token.first; }

    void next() {
        if (token.second == end)
            token.first = token.second = 0;
        else {
            token.first=token.second+1;
            token.second=(find(token.first, end, '.'));
        }
    }

    bool match1(char c) const {
        return token.second==token.first+1 && *token.first == c;
    }

    bool match(const Token& token2) {
        ptrdiff_t l=len();
        return l == token2.second-token2.first &&
            strncmp(token.first, token2.first, l) == 0;
    }

    ptrdiff_t len() const { return token.second - token.first; }

    Token token;
    const char* end;
};

class Normalizer : public TokenIterator {
  public:
    Normalizer(string& p)
        : TokenIterator(&p[0], &p[0]+p.size()), pattern(p)
    { normalize(); }

  private:
    // Apply 2 transformations:  #.*  -> *.# and #.# -> #
    void normalize() {
        while (!finished()) {
            if (match1('#')) {
                const char* hash1=token.first;
                next();
                if (!finished()) {
                    if (match1('#')) { // Erase #.# -> #
                        pattern.erase(hash1-pattern.data(), 2);
                        token.first -= 2;
                        token.second -= 2;
                        end -= 2;
                    }
                    else if (match1('*'))  { // Swap #.* -> *.#
                        swap(*const_cast<char*>(hash1),
                             *const_cast<char*>(token.first));
                    }
                }
            }
            else
                next();
        }
    }

    string& pattern;
};

class Matcher {
  public:
    Matcher(const string& p, const string& k)
        : matched(), pattern(&p[0], &p[0]+p.size()), key(&k[0], &k[0]+k.size())
    { matched = match(); }
    
    operator bool() const { return matched; }

  private:
    Matcher(const char* bp, const char* ep, const char* bk, const char* ek)
        : matched(), pattern(bp,ep), key(bk,ek) { matched = match(); }

    bool match() {
        // Invariant: pattern and key match up to but excluding
        // pattern.token and key.token
        while (!pattern.finished() && !key.finished()) {
            if (pattern.match1('*') && !key.finished()) {
                pattern.next();
                key.next();
            }
            else if (pattern.match1('#')) {
                pattern.next();
                if (pattern.finished()) return true; // Trailing # matches anything.
                while (!key.finished()) {
                    if (Matcher(pattern.token.first, pattern.end,
                                key.token.first, key.end))
                        return true;
                    key.next();
                }
                return false;
            }
            else if (pattern.len() == key.len() &&
                     equal(pattern.token.first,pattern.token.second,key.token.first)) {
                pattern.next();
                key.next();
            }
            else
                return false;
        }
        if (!pattern.finished() && pattern.match1('#'))
            pattern.next();     // Trailing # matches empty.
        return pattern.finished() && key.finished(); 
    }

    bool matched;
    TokenIterator pattern, key;
};
}

// Convert sequences of * and # to a sequence of * followed by a single #
string TopicExchange::normalize(const string& pattern) {
    string normal(pattern);
    Normalizer n(normal);
    return normal;
}

bool TopicExchange::match(const string& pattern, const string& key)  
{
    return Matcher(pattern, key);
}

TopicExchange::TopicExchange(const string& _name, Manageable* _parent, Broker* b) : Exchange(_name, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}

TopicExchange::TopicExchange(const std::string& _name, bool _durable,
                             const FieldTable& _args, Manageable* _parent, Broker* b) :
    Exchange(_name, _durable, _args, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}

bool TopicExchange::bind(Queue::shared_ptr queue, const string& routingKey, const FieldTable* args)
{
    string fedOp(args ? args->getAsString(qpidFedOp) : fedOpBind);
    string fedTags(args ? args->getAsString(qpidFedTags) : "");
    string fedOrigin(args ? args->getAsString(qpidFedOrigin) : "");
    bool propagate = false;
    bool reallyUnbind;
    string routingPattern = normalize(routingKey);

    if (args == 0 || fedOp.empty() || fedOp == fedOpBind) {
        RWlock::ScopedWlock l(lock);
        if (isBound(queue, routingPattern)) {
            return false;
        } else {
            Binding::shared_ptr binding (new Binding (routingPattern, queue, this, FieldTable(), fedOrigin));
            BoundKey& bk = bindings[routingPattern];
            bk.bindingVector.push_back(binding);
            propagate = bk.fedBinding.addOrigin(fedOrigin);
            if (mgmtExchange != 0) {
                mgmtExchange->inc_bindingCount();
            }
        }
    } else if (fedOp == fedOpUnbind) {
        {
            RWlock::ScopedWlock l(lock);
            BoundKey& bk = bindings[routingPattern];
            propagate = bk.fedBinding.delOrigin(fedOrigin);
            reallyUnbind = bk.fedBinding.count() == 0;
        }
        if (reallyUnbind)
            unbind(queue, routingPattern, 0);
    } else if (fedOp == fedOpReorigin) {
        /** gather up all the keys that need rebinding in a local vector
         * while holding the lock.  Then propagate once the lock is
         * released
         */
        std::vector<std::string> keys2prop;
        {
            RWlock::ScopedRlock l(lock);    
            for (BindingMap::iterator iter = bindings.begin();
                 iter != bindings.end(); iter++) {
                const BoundKey& bk = iter->second;
                
                if (bk.fedBinding.hasLocal()) {
                    keys2prop.push_back(iter->first);
                }
            }
        }   /* lock dropped */
        for (std::vector<std::string>::const_iterator key = keys2prop.begin();
             key != keys2prop.end(); key++) {
            propagateFedOp( *key, string(), fedOpBind, string());
        }
    }

    routeIVE();
    if (propagate)
        propagateFedOp(routingKey, fedTags, fedOp, fedOrigin);
    return true;
}

bool TopicExchange::unbind(Queue::shared_ptr queue, const string& constRoutingKey, const FieldTable* /*args*/){
    RWlock::ScopedWlock l(lock);
    string routingKey = normalize(constRoutingKey);

    BindingMap::iterator bi = bindings.find(routingKey);
    if (bi == bindings.end()) return false;
    BoundKey& bk = bi->second;
    Binding::vector& qv(bk.bindingVector);
    bool propagate = false;

    Binding::vector::iterator q;
    for (q = qv.begin(); q != qv.end(); q++)
        if ((*q)->queue == queue)
            break;
    if(q == qv.end()) return false;
    qv.erase(q);
    propagate = bk.fedBinding.delOrigin();
    if(qv.empty()) bindings.erase(bi);
    if (mgmtExchange != 0) {
        mgmtExchange->dec_bindingCount();
    }

    if (propagate)
        propagateFedOp(routingKey, string(), fedOpUnbind, string());
    return true;
}

bool TopicExchange::isBound(Queue::shared_ptr queue, const string& pattern)
{
    BindingMap::iterator bi = bindings.find(pattern);
    if (bi == bindings.end()) return false;
    Binding::vector& qv(bi->second.bindingVector);
    Binding::vector::iterator q;
    for (q = qv.begin(); q != qv.end(); q++)
        if ((*q)->queue == queue)
            break;
    return q != qv.end();
}

void TopicExchange::route(Deliverable& msg, const string& routingKey, const FieldTable* /*args*/)
{
    Binding::vector mb;
    BindingList b(new std::vector<boost::shared_ptr<qpid::broker::Exchange::Binding> >);
    PreRoute pr(msg, this);
    {
        RWlock::ScopedRlock l(lock);
        for (BindingMap::iterator i = bindings.begin(); i != bindings.end(); ++i) {
            if (match(i->first, routingKey)) {
                Binding::vector& qv(i->second.bindingVector);
                for(Binding::vector::iterator j = qv.begin(); j != qv.end(); j++){
                    b->push_back(*j);
                }
            }
        }
    }
    doRoute(msg, b);
}

bool TopicExchange::isBound(Queue::shared_ptr queue, const string* const routingKey, const FieldTable* const)
{
    RWlock::ScopedRlock l(lock);
    if (routingKey && queue) {
        string key(normalize(*routingKey));
        return isBound(queue, key);
    } else if (!routingKey && !queue) {
        return bindings.size() > 0;
    } else if (routingKey) {
        for (BindingMap::iterator i = bindings.begin(); i != bindings.end(); ++i) {
            if (match(i->first, *routingKey)) 
                return true;
            }
    } else {
        for (BindingMap::iterator i = bindings.begin(); i != bindings.end(); ++i) {
            Binding::vector& qv(i->second.bindingVector);
            Binding::vector::iterator q;
            for (q = qv.begin(); q != qv.end(); q++)
                if ((*q)->queue == queue)
                    return true;
        }
    }
    return false;
    return queue && routingKey;
}

TopicExchange::~TopicExchange() {}

const std::string TopicExchange::typeName("topic");

}} // namespace qpid::broker
