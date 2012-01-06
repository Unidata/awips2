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
#include "qpid/cluster/ClusterMap.h"
#include "qpid/Url.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/log/Statement.h"
#include <boost/bind.hpp>
#include <algorithm>
#include <functional>
#include <iterator>
#include <ostream>

using namespace std;
using namespace boost;

namespace qpid {
using namespace framing;

namespace cluster {

namespace {

void addFieldTableValue(FieldTable::ValueMap::value_type vt, ClusterMap::Map& map, ClusterMap::Set& set) {
    MemberId id(vt.first);
    set.insert(id);
    string url = vt.second->get<string>();
    if (!url.empty())
        map.insert(ClusterMap::Map::value_type(id, Url(url)));
}

void insertFieldTableFromMapValue(FieldTable& ft, const ClusterMap::Map::value_type& vt) {
    ft.setString(vt.first.str(), vt.second.str());
}

void assignFieldTable(FieldTable& ft, const ClusterMap::Map& map) {
    ft.clear();
    for_each(map.begin(), map.end(), bind(&insertFieldTableFromMapValue, ref(ft), _1));
}

}

ClusterMap::ClusterMap() : frameSeq(0) {}

ClusterMap::ClusterMap(const Map& map) : frameSeq(0) {
    transform(map.begin(), map.end(), inserter(alive, alive.begin()), bind(&Map::value_type::first, _1));
    members = map;
}

ClusterMap::ClusterMap(const FieldTable& joinersFt, const FieldTable& membersFt, framing::SequenceNumber frameSeq_)
  : frameSeq(frameSeq_)
{
    for_each(joinersFt.begin(), joinersFt.end(), bind(&addFieldTableValue, _1, ref(joiners), ref(alive)));
    for_each(membersFt.begin(), membersFt.end(), bind(&addFieldTableValue, _1, ref(members), ref(alive)));
}

void ClusterMap::toMethodBody(framing::ClusterConnectionMembershipBody& b) const {
    b.getJoiners().clear();
    for_each(joiners.begin(), joiners.end(), bind(&insertFieldTableFromMapValue, ref(b.getJoiners()), _1));
    for(Set::const_iterator i = alive.begin(); i != alive.end(); ++i) {
        if (!isMember(*i) && !isJoiner(*i))
            b.getJoiners().setString(i->str(), string());
    }
    b.getMembers().clear();
    for_each(members.begin(), members.end(), bind(&insertFieldTableFromMapValue, ref(b.getMembers()), _1));
    b.setFrameSeq(frameSeq);
}

Url ClusterMap::getUrl(const Map& map, const  MemberId& id) {
    Map::const_iterator i = map.find(id);
    return i == map.end() ? Url() : i->second;
}
     
MemberId ClusterMap::firstJoiner() const {
    return joiners.empty() ? MemberId() : joiners.begin()->first;
}

vector<string> ClusterMap::memberIds() const {
    vector<string> ids;
    for (Map::const_iterator iter = members.begin();
         iter != members.end(); iter++) {
        stringstream stream;
        stream << iter->first;
        ids.push_back(stream.str());
    }
    return ids;
}

vector<Url> ClusterMap::memberUrls() const {
    vector<Url> urls(members.size());
    transform(members.begin(), members.end(), urls.begin(),
                   bind(&Map::value_type::second, _1));
    return urls;
}

ClusterMap::Set ClusterMap::getAlive() const { return alive; }

ClusterMap::Set ClusterMap::getMembers() const {
    Set s;
    transform(members.begin(), members.end(), inserter(s, s.begin()),
                   bind(&Map::value_type::first, _1));
    return s;
}

ostream& operator<<(ostream& o, const ClusterMap::Map& m) {
    ostream_iterator<MemberId> oi(o);
    transform(m.begin(), m.end(), oi, bind(&ClusterMap::Map::value_type::first, _1));
    return o;
}

ostream& operator<<(ostream& o, const ClusterMap& m) {
    for (ClusterMap::Set::const_iterator i = m.alive.begin(); i != m.alive.end(); ++i) {
        o << *i;
        if (m.isMember(*i)) o << "(member)";
        else if (m.isJoiner(*i)) o << "(joiner)";
        else o << "(unknown)";
        o << " ";
    }
    return o;
}

bool ClusterMap::updateRequest(const MemberId& id, const string& url) {
    if (isAlive(id)) {
        joiners[id] = Url(url);
        return true;
    }
    return false;
}

bool ClusterMap::ready(const MemberId& id, const Url& url) {
    return isAlive(id) &&  members.insert(Map::value_type(id,url)).second;
}

bool ClusterMap::configChange(const Set& update) {
    bool memberChange = false;
    Set removed;
    set_difference(alive.begin(), alive.end(),
                        update.begin(), update.end(),
                        inserter(removed, removed.begin()));
    alive = update;
    for (Set::const_iterator i = removed.begin(); i != removed.end(); ++i) {
        memberChange = memberChange || members.erase(*i);
        joiners.erase(*i);
    }
    return memberChange;
}

optional<Url> ClusterMap::updateOffer(const MemberId& from, const MemberId& to) {
    Map::iterator i = joiners.find(to);
    if (isAlive(from) && i != joiners.end()) {
        Url url= i->second;
        joiners.erase(i);       // No longer a potential updatee.
        return url;
    }
    return optional<Url>();
}

}} // namespace qpid::cluster
