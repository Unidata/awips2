#ifndef QPID_CLUSTER_CLUSTERMAP_H
#define QPID_CLUSTER_CLUSTERMAP_H

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

#include "MemberSet.h"
#include "qpid/Url.h"
#include "qpid/framing/ClusterConnectionMembershipBody.h"
#include "qpid/framing/SequenceNumber.h"

#include <boost/function.hpp>
#include <boost/optional.hpp>

#include <vector>
#include <deque>
#include <map>
#include <iosfwd>

namespace qpid {
namespace cluster {

/**
 * Map of established cluster members and joiners waiting for an update,
 * along with other cluster state that must be updated.
 */
class ClusterMap {
  public:
    typedef std::map<MemberId, Url> Map;
    typedef std::set<MemberId> Set;

    ClusterMap();
    ClusterMap(const Map& map);
    ClusterMap(const framing::FieldTable& joiners, const framing::FieldTable& members, framing::SequenceNumber frameSeq);

    /** Update from config change.
     *@return true if member set changed.
     */
    bool configChange(const Set& members);

    bool isJoiner(const MemberId& id) const { return joiners.find(id) != joiners.end(); }
    bool isMember(const MemberId& id) const { return members.find(id) != members.end(); }
    bool isAlive(const MemberId& id) const { return alive.find(id) != alive.end(); }
    
    Url getJoinerUrl(const MemberId& id) { return getUrl(joiners, id); }
    Url getMemberUrl(const MemberId& id) { return getUrl(members, id); }

    /** First joiner in the cluster in ID order, target for offers */
    MemberId firstJoiner() const;

    /** Convert map contents to a cluster control body. */
    void toMethodBody(framing::ClusterConnectionMembershipBody&) const;

    size_t aliveCount() const { return alive.size(); }
    size_t memberCount() const { return members.size(); }
    std::vector<std::string> memberIds() const;
    std::vector<Url> memberUrls() const;
    Set getAlive() const;
    Set getMembers() const;

    bool updateRequest(const MemberId& id, const std::string& url);       
    /** Return non-empty Url if accepted */
    boost::optional<Url> updateOffer(const MemberId& from, const MemberId& to);

    /**@return true If this is a new member */ 
    bool ready(const MemberId& id, const Url&);

    framing::SequenceNumber getFrameSeq() { return frameSeq; }
    framing::SequenceNumber incrementFrameSeq() { return ++frameSeq; }
    
    /** Clear out all knowledge of joiners & members, just keep alive set */
    void clearStatus() { joiners.clear(); members.clear(); }
    
  private:
    Url getUrl(const Map& map, const  MemberId& id);
    
    Map joiners, members;
    Set alive;
    framing::SequenceNumber frameSeq;

  friend std::ostream& operator<<(std::ostream&, const Map&);
  friend std::ostream& operator<<(std::ostream&, const ClusterMap&);
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_CLUSTERMAP_H*/
