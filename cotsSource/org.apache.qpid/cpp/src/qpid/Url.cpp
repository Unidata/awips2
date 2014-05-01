/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/Url.h"
#include "qpid/Exception.h"
#include "qpid/Msg.h"
#include "qpid/sys/SystemInfo.h"
#include "qpid/sys/StrError.h"

#include <boost/lexical_cast.hpp>

#include <algorithm>

#include <string.h>

using namespace std;
using boost::lexical_cast;

namespace qpid {

Url::Invalid::Invalid(const string& s) : Exception(s) {}

Url Url::getHostNameUrl(uint16_t port) {
    TcpAddress address(std::string(), port);
    if (!sys::SystemInfo::getLocalHostname(address))
        throw Url::Invalid(QPID_MSG("Cannot get host name: " << qpid::sys::strError(errno)));
    return Url(address);
}

Url Url::getIpAddressesUrl(uint16_t port) {
    Url url;
    sys::SystemInfo::getLocalIpAddresses(port, url);
    return url;
}

string Url::str() const {
    if (cache.empty() && !this->empty()) {
        ostringstream os;
        os << *this;
        cache = os.str();
    }
    return cache;
}

ostream& operator<<(ostream& os, const Url& url) {
    Url::const_iterator i = url.begin();
    os << "amqp:";
    if (i!=url.end()) {
        os << *i++;
        while (i != url.end()) 
            os << "," << *i++;
    }
    return os;
}


/** Simple recursive-descent parser for url grammar in AMQP 0-10 spec:

        amqp_url          = "amqp:" prot_addr_list
        prot_addr_list    = [prot_addr ","]* prot_addr
        prot_addr         = tcp_prot_addr | tls_prot_addr

        tcp_prot_addr     = tcp_id tcp_addr
        tcp_id            = "tcp:" | ""
        tcp_addr          = [host [":" port] ]
        host              = <as per http://www.ietf.org/rfc/rfc3986.txt>
        port              = number]]>
*/
class UrlParser {
  public:
    UrlParser(Url& u, const char* s) : url(u), text(s), end(s+strlen(s)), i(s) {}
    bool parse() { return literal("amqp:") && list(&UrlParser::protAddr, &UrlParser::comma) && i == end; }

  private:
    typedef bool (UrlParser::*Rule)();

    bool comma() { return literal(","); }

    //  NOTE: tcpAddr must be last since it is allowed to omit it's tcp: tag.
    bool protAddr() { return exampleAddr() || tcpAddr(); }

    bool tcpAddr() {
        TcpAddress addr;
        literal("tcp:");        // Don't check result, allowed to be absent.
        return addIf(host(addr.host) && (literal(":") ? port(addr.port) : true), addr);
    }
    
    // Placeholder address type till we have multiple address types. Address is a single char.
    bool exampleAddr () {
        if (literal("example:") && i < end) {
            ExampleAddress ex(*i++);
            url.push_back(ex);
            return true;
        }
        return false;
    }

    // FIXME aconway 2008-11-20: this does not implement http://www.ietf.org/rfc/rfc3986.txt.
    // Works for DNS names and ipv4 literals but won't handle ipv6.
    bool host(string& h) {
        const char* start=i;
        while (unreserved() || pctEncoded())
            ;
        if (start == i) h = LOCALHOST; // Default
        else h.assign(start, i);
        return true;
    }

    bool unreserved() { return (::isalnum(*i) || ::strchr("-._~", *i)) && advance(); }

    bool pctEncoded() { return literal("%") && hexDigit() && hexDigit(); }

    bool hexDigit() { return i < end && ::strchr("01234567890abcdefABCDEF", *i) && advance(); }
    
    bool port(uint16_t& p) { return decimalInt(p); }

    template <class AddrType> bool addIf(bool ok, const AddrType& addr) { if (ok) url.push_back(addr); return ok; }
    
    template <class IntType> bool decimalInt(IntType& n) {
        const char* start = i;
        while (decDigit())
            ;
        try {
            n = lexical_cast<IntType>(string(start, i)); 
            return true;
        } catch(...) { return false; }
    }

    bool decDigit() { return i < end && ::isdigit(*i) && advance(); }

    bool literal(const char* s) {
        int n = ::strlen(s);
        if (n <= end-i && equal(s, s+n, i)) return advance(n);
        return false;
    };

    bool noop() { return true; }

    /** List of item, separated by separator, with min & max bounds. */
    bool list(Rule item, Rule separator, size_t min=0, size_t max=UNLIMITED) {
        assert(max > 0);
        assert(max >= min);
        if (!(this->*item)()) return min == 0; // Empty list.
        size_t n = 1;
        while (n < max && i < end) {
            if (!(this->*separator)()) break;
            if (i == end || !(this->*item)()) return false; // Separator with no item.
            ++n;
        }
        return n >= min;
    }

    /** List of items with no separator */
    bool list(Rule item, size_t min=0, size_t max=UNLIMITED) { return list(item, &UrlParser::noop, min, max); }

    bool advance(size_t n=1) {
        if (i+n > end) return false;
        i += n;
        return true;
    }

    static const size_t UNLIMITED = size_t(~1);
    static const std::string LOCALHOST;

    Url& url;
    const char* text;
    const char* end;
    const char* i;
};

const string UrlParser::LOCALHOST("127.0.0.1");

void Url::parse(const char* url) {
    parseNoThrow(url);
    if (empty())
        throw Url::Invalid(QPID_MSG("Invalid URL: " << url));
}

void Url::parseNoThrow(const char* url) {
    cache.clear();
    if (!UrlParser(*this, url).parse())
        clear();
}

void Url::throwIfEmpty() const {
    if (empty())
        throw Url::Invalid("URL contains no addresses");
}

std::istream& operator>>(std::istream& is, Url& url) {
    std::string s;
    is >> s;
    url.parse(s);
    return is;
}

} // namespace qpid
