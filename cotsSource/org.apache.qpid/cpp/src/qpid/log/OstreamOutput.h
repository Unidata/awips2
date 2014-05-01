#ifndef QPID_LOG_OSTREAMOUTPUT_H
#define QPID_LOG_OSTREAMOUTPUT_H

/*
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/log/Logger.h"
#include <boost/scoped_ptr.hpp>
#include <fstream>
#include <ostream>

namespace qpid {
namespace log {

/**
 * OstreamOutput is a reusable logging sink that directs logging to a C++
 * ostream.
 */
class OstreamOutput : public qpid::log::Logger::Output {
public:
    QPID_COMMON_EXTERN OstreamOutput(std::ostream& o);
    QPID_COMMON_EXTERN OstreamOutput(const std::string& file);

    virtual void log(const Statement&, const std::string& m);

private:    
    std::ostream* out;
    boost::scoped_ptr<std::ostream> mine;
};

}} // namespace qpid::log

#endif /*!QPID_LOG_OSTREAMOUTPUT_H*/
