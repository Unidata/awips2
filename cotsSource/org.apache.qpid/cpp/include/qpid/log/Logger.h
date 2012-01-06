#ifndef QPID_LOG_LOGGER_H
#define QPID_LOG_LOGGER_H

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

#include "qpid/log/Selector.h"
#include "qpid/log/Options.h"
#include "qpid/sys/Mutex.h"
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/noncopyable.hpp>
#include <set>
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace log {

/**
 * Central logging agent.
 *
 * Thread safe, singleton.
 *
 * The Logger provides all needed functionality for selecting and
 * formatting logging output. The actual outputting of log records
 * is handled by Logger::Output-derived classes instantiated by the
 * platform's sink-related options.
 */
class Logger : private boost::noncopyable {
  public:
    /** Flags indicating what to include in the log output */
    enum FormatFlag { FILE=1, LINE=2, FUNCTION=4, LEVEL=8, TIME=16, THREAD=32};

    /**
     * Logging output sink.
     *
     * The Output sink provides an interface to direct logging output to.
     * Logging sinks are primarily platform-specific as provided for on
     * each platform.
     *
     * Implementations of Output must be thread safe.
     */
    class Output {
      public:
        QPID_COMMON_EXTERN Output();
        QPID_COMMON_EXTERN virtual ~Output();
        /** Receives the statemnt of origin and formatted message to log. */
        virtual void log(const Statement&, const std::string&) =0;
    };

    QPID_COMMON_EXTERN static Logger& instance();

    QPID_COMMON_EXTERN Logger();
    QPID_COMMON_EXTERN ~Logger();

    /** Select the messages to be logged. */
    QPID_COMMON_EXTERN void select(const Selector& s);

    /** Set the formatting flags, bitwise OR of FormatFlag values. */
    QPID_COMMON_EXTERN void format(int formatFlags);

    /** Set format flags from options object.
     *@returns computed flags.
     */
    QPID_COMMON_EXTERN int format(const Options&);

    /** Configure logger from Options */
    QPID_COMMON_EXTERN void configure(const Options& o);

    /** Add a statement. */
    QPID_COMMON_EXTERN void add(Statement& s);

    /** Log a message. */
    QPID_COMMON_EXTERN void log(const Statement&, const std::string&);

    /** Add an output destination for messages */
    QPID_COMMON_EXTERN void output(std::auto_ptr<Output> out);

    /** Set a prefix for all messages */
    QPID_COMMON_EXTERN void setPrefix(const std::string& prefix);

    /** Reset the logger. */
    QPID_COMMON_EXTERN void clear();

    /** Get the options used to configure the logger. */
    QPID_COMMON_EXTERN const Options& getOptions() const { return options; }


  private:
    typedef boost::ptr_vector<Output> Outputs;
    typedef std::set<Statement*> Statements;

    sys::Mutex lock;
    inline void enable_unlocked(Statement* s);

    Statements statements;
    Outputs outputs;
    Selector selector;
    int flags;
    std::string prefix;
    Options options;
};

}} // namespace qpid::log


#endif  /*!QPID_LOG_LOGGER_H*/
