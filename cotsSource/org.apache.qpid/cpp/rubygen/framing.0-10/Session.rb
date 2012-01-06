#!/usr/bin/env ruby
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# Usage: output_directory xml_spec_file [xml_spec_file...]
# 
$: << '..'
require 'cppgen'

class CppGen
  def session_methods(sync_default)
    excludes = ["connection", "session", "file", "stream"]
    gen_methods=@amqp.methods_on(@chassis).reject { |m|
      excludes.include? m.parent.name or m.body_name.include?("010")
    }
    gen_methods.each { |m| m.set_sync_default(sync_default) }
  end

  
  # Generates a doxygen comment for AmqpMethod m.
  def doxygen(m)
    doxygen_comment {
      genl m.doc
      genl
      m.fields_c.each { |f|
        genl "@param #{f.cppname}"
        genl f.doc if f.doc
        genl
      }
    }
  end
end

# Sync vs. async APIs
module SyncAsync
  def sync_prefix() @async ? "Async" : ""  end
  def sync_adjective() @async ? "asynchronous" : "synchronous" end
  def sync_convert() @async ? "async" :  "sync" end


  def decl_ctor_opeq()
    genl
    genl "QPID_CLIENT_EXTERN #{@classname}();"
    genl "QPID_CLIENT_EXTERN #{@classname}(const #{@version_base}& other);"
    genl "QPID_CLIENT_EXTERN #{@classname}& operator=(const #{@version_base}& other);"
  end

  def defn_ctor_opeq(inline="")
    genl
    genl "#{inline} #{@classname}::#{@classname}() {}"
    scope("#{inline} #{@classname}::#{@classname}(const #{@version_base}& other) {") {
      genl "*this = other;"
    }
    scope("#{inline} #{@classname}& #{@classname}::operator=(const #{@version_base}& other) {") {
      genl "impl = static_cast<const #{@classname}&>(other).impl;"
      genl "return *this;"
    }
  end

  def sync_default() !@async end
end

class ContentField               # For extra content parameters
  def cppname() "content"  end
  def signature() "const Message& content" end
  def sig_default() signature+"="+"Message(std::string())" end
  def unpack() "p[arg::content|Message(std::string())]"; end
  def doc() "Message content"; end
end

class SyncField               # For extra sync parameters
  def initialize(default_value) @default_value=default_value ? "true" : "false" end
  def cppname() "sync"  end
  def signature() "bool sync" end
  def sig_default() signature+"="+@default_value end
  def unpack() "p[arg::sync|#{@default_value}]"; end
  def doc() "If true the broker will respond with completion status as soon as possible."; end
end

class AmqpField
  def unpack() "p[arg::#{cppname}|#{default_value}]"; end
  def sig_default() signature+"="+default_value; end
end

class AmqpMethod
  def set_sync_default(sync_default) @sync_default=sync_default end
  def fields_c() result = fields + (content ? [ContentField.new] : []) + [SyncField.new(@sync_default)] end
  def param_names_c() fields_c.map { |f| f.cppname} end
  def signature_c()  fields_c.map { |f| f.signature }; end
  def sig_c_default()  fields_c.map { |f| f.sig_default }; end
  def argpack_name() "#{parent.cppname}#{name.caps}Parameters"; end
  def argpack_type()
    "boost::parameter::parameters<" +
      fields_c.map { |f| "arg::keyword_tags::"+f.cppname }.join(',') +
      ">"
  end

  def return_type(async)
    if (async)
      return "TypedResult<qpid::framing::#{result.cpptype.ret_by_val}>" if (result)
      return "Completion"
    else
      return "qpid::framing::#{result.cpptype.ret_by_val}" if (result)
      return "void"
    end
  end

  def session_function() "#{parent.name.lcaps}#{name.caps}"; end
end

class SessionNoKeywordGen < CppGen
  include SyncAsync
  
  def initialize(outdir, amqp, async)
    super(outdir, amqp)
    @async=async
    @chassis="server"
    @namespace,@classname,@file=
      parse_classname "qpid::client::no_keyword::#{sync_prefix}Session_#{@amqp.version.bars}"
    @version_base="SessionBase_#{@amqp.major}_#{@amqp.minor}"
  end

  def generate()
    public_api("#{@file}.h")
    h_file(@file) {
      include "qpid/client/#{@version_base}.h"
      include "qpid/client/ClientImportExport.h"
      namespace(@namespace) { 
        doxygen_comment {
          genl "AMQP #{@amqp.version} #{sync_adjective} session API."
          genl @amqp.class_("session").doc
          # FIXME aconway 2008-05-23: additional doc on sync/async use.
        }
        cpp_class(@classname, "public #{@version_base}") {
          public
          decl_ctor_opeq()
          session_methods(sync_default).each { |m|
            genl
            doxygen(m)
            args=m.sig_c_default.join(", ") 
            genl "QPID_CLIENT_EXTERN #{m.return_type(@async)} #{m.session_function}(#{args});" 
          }
        }
      }}

    cpp_file(@file) { 
      include "qpid/client/#{@classname}"
      include "qpid/framing/all_method_bodies.h"
      include "qpid/client/SessionImpl.h"
      include "qpid/client/MessageImpl.h"
      include "qpid/client/PrivateImplRef.h"
      include "qpid/client/CompletionImpl.h"
      include "<boost/intrusive_ptr.hpp>"
      namespace(@namespace) {
        genl "using namespace framing;"
        session_methods(sync_default).each { |m|
          genl
          sig=m.signature_c.join(", ")
          func="#{@classname}::#{m.session_function}"
          scope("#{m.return_type(@async)} #{func}(#{sig}) {") {
            args=(["ProtocolVersion(#{@amqp.major},#{@amqp.minor})"]+m.param_names).join(", ")
            genl "#{m.body_name} body(#{args});";
            genl "body.setSync(sync);"
            sendargs="body"
            sendargs << ", *MessageImpl::get(content)" if m.content
            async_retval="#{m.return_type(true)}(new CompletionImpl(impl->send(#{sendargs}), impl))"
            if @async then
              genl "return #{async_retval};"
            else
              if m.result
                genl "return #{async_retval}.get();"
              else
                genl "#{async_retval}.wait();"
              end
            end
          }}
        defn_ctor_opeq()
      }}
  end
end

class SessionGen < CppGen
  include SyncAsync

  def initialize(outdir, amqp, async)
    super(outdir, amqp)
    @async=async
    @chassis="server"
    session="#{sync_prefix}Session_#{@amqp.version.bars}"
    @base="no_keyword::#{session}"
    @fqclass=FqClass.new "qpid::client::#{session}"
    @classname=@fqclass.name
    @fqbase=FqClass.new("qpid::client::#{@base}")
    @version_base="SessionBase_#{@amqp.major}_#{@amqp.minor}"
  end

  def gen_keyword_decl(m)
    return if m.fields_c.empty?     # Inherited function will do.
    scope("BOOST_PARAMETER_MEMFUN(#{m.return_type(@async)}, #{m.session_function}, 0, #{m.fields_c.size}, #{m.argpack_name}) {") {
      scope("return #{@base}::#{m.session_function}(",");") {
        gen m.fields_c.map { |f| f.unpack() }.join(",\n")
      }
    }
    genl
  end

  def generate()
    keyword_methods=session_methods(sync_default).reject { |m| m.fields_c.empty? }
    max_arity = keyword_methods.map{ |m| m.fields_c.size }.max

    public_api("qpid/client/arg.h")
    h_file("qpid/client/arg.h") {
      # Generate keyword tag declarations.
      genl "#define BOOST_PARAMETER_MAX_ARITY #{max_arity}"
      include "<boost/parameter.hpp>"
      namespace("qpid::client::arg") { 
        keyword_methods.map{ |m| m.param_names_c }.flatten.uniq.each { |k|
          genl "BOOST_PARAMETER_KEYWORD(keyword_tags, #{k})"
        }}
    }    
    public_api("#{@fqclass.file}.h")
    h_file(@fqclass.file) {
      include @fqbase.file
      include "qpid/client/arg"
      include "qpid/client/ClientImportExport"
      namespace("qpid::client") {
        # Doxygen comment.
        doxygen_comment {
          genl "AMQP #{@amqp.version} session API with keyword arguments."
          genl <<EOS
This class provides the same set of functions as #{@base}, but also
allows parameters be passed using keywords. The keyword is the
parameter name in the namespace "arg".

For example given the normal function "foo(int x=0, int y=0, int z=0)"
you could call it in either of the following ways:

@code
session.foo(1,2,3);             // Normal no keywords
session.foo(arg::z=3, arg::x=1); // Keywords and a default
@endcode

The keyword functions are easy to use but their declarations are hard
to read. You may find it easier to read the documentation for #{@base}
which provides the same set of functions using normal non-keyword
declarations.

\\ingroup clientapi


\\details 

<h2>Publishing Messages</h2>
<ul>
<li><p>messageTransfer()</p>
<pre>session.messageTransfer(arg::content=message, arg::destination="amq.topic");</pre></li>
<li><p>messageTransfer() &mdash; asynchronous</p>
<pre>#include &lt;qpid/client/AsyncSession.h>

for (int i=0; i&lt;10; i++) {
    message.setData(message_data.str());
    async(session).messageTransfer(arg::content=message,  arg::destination="amq.direct");        
}

session.sync();
</pre>
</li>
</ul>

<h2>Exchanges</h2>
<ul>
<li><p>exchangeBind()</p>
<pre>session.exchangeBind(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=routing_key);</pre>
</li>
<li><p>exchangeUnbind()</p>
<pre>session.exchangeUnBind(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=routing_key);</pre></li>
<li><p>exchangeBound()</p>
<pre>if (session.exchangeBound(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=rk)){...}</pre>
<pre>if (session.exchangeBound(arg::exchange="amq.topic", arg::queue=queue)){...}</pre>
</li>
<li><p>exchangeDeclare()</p>
<pre>session.exchangeDeclare(arg::exchange="my.topic", arg::type="topic");</pre>
<pre>session.exchangeDeclare(arg::exchange="xml", arg::type="xml");</pre>
</li>
<li><p>exchangeDelete()</p>
<pre>session.exchangeDeclare(arg::exchange="my.topic");</pre>
<pre>session.exchangeDeclare(arg::exchange="xml", arg::ifUnused=true);</pre>
</li>
<li><p>exchangeQuery()</p>
<pre>ExchangeQueryResult eqr = session.exchangeQuery(arg::exchange="my.topic");</pre></li>
</ul>


<h2>Configuring exchanges in session.exchangeDeclare</h2>

<pre>arg::durable=true</pre>
<p>Default: false.</p>
<p>If durable=true, an exchange remains active even if the server is restarted. If durable=false, an exchange is purged when a server restarts.</p>

<pre>arg::autoDelete=true</pre>
<p>Default: false.</p>
<p>If autoDelete=true, deleting the last binding for an exchange also deletes the exchange.</p>

<pre>arg::alternatExchange="my.exchange"</pre>
<p>Default: none.</p>
<p>If an alternate exchange is specified, messages that can not be delivered to any queue are sent to the alternate exchange.</p>

<h2>Queues</h2>
<ul>
<li><p>queueDeclare()</p>
<pre>session.queueDeclare(arg::queue="message_queue");</pre>
</li>
<li><p>queueDelete()</p>
<pre>session.queueDelete(arg::queue="message_queue");</pre></li>
<li><p>queuePurge()</p>
<pre>session.queuePurge(arg::queue="message_queue");</pre></li>
<li><p>queueQuery()</p>
<pre>QueueQueryResult qqr = session.queueQuery(arg::queue="message_queue");</pre></li>
</ul>


<h2>Configuring queues with session.queueDeclare</h2>
<pre>arg::durable=true</pre>
<p>Default: false.</p>
<p>If durable=true, a queue remains active if the server is restarted. If durable=false, a queue and its contents are lost when a server restarts.</p>
<br/>

<pre>arg::autoDelete=true</pre>
<p>Default: false.</p>
<p>If autoDelete=true, the queue is deleted when the last active Subscription to the Queue is canceled.</p>
<br/>

<pre>arg::exclusive=true</pre>
<p>Default: false.</p>
<p>If exclusive=true, only the Session that created a queue can access it.</p>
<br/>

<pre>arg::alternateExchange="my.exchange"</pre>
<p>Default: none. </p>
<p>If an alternate exchange is specified, messages are routed to it if (1) they are rejected by a client, or (2) they remain on the queue when it is deleted.</p>
<br/>


<h2>Accepting, Acquiring, Rejecting, or Releasing Messages</h2>
<ul>
<li><p>messageAccept()  &mdash; acknowledges messages</p>
<pre>SequenceSet tobeAccepted; 
toAccepted.add(msg.getId()); 
session.messageAccept(toBeAccepted);</pre>
</li>
<li><p>messageAcquire()</p>
<pre>SequenceSet tobeAcquired;
toBeAcquired.add(msg.getId()); 
session.messageAcquire(toBeAcquired);</pre>
</li>
<li><p>messageReject()</p>
<pre>SequenceSet tobeRejected; 
toRejected.add(msg.getId()); 
session.messageReject(toBeRejected);</pre>
</li>
<li><p>messageRelease()</p>
<pre>SequenceSet tobeReleased; 
toReleased.add(msg.getId()); 
session.messageRelease(toBeReleased);</pre></li>
</ul>

<h2>Transactions</h2>
<ul>
<li><p>txSelect()</p>
<pre>session.txSelect();</pre>
</li>
<li><p>txCommit()</p>
<pre>session.txSelect();</pre></li>
<li><p>txRollback()</p>
<pre>session.txRollback();</pre></li>
</ul>


EOS
        }
        # Session class.
        cpp_class(@classname,"public #{@base}") {
          public
          decl_ctor_opeq()
          private
          keyword_methods.each { |m| typedef m.argpack_type, m.argpack_name }
          genl "friend class Connection;"
          public
          keyword_methods.each { |m| gen_keyword_decl(m) }
        }
        genl "/** Conversion to #{@classname} from another session type */"
        genl "inline #{@classname} #{sync_convert}(const #{@version_base}& other) { return #{@clasname}(other); }"
        defn_ctor_opeq("inline")
      }}
  end
end

SessionNoKeywordGen.new($outdir, $amqp, true).generate()
SessionNoKeywordGen.new($outdir, $amqp, false).generate()
SessionGen.new($outdir, $amqp, true).generate()
SessionGen.new($outdir, $amqp, false).generate()

