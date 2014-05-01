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

class StructGen < CppGen

  def initialize(outdir, amqp)
    super(outdir, amqp)
  end

  SizeMap={
    "Octet"=>1,
    "Short"=>2,
    "Long"=>4,
    "LongLong"=>8,
    "int8"=>1,
    "int16"=>2,
    "int32"=>4,
    "int64"=>8,
    "uint8"=>1,
    "uint16"=>2,
    "uint32"=>4,
    "uint64"=>8,
    "timestamp"=>8
  }

  StringSizeMap={
    "LongString"=>4,
    "MediumString"=>2,
    "ShortString"=>1
  }
  
  SizeType={
    1=>"Octet",
    2=>"Short",
    4=>"Long",
    8=>"LongLong"
  }

  ValueTypes=["uint8_t", "uint16_t", "uint32_t", "uint64_t"]

  def is_packed(s) s.pack and s.pack != "0" end

  def execution_header?(s)
    s.is_a? AmqpMethod and not s.parent.control?
    # s.kind_of? AmqpMethod and s.parent.name.include?("010") and not s.parent.control?
  end

  def has_bitfields_only(s)
    s.fields.select {|f| f.type_ != "bit"}.empty?
  end

  def default_initialisation(s)
    params = s.fields.select {|f| ValueTypes.include?(f.cpptype.name) || (!is_packed(s) && f.type_ == "bit")}
    strings = params.collect {|f| "#{f.cppname}(#{f.default_value})"}   
    strings << "flags(0)" if (is_packed(s))
    if strings.empty?
      return ""
    else
      return " : " + strings.join(", ")
    end
  end

  def printable_form(f)
    if (f.cpptype.name == "uint8_t")
      return "(int) " + f.cppname
    elsif (f.type_ == "bit")
      return "get#{f.name.caps}()"
    else
      return f.cppname
    end
  end

  def flag_mask(s, i)
    pos = s.pack.to_i*8 - 8 - (i/8)*8 + (i % 8)
    return "(1 << #{pos})"
  end

  def encode_packed_struct(s)
    genl s.cpp_pack_type.encode('flags', 'buffer')
    process_packed_fields(s) { |f, i| encode_packed_field(s, f, i) unless f.type_ == "bit" }
  end

  def decode_packed_struct(s)
    genl "#{s.cpp_pack_type.decode('flags', 'buffer')}"
    process_packed_fields(s) { |f, i| decode_packed_field(s, f, i) unless f.type_ == "bit" }
  end

  def size_packed_struct(s)
    genl "total += #{s.pack};"
    process_packed_fields(s) { |f, i| size_packed_field(s, f, i) unless f.type_ == "bit" }
  end

  def print_packed_struct(s)
    process_packed_fields(s) { |f, i| print_packed_field(s, f, i) }
  end

  def encode_packed_field(s, f, i)
    genl "if (flags & #{flag_mask(s, i)})"
    indent { genl f.cpptype.encode(f.cppname,"buffer") }
  end

  def decode_packed_field(s, f, i)
    genl "if (flags & #{flag_mask(s, i)})"
    indent { genl f.cpptype.decode(f.cppname,"buffer") }
  end

  def size_packed_field(s, f, i)
      genl "if (flags & #{flag_mask(s, i)})"
      indent { generate_size(f, []) }
  end

  def print_packed_field(s, f, i)
    classname = s.cppname
    if (s.kind_of? AmqpMethod)
      classname = s.body_name
    end
    genl "if (flags & #{flag_mask(s, i)})"
    indent { 
      unless (classname == "ConnectionStartOkBody" && f.name == "response")
        genl "out << \"#{f.name}=\" << #{printable_form(f)} << \"; \";"
      else
        genl "out << \"response=\" << \"xxxxxx\" << \"; \";"
      end
    }
  end

  def generate_encode(f, combined)
    if (f.type_ == "bit")
      genl "uint8_t #{f.cppname}_bits = #{f.cppname};"
      count = 0
      combined.each { |c| genl "#{f.cppname}_bits |= #{c.cppname} << #{count += 1};" }
      genl "buffer.putOctet(#{f.cppname}_bits);"
    else
      genl f.cpptype.encode(f.cppname,"buffer")
    end
  end

  def generate_decode(f, combined)
    if (f.type_ == "bit")
      genl "uint8_t #{f.cppname}_bits = buffer.getOctet();"
      genl "#{f.cppname} = 1 & #{f.cppname}_bits;"
      count = 0
      combined.each { |c| genl "#{c.cppname} = (1 << #{count += 1}) & #{f.cppname}_bits;" }
    else
      genl f.cpptype.decode(f.cppname,"buffer")
    end
  end

  def generate_size(f, combined)
    if (f.type_ == "bit")
      names = ([f] + combined).collect {|g| g.cppname}
      genl "total += 1;//#{names.join(", ")}"
    else
      size = SizeMap[f.cpptype.encoded]
      if (size)
        genl "total += #{size};//#{f.cppname}"
      elsif (f.cpptype.name == "SequenceNumberSet")
        genl "total += #{f.cppname}.encodedSize();"
      elsif (size = StringSizeMap[f.cpptype.encoded])
        genl "total += #{size} + #{f.cppname}.size();"
      else
        genl "total += #{f.cppname}.encodedSize();"
      end
    end
  end

  def process_packed_fields(s)
    s.fields.each { |f| yield f, s.fields.index(f) }
  end

  def process_fields(s)
    last = nil
    count = 0  
    bits = []
    s.fields.each { 
      |f| if (last and last.bit? and f.bit? and count < 7) 
            count += 1
            bits << f
          else
            if (last and last.bit?)
              yield last, bits
              count = 0
              bits = []
            end
            if (not f.bit?)
              yield f
            end
            last = f
          end
    }
    if (last and last.bit?)
      yield last, bits
    end
  end

  def all_fields_via_accessors(s)
    s.fields.collect { |f| "get#{f.name.caps}()" }.join(", ")
  end

  def methodbody_extra_defs(s)
    if (s.parent.control?) 
      genl "virtual uint8_t type() const { return 0;/*control segment*/ }"
    end
    

    gen <<EOS
    typedef #{s.result ? s.result.cpptype.name : 'void'} ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.#{s.cppname}(#{all_fields_via_accessors(s)});
    }

    using  AMQMethodBody::accept;
    void accept(MethodBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }

    ClassId amqpClassId() const { return CLASS_ID; }
    MethodId amqpMethodId() const { return METHOD_ID; }
    bool isContentBearing() const { return  #{s.content ? "true" : "false" }; }
    bool resultExpected() const { return  #{s.result ? "true" : "false"}; }
    bool responseExpected() const { return  #{s.responses().empty? ? "false" : "true"}; }
EOS
  end

  def define_constructor(name, s)
    if (s.fields.size > 0)
      genl "#{name}("
      if (s.kind_of? AmqpMethod)
        indent {gen "ProtocolVersion, "}
      end
      indent { gen s.fields.collect { |f| "#{f.cpptype.param} _#{f.cppname}" }.join(",\n") }
      genl ") : "
      if (is_packed(s))
        initialisers = s.fields.select { |f| f.type_ != "bit"}.collect { |f| "#{f.cppname}(_#{f.cppname})"}

        initialisers << "flags(0)"
        indent { gen initialisers.join(",\n") }
        genl "{"
        indent {
          process_packed_fields(s) { |f, i| genl "set#{f.name.caps}(_#{f.cppname});" if f.type_ == "bit"}
          process_packed_fields(s) { |f, i| genl "flags |= #{flag_mask(s, i)};" unless f.type_ == "bit"}
        }
        genl "}"          
      else
        indent { gen s.fields.collect { |f| " #{f.cppname}(_#{f.cppname})" }.join(",\n") }
        genl "{}"
      end
    end
    #default constructors:
    if (s.kind_of? AmqpMethod)
      genl "#{name}(ProtocolVersion=ProtocolVersion()) #{default_initialisation(s)} {}"
    end
    if (s.kind_of? AmqpStruct)
      genl "#{name}() #{default_initialisation(s)} {}"
    end
  end

  def define_packed_field_accessors(s, f, i)
    if (s.kind_of? AmqpMethod) 
      define_packed_field_accessors_for_method(s, f, i)
    else
      define_packed_field_accessors_for_struct(s, f, i)
    end
  end

  def define_packed_field_accessors_for_struct(s, f, i)
    if (f.type_ == "bit")
      genl "void #{s.cppname}::set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname}) {"
      indent {
        genl "if (_#{f.cppname}) flags |= #{flag_mask(s, i)};"
        genl "else flags &= ~#{flag_mask(s, i)};"
      }
      genl "}"
      genl "#{f.cpptype.ret} #{s.cppname}::get#{f.name.caps}() const { return flags & #{flag_mask(s, i)}; }"
    else 
      genl "void #{s.cppname}::set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname}) {"
      indent {
        genl "#{f.cppname} = _#{f.cppname};"
        genl "flags |= #{flag_mask(s, i)};"
      }
      genl "}"
      genl "#{f.cpptype.ret} #{s.cppname}::get#{f.name.caps}() const { return #{f.cppname}; }"
      if (f.cpptype.name == "FieldTable")
        genl "#{f.cpptype.name}& #{s.cppname}::get#{f.name.caps}() {"
        indent { 
          genl "flags |= #{flag_mask(s, i)};"#treat the field table as having been 'set'
          genl "return #{f.cppname};" 
        }
        genl "}"
      end
      genl "bool #{s.cppname}::has#{f.name.caps}() const { return flags & #{flag_mask(s, i)}; }"
      genl "void #{s.cppname}::clear#{f.name.caps}Flag() { flags &= ~#{flag_mask(s, i)}; }"
    end
    genl ""
  end

  def define_packed_field_accessors_for_method(s, f, i)
    if (f.type_ == "bit")
      genl "void #{s.body_name}::set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname}) {"
      indent {
        genl "if (_#{f.cppname}) flags |= #{flag_mask(s, i)};"
        genl "else flags &= ~#{flag_mask(s, i)};"
      }
      genl "}"
      genl "#{f.cpptype.ret} #{s.body_name}::get#{f.name.caps}() const { return flags & #{flag_mask(s, i)}; }"
    else 
      genl "void #{s.body_name}::set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname}) {"
      indent {
        genl "#{f.cppname} = _#{f.cppname};"
        genl "flags |= #{flag_mask(s, i)};"
      }
      genl "}"
      genl "#{f.cpptype.ret} #{s.body_name}::get#{f.name.caps}() const { return #{f.cppname}; }"
      if (f.cpptype.name == "FieldTable")
        genl "#{f.cpptype.name}& #{s.body_name}::get#{f.name.caps}() {"
        indent { 
          genl "flags |= #{flag_mask(s, i)};"#treat the field table as having been 'set'
          genl "return #{f.cppname};" 
        }
        genl "}"
      end
      genl "bool #{s.body_name}::has#{f.name.caps}() const { return flags & #{flag_mask(s, i)}; }"
      genl "void #{s.body_name}::clear#{f.name.caps}Flag() { flags &= ~#{flag_mask(s, i)}; }"
    end
    genl ""
  end

  def define_packed_accessors(s)
    process_packed_fields(s) { |f, i| define_packed_field_accessors(s, f, i) }
  end

  def declare_packed_accessors(f)
    genl "QPID_COMMON_EXTERN void set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname});";
    genl "QPID_COMMON_EXTERN #{f.cpptype.ret} get#{f.name.caps}() const;"
    if (f.cpptype.name == "FieldTable")
      genl "QPID_COMMON_EXTERN #{f.cpptype.name}& get#{f.name.caps}();"
    end
    if (f.type_ != "bit")
      #extra 'accessors' for packed fields:
      genl "QPID_COMMON_EXTERN bool has#{f.name.caps}() const;"
      genl "QPID_COMMON_EXTERN void clear#{f.name.caps}Flag();"
    end
  end

  def define_accessors(f)
    genl "void set#{f.name.caps}(#{f.cpptype.param} _#{f.cppname}) { #{f.cppname} = _#{f.cppname}; }"
    genl "#{f.cpptype.ret} get#{f.name.caps}() const { return #{f.cppname}; }"
    if (f.cpptype.name == "FieldTable")
      genl "#{f.cpptype.name}& get#{f.name.caps}() { return #{f.cppname}; }"
    end
  end

  def define_struct(s)
    classname = s.cppname
    inheritance = ""
    if (s.kind_of? AmqpMethod)
      classname = s.body_name
      if (execution_header?(s))
        inheritance = ": public ModelMethod"
      else
        inheritance = ": public AMQMethodBody"
      end
    else
      public_api("qpid/framing/#{classname}.h") # Non-method structs are public
    end

    h_file("qpid/framing/#{classname}.h") {       
      if (s.kind_of? AmqpMethod)
        gen <<EOS
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/AMQP_ServerOperations.h"
#include "qpid/framing/MethodBodyConstVisitor.h"
EOS
      end
      include "qpid/framing/ModelMethod.h" if (execution_header?(s))

      s.fields.each { |f| include "qpid/framing/#{f.cpptype.name}" if f.struct?}

      gen <<EOS

#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class #{classname} #{inheritance} {
EOS
  if (is_packed(s))
    indent { s.fields.each { |f| genl "#{f.cpptype.name} #{f.cppname};" unless f.type_ == "bit"} }
    indent {
      genl "#{s.cpp_pack_type.name} flags;"
    }
  else
    indent { s.fields.each { |f| genl "#{f.cpptype.name} #{f.cppname};" } }
  end
  genl "public:"
  if (s.kind_of? AmqpMethod)
    indent { genl "static const ClassId CLASS_ID = #{s.parent.code};" }
    indent { genl "static const MethodId METHOD_ID = #{s.code};" }
  end

  if (s.kind_of? AmqpStruct)
    if (s.code)
      indent { genl "static const uint16_t TYPE = #{s.full_code};" }
    end
  end

  indent { 
    define_constructor(classname, s)
    genl ""
    if (is_packed(s))
      s.fields.each { |f| declare_packed_accessors(f) } 
    else
      s.fields.each { |f| define_accessors(f) } 
    end
  }
  if (s.kind_of? AmqpMethod)
    methodbody_extra_defs(s)
  end
  if (s.kind_of? AmqpStruct)
    indent {genl "QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const #{classname}&);" }
  end

  gen <<EOS
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class #{classname} */

}}
EOS
    }
    cpp_file("qpid/framing/#{classname}.cpp") { 
      if (is_packed(s) || s.fields.size > 0 || execution_header?(s))
        buffer = "buffer"
      else
        buffer = "/*buffer*/"
      end
      gen <<EOS
#include "qpid/framing/#{classname}.h"
#include "qpid/framing/reply_exceptions.h"

using namespace qpid::framing;

EOS
    
      if (is_packed(s))
        define_packed_accessors(s)
      end
      gen <<EOS
void #{classname}::encodeStructBody(Buffer& #{buffer}) const
{
EOS
      if (execution_header?(s))
        genl "encodeHeader(buffer);"
      end

      if (is_packed(s))
        indent {encode_packed_struct(s)}
      else 
        indent { process_fields(s) { |f, combined| generate_encode(f, combined) } } 
      end
      gen <<EOS
}

void #{classname}::encode(Buffer& buffer) const
{
EOS
      indent {
        if (s.kind_of? AmqpStruct)
          if (s.code)
            genl "buffer.put#{SizeType[s.size.to_i]}(bodySize() + 2/*typecode*/);" if s.size and s.size.to_i != 0
            genl "buffer.putShort(TYPE);" 
          else
            genl "buffer.put#{SizeType[s.size.to_i]}(bodySize());" if s.size and s.size.to_i != 0
          end
        end
        genl "encodeStructBody(buffer);"
      }
      gen <<EOS
}

void #{classname}::decodeStructBody(Buffer& #{buffer}, uint32_t /*size*/)
{
EOS
      if (execution_header?(s))
        genl "decodeHeader(buffer);"
      end

      if (is_packed(s))
        indent {decode_packed_struct(s)}
      else 
        indent { process_fields(s) { |f, combined| generate_decode(f, combined) } } 
      end
      gen <<EOS
}

void #{classname}::decode(Buffer& buffer, uint32_t /*size*/)
{
EOS
      indent {
        if (s.kind_of? AmqpStruct)
          genl "buffer.get#{SizeType[s.size.to_i]}();" if s.size and s.size.to_i != 0
          genl "if (TYPE != buffer.getShort()) throw FramingErrorException(\"Bad type code for struct\");" if s.code
        end
        genl "decodeStructBody(buffer);"
      }
      gen <<EOS
}

uint32_t #{classname}::bodySize() const
{
    uint32_t total = 0;
EOS
      if (execution_header?(s))
        genl "total += headerSize();"
      end

      if (is_packed(s))
        indent {size_packed_struct(s)}
      else 
        indent { process_fields(s) { |f, combined| generate_size(f, combined) } } 
      end
      gen <<EOS
    return total;
}

uint32_t #{classname}::encodedSize() const {
    uint32_t total = bodySize();
EOS
        if (s.kind_of? AmqpStruct)
          genl "total += #{s.size}/*size field*/;" if s.size
          genl "total += 2/*typecode*/;" if s.code
        end
      gen <<EOS
    return total;
}

void #{classname}::print(std::ostream& out) const
{
    out << "{#{classname}: ";
EOS
      if (is_packed(s))
        indent {print_packed_struct(s)}
      else 
        copy = Array.new(s.fields)
        f = copy.shift
        
        indent { 
          genl "out << \"#{f.name}=\" << #{printable_form(f)};" if f
          copy.each { |f| genl "out << \"; #{f.name}=\" << #{printable_form(f)};" } 
        } 
      end
      gen <<EOS
    out << "}";
}
EOS

      if (s.kind_of? AmqpStruct)
        gen <<EOS
namespace qpid{
namespace framing{

    std::ostream& operator<<(std::ostream& out, const #{classname}& s) 
    {
      s.print(out);
      return out;
    }

}
}
EOS
      end 
}
  end

  def generate()
    structs = @amqp.collect_all(AmqpStruct).select { |s| not ["command-fragment"].include?(s.name) }
    structs.each { |s| define_struct(s) }
    @amqp.methods_.each { |m| define_struct(m) }
    #generate a single include file containing the list of structs for convenience
    public_api("qpid/framing/amqp_structs.h")
    h_file("qpid/framing/amqp_structs.h") { structs.each { |s| genl "#include \"qpid/framing/#{s.cppname}.h\"" } }
  end
end

StructGen.new($outdir, $amqp).generate()

