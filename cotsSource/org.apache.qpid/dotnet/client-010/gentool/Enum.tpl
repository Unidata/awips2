using System;
namespace org.apache.qpid.transport
{
${
from genutil import *

vtype = jtype(resolve_type(type))

out("   public enum $name : $vtype")

choices = [(scream(ch["@name"]), "= %s" % (ch["@value"]))
           for ch in type.query["enum/choice"]]
}
   {
    $(",\n    ".join(["%s%s" % ch for ch in choices]))
    }

${

out("   public struct $name")
out("Getter")
}
   {
    public static $name Get($vtype value)
    {
        switch (value)
        {
${
choices = [(scream(ch["@name"]), "%s" % (ch["@value"]))
           for ch in type.query["enum/choice"]]

for ch, value in choices:
  out('          case $value: return $name.$ch;\n')
}        default: throw new Exception("no such value: " + value);
        }
    }
 }
}
