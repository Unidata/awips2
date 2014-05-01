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

"""
This module loads protocol metadata into python objects. It provides
access to spec metadata via a python object model, and can also
dynamically creating python methods, classes, and modules based on the
spec metadata. All the generated methods have proper signatures and
doc strings based on the spec metadata so the python help system can
be used to browse the spec documentation. The generated methods all
dispatch to the self.invoke(meth, args) callback of the containing
class so that the generated code can be reused in a variety of
situations.
"""

import os, mllib, spec08

def default():
  try:
    amqp_spec = os.environ["AMQP_SPEC"]
    return amqp_spec
  except KeyError:
    try:
      from qpid_config import amqp_spec
      return amqp_spec
    except ImportError:
      raise Exception("unable to locate the amqp specification, please set "
                      "the AMQP_SPEC environment variable or supply "
                      "qpid_config.py on the PYTHONPATH")

def load(specfile, *errata):
  for name in (specfile,) + errata:
    if not os.path.exists(name):
      raise IOError("No such file or directory: '%s'" % name)

  doc = mllib.xml_parse(specfile)
  major = doc["amqp/@major"]
  minor = doc["amqp/@minor"]

  if major == "0" and minor == "10":
    return None
  else:
    return spec08.load(specfile, *errata)

SPEC = load(default())
