================================================================================
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
================================================================================

AMQP MULTI_VERSION CODE GENERATOR

This directory contains the first part of the new multi-AMQP-version code
generator. The Java generation is almost complete, C++ will follow.

NOTE: The generator has NOT been integrated into the current build, and is
included here to run stand-alone for the purposes of review and comment. As
currently configured, this generator will not interact with any file or
directory outside of this directory.

To build (from this directory):
rm org/apache/qpid/gentools/*.class
javac  org/apache/qpid/gentools/Main.java

Make sure you are using Sun's JDK1.5.0; Eclipse and gcj do not work.

To run (from this directory):
java org/apache/qpid/gentools/Main -j [xml_spec_file, ...]

XML test files are located in the xml-src directory. Pay attention to the
Basic class and Basic.Consume method - these were the primary test vehicles
for this generator. *** NOTE *** These files do not represent any current or
future version of the AMQP specification - do not use in production!

Folders:
--------
org/apache/qpid/gentools/: Source.
xml-src/: Test AMQP specification files.
templ.java/: Templates for java code generation.
out.java/: Output folder for generated Java files (will be created with use
           of -j flag on command-line).
templ.cpp/: (Future:) Templates for C++ code generation.
out.cpp/: Output folder for generated C++ files (will be created with use
           of -c flag on command-line).

For a more detaild description of the generator, see the Qpid Wiki
(http://cwiki.apache.org/qpid/multiple-amqp-version-support.html).

Please send comments and bugs to me (kim.vdriet [at] redhat.com) or via the
Apache Qpid list (qpid-dev [at] incubator.apache.org).

Kim van der Riet
