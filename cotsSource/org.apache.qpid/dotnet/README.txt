Info
====

There are two separate .NET clients: one that implements AMQP 0-8 (and
can communicate with the Java broker) and another that implements
0-10 (and can communicate with the C++ broker).

This README contains instructions for building the 0-8 client.

Instructions for building and installing the 0-10 client are located in client-010/README.txt.

Setup
=====

Essential:

  .NET 2.0 or later
  Ant 1.6.5 (Java build tool, http://ant.apache.org)
  
Either:
  NAnt 0.85 - only required for builds outside Visual Studio
OR
  Microsoft Visual Studio 2008 (VS2008)
  
Ensure that your PATH includes ant, e.g.:

  $ PATH=c:\java\ant\bin:%PATH%
  
If using nant, set up PATH to include Nant.exe, e.g.:

  $ set PATH=C:\dotnet\nant\bin;%PATH%

If using msbuild, it is recommended to use a "Visual Studio Command Prompt"

Building
========

Generate framing from /Qpid.Common/amqp.xml specification file by running this script:

  $ build-framing.bat

Alternatively, just switch to /Qpid.Common and run "ant" there.

You can build from Visual Studio 2008 or from the command-line by running msbuild.

The script build-msbuild.bat provides some standard options to do a full build.

If you are using nant, the script build-nant.bat contains standard arguments that do a full build.
 
To build for Mono on Linux (to bin/mono-2.0) the build-mono shell script is provided.

Releasing
=========

nant can be used to create a release zip archive. A script is provided:

For .NET 2.0

  $ release net-2.0

Generates ./bin/net-2.0/release/Qpid.NET-net-2.0-yyyyMMdd.zip

For Mono

  $ release mono-2.0

Generates ./bin/mono-2.0/release/Qpid.NET-mono-2.0-yyyyMMdd.zip

