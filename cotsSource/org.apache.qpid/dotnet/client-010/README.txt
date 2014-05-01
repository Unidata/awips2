Info
====

AMQP 0.10 Native .NET client supporting WCF and xcel

In order to build this client from the sources you'll need the following folders :
- <project home>/java/lib
- <project home>/python
- <project home>/specs


Setup
=====

Install:
  Microsoft Visual Studio 2008 (VS2008). It's also possible to build with vs2005 by creating a new solution and adding Client.csproj
  NAnt 0.85 - only required for builds outside VS2008 (.net 1.1, .net 2.0, mono 2.0)
  Ant 1.6.5 (requires Java)
  Cygwin (or alternatively build via cmd but alter instructions below accordingly)

Set up PATH to include Nant.exe:

  $ PATH=/cygdrive/c/WINDOWS/Microsoft.NET/Framework/v2.0.50727:$PATH

Set up PATH to include ant:

  $ PATH=$ANT_HOME/bin:$PATH


Building
========

Generate code from <project home>/dotnet/client-010/gentool:

  $ cd <project home>/dotnet/client-010/gentool
  $ ant

You can build from Visual Studio 2008 normally. Alternatively, you
can build debug releases for any supported framework from the 
command line using Nant:

To build .NET 2.0 executables (to bin/net-2.0):

  $ cd <project home>/dotnet/client-010/
  $ nant


To build for Mono on Linux (to bin/mono-2.0):

  $ cd <project home>/dotnet/client-010/
  $ nant -t:mono-2.0

Releasing
=========

For .NET 2.0

  $ cd <project home>/dotnet/client-010/
  $ nant release-pkg

Generates ./bin/net-2.0/release/Qpid.NET-net-2.0-yyyyMMdd.zip

For Mono

  $ cd <project home>/dotnet/client-010/
  $ nant -t:mono-2.0 release-pkg

Generates ./bin/mono-2.0/release/Qpid.NET-mono-2.0-yyyyMMdd.zip

